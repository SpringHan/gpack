;;; gpack.el --- Emacs package manager, lighter with more features -*- lexical-binding: t -*-

;; Author: SpringHan
;; Maintainer: SpringHan
;; Version: 1.0
;; Gpacks: ((emacs "26.3"))
;; Homepage: https://github.com/SpringHan/gpack
;; Keywords: package-manager


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:
;; `Gpack' can help you to manage your packages easily.

;;; Code:

(defgroup gpack nil
  "The group for `gpack'."
  :group 'applications)

(defcustom gpack-repo-directory "~/.emacs.d/third-party/"
  "The directory for the repo."
  :type 'string
  :group 'gpack)

(defcustom gpack-clone-process nil
  "The process of gpack-clone."
  :type 'process
  :group 'gpack)

(defcustom gpack-temp-load-path nil
  "The temp `load-path'."
  :type 'symbol
  :group 'gpack)

(defcustom gpack-temp-name ""
  "The temp name."
  :type 'string
  :group 'gpack)

(defcustom gpack-temp-autoload-p nil
  "If autoload."
  :type 'symbol
  :group 'gpack)

(defcustom gpack-temp-package-name nil
  "The package's name."
  :type 'symbol
  :group 'gpack)

(defconst gpack-clone-output-buffer "*Gpack-Clone-Output*"
  "The buffer name of clone output.")

(defun gpack-download (name)
  "Download the package if it has not downloaded.
Argument NAME the package's name."
  (unless (package-installed-p name)
    (package-refresh-contents)
    (package-install name)))

(defun gpack-load-path (path)
  "Define the load PATH."
  (if (stringp path)
      (add-to-list 'load-path path)
    (add-to-list (cdr-safe path) (car path))))

(defun gpack-set-variable (vars)
  "Set variables.
Argument VARS is the variables' list."
  (when (listp vars)
    (if (symbolp (car vars))
        (set (car vars) (eval (cdr-safe vars)))
      (dolist (var-list vars)
        (set (car var-list) (eval (cdr-safe var-list)))))))

(defun gpack-add-hook (hlist)
  "Add hooks.
Argument HLIST is the hook list."
  (when (listp hlist)
    (if (symbolp (car hlist))
        (gpack-hook--by-list (car hlist) (cdr-safe hlist))
      (if (symbolp (cdr-safe hlist))
          (gpack-hook--list-add (car hlist) (cdr-safe hlist))
        (dolist (hook hlist)
          (if (and (listp (car hook)) (symbolp (cdr-safe hook)))
              (gpack-hook--list-add (car hook) (cdr-safe hook))
            (gpack-hook--by-list (car hook) (cdr-safe hook))))))))

(defun gpack-hook--list-add (hooks function)
  "Apply FUNCTION to each HOOKS."
  (dolist (hook hooks)
    (add-hook hook function)))

(defun gpack-hook--by-list (hook function)
  "Add HOOK with its FUNCTION."
  (if (symbolp function)
      (add-hook hook function)
    (if (eq (car function) 'lambda)
        (add-hook hook function)
      (dolist (func function)
        (add-hook hook func)))))

(defun gpack-define-key (klist)
  "Define keybindings.
Argument KLIST is the key list."
  (when (listp klist)
    (if (not (listp (car klist)))
        (gpack-key--item-define klist)
      (dolist (key-list klist)
        (gpack-key--item-define key-list)))))

(defun gpack-key--item-define (item)
  "Define key by a ITEM."
  (pcase (type-of (car item))
    ('string (global-set-key (kbd (car item)) (cdr-safe item)))
    ('symbol (gpack-key--by-map (car item) (cdr-safe item)))))

(defun gpack-key--by-map (map key-info)
  "Define key with MAP.
Argument KEY-INFO is the key list for MAP."
  (if (stringp (car key-info))
      (define-key (symbol-value map) (kbd (car key-info)) (cdr-safe key-info))
    (if (eq (car key-info) 'lambda)
        (define-key (symbol-value map) (kbd (car key-info)) (cdr-safe key-info))
      (dolist (key key-info)
        (define-key (symbol-value map) (kbd (car key)) (cdr-safe key))))))

(defun gpack-repo (repo)
  "Get the package from remote git repository.
Argument REPO is the repository's info."
  (unless (file-exists-p gpack-repo-directory)
    (make-directory gpack-repo-directory))
  (let (dir dir-name)
    (if (stringp repo)
        (progn
          (setq dir (gpack-repo--get-repo-name
                     (gpack-repo--get-repo-url repo)))
          (setq dir-name (concat gpack-repo-directory dir))
          (if (file-exists-p dir-name)
              (gpack-repo--require dir-name
                                             gpack-temp-package-name
                                             'load-path)
            (gpack-repo--clone (gpack-repo--get-repo-url repo)
                                         dir 1)))
      ;; If the repo is a list
      (let ((args (cdr repo))
            (depth 1)
            name load)
        (while (keywordp (car-safe args))
          (pcase (pop args)
            (:save (setq name (pop args)))
            (:load (setq load (pop args)))
            (:depth (setq depth (pop args)))))
        (setq dir (if name
                      name
                    (gpack-repo--get-repo-name
                     (gpack-repo--get-repo-url
                      (car repo)))))
        (setq dir-name (concat gpack-repo-directory dir))
        (if (file-exists-p dir-name)
            (gpack-repo--require dir-name
                                           gpack-temp-package-name
                                           (if load
                                               load
                                             'load-path))
          (gpack-repo--clone
           (gpack-repo--get-repo-url
            (car repo))
           (if name
               name
             dir)
           depth
           (when load
             load)))))))

(defun gpack-repo--require (dir name path)
  "Require the package.
Argument DIR is the directory for package.
Argument NAME is the name of package.
Argument PATH is the `load-path' of package."
  (add-to-list path dir)
  (when gpack-temp-package-name
    (if gpack-temp-autoload-p
        (autoload (symbol-name name) name)
      (require name))
    (setq gpack-temp-autoload-p nil
          gpack-temp-package-name nil)))

(defun gpack-repo--clone (url name depth &optional load)
  "Clone the repository from URL.
Argument NAME is the repository's name at locale.
Argument DEPTH is the depth for cloning.
Optional argument LOAD is the `load-path' for package."
  (split-window nil nil 'above)
  (switch-to-buffer gpack-clone-output-buffer)
  (setq gpack-temp-load-path (if load
                                           load
                                         'load-path)
        gpack-temp-name name)
  (if depth
      (setq gpack-clone-process
            (start-process "Gpack-Clone"
                           gpack-clone-output-buffer
                           "git"
                           "clone"
                           url
                           (expand-file-name name gpack-repo-directory)
                           (format "--depth=%d" depth)))
    (setq gpack-clone-process
          (start-process "Gpack-Clone"
                         gpack-clone-output-buffer
                         "git"
                         "clone"
                         url
                         (expand-file-name name gpack-repo-directory))))
  (set-process-sentinel gpack-clone-process
                        #'gpack-clone--sentinel)
  (while (process-live-p gpack-clone-process)
    (read-char)))

(defun gpack-clone--sentinel (process event)
  "Sentinel for clone process."
  (when (memq (process-status process) '(exit signal))
    (setq event (substring event 0 -1))
    (when (string-match "^finished" event)
      (message "[Gpack]: Clone finished.")
      (kill-buffer-and-window)
      (add-to-list gpack-temp-load-path
                   (concat gpack-repo-directory
                           gpack-temp-name))
      (when gpack-temp-package-name
        (if gpack-temp-autoload-p
            (autoload
              gpack-temp-package-name
              (symbol-name gpack-temp-package-name))
          (require gpack-temp-package-name)))
      (setq gpack-clone-process nil
            gpack-temp-load-path nil
            gpack-temp-name ""
            gpack-temp-autoload-p nil
            gpack-temp-package-name nil))))

(defun gpack-repo--get-repo-name (string)
  "Return the repo's name in STRING."
  (if (not (string-match-p "^https://\\(?:.*\\)" string))
      string
    (let ((result
           (progn (string-match "^https://\\(?:.*\\..*\\)/\\(?:.*/\\)\\(.*\\)\\(?:\\.git\\)?"
                                string)
                  (match-string 1 string))))
      (when (string-match-p "^\\(?:.*\\)\\.git" result)
        (setq result (progn
                       (string-match "^\\(.*\\)\\.git" result)
                       (match-string 1 result))))
      result)))

(defun gpack-repo--get-repo-url (string)
  "Return the repo's url.
Argument STRING is the string which includes url."
  (if (not (string-match-p "^https://\\(?:.*\\)" string))
      (format "https://github.com/%s" string)
    string))

(defmacro gpack-read-args (name args)
  "Read ARGS and do the actions.
Argument NAME is the package's name."
  (declare (debug t))
  (let ((requirep t)
        (unrequire nil)
        hook var key repo load-path before config autoload outside)
    (while (keywordp (car-safe args))
      (pcase (pop args)
        (:hook (setq hook (pop args)))
        (:var (setq var (pop args)))
        (:key (setq key (pop args)))
        (:repo (setq repo (pop args)
                     requirep nil))
        (:load-path (setq load-path (pop args)
                          outside t))
        (:before (setq before (pop args)))
        (:config (setq config (pop args)))
        (:autoload (setq autoload t
                         requirep nil))
        (:un-require (setq requirep nil
                           unrequire t))))
    `(progn
       ,(when before
          `(eval ',before))
       ,(when load-path
          `(gpack-load-path ',load-path))
       ,(when repo
          `(progn (setq gpack-temp-package-name (if (or ,requirep
                                                                  (null ,unrequire))
                                                              ',name
                                                            nil)
                        gpack-temp-autoload-p ,autoload)
                  (gpack-repo ',repo)))
       (when ,requirep
         (when (and (not (require ',name nil t))
                    (null outside))
           (gpack-download ',name)
           (require ',name)))
       (when (and ,autoload (null ',repo))
         (autoload ',name (symbol-name ',name)))
       ,(when config
          (if requirep
              `(eval ',config)
            `(eval-after-load ',name ',config)))
       ,(when var
          `(gpack-set-variable ',var)
          )
       ,(when hook
          `(gpack-add-hook ',hook))
       ,(when key
          `(gpack-define-key ',key)))))

(defmacro gpack (name &rest args)
  "Main macro for `gpack'.

NAME is the package's name.

Keywords:

:hook           Define the hook(s) for package.

:var            Define the variable(s) for package.

:key            Define the keybindings for package.

:repo           Define the git repository's path.

:load-path      Define the path for local package.

:before         Do the things before loading package.

:config         The configuration for package.

:disable        Disable the package.

:autoload       Autoload the package.

:un-require     Do not require the package."
  (declare (indent 1))
  (unless (memq :disable args)
    `(gpack-read-args ,name ,args)))

(provide 'gpack)

;;; gpack.el ends here
