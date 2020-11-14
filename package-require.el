;;; package-require.el --- Emacs package manager, lighter with more features -*- lexical-binding: t -*-

;; Author: SpringHan
;; Maintainer: SpringHan
;; Version: 1.0
;; Package-Requires: (emacs "26.3")
;; Homepage: https://github.com/SpringHan/package-require
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
;; `Package-Require' can help you to manage your packages easily.

;;; Code:

(defgroup package-require nil
  "The group for `package-require'."
  :group 'applications)

(defcustom package-require-repo-directory "~/.emacs.d/third-party/"
  "The directory for the repo."
  :type 'string
  :group 'package-require)

(defcustom package-require-clone-process nil
  "The process of package-require-clone."
  :type 'process
  :group 'package-require)

(defcustom package-require-temp-load-path nil
  "The temp `load-path'."
  :type 'symbol
  :group 'package-require)

(defcustom package-require-temp-name ""
  "The temp name."
  :type 'string
  :group 'package-require)

(defcustom package-require-temp-autoload-p nil
  "If autoload."
  :type 'symbol
  :group 'package-require)

(defcustom package-require-temp-package-name nil
  "The package's name."
  :type 'symbol
  :group 'package-require)

(defconst package-require-clone-output-buffer "*Package-Require-Clone-Output*"
  "The buffer name of clone output.")

(defun package-require-download (name)
  "Download the package if it has not downloaded.
Argument NAME the package's name."
  (unless (package-installed-p name)
    (package-refresh-contents)
    (package-install name)))

(defun package-require-load-path (path)
  "Define the load PATH."
  (if (stringp path)
      (add-to-list 'load-path path)
    (add-to-list (cdr-safe path) (car path))))

(defun package-require-set-variable (vars)
  "Set variables.
Argument VARS is the variables' list."
  (when (listp vars)
    (if (symbolp (car vars))
        (set (car vars) (eval (cdr-safe vars)))
      (dolist (var-list vars)
        (set (car var-list) (eval (cdr-safe var-list)))))))

(defun package-require-add-hook (hlist)
  "Add hooks.
Argument HLIST is the hook list."
  (when (listp hlist)
    (if (symbolp (car hlist))
        (package-require-hook--by-list (car hlist) (cdr-safe hlist))
      (if (symbolp (cdr-safe hlist))
          (package-require-hook--list-add (car hlist) (cdr-safe hlist))
        (dolist (hook hlist)
          (if (and (listp (car hook)) (symbolp (cdr-safe hook)))
              (package-require-hook--list-add (car hook) (cdr-safe hook))
            (package-require-hook--by-list (car hook) (cdr-safe hook))))))))

(defun package-require-hook--list-add (hooks function)
  "Apply FUNCTION to each HOOKS."
  (dolist (hook hooks)
    (add-hook hook function)))

(defun package-require-hook--by-list (hook function)
  "Add HOOK with its FUNCTION."
  (if (symbolp function)
      (add-hook hook function)
    (if (eq (car function) 'lambda)
        (add-hook hook function)
      (dolist (func function)
        (add-hook hook func)))))

(defun package-require-define-key (klist)
  "Define keybindings.
Argument KLIST is the key list."
  (when (listp klist)
    (if (not (listp (car klist)))
        (package-require-key--item-define klist)
      (dolist (key-list klist)
        (package-require-key--item-define key-list)))))

(defun package-require-key--item-define (item)
  "Define key by a ITEM."
  (pcase (type-of (car item))
    ('string (global-set-key (kbd (car item)) (cdr-safe item)))
    ('symbol (package-require-key--by-map (car item) (cdr-safe item)))))

(defun package-require-key--by-map (map key-info)
  "Define key with MAP.
Argument KEY-INFO is the key list for MAP."
  (if (stringp (car key-info))
      (define-key (symbol-value map) (kbd (car key-info)) (cdr-safe key-info))
    (if (eq (car key-info) 'lambda)
        (define-key (symbol-value map) (kbd (car key-info)) (cdr-safe key-info))
      (dolist (key key-info)
        (define-key (symbol-value map) (kbd (car key)) (cdr-safe key))))))

(defun package-require-repo (repo)
  "Get the package from remote git repository.
Argument REPO is the repository's info."
  (unless (file-exists-p package-require-repo-directory)
    (make-directory package-require-repo-directory))
  (let (dir dir-name)
    (if (stringp repo)
        (progn
          (setq dir (package-require-repo--get-repo-name
                     (package-require-repo--get-repo-url repo)))
          (setq dir-name (concat package-require-repo-directory dir))
          (if (file-exists-p dir-name)
              (package-require-repo--require dir-name
                                             package-require-temp-package-name
                                             'load-path)
            (package-require-repo--clone (package-require-repo--get-repo-url repo)
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
                    (package-require-repo--get-repo-name
                     (package-require-repo--get-repo-url
                      (car repo)))))
        (setq dir-name (concat package-require-repo-directory dir))
        (if (file-exists-p dir-name)
            (package-require-repo--require dir-name
                                           package-require-temp-package-name
                                           (if load
                                               load
                                             'load-path))
          (package-require-repo--clone
           (package-require-repo--get-repo-url
            (car repo))
           (if name
               name
             dir)
           depth
           (when load
             load)))))))

(defun package-require-repo--require (dir name path)
  "Require the package.
Argument DIR is the directory for package.
Argument NAME is the name of package.
Argument PATH is the `load-path' of package."
  (add-to-list path dir)
  (when package-require-temp-package-name
    (if package-require-temp-autoload-p
        (autoload (symbol-name name) name)
      (require name))
    (setq package-require-temp-autoload-p nil
          package-require-temp-package-name nil)))

(defun package-require-repo--clone (url name depth &optional load)
  "Clone the repository from URL.
Argument NAME is the repository's name at locale.
Argument DEPTH is the depth for cloning.
Optional argument LOAD is the `load-path' for package."
  (split-window nil nil 'above)
  (switch-to-buffer package-require-clone-output-buffer)
  (setq package-require-temp-load-path (if load
                                           load
                                         'load-path)
        package-require-temp-name name)
  (if depth
      (setq package-require-clone-process
            (start-process "Package-Require-Clone"
                           package-require-clone-output-buffer
                           "git"
                           "clone"
                           url
                           (expand-file-name name package-require-repo-directory)
                           (format "--depth=%d" depth)))
    (setq package-require-clone-process
          (start-process "Package-Require-Clone"
                         package-require-clone-output-buffer
                         "git"
                         "clone"
                         url
                         (expand-file-name name package-require-repo-directory))))
  (set-process-sentinel package-require-clone-process
                        #'package-require-clone--sentinel))

(defun package-require-clone--sentinel (process event)
  "Sentinel for clone process."
  (when (memq (process-status process) '(exit signal))
    (setq event (substring event 0 -1))
    (when (string-match "^finished" event)
      (message "[Package-Require]: Clone finished.")
      (kill-buffer-and-window)
      (add-to-list package-require-temp-load-path
                   (concat package-require-repo-directory
                           package-require-temp-name))
      (when package-require-temp-package-name
        (if package-require-temp-autoload-p
            (autoload
              package-require-temp-package-name
              (symbol-name package-require-temp-package-name))
          (require package-require-temp-package-name)))
      (setq package-require-clone-process nil
            package-require-temp-load-path nil
            package-require-temp-name ""
            package-require-temp-autoload-p nil
            package-require-temp-package-name nil))))

(defun package-require-repo--get-repo-name (string)
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

(defun package-require-repo--get-repo-url (string)
  "Return the repo's url.
Argument STRING is the string which includes url."
  (if (not (string-match-p "^https://\\(?:.*\\)" string))
      (format "https://github.com/%s" string)
    string))

(defmacro package-require-read-args (name args)
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
          `(package-require-load-path ',load-path))
       ,(when repo
          `(progn (setq package-require-temp-package-name (if (or ,requirep
                                                                  (null ,unrequire))
                                                              ',name
                                                            nil)
                        package-require-temp-autoload-p ,autoload)
                  (package-require-repo ',repo)))
       (when ,requirep
         (when (and (not (require ',name nil t))
                    (null outside))
           (package-require-download ',name)
           (require ',name)))
       ,(when config
          (if requirep
              `(eval ',config)
            `(eval-after-load ',name ',config)))
       ,(when var
          ;; `(package-require-get-variable ,var)
          `(package-require-set-variable ',var)
          )
       ,(when hook
          `(package-require-add-hook ',hook))
       ,(when key
          `(package-require-define-key ',key)))))

(defmacro package-require (name &rest args)
  "Main macro for `package-require'.

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
    `(package-require-read-args ,name ,args)))

(provide 'package-require)

;;; package-require.el ends here
