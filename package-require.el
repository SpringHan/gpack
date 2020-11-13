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

;;; Code:

(defgroup package-require nil
	"The group for `package-require'."
	:group 'applications)

(defcustom package-require-repo-directory "~/.emacs.d/third-party/"
	"The directory for the repo."
	:type 'string
	:group 'package-require)

(defun package-require-download (name)
	"Download the package if it has not downloaded."
	(unless (package-installed-p name)
		(package-refresh-contents)
		(package-install name)))

(defun package-require-load-path (path)
	"Define the load path."
	(if (stringp path)
			(add-to-list 'load-path path)
		(add-to-list (cdr-safe path) (car path))))

(defmacro package-require-get-variable (vlist)
	"Get variables."
	(declare (debug t))
	(if (symbolp `',(car vlist))
			`(package-require-set-variable (list ',(car vlist) ,(cdr-safe vlist)))
		(let (var-list tmp)
			(dolist (var vlist)
				(setq var-list (append var-list (list `',(car var) `,(cdr-safe var)))))
			`(package-require-set-variable ,var-list))))

(defun package-require-set-variable (vars)
	"Set variables."
	(when (listp vars)
		(if (symbolp (car vars))
				;; (setq (car vars) (cadr vars))
				(set (car vars) (cdr-safe vars))
			(dolist (var-list vars)
				;; (setq (car var-list) (cadr var-list)))
				(set (car var-list) (cdr-safe var-list)))
			)))

(defun package-require-add-hook (hlist)
	"Add hooks."
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
	"Define keybindings."
	(when (listp klist)
		(if (not (listp (car klist)))
				(package-require-key--item-define klist)
			(dolist (key-list klist)
				(package-require-key--item-define key-list)))))

(defun package-require-key--item-define (item)
	"Define key by a item."
	(pcase (type-of (car item))
		('string (global-set-key (kbd (car item)) (cdr-safe item)))
		('symbol (package-require-key--by-map (car item) (cdr-safe item)))))

(defun package-require-key--by-map (map key-info)
	"Define key with map."
	(if (stringp (car key-info))
			(define-key (symbol-value map) (car key-info) (cdr-safe key-info))
		(if (eq (car key-info) 'lambda)
				(define-key (symbol-value map) (car key-info) (cdr-safe key-info))
			(dolist (key key-info)
				(define-key (symbol-value map) (car key) (cdr-safe key))))))

(defmacro package-require-read-args (name args)
	"Read ARGS and do the actions."
	(declare (debug t))
	(let ((requirep t)
				hook var key repo load-path before config autoload)
		(while (keywordp (car-safe args))
			(pcase (pop args)
				(:hook (setq hook (pop args)))
				(:var (setq var (pop args)))
				(:key (setq key (pop args)))
				(:repo (setq repo (pop args)))
				(:load-path (setq load-path (pop args)))
				(:before (setq before (pop args)))
				(:config (setq config (pop args)))
				(:autoload (setq autoload (pop args)
												 requirep nil))
				(:un-require (setq requirep nil))))
		`(progn
			 ,(when before
					`(eval ',before))
			 ,(when load-path
					`(package-require-load-path ',load-path))
			 ,(when repo
					`(package-require-repo ,repo))
			 (when ,requirep
				 (when (not (require ',name nil t))
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
