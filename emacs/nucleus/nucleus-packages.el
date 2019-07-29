;;; nucleus-packages.el --- the heart of every cell -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;;         Henrik Lissner <henrik@lissner.net>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; URL: https://github.com/d12frosted/environment/emacs
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;; Most of the code was borrowed from hlissner/doom-emacs.
;;
;;; Commentary:
;;
;; > Emacs package management is opinionated, and so am I. I've bound together
;; > `use-package', `quelpa' and package.el to create my own, rolling-release,
;; > lazily-loaded package management system for Emacs. (c) hlissner
;;
;; The three key commands are:
;;
;; - `bin/nucleus install`: Installs packages that are wanted, but not
;;   installed.
;; - `bin/nucleus update`: Updates packages that are out-of-date.
;; - `bin/nucleus autoremove`: Uninstalls packages that are no longer
;;   needed.
;;
;; This system reads packages.el files located in each activated
;; module (and one in `nucleus-dir'). These contain `package!' blocks
;; that tell nucleus what plugins to install and where from.
;;
;; You should be able to use package.el commands without any
;; conflicts.
;;
;; See nucleus/autoload/packages.el for more functions.
;;
;;; Code:

(defconst nucleus-elpa-mirror-dir (concat nucleus-projects-dir "elpa-mirror/")
  "Directory for elpa-mirror.")

(defvar nucleus-packages ()
  "A list of enabled packages. Each element is a sublist, whose
CAR is the package's name as a symbol, and whose CDR is the plist
supplied to its `package!' declaration. Set by
`nucleus-initialize-packages'.")

(defvar nucleus-core-packages '(persistent-soft use-package quelpa async)
  "A list of packages that must be installed (and will be
auto-installed if missing) and shouldn't be deleted.")

(defvar nucleus-disabled-packages ()
  "A list of packages that should be ignored by `def-package!'.")

(defvar nucleus-package-archives-location 'mirror
  "Location of package-archives.

Can be either mirror or origin.

Mirror means that https://github.com/d12frosted/elpa-mirror will
be used for fetching packages. When `nucleus-elpa-mirror-dir'
exists, a local copy will be used.

Origin means that we will go straight to elpa.gnu.org, melpa.org
and orgmode.org.")

(setq package--init-file-ensured t
      package-user-dir (expand-file-name "elpa" nucleus-packages-dir)
      package-gnupghome-dir (expand-file-name "gpg" nucleus-packages-dir)
      package-enable-at-startup nil
      package-archives
      (cond
       ((eq nucleus-package-archives-location 'origin)
        '(("gnu"   . "https://elpa.gnu.org/packages/")
          ("melpa" . "https://melpa.org/packages/")
          ("org"   . "https://orgmode.org/elpa/")))
       ((file-exists-p nucleus-elpa-mirror-dir)
        `(("gnu"   . ,(concat nucleus-elpa-mirror-dir "gnu/"))
          ("melpa" . ,(concat nucleus-elpa-mirror-dir "melpa/"))
          ("org"   . ,(concat nucleus-elpa-mirror-dir "org/"))))
       (t
        '(("gnu"   . "https://gitlab.com/d12frosted/elpa-mirror/raw/master/gnu/")
          ("melpa" . "https://gitlab.com/d12frosted/elpa-mirror/raw/master/melpa/")
          ("org"   . "https://gitlab.com/d12frosted/elpa-mirror/raw/master/org/"))))

      ;; Don't track MELPA, we'll use package.el for that
      quelpa-checkout-melpa-p nil
      quelpa-update-melpa-p nil
      quelpa-melpa-recipe-stores nil
      quelpa-self-upgrade-p nil
      quelpa-verbose nucleus-debug-mode
      quelpa-dir (expand-file-name "quelpa" nucleus-packages-dir))

;; accommodate INSECURE setting
(unless gnutls-verify-error
  (dolist (archive package-archives)
    (setcdr archive
	    (replace-regexp-in-string
	     "^https://"
	     "http://"
	     (cdr archive)
	     t
	     nil))))

;;
;; Bootstrapper

(defun nucleus-initialize-packages (&optional force-p)
  "Ensures that package management system, package.el and quelpa
are initialized, and `nucleus-packages', `packages-alist' and
`quelpa-cache' are populated, if they aren't already.

If FORCE-P is non-nil, do it anyway.

If FORCE-P is 'internal, only (re)populate `nucleus-packages'.

Use this before any of package.el, quelpa or package management's
API to ensure all the necessary package metadata is initialized
and available for them."
  (with-temp-buffer ; prevent buffer-local settings from propagating
    (let ((load-prefer-newer t)) ; reduce stale code issues
      ;; package.el and quelpa handle themselves if their state changes during
      ;; the current session, but if you change an packages.el file in a module,
      ;; there's no non-trivial way to detect that, so we give you a way to
      ;; reload only nucleus-packages (by passing 'internal as FORCE-P).
      (unless (eq force-p 'internal)
        ;; `package-alist'
        (when (or force-p (not (bound-and-true-p package-alist)))
          (nucleus-ensure-packages-initialized 'force)
          (setq load-path (cl-remove-if-not #'file-directory-p load-path)))
        ;; `quelpa-cache'
        (when (or force-p (not (bound-and-true-p quelpa-cache)))
          ;; ensure un-byte-compiled version of quelpa is loaded
          (unless (featurep 'quelpa)
            (load (locate-library "quelpa.el") nil t t))
          (setq quelpa-initialized-p nil)
          (or (quelpa-setup-p)
              (error "Could not initialize quelpa"))))
      ;; `nucleus-packages'
      (when (or force-p (not nucleus-packages))
        (cl-flet
            ((_load
              (lambda (file &optional noerror)
                (condition-case e
                    (load file noerror t t)
                  ((debug error)
                   (signal 'nucleus-package-error
                           (list (or (nucleus-module-from-path file)
                                     '(:private . packages))
                                 e)))))))
          (let ((nucleus-modules (nucleus-modules))
                (nucleus--stage 'packages)
                (noninteractive t))
            (setq nucleus-packages nil)
            (_load (expand-file-name "packages.el" nucleus-dir))
            ;; We load the private packages file twice to ensure disabled
            ;; packages are seen ASAP, and a second time to ensure privately
            ;; overridden packages are properly overwritten.
            (let ((private-packages (expand-file-name "packages.el" nucleus-emacs-dir)))
              (_load private-packages t)
              (cl-loop for key being the hash-keys of nucleus-modules
                       for path = (nucleus-module-path (car key) (cdr key) "packages.el")
                       do (let ((nucleus--current-module key)) (_load path t)))
              (_load private-packages t)
              (setq nucleus-packages (reverse nucleus-packages)))))))))

;;
;; Package API

(defun nucleus-ensure-packages-initialized (&optional force-p)
  "Make sure package.el is initialized."
  (when (or force-p (not (bound-and-true-p package--initialized)))
    (require 'package)
    (setq package-activated-list nil
          package--initialized nil)
    (let (byte-compile-warnings)
      (condition-case _
          (package-initialize)
        ('error (package-refresh-contents)
                (setq nucleus--refreshed-p t)
                (package-initialize))))))

(defun nucleus-ensure-core-packages ()
  "Make sure `nucleus-core-packages' are installed."
  (when-let* ((core-packages (cl-remove-if #'package-installed-p nucleus-core-packages)))
    (message "Installing core packages")
    (unless nucleus--refreshed-p
      (package-refresh-contents))
    (dolist (package core-packages)
      (let ((inhibit-message t))
        (package-install package))
      (if (package-installed-p package)
          (message "✓ Installed %s" package)
        (error "✕ Couldn't install %s" package)))
    (message "Installing core packages...done")))

;;
;; Module package macros

(cl-defmacro package! (name &rest plist &key recipe pin disable _ignore _freeze)
  "Declares a package and how to install it (if applicable).

This macro is declarative and does not load nor install
packages. It is used to populate `nucleus-packages' with metadata
about the packages nucleus needs to keep track of.

Only use this macro in a module's packages.el file.

Accepts the following properties:

 :recipe RECIPE
   Takes a MELPA-style recipe (see `quelpa-recipe' in `quelpa'
   for an example); for packages to be installed from external
   sources.

 :pin ARCHIVE-NAME
   Instructs ELPA to only look for this package in
   ARCHIVE-NAME. e.g. \"org\".  Ignored if RECIPE is present.

 :disable BOOL 
   Do not install or update this package AND disable all of its
   `def-package!'  blocks.

 :ignore FORM
   Do not install this package.

 :freeze FORM
   Do not update this package if FORM is non-nil.

Returns t if package is successfully registered, and nil if it
was disabled elsewhere."
  (declare (indent defun))
  (nucleus--assert-stage-p 'packages #'package!)
  (let ((plist (append plist (cdr (assq name nucleus-packages)))))
    (when recipe
      (when (cl-evenp (length recipe))
        (setq plist (plist-put plist :recipe (cons name recipe))))
      (setq pin nil
            plist (plist-put plist :pin nil)))
    (when (file-in-directory-p (FILE!) nucleus-emacs-dir)
      (setq plist (plist-put plist :private t)))
    (let (newplist)
      (while plist
        (unless (null (cadr plist))
          (push (cadr plist) newplist)
          (push (car plist) newplist))
        (pop plist)
        (pop plist))
      (setq plist newplist))
    (macroexp-progn
     (append (if disable `((add-to-list 'nucleus-disabled-packages ',name nil #'eq)))
             (if pin `((setf (alist-get ',name package-pinned-packages) ,pin)))
             `((setf (alist-get ',name nucleus-packages) ',plist)
               (not (memq ',name nucleus-disabled-packages)))))))

(defmacro depends-on! (module submodule &optional flags)
  "Declares that this module depends on another.

Only use this macro in a module's packages.el file.

MODULE is a keyword, and SUBMODULE is a symbol. Under the hood,
this simply loads MODULE SUBMODULE's packages.el file."
  (nucleus--assert-stage-p 'packages #'depends-on!)
  `(let ((nucleus-modules ,nucleus-modules)
         (flags ,flags))
     (when flags
       (nucleus-module-put ,module ',submodule :flags flags))
     (load! "packages" ,(nucleus-module-locate-path module submodule) t)))

(provide 'nucleus-packages)

;;; nucleus-packages.el ends here
