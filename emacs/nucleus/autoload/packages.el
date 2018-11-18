;;; packages.el --- the heart of every cell -*- lexical-binding: t; -*-
;;
;;; Copyright (c) 2015-2018 Boris Buliga
;;
;;; Author: Boris Buliga <boris@d12frosted.io>
;;; URL: https://github.com/d12frosted/environment/emacs
;;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;; Most of the code was borrowed from hlissner/doom-emacs.
;;
;;; Commentary:
;;
;;; Code:

(load! "cache")

;;; Private functions
(defun nucleus--packages-choose (prompt)
  (let ((table (cl-loop for pkg in package-alist
                        unless (package-built-in-p (cdr pkg))
                        collect (cons (package-desc-full-name (cdr pkg))
                                      (cdr pkg)))))
    (cdr (assoc (completing-read prompt
                                 (mapcar #'car table)
                                 nil t)
                table))))

(defmacro nucleus--condition-case! (&rest body)
  `(condition-case-unless-debug e
       (progn ,@body)
     ('user-error
      (print! (bold (red "  NOTICE: %s")) e))
     ('file-error
      (print! (bold (red "  FILE ERROR: %s")) (error-message-string e))
      (print! "  Trying again...")
      (quiet! (nucleus-refresh-packages-maybe t))
      ,@body)
     ('error
      (print! (bold (red "  FATAL ERROR: %s\n  Run again with the -d flag for details")) e))))

(defun nucleus--refresh-pkg-cache ()
  "Clear the cache for `nucleus-refresh-packages-maybe'."
  (setq nucleus--refreshed-p nil)
  (nucleus-cache-set 'last-pkg-refresh nil))

;;
;; Library

;;;###autoload
(defun nucleus-refresh-packages-maybe (&optional force-p)
  "Refresh ELPA packages, if it hasn't been refreshed recently."
  (when force-p
    (nucleus--refresh-pkg-cache))
  (unless (or (nucleus-cache-get 'last-pkg-refresh)
              nucleus--refreshed-p)
    (condition-case e
        (progn
          (message "Refreshing package archives")
          (package-refresh-contents)
          (nucleus-cache-set 'last-pkg-refresh t 1200))
    ((debug error)
     (nucleus--refresh-pkg-cache)
     (signal 'nucleus-error e)))))

;;;###autoload
(defun nucleus-package-backend (name &optional noerror)
  "Get which backend the package NAME was installed with. Can
either be elpa or quelpa. Throws an error if NOERROR is nil and
the package isn't installed."
  (cl-check-type name symbol)
  (cond ((assq name quelpa-cache)  'quelpa)
        ((assq name package-alist) 'elpa)
        ((package-built-in-p name) 'emacs)
        ((not noerror) (error "%s package is not installed" name))))

;;;###autoload
(defun nucleus-package-outdated-p (name)
  "Determine whether NAME (a symbol) is outdated or not. If
outdated, returns a list, whose car is NAME, and cdr the current
version list and latest version list of the package."
  (cl-check-type name symbol)
  (when-let* ((desc (cadr (assq name package-alist))))
    (let* ((old-version (package-desc-version desc))
           (new-version
            (pcase (nucleus-package-backend name)
              ('quelpa
               (let ((recipe (plist-get (cdr (assq name nucleus-packages)) :recipe))
                     (dir (expand-file-name (symbol-name name) quelpa-build-dir))
                     (inhibit-message (not nucleus-debug-mode))
                     (quelpa-upgrade-p t))
                 (if-let* ((ver (quelpa-checkout recipe dir)))
                     (version-to-list ver)
                   old-version)))
              ('elpa
               (let ((desc (cadr (assq name package-archive-contents))))
                 (when (package-desc-p desc)
                   (package-desc-version desc)))))))
      (unless (and (listp old-version) (listp new-version))
        (error "Couldn't get version for %s" name))
      (when (version-list-< old-version new-version)
        (list name old-version new-version)))))

;;;###autoload
(defun nucleus-package-installed-p (name)
  "TODO"
  (and (package-installed-p name)
       (when-let* ((desc (cadr (assq name package-alist))))
         (let ((dir (package-desc-dir desc)))
           (file-directory-p dir)))))

;;;###autoload
(defun nucleus-package-prop (name prop &optional eval)
  "Return PROPerty in NAME's plist."
  (cl-check-type name symbol)
  (cl-check-type prop keyword)
  (let ((value (plist-get (cdr (assq name nucleus-packages)) prop)))
    (if eval (eval value) value)))

;;;###autoload
(defun nucleus-package-different-backend-p (name)
  "Return t if a package named NAME (a symbol) has a new backend
than what it was installed with. Returns nil otherwise, or if
package isn't installed."
  (cl-check-type name symbol)
  (nucleus-initialize-packages)
  (and (package-installed-p name)
       (let* ((plist (cdr (assq name nucleus-packages)))
              (old-backend (nucleus-package-backend name 'noerror))
              (new-backend (if (plist-get plist :recipe) 'quelpa 'elpa)))
         (not (eq old-backend new-backend)))))

;;;###autoload
(defun nucleus-package-different-recipe-p (name)
  "Return t if a package named NAME (a symbol) has a different
recipe than it was installed with."
  (cl-check-type name symbol)
  (nucleus-initialize-packages)
  (and (package-installed-p name)
       (when-let* ((quelpa-recipe (assq name quelpa-cache))
                   (nucleus-recipe   (assq name nucleus-packages)))
         (not (equal (cdr quelpa-recipe)
                     (cdr (plist-get (cdr nucleus-recipe) :recipe)))))))

;;;###autoload
(cl-defun nucleus-get-packages (&key (installed 'any)
                                     (private 'any)
                                     (disabled 'any)
                                     (pinned 'any)
                                     (ignored 'any)
                                     (sort t)
                                     changed
                                     backend
                                     deps)
  "Retrieves a list of primary packages (i.e. non-dependencies).
Each element is a cons cell, whose car is the package symbol and
whose cdr is the quelpa recipe
(if any).

You can build a filtering criteria using one or more of the
following properties:

  :backend BACKEND
    Can be 'quelpa, 'elpa or 'emacs

  :installed BOOL
    Only return installed packages (t) or uninstalled
    packages (nil)

  :private BOOL
    Only return private packages (t) or non-private
    packages (nil)

  :disabled BOOL
    Only return packages that are disabled (t) or otherwise (nil)

  :ignored BOOL
    Only return packages that are ignored (t) or otherwise (nil)

  :pinned BOOL|ARCHIVE
    Only return packages that are pinned (t), not pinned (nil) or
    pinned to a specific archive (stringp)

  :deps BOOL
    Includes the package's dependencies (t).

The resulting list is sorted unless :sort nil is passed to this
function.

Warning: this function is expensive, as it re-evaluates your all
packages.el files."
  (nucleus-initialize-packages)
  (cl-remove-duplicates
   (cl-loop with packages = (append (mapcar #'list nucleus-core-packages)
                                    nucleus-packages)
            for (sym . plist)
            in (if sort
                   (cl-sort (copy-sequence packages) #'string-lessp :key #'car)
                 packages)
            if (and (or (not backend)
                        (eq (nucleus-package-backend sym t) backend))
                    (or (eq ignored 'any)
                        (let* ((form (plist-get plist :ignore))
                               (value (eval form)))
                          (if ignored value (not value))))
                    (or (eq disabled 'any)
                        (if disabled
                            (plist-get plist :disable)
                          (not (plist-get plist :disable))))
                    (or (eq installed 'any)
                        (if installed
                            (nucleus-package-installed-p sym)
                          (not (nucleus-package-installed-p sym))))
                    (or (eq private 'any)
                        (if private
                            (plist-get plist :private)
                          (not (plist-get plist :private))))
                    (or (eq pinned 'any)
                        (cond ((eq pinned 't)
                               (plist-get plist :pin))
                              ((null pinned)
                               (not (plist-get plist :pin)))
                              ((equal (plist-get plist :pin) pinned)))))
            collect (cons sym plist)
            and if (and deps (not (package-built-in-p sym)))
            nconc
            (cl-loop for pkg in (nucleus-get-dependencies-for sym 'recursive 'noerror)
                     if (or (eq installed 'any)
                            (if installed
                                (nucleus-package-installed-p pkg)
                              (not (nucleus-package-installed-p pkg))))
                     collect (cons pkg (cdr (assq pkg nucleus-packages)))))
   :key #'car))

;;;###autoload
(defun nucleus-get-package-alist ()
  "Returns a list of all desired packages, their dependencies and
their desc objects, in the order of their `package! blocks.'"
  (nucleus-initialize-packages)
  (cl-remove-duplicates
   (cl-loop for name in (append nucleus-core-packages (mapcar #'car nucleus-packages))
            if (assq name package-alist)
            nconc (cl-loop for dep in (package--get-deps name)
                           if (assq dep package-alist)
                           collect (cons dep (cadr it)))
            and collect (cons name (cadr it)))
   :key #'car
   :from-end t))

;;;###autoload
(defun nucleus-get-depending-on (name &optional noerror)
  "Return a list of packages that depend on the package named
NAME."
  (cl-check-type name symbol)
  (unless (package-built-in-p name)
    (if-let* ((desc (cadr (assq name package-alist))))
        (mapcar #'package-desc-name (package--used-elsewhere-p desc nil t))
      (unless noerror
        (error "Couldn't find %s, is it installed?" name)))))

;;;###autoload
(defun nucleus-get-dependencies-for (name &optional recursive noerror)
  "Return a list of dependencies for a package."
  (cl-check-type name symbol)
  ;; can't get dependencies for built-in packages
  (unless (package-built-in-p name)
    (if-let* ((desc (cadr (assq name package-alist))))
        (let* ((deps (mapcar #'car (package-desc-reqs desc)))
               (deps (cl-remove-if #'package-built-in-p deps)))
          (if recursive
              (nconc deps (mapcan (lambda (dep) (nucleus-get-dependencies-for dep t t))
                                  deps))
            deps))
      (unless noerror
        (error "Couldn't find %s, is it installed?" name)))))

;;;###autoload
(defun nucleus-get-outdated-packages (&optional include-frozen-p)
  "Return a list of packages that are out of date. Each element is a list,
containing (PACKAGE-SYMBOL OLD-VERSION-LIST NEW-VERSION-LIST).

If INCLUDE-FROZEN-P is non-nil, check frozen packages as well.

Used by `nucleus-packages-update'."
  (nucleus-initialize-packages t)
  (nucleus-refresh-packages-maybe nucleus-debug-mode)
  (require 'async)
  (let (quelpa-pkgs elpa-pkgs)
    ;; Separate quelpa from elpa packages
    (dolist (pkg (mapcar #'car package-alist))
      (when (and (or (not (nucleus-package-prop pkg :freeze 'eval))
                     include-frozen-p)
                 (not (nucleus-package-prop pkg :ignore 'eval))
                 (not (nucleus-package-different-backend-p pkg)))
        (push pkg
              (if (eq (nucleus-package-backend pkg) 'quelpa)
                  quelpa-pkgs
                elpa-pkgs))))
    ;; The bottleneck in this process is quelpa's version checks, so check them
    ;; asynchronously.
    (let (futures)
      (dolist (pkg quelpa-pkgs)
        (when nucleus-debug-mode
          (message "New thread for: %s" pkg))
        (push (async-start
               `(lambda ()
                  (let ((gc-cons-threshold ,nucleus-gc-cons-upper-limit)
                        (nucleus-init-p t)
                        (noninteractive t)
                        (load-path ',load-path)
                        (package-alist ',package-alist)
                        (package-archive-contents ',package-archive-contents)
                        (package-selected-packages ',package-selected-packages)
                        (nucleus-packages ',nucleus-packages)
                        (nucleus-modules ',nucleus-modules)
                        (quelpa-cache ',quelpa-cache)
                        (user-emacs-directory ,user-emacs-directory)
                        nucleus-emacs-dir)
                    (load ,(expand-file-name "nucleus.el" nucleus-dir))
                    (load ,(expand-file-name "autoload/packages.el" nucleus-dir))
                    (require 'package)
                    (require 'quelpa)
                    (nucleus-package-outdated-p ',pkg))))
              futures))
      (delq nil
            (append (mapcar #'nucleus-package-outdated-p elpa-pkgs)
                    (mapcar #'async-get (reverse futures)))))))

;;;###autoload
(defun nucleus-get-orphaned-packages ()
  "Return a list of symbols representing packages that are no
longer needed or depended on.

Used by `nucleus-packages-autoremove'."
  (let ((package-selected-packages
         (mapcar #'car (nucleus-get-packages :ignored nil :disabled nil))))
    (append (package--removable-packages)
            (cl-loop for pkg in package-selected-packages
                     if (and (nucleus-package-different-backend-p pkg)
                             (not (package-built-in-p pkg)))
                     collect pkg))))

;;;###autoload
(defun nucleus-get-missing-packages (&optional include-ignored-p)
  "Return a list of requested packages that aren't installed or
built-in, but are enabled (with a `package!' directive). Each
element is a list whose CAR is the package symbol, and whose CDR
is a plist taken from that package's `package!' declaration.

If INCLUDE-IGNORED-P is non-nil, includes missing packages that
are ignored, i.e. they have an :ignore property.

Used by `nucleus-packages-install'."
  (nucleus-initialize-packages)
  (cl-loop for (name . plist)
           in (nucleus-get-packages :ignored (if include-ignored-p 'any)
                                 :disabled nil
                                 :deps t
                                 :sort nil)
           if (and (or (plist-get plist :pin)
                       (not (package-built-in-p name)))
                   (or (not (nucleus-package-installed-p name))
                       (nucleus-package-different-backend-p name)
                       (nucleus-package-different-recipe-p name)))
           collect (cons name plist)))

;;
;; Main functions

(defun nucleus--delete-package-files (name-or-desc)
  (let ((pkg-build-dir
         (if (package-desc-p name-or-desc)
             (package-desc-dir name-or-desc)
           (expand-file-name (symbol-name name-or-desc) quelpa-build-dir))))
    (when (file-directory-p pkg-build-dir)
      (delete-directory pkg-build-dir t))))

;;;###autoload
(defun nucleus-install-package (name &optional plist)
  "Installs package NAME with optional quelpa RECIPE (see
`quelpa-recipe' for an example; the package name can be
omitted)."
  (cl-check-type name symbol)
  (nucleus-initialize-packages)
  (when (and (package-installed-p name)
             (not (package-built-in-p name)))
    (if (or (nucleus-package-different-backend-p name)
            (nucleus-package-different-recipe-p name))
        (nucleus-delete-package name t)
      (user-error "%s is already installed" name)))
  (let* ((inhibit-message (not nucleus-debug-mode))
         (plist (or plist (cdr (assq name nucleus-packages)))))
    (if-let* ((recipe (plist-get plist :recipe)))
        (condition-case e
            (let (quelpa-upgrade-p)
              (quelpa recipe))
          ((debug error)
           (nucleus--delete-package-files name)
           (signal (car e) (cdr e))))
      (package-install name))
    (if (not (package-installed-p name))
        (nucleus--delete-package-files name)
      (setf (alist-get name nucleus-packages) plist)
      name)))

;;;###autoload
(defun nucleus-update-package (name &optional force-p)
  "Updates package NAME (a symbol) if it is out of date, using
quelpa or package.el as appropriate."
  (cl-check-type name symbol)
  (nucleus-initialize-packages)
  (unless (package-installed-p name)
    (user-error "%s isn't installed" name))
  (when (nucleus-package-different-backend-p name)
    (user-error "%s's backend has changed and must be uninstalled first" name))
  (when (or force-p (nucleus-package-outdated-p name))
    (let ((inhibit-message (not nucleus-debug-mode))
          (desc (cadr (assq name package-alist))))
      (pcase (nucleus-package-backend name)
        (`quelpa
         (condition-case e
             (let ((quelpa-upgrade-p t))
               (quelpa (assq name quelpa-cache)))
           ((debug error)
            (nucleus--delete-package-files name)
            (signal (car e) (cdr e)))))
        (`elpa
         (let* ((archive (cadr (assq name package-archive-contents)))
                (packages
                 (if (package-desc-p archive)
                     (package-compute-transaction (list archive) (package-desc-reqs archive))
                   (package-compute-transaction () (list (list archive))))))
           (package-download-transaction packages))))
      (unless (nucleus-package-outdated-p name)
        (nucleus--delete-package-files desc)
        t))))

;;;###autoload
(defun nucleus-delete-package (name &optional force-p)
  "Uninstalls package NAME if it exists, and clears it from
`quelpa-cache'."
  (cl-check-type name symbol)
  (nucleus-initialize-packages)
  (unless (package-installed-p name)
    (user-error "%s isn't installed" name))
  (let ((inhibit-message (not nucleus-debug-mode))
        (spec (assq name quelpa-cache))
        quelpa-p)
    (when spec
      (setq quelpa-cache (delq spec quelpa-cache))
      (quelpa-save-cache)
      (setq quelpa-p t))
    (package-delete (cadr (assq name package-alist)) force-p)
    (nucleus--delete-package-files name)
    (not (package-installed-p name))))

;;
;; Interactive commands

;;;###autoload
(defun nucleus/update-package (pkg)
  "Prompts the user with a list of outdated packages and updates
the selected package. Use this interactively. Use
`nucleus-update-package' for direct calls."
  (declare (interactive-only t))
  (interactive
   (let* ((packages (nucleus-get-outdated-packages))
          (package (if packages
                       (completing-read "Update package: "
                                        (mapcar #'car packages)
                                        nil t)
                     (user-error "All packages are up to date"))))
     (list (cdr (assq (car (assoc package package-alist)) packages)))))
  (nucleus-initialize-packages)
  (cl-destructuring-bind (package old-version new-version) pkg
    (if-let* ((desc (nucleus-package-outdated-p package)))
        (let ((old-v-str (package-version-join old-version))
              (new-v-str (package-version-join new-version)))
          (if (y-or-n-p (format "%s will be updated from %s to %s. Update?"
                                package old-v-str new-v-str))
              (message "%s %s (%s => %s)"
                       (if (nucleus-update-package package t) "Updated" "Failed to update")
                       package old-v-str new-v-str)
            (message "Aborted")))
      (message "%s is up-to-date" package))))

;;
;; Advice

;;;###autoload
(defun nucleus*package-delete (desc &rest _)
  "Update `quelpa-cache' upon a successful `package-delete'."
  (nucleus-initialize-packages)
  (let ((name (package-desc-name desc)))
    (unless (package-installed-p name)
      (when-let* ((spec (assq name quelpa-cache)))
        (setq quelpa-cache (delq spec quelpa-cache))
        (quelpa-save-cache)
        (nucleus--delete-package-files name)))))

;;
;; Make package.el cooperate with nucleus

;; Updates QUELPA after deleting a package
;;;###autoload
(advice-add #'package-delete
 :after #'nucleus*package-delete)

;; Replace with nucleus variants
;;;###autoload
(advice-add #'package-autoremove
 :override (λ! (nucleus-packages-autoremove current-prefix-arg)))

;;;###autoload
(advice-add #'package-install-selected-packages
 :override (λ! (nucleus-packages-install current-prefix-arg)))

