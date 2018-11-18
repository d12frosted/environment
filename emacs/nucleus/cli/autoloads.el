;;; autoloads.el --- the heart of every cell -*- lexical-binding: t; -*-
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

(dispatcher! (autoloads a) (nucleus-reload-autoloads nil 'force)
  "Regenerates autoloads file.

This file tells Emacs where to find your module's autoloaded
functions and plugins.")

;; external variables
(defvar autoload-timestamps)
(defvar generated-autoload-load-name)
(defvar generated-autoload-file)

;;
;; Helpers

(defvar nucleus-autoload-excluded-packages '(marshal gh)
  "Packages that have silly or destructive autoload files that
try to load everyone in the universe and their dog, causing
errors that make babies cry. No one wants that.")

(defun nucleus-delete-autoloads-file (file)
  "Delete FILE (an autoloads file), and delete the accompanying
*.elc file, if it exists."
  (cl-check-type file string)
  (when (file-exists-p file)
    (when-let* ((buf (find-buffer-visiting nucleus-autoload-file)))
      (with-current-buffer buf
        (set-buffer-modified-p nil))
      (kill-buffer buf))
    (delete-file file)
    (ignore-errors (delete-file (byte-compile-dest-file file)))
    (message "Deleted old %s" (file-name-nondirectory file))))

(defun nucleus--warn-refresh-session ()
  (print! (bold (green "\nFinished!")))
  (message "If you have a running Emacs Session, you will need to restart it or")
  (message "reload Emacs for changes to take effect:\n")
  (when (fboundp '+workspace/restart-emacs-then-restore)
    (message "  M-x +workspace/restart-emacs-then-restore"))
  (message "  M-x restart-emacs")
  (message "  M-x nucleus/reload"))

(defun nucleus--do-load (&rest files)
  (if (and noninteractive (not (daemonp)))
      (add-hook 'kill-emacs-hook #'nucleus--warn-refresh-session)
    (dolist (file files)
      (load-file (byte-compile-dest-file file)))))

(defun nucleus--byte-compile-file (file)
  (let ((short-name (file-name-nondirectory file))
        (byte-compile-dynamic-docstrings t))
    (condition-case e
        (when (byte-compile-file file)
          ;; Give autoloads file a chance to report error
          (load (if nucleus-debug-mode
                    file
                  (byte-compile-dest-file file))
                nil t)
          (unless noninteractive
            (message "Finished compiling %s" short-name)))
      ((debug error)
       (let ((backup-file (concat file ".bk")))
         (message "Copied backup to %s" backup-file)
         (copy-file file backup-file 'overwrite))
       (nucleus-delete-autoloads-file file)
       (signal 'nucleus-autoload-error (list short-name e))))))

(defun nucleus-reload-autoloads (&optional file force-p)
  "Reloads FILE (an autoload file), if it needs reloading.

FILE should be one of `nucleus-autoload-file' or
`nucleus-package-autoload-file'. If it is nil, it will try to
reload both. If FORCE-P (universal argument) do it even if it
doesn't need reloading!"
  (or (null file)
      (stringp file)
      (signal 'wrong-type-argument (list 'stringp file)))
  (if (stringp file)
      (cond ((file-equal-p file nucleus-autoload-file)
             (nucleus-reload-nucleus-autoloads force-p))
            ((file-equal-p file nucleus-package-autoload-file)
             (nucleus-reload-package-autoloads force-p))
            ((error "Invalid autoloads file: %s" file)))
    (nucleus-reload-nucleus-autoloads force-p)
    (nucleus-reload-package-autoloads force-p)))

;;
;; Nucleus autoloads

(defun nucleus--file-cookie-p (file)
  "Returns the return value of the ;;;###if predicate form in
FILE."
  (with-temp-buffer
    (insert-file-contents-literally file nil 0 256)
    (if (and (re-search-forward "^;;;###if " nil t)
             (<= (line-number-at-pos) 3))
        (let ((load-file-name file))
          (eval (sexp-at-point)))
      t)))

(defun nucleus--generate-header (func)
  (goto-char (point-min))
  (insert ";; -*- lexical-binding:t -*-\n"
          ";; This file is autogenerated by `" (symbol-name func) "', DO NOT EDIT !!\n\n"))

(defun nucleus--generate-autoloads (targets)
  (require 'autoload)
  (dolist (file targets)
    (let* ((file (file-truename file))
           (generated-autoload-file nucleus-autoload-file)
           (generated-autoload-load-name (file-name-sans-extension file))
           (noninteractive (not nucleus-debug-mode))
           autoload-timestamps)
      (print!
       (cond ((not (nucleus--file-cookie-p file))
              "⚠ Ignoring %s")
             ((autoload-generate-file-autoloads file (current-buffer))
              (yellow "✕ Nothing in %s"))
             ((green "✓ Scanned %s")))
       (if (file-in-directory-p file default-directory)
           (file-relative-name file)
         (abbreviate-file-name file))))))

(defun nucleus--expand-autoloads ()
  (let ((load-path
         ;; NOTE With `nucleus-emacs-dir' in `load-path', nucleus
         ;; autoloads files will be unable to declare autoloads for
         ;; the built-in autoload.el Emacs package, should
         ;; ~/.emacs.d/autoload.el exist. Not sure why they'd want to
         ;; though, so it's an acceptable compromise.
         (append (list nucleus-emacs-dir)
                 nucleus-modules-dirs
                 load-path))
        cache)
    (while (re-search-forward "^\\s-*(autoload\\s-+'[^ ]+\\s-+\"\\([^\"]*\\)\"" nil t)
      (let ((path (match-string 1)))
        (replace-match
         (or (cdr (assoc path cache))
             (when-let* ((libpath (locate-library path))
                         (libpath (file-name-sans-extension libpath)))
               (push (cons path (abbreviate-file-name libpath)) cache)
               libpath)
             path)
         t t nil 1)))))

(defun nucleus--generate-autodefs (targets enabled-targets)
  (goto-char (point-max))
  (search-backward ";;;***" nil t)
  (save-excursion (insert "\n"))
  (dolist (path targets)
    (insert
     (with-temp-buffer
       (insert-file-contents path)
       (let ((member-p (or (member path enabled-targets)
                           (file-in-directory-p path nucleus-dir)))
             forms)
         (while (re-search-forward "^;;;###autodef *\\([^\n]+\\)?\n" nil t)
           (let* ((sexp (sexp-at-point))
                  (pred (match-string 1))
                  (type (car sexp))
                  (name (nucleus-unquote (cadr sexp)))
                  (origin (cond ((nucleus-module-from-path path))
                                ((file-in-directory-p path nucleus-emacs-dir)
                                 `(:private . ,(intern (file-name-base path))))
                                ((file-in-directory-p path nucleus-emacs-dir)
                                 `(:core . ,(intern (file-name-base path))))))
                  (nucleus-file-form
                   `(put ',name 'nucleus-file ,(abbreviate-file-name path))))
             (cond ((memq type '(defun defmacro cl-defun cl-defmacro))
                    (cl-destructuring-bind (_ name arglist &rest body) sexp
                      (let ((docstring (if (stringp (car body))
                                           (pop body)
                                         "No documentation.")))
                        (push (cond ((not (and member-p
                                               (or (null pred)
                                                   (let ((load-file-name path))
                                                     (eval (read pred) t)))))
                                     (push nucleus-file-form forms)
                                     (setq docstring (format "THIS FUNCTION DOES NOTHING BECAUSE %s IS DISABLED\n\n%s"
                                                             origin docstring))
                                     (condition-case-unless-debug e
                                         (append (list (pcase type
                                                         (`defun 'defmacro)
                                                         (`cl-defun `cl-defmacro)
                                                         (_ type))
                                                       name arglist docstring)
                                                 (cl-loop for arg in arglist
                                                          if (and (symbolp arg)
                                                                  (not (keywordp arg))
                                                                  (not (memq arg cl--lambda-list-keywords)))
                                                          collect arg into syms
                                                          else if (listp arg)
                                                          collect (car arg) into syms
                                                          finally return (if syms `((ignore ,@syms)))))
                                       ('error
                                        (message "Ignoring autodef %s (%s)"
                                                 name e)
                                        nil)))
                                    ((make-autoload sexp (abbreviate-file-name (file-name-sans-extension path)))))
                              forms)
                        (push `(put ',name 'nucleus-module ',origin) forms))))

                   ((eq type 'defalias)
                    (cl-destructuring-bind (_type name target &optional docstring) sexp
                      (let ((name (nucleus-unquote name))
                            (target (nucleus-unquote target)))
                        (unless (and member-p
                                     (or (null pred)
                                         (let ((load-file-name path))
                                           (eval (read pred) t))))
                          (setq target #'ignore))
                        (push nucleus-file-form forms)
                        (push `(put ',name 'nucleus-module ',origin) forms)
                        (push `(defalias ',name #',target ,docstring)
                              forms))))

                   ((and member-p
                         (or (null pred)
                             (eval (read pred) t)))
                    (push sexp forms)))))
         (if forms
             (concat (string-join (mapcar #'prin1-to-string (reverse forms)) "\n")
                     "\n")
           ""))))))

(defun nucleus--cleanup-autoloads ()
  (goto-char (point-min))
  (when (re-search-forward "^;;\\(;[^\n]*\\| no-byte-compile: t\\)\n" nil t)
    (replace-match "" t t)))

(defun nucleus-reload-nucleus-autoloads (&optional force-p)
  "Refreshes the autoloads.el file, specified by
`nucleus-autoload-file', if necessary (or if FORCE-P is non-nil).

It scans and reads core/autoload/*.el, modules/*/*/autoload.el
and modules/*/*/autoload/*.el, and generates
`nucleus-autoload-file'. This file tells Emacs where to find
lazy-loaded functions.

This should be run whenever your `nucleus!' block, or a module
autoload file, is modified."
  (let* ((default-directory nucleus-emacs-dir)
         (nucleus-modules (nucleus-modules))
         (targets
          (file-expand-wildcards
           (expand-file-name "autoload/*.el" nucleus-dir)))
         (enabled-targets (copy-sequence targets))
         case-fold-search)
    (dolist (path (nucleus-module-load-path t))
      (let* ((auto-dir  (expand-file-name "autoload" path))
             (auto-file (expand-file-name "autoload.el" path))
             (module    (nucleus-module-from-path auto-file))
             (module-p  (or (nucleus-module-p (car module) (cdr module))
                            (file-equal-p path nucleus-emacs-dir))))
        (when (file-exists-p auto-file)
          (push auto-file targets)
          (if module-p (push auto-file enabled-targets)))
        (dolist (file (nucleus-files-in auto-dir :match "\\.el$" :full t))
          (push file targets)
          (if module-p (push file enabled-targets)))))
    (if (and (not force-p)
             (not nucleus-emacs-changed-p)
             (file-exists-p nucleus-autoload-file)
             (not (file-newer-than-file-p (expand-file-name "dna.el" nucleus-emacs-dir)
                                          nucleus-autoload-file))
             (not (cl-loop for file in targets
                           if (file-newer-than-file-p file nucleus-autoload-file)
                           return t)))
        (progn (print! (green "Nucleus autoloads is up-to-date"))
               (nucleus-initialize-autoloads nucleus-autoload-file)
               nil)
      (nucleus-delete-autoloads-file nucleus-autoload-file)
      (message "Generating new autoloads.el")
      (make-directory (file-name-directory nucleus-autoload-file) t)
      (with-temp-file nucleus-autoload-file
        (nucleus--generate-header 'nucleus-reload-nucleus-autoloads)
        (save-excursion
          (nucleus--generate-autoloads (reverse enabled-targets)))
          ;; Replace autoload paths (only for module autoloads) with
          ;; absolute paths for faster resolution during load and
          ;; simpler `load-path'
        (save-excursion
          (nucleus--expand-autoloads)
          (print! (green "✓ Expanded module autoload paths")))
        ;; Generates stub definitions for functions/macros defined in
        ;; disabled modules, so that you will never get a
        ;; void-function when you use them.
        (save-excursion
          (nucleus--generate-autodefs (reverse targets) enabled-targets)
          (print! (green "✓ Generated autodefs")))
        ;; Remove byte-compile inhibiting file variables so we can
        ;; byte-compile the file, and autoload comments.
        (nucleus--cleanup-autoloads)
        (print! (green "✓ Clean up autoloads")))
      ;; Byte compile it to give the file a chance to reveal errors.
      (nucleus--byte-compile-file nucleus-autoload-file)
      (nucleus--do-load nucleus-autoload-file)
      t)))

;;
;; Package autoloads

(defun nucleus--generate-package-autoloads ()
  (dolist (spec (nucleus-get-package-alist))
    (if-let* ((pkg  (car spec))
              (desc (cdr spec)))
        (unless (memq pkg nucleus-autoload-excluded-packages)
          (let ((file (concat (package--autoloads-file-name desc) ".el")))
            (when (file-exists-p file)
              (insert "(let ((load-file-name " (prin1-to-string (abbreviate-file-name file)) "))\n")
              (insert-file-contents file)
              (while (re-search-forward "^\\(?:;;\\(.*\n\\)\\|\n\\|(provide '[^\n]+\\)" nil t)
                (unless (nth 8 (syntax-ppss))
                  (replace-match "" t t)))
              (unless (bolp) (insert "\n"))
              (insert ")\n"))))
      (message "Couldn't find package desc for %s" (car spec)))))

(defun nucleus--generate-var-cache ()
  (nucleus-initialize-packages)
  (prin1 `(setq load-path ',load-path
                auto-mode-alist ',auto-mode-alist
                Info-directory-list ',Info-directory-list
                nucleus-disabled-packages ',nucleus-disabled-packages
                package-activated-list ',package-activated-list)
         (current-buffer)))

(defun nucleus--cleanup-package-autoloads ()
  (while (re-search-forward "^\\s-*\\((\\(?:add-to-list\\|\\(?:when\\|if\\) (boundp\\)\\s-+'\\(?:load-path\\|auto-mode-alist\\)\\)" nil t)
    (goto-char (match-beginning 1))
    (kill-sexp)))

(defun nucleus-reload-package-autoloads (&optional force-p)
  "Compiles `nucleus-package-autoload-file' from the autoloads
files of all installed packages. It also caches `load-path',
`Info-directory-list', `nucleus-disabled-packages',
`package-activated-list' and `auto-mode-alist'.

Will do nothing if none of your installed packages have been
modified. If FORCE-P (universal argument) is non-nil, regenerate
it anyway.

This should be run whenever your `nucleus!' block or update your
packages."
  (if (and (not force-p)
           (not nucleus-emacs-changed-p)
           (file-exists-p nucleus-package-autoload-file)
           (not (file-newer-than-file-p nucleus-packages-dir nucleus-package-autoload-file))
           (not (ignore-errors
                  (cl-loop for key being the hash-keys of (nucleus-modules)
                           for path = (nucleus-module-path (car key) (cdr key) "packages.el")
                           if (file-newer-than-file-p path nucleus-package-autoload-file)
                           return t))))
      (ignore (print! (green "Nucleus package autoloads is up-to-date"))
              (nucleus-initialize-autoloads nucleus-package-autoload-file))
    (let (case-fold-search)
      (nucleus-delete-autoloads-file nucleus-package-autoload-file)
      (with-temp-file nucleus-package-autoload-file
        (nucleus--generate-header 'nucleus-reload-package-autoloads)
        (save-excursion
          ;; Cache the important and expensive-to-initialize state
          ;; here.
          (nucleus--generate-var-cache)
          (print! (green "✓ Cached package state"))
          ;; Loop through packages and concatenate all their autoloads
          ;; files.
          (nucleus--generate-package-autoloads)
          (print! (green "✓ Package autoloads included")))
        ;; Remove `load-path' and `auto-mode-alist' modifications
        ;; (most of them, at least); they are cached later, so all
        ;; those membership checks are unnecessary overhead.
        (nucleus--cleanup-package-autoloads)
        (print! (green "✓ Removed load-path/auto-mode-alist entries"))))
    (nucleus--byte-compile-file nucleus-package-autoload-file)
    (nucleus--do-load nucleus-package-autoload-file)
    t))