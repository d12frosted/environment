;;; byte-compiles.el --- the heart of every cell -*- lexical-binding: t; -*-
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
;;; Code:

(dispatcher! (compile c) (nucleus-byte-compile args)
  "Byte-compiles your config or selected modules.

  compile [TARGETS...]
  compile :nucleus :private lang/haskell
  compile feature lang

Accepts :nucleus, :private and :plugins as special arguments,
indicating you want to byte-compile nucleus files, your
private config or your ELPA plugins, respectively.")

(dispatcher! (recompile rc) (nucleus-byte-compile args 'recompile)
  "Re-byte-compiles outdated *.elc files.")

(dispatcher! clean (nucleus-clean-byte-compiled-files)
  "Delete all *.elc files.")

;;
;; Helpers

(defun nucleus--byte-compile-ignore-file-p (path)
  (let ((filename (file-name-nondirectory path)))
    (or (string-prefix-p "." filename)
        (string-prefix-p "test-" filename)
        (not (equal (file-name-extension path) "el")))))

(defun nucleus-byte-compile (&optional modules recompile-p)
  "Byte compiles your emacs configuration.

init.el is always byte-compiled by this.

If MODULES is specified (a list of module strings,
e.g. \"lang/php\"), those are byte-compiled. Otherwise, all
enabled modules are byte-compiled, including nucleus. It always
ignores unit tests and files with `no-byte-compile' enabled.

WARNING: byte-compilation yields marginal gains and makes debugging new issues
difficult. It is recommended you don't use it unless you understand the
reprecussions.

Use `nucleus-clean-byte-compiled-files' or `make clean' to reverse
byte-compilation.

If RECOMPILE-P is non-nil, only recompile out-of-date files."
  (let ((default-directory nucleus-emacs-dir)
        (total-ok   0)
        (total-fail 0)
        (total-noop 0)
        compile-plugins-p
        targets)
    (dolist (module (delete-dups modules) (nreverse targets))
      (pcase module
        (":core"    (push nucleus-dir targets))
        (":private" (push nucleus-emacs-dir targets))
        (":plugins"
         (cl-loop for (_name . desc) in (nucleus-get-package-alist)
                  do (package--compile desc))
         (setq compile-plugins-p t
               modules (delete ":plugins" modules)))
        ((pred file-directory-p)
         (push module targets))
        ((pred (string-match "^\\([^/]+\\)/\\([^/]+\\)$"))
         (push (nucleus-module-locate-path
                (nucleus-keyword-intern (match-string 1 module))
                (intern (match-string 2 module)))
               targets))))
    (cl-block 'byte-compile
      ;; If we're just here to byte-compile our plugins, we're done!
      (and (not modules)
           compile-plugins-p
           (cl-return-from 'byte-compile t))
      (unless (or (equal modules '(":nucleus"))
                  recompile-p)
        (unless (or nucleus-auto-accept
                    (y-or-n-p
                     (concat "Warning: byte compiling is for advanced users. It will interfere with your\n"
                             "efforts to debug issues. It is not recommended you do it if you frequently\n"
                             "tinker with your Emacs config.\n\n"
                             "Alternatively, use `bin/nucleus compile :core` instead to byte-compile only the\n"
                             "nucleus files, as these don't change often.\n\n"
                             "If you have issues, please make sure byte-compilation isn't the cause by using\n"
                             "`bin/nucleus clean` to clear out your *.elc files.\n\n"
                             "Byte-compile anyway?")))
          (message "Aborting.")
          (cl-return-from 'byte-compile)))
      (and (not recompile-p)
           (or (null modules) (equal modules '(":nucleus")))
           (nucleus-clean-byte-compiled-files))
      (let (nucleus-emacs-changed-p
            noninteractive)
        ;; But first we must be sure that nucleus and config have been
        ;; fully loaded. Which usually aren't so in an noninteractive
        ;; session.
        (unless (and (nucleus-initialize-autoloads nucleus-autoload-file)
                     (nucleus-initialize-autoloads nucleus-package-autoload-file))
          (nucleus-reload-autoloads))
        (nucleus-initialize)
        (nucleus-initialize-modules 'force))
      ;; If no targets were supplied, then we use your module list.
      (unless modules
        (setq targets (append (list nucleus-dir)
                              (nucleus-module-load-path))))
      ;; Assemble el files we want to compile; taking into account that
      ;; MODULES may be a list of MODULE/SUBMODULE strings from the command
      ;; line.
      (let ((target-files (nucleus-files-in targets :filter #'nucleus--byte-compile-ignore-file-p))
            (load-path load-path)
            kill-emacs-hook kill-buffer-query-functions)
        (unless target-files
          (if targets
              (message "Couldn't find any valid targets")
            (message "No targets to %scompile" (if recompile-p "re" "")))
          (cl-return-from 'byte-compile))
        (require 'use-package)
        (condition-case e
            (let ((use-package-defaults use-package-defaults)
                  (use-package-expand-minimally t))
              ;; Prevent packages from being loaded at compile time if they
              ;; don't meet their own predicates.
              (push (list :no-require t
                          (lambda (_name args)
                            (or (when-let* ((pred (or (plist-get args :if)
                                                      (plist-get args :when))))
                                  (not (eval pred t)))
                                (when-let* ((pred (plist-get args :unless)))
                                  (eval pred t)))))
                    use-package-defaults)
              ;; Always compile private init file
              (push (expand-file-name "init.el" nucleus-emacs-dir) target-files)
              (push (expand-file-name "init.el" nucleus-emacs-dir)   target-files)
              (dolist (target (cl-delete-duplicates (mapcar #'file-truename target-files) :test #'equal))
                (if (or (not recompile-p)
                        (let ((elc-file (byte-compile-dest-file target)))
                          (and (file-exists-p elc-file)
                               (file-newer-than-file-p target elc-file))))
                    (let ((result (if (or (string-match-p "/\\(?:packages\\|doctor\\)\\.el$" target)
                                          (not (nucleus--file-cookie-p target)))
                                      'no-byte-compile
                                    (byte-compile-file target)))
                          (short-name (if (file-in-directory-p target nucleus-emacs-dir)
                                          (file-relative-name target nucleus-emacs-dir)
                                        (abbreviate-file-name target))))
                      (cl-incf
                       (cond ((eq result 'no-byte-compile)
                              (print! (dark (white "⚠ Ignored %s")) short-name)
                              total-noop)
                             ((null result)
                              (print! (red "✕ Failed to compile %s") short-name)
                              total-fail)
                             (t
                              (print! (green "✓ Compiled %s") short-name)
                              (load target t t)
                              total-ok))))
                  (cl-incf total-noop)))
              (print! (bold (color (if (= total-fail 0) 'green 'red)
                                   "%s %d/%d file(s) (%d ignored)"))
                      (if recompile-p "Recompiled" "Compiled")
                      total-ok (- (length target-files) total-noop)
                      total-noop)
              (or (= total-fail 0)
               (error "Failed to compile some files")))
          ((debug error)
           (print! (red "\nThere were breaking errors.\n\n%s")
                   "Reverting changes...")
           (signal 'nucleus-error (list 'byte-compile e))))))))

(defun nucleus-clean-byte-compiled-files ()
  "Delete all the compiled elc files in your Emacs configuration and private
module. This does not include your byte-compiled, third party packages.'"
  (cl-loop with default-directory = nucleus-emacs-dir
           for path in (append (nucleus-files-in nucleus-emacs-dir :match "\\.elc$" :depth 0)
                               (nucleus-files-in nucleus-emacs-dir :match "\\.elc$" :depth 1)
                               (nucleus-files-in nucleus-dir :match "\\.elc$")
                               (nucleus-files-in nucleus-modules-dirs :match "\\.elc$" :depth 4))
           for truepath = (file-truename path)
           if (file-exists-p path)
           do (delete-file path)
           and do
           (print! (green "✓ Deleted %s")
                   (if (file-in-directory-p truepath default-directory)
                       (file-relative-name truepath)
                     (abbreviate-file-name truepath)))
           finally do (print! (bold (green "Everything is clean")))))
