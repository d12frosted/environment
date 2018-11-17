;;; core/cli/quickstart.el -*- lexical-binding: t; -*-

(dispatcher! (quickstart qs) (nucleus-quickstart)
  "Quickly deploy a private module and Doom.

This deploys a barebones config to ~/.nucleus.d. The destination can be changed
with the -p option, e.g.

  nucleus -p ~/.config/nucleus quickstart

This command will refuse to overwrite the private directory if it already
exists.")


;;
;; Library

(defun nucleus-quickstart ()
  "Quickly deploy a private module and Doom.

This deploys a barebones config to `nucleus-private-dir', installs all missing
packages and regenerates the autoloads file."
  ;; Create `nucleus-private-dir'
  (let ((short-private-dir (abbreviate-file-name nucleus-private-dir)))
    (if (file-directory-p nucleus-private-dir)
        (print! (yellow "%s directory already exists. Skipping.") short-private-dir)
      (print! "Creating %s" short-private-dir)
      (make-directory nucleus-private-dir t)
      (print! (green "Done!"))
      ;; Create init.el, config.el & packages.el
      (dolist (file (list (cons "init.el"
                                (lambda ()
                                  (insert-file-contents (expand-file-name "init.example.el" nucleus-emacs-dir))))
                          (cons "config.el"
                                (lambda ()
                                  (insert (format ";;; %sconfig.el -*- lexical-binding: t; -*-\n\n"
                                                  short-private-dir)
                                          ";; Place your private configuration here\n")))
                          (cons "packages.el"
                                (lambda ()
                                  (insert (format ";; -*- no-byte-compile: t; -*-\n;;; %spackages.el\n\n"
                                                  short-private-dir)
                                          ";;; Examples:\n"
                                          ";; (package! some-package)\n"
                                          ";; (package! another-package :recipe (:fetcher github :repo \"username/repo\"))\n"
                                          ";; (package! builtin-package :disable t)\n")))))
        (cl-destructuring-bind (path . fn) file
          (print! "Creating %s%s" short-private-dir path)
          (with-temp-file (expand-file-name path nucleus-private-dir)
            (funcall fn))
          (print! (green "Done!"))))))
  ;; Ask if Emacs.app should be patched
  (when IS-MAC
    (message "MacOS detected")
    (condition-case e
        (nucleus-patch-macos nil (nucleus--find-emacsapp-path))
      (user-error (message "%s" (error-message-string e)))))
  ;; Install Doom packages
  (print! "Installing plugins")
  (nucleus-packages-install nucleus-auto-accept)
  (print! "Regenerating autoloads files")
  (nucleus-reload-autoloads nil 'force-p)
  (print! (bold (green "\nFinished! Doom is ready to go!\n")))
  (with-temp-buffer
    (nucleus-template-insert "QUICKSTART_INTRO")
    (print! (buffer-string))))
