;;; nucleus-debug.el --- the heart of every cell -*- lexical-binding: t; -*-
;;
;;; Copyright (c) 2015-2018 Boris Buliga
;;
;;; Author: Boris Buliga <boris@d12frosted.io>
;;; URL: https://github.com/d12frosted/environment/emacs
;;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

;;;###autoload
(defun nucleus/am-i-secure ()
  "Test to see if your root certificates are securely configured
in Emacs."
  (declare (interactive-only t))
  (interactive)
  (unless (string-match-p "\\_<GNUTLS\\_>" system-configuration-features)
    (warn "gnutls support isn't built into Emacs, there may be problems"))
  (if-let* ((bad-hosts
             (cl-loop for bad
                      in '("https://wrong.host.badssl.com/"
                           "https://self-signed.badssl.com/")
                      if (condition-case _e
                             (url-retrieve-synchronously bad)
                           (error nil))
                      collect bad)))
      (error "tls seems to be misconfigured (it got %s)."
             bad-hosts)
    (url-retrieve "https://badssl.com"
                  (lambda (status)
                    (if (or (not status) (plist-member status :error))
                        (warn "Something went wrong.\n\n%s" (pp-to-string status))
                      (message "Your trust roots are set up properly.\n\n%s" (pp-to-string status))
                      t)))))

;;;###autoload
(defun nucleus/copy-backtrace ()
  "Copy the contents of the *Backtrace* window into your
clipboard for easy pasting into a bug report or discord."
  (interactive)
  (if-let* ((buf (get-buffer "*Backtrace*")))
      (with-current-buffer buf
        (kill-new
         (string-trim (buffer-string))))
    (user-error "No backtrace buffer detected")))

;;; Vanilla sandbox

(defvar nucleus--sandbox-init-nucleus-p nil)

(defun nucleus--run-vanilla-sandbox (&optional load-nucleus-p)
  (interactive)
  (let ((contents (buffer-string))
        (file (make-temp-file "nucleus-sandbox-")))
    (with-temp-file file
      (insert
       (prin1-to-string
        `(cond (,load-nucleus-p
                (setq nucleus-emacs-dir "/tmp/does/not/exist"
                      nucleus-modules ,nucleus-modules)
                (load ,user-init-file))
               ((setq package--init-file-ensured t
                      package-user-dir ,package-user-dir
                      package-archives ',package-archives
                      user-emacs-directory ,nucleus-emacs-dir)
                (package-initialize))))
       "\n(unwind-protect (progn\n" contents "\n)\n"
       (format "(delete-file %S))" file)))
    (let ((args (list "-Q" "-l" file)))
      (require 'restart-emacs)
      (condition-case e
          (cond ((display-graphic-p)
                 (if (memq system-type '(windows-nt ms-dos))
                     (restart-emacs--start-gui-on-windows args)
                   (restart-emacs--start-gui-using-sh args)))
                ((memq system-type '(windows-nt ms-dos))
                 (user-error "Cannot start another Emacs from Windows shell."))
                ((suspend-emacs
                  (format "%s %s -nw; fg"
                          (shell-quote-argument (restart-emacs--get-emacs-binary))
                          (string-join (mapcar #'shell-quote-argument args) " ")))))
        (error
         (delete-file file)
         (signal (car e) (cdr e)))))))

(defun nucleus--run-vanilla-nucleus-sandbox ()
  (interactive)
  (nucleus--run-vanilla-sandbox t))

;;;###autoload
(defun nucleus/open-vanilla-sandbox ()
  "Open an Emacs Lisp buffer destinated to run in a blank Emacs session (and
optionally load only Nucleus and its modules, without your private config).

This provides a testbed for debugging code without Nucleus (or your private config)
standing in the way, and without sacrificing access to installed packages."
  (interactive)
  (let* ((buffer-name "*nucleus:vanilla-sandbox*")
         (exists (get-buffer buffer-name))
         (buf (get-buffer-create buffer-name)))
    (with-current-buffer buf
      (emacs-lisp-mode)
      (local-set-key (kbd "C-c C-c") #'nucleus--run-vanilla-sandbox)
      (local-set-key (kbd "C-c C-d") #'nucleus--run-vanilla-nucleus-sandbox)
      (local-set-key (kbd "C-c C-k") #'kill-this-buffer)
      (setq header-line-format "C-c C-c to run the session / C-c C-d to run it with vanilla Nucleus loaded / C-c C-k to abort it")
      (setq-local default-directory nucleus-emacs-dir)
      (unless (buffer-live-p exists)
        (nucleus-template-insert "VANILLA_SANDBOX"))
      (goto-char (point-max)))
    (pop-to-buffer buf)))

;;; Profiling

(defvar nucleus--profiler nil)

;;;###autoload
(defun nucleus/toggle-profiler ()
  "Toggle the Emacs profiler. Run it again to see the profiling report."
  (interactive)
  (if (not nucleus--profiler)
      (profiler-start 'cpu+mem)
    (profiler-report)
    (profiler-stop))
  (setq nucleus--profiler (not nucleus--profiler)))

;;;###autoload
(defun nucleus/profile-emacs ()
  "Profile the startup time of Emacs in the background with ESUP.
If INIT-FILE is non-nil, profile that instead of USER-INIT-FILE."
  (interactive)
  (require 'esup)
  (let ((init-file esup-user-init-file))
    (message "Starting esup...")
    (esup-reset)
    (setq esup-server-process (esup-server-create (esup-select-port)))
    (setq esup-server-port (process-contact esup-server-process :service))
    (message "esup process started on port %s" esup-server-port)
    (let ((process-args
           (append `("*esup-child*"
                     "*esup-child*"
                     ,esup-emacs-path
                     "-Q"
                     "--eval=(setq after-init-time nil)"
                     "-L" ,esup-load-path)
                   (when (bound-and-true-p early-init-file)
                     `("-l" ,early-init-file))
                   `("-l" "esup-child"
                     ,(format "--eval=(let ((load-file-name \"%s\")) (esup-child-run \"%s\" \"%s\" %d))"
                              init-file
                              init-file
                              esup-server-port
                              esup-depth)
                     "--eval=(nucleus|run-all-startup-hooks)"))))
      (when esup-run-as-batch-p
        (setq process-args (append process-args '("--batch"))))
      (setq esup-child-process (apply #'start-process process-args)))
    (set-process-sentinel esup-child-process 'esup-child-process-sentinel)))

;;;###autoload
(advice-add #'esup :override #'nucleus/profile-emacs)

(provide 'nucleus-debug)

;;; nucleus-debug.el ends here
