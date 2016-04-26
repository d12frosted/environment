;;; funcs.el --- d12frosted-core layer funcs file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Boris Buliga <d12frosted@d12frosted.local>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defun d12/buffer-contains-substring? (string)
  "Check if current buffer contains substring."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (search-forward string nil t))))

(defun d12/goto-line-and-center ()
  "Same as `goto-line', but recenter after it."
  (interactive)
  (call-interactively 'goto-line)
  (call-interactively 'recenter-top-bottom))

(defmacro d12|rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after d12|rename-modeline-hack activate)
        (setq mode-name ,new-name))))

(defun d12/string-equal (s1 s2)
  "Wrapper for `string-equal' that checks that s1 and s2 are
string before that."
  (and (stringp s1)
       (stringp s2)
       (string-equal s1 s2)))

(defun d12/string-to-number (string default)
  "Wrapper for `string-to-number' that checks that STRING is
string before that."
  (if (and (stringp string)
           (not (string-empty-p string)))
      (string-to-number string)
    default))

;;; Miscellaneous

(defadvice spacemacs-buffer//insert-image-banner (after d12//spacemacs-title-advice activate)
  "Change the default title in *spacemacs* buffer."
  (save-excursion
    (goto-char (point-min))
    (search-forward "[S P A C E M A C S]")
    (replace-match "[A N I M A C S]")))

(defun configuration-layer/get-owner (pkg &optional print)
  (interactive "SEnter package name: \np")
  (let ((owner (cdr (assoc pkg
                           (mapcar (lambda (pkg)
                                     (cons (oref pkg :name)
                                           (oref pkg :owner)))
                                   configuration-layer--packages)))))
    (when print
      (message "Owner of %S is %S" pkg owner))
    owner))

(defun d12/setup-M-h ()
  "Setup M-h key binding on OS X in GUI."
  (when (and (spacemacs/system-is-mac)
             (display-graphic-p))
    (bind-key "M-h" 'ns-do-hide-emacs)
    (-map (lambda (mode)
            (add-hook (intern (concat (symbol-name mode) "-hook"))
                      `(lambda ()
                         (define-key
                           (symbol-value (intern ,(concat (symbol-name mode) "-map")))
                           (kbd "M-h")
                           nil))))
          '(org-mode
            company-quickhelp-mode))))

(defun d12/align-repeat (start end regexp)
  "Repeat alignment with respect to the given regular
expression."
  (interactive "r\nsAlign regexp: ")
  (align-regexp start end
                (concat "\\(\\s-*\\)" regexp) 1 1 t))

(defun d12/projectile-replace-regexp ()
  "Replace a string in the project using `tags-query-replace'.
Less efficient than `projectile-replace' but at least allows
usage of regular expressions. See
https://github.com/bbatsov/projectile/issues/576 for more details
on `projectile-replace' issue with regexps."
  (interactive "P")
  (let* ((old-text (read-string
                    (projectile-prepend-project-name "Replace: ")
                    (projectile-symbol-or-selection-at-point)))
         (new-text (read-string
                    (projectile-prepend-project-name
                     (format "Replace %s with: " old-text))))
         (files (-map (lambda (f) (concat (projectile-project-root) f)) (projectile-current-project-files))))
    (tags-query-replace old-text new-text nil (cons 'list files))))

(defun d12/refill-paragraphs-to-be-one-line ()
  "Fill individual paragraphs with large fill column"
  (interactive)
  (let ((fill-column 10000000))
    (fill-individual-paragraphs (point-min) (point-max))))

;;; funcs.el ends here
