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

;;; File operations

(defun d12/get-string-from-file (filepath)
  "Return filepath's file content."
  (with-temp-buffer
    (insert-file-contents filepath)
    (buffer-string)))

;;; Buffer operations

(defun d12/buffer-contains-substring? (string)
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (search-forward string nil t))))

;;; Navigation

(defun d12/goto-line-and-center ()
  (interactive)
  (call-interactively 'goto-line)
  (call-interactively 'recenter-top-bottom))

;;; Modeline

(defmacro d12|rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after d12|rename-modeline-hack activate)
        (setq mode-name ,new-name))))

;;; String helpers

(defun d12/string-equal (s1 s2)
  (and (stringp s1)
       (stringp s2)
       (string-equal s1 s2)))

(defun d12/string-to-number (string default)
  (if (and (stringp string)
           (not (string-empty-p string)))
      (string-to-number string)
    default))

;;; Text manipulations

(defun d12/copy-line-or-region (&optional copy-func)
  "Copy current line (with newline character) or region. When
`universal-argument' is called first, copy whole buffer (but
respect `narrow-to-region').

When `copy-func' is provided, it is used to copy line or region
instead of `kill-ring-save'"
  (interactive)
  (d12//funcall-on-line-or-region
   (if (fboundp copy-func)
       copy-func
     'copy-region-as-kill)))

(defun d12/kill-line-or-region (&optional kill-func)
  "Cut current line or region. When `universal-argument' is
called first, cut whole buffer (but respect `narrow-to-region').

When `kill-func' is provided, it is used to copy line or region
instead of `kill-region'"
  (interactive)
  (d12//funcall-on-line-or-region
   (if (fboundp kill-func)
       kill-func
     'kill-region)))

(defun d12/delete-line-or-region (&optional delete-func)
  "Delete current line or region without putting it to kill-ring.
When `universal-argument' is called first, delete whole
buffer (but respect `narrow-to-region').

When `kill-func' is provided, it is used to copy line or region
instead of `kill-region'"
  (interactive)
  (d12//funcall-on-line-or-region
   (if (fboundp delete-func)
       delete-func
     'delete-region)))

(defun d12//funcall-on-line-or-region (func)
  "Call function `f' on current line or region."
  (if current-prefix-arg
      (funcall func (point-min) (point-max))
    (if (use-region-p)
        (funcall func (region-beginning) (region-end) t)
      (funcall func (line-beginning-position) (line-beginning-position 2) nil))))

;; http://endlessparentheses.com/a-comment-or-uncomment-sexp-command.html?source=rss
(defun uncomment-sexp (&optional n)
  "Uncomment a sexp around point."
  (interactive "P")
  (let* ((initial-point (point-marker))
         (p)
         (end (save-excursion
                (when (elt (syntax-ppss) 4)
                  (re-search-backward comment-start-skip
                                      (line-beginning-position)
                                      t))
                (setq p (point-marker))
                (comment-forward (point-max))
                (point-marker)))
         (beg (save-excursion
                (forward-line 0)
                (while (= end (save-excursion
                                (comment-forward (point-max))
                                (point)))
                  (forward-line -1))
                (goto-char (line-end-position))
                (re-search-backward comment-start-skip
                                    (line-beginning-position)
                                    t)
                (while (looking-at-p comment-start-skip)
                  (forward-char -1))
                (point-marker))))
    (unless (= beg end)
      (uncomment-region beg end)
      (goto-char p)
      ;; Indentify the "top-level" sexp inside the comment.
      (while (and (ignore-errors (backward-up-list) t)
                  (>= (point) beg))
        (skip-chars-backward (rx (syntax expression-prefix)))
        (setq p (point-marker)))
      ;; Re-comment everything before it.
      (ignore-errors
        (comment-region beg p))
      ;; And everything after it.
      (goto-char p)
      (forward-sexp (or n 1))
      (skip-chars-forward "\r\n[:blank:]")
      (if (< (point) end)
          (ignore-errors
            (comment-region (point) end))
        ;; If this is a closing delimiter, pull it up.
        (goto-char end)
        (skip-chars-forward "\r\n[:blank:]")
        (when (= 5 (car (syntax-after (point))))
          (delete-indentation))))
    ;; Without a prefix, it's more useful to leave point where
    ;; it was.
    (unless n
      (goto-char initial-point))))

(defun comment-sexp--raw ()
  "Comment the sexp at point or ahead of point."
  (pcase (or (bounds-of-thing-at-point 'sexp)
             (save-excursion
               (skip-chars-forward "\r\n[:blank:]")
               (bounds-of-thing-at-point 'sexp)))
    (`(,l . ,r)
     (goto-char r)
     (skip-chars-forward "\r\n[:blank:]")
     (comment-region l r)
     (skip-chars-forward "\r\n[:blank:]"))))

(defun comment-or-uncomment-sexp (&optional n)
  "Comment the sexp at point and move past it. If already
inside (or before) a comment, uncomment instead. With a prefix
argument N, (un)comment that many sexps."
  (interactive "P")
  (if (or (elt (syntax-ppss) 4)
          (< (save-excursion
               (skip-chars-forward "\r\n[:blank:]")
               (point))
             (save-excursion
               (comment-forward 1)
               (point))))
      (uncomment-sexp n)
    (dotimes (_ (or n 1))
      (comment-sexp--raw))))

;;; Miscellaneous

;; rename title in init screen
(defadvice spacemacs-buffer//insert-image-banner (after d12//spacemacs-title-advice activate)
  "Change the default title in *spacemacs* banner."
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

(defun d12//init-haskell-interactive-mode ()
  (setq-local evil-move-cursor-back nil))

(defun align-repeat (start end regexp)
  "Repeat alignment with respect to the given regular
     expression."
  (interactive "r\nsAlign regexp: ")
  (align-regexp start end
                (concat "\\(\\s-*\\)" regexp) 1 1 t))

(defun align-c-func-call-in-a-very-strange-way-that-i-dont-like (start end)
  "Some people are using very odd code style. This function at
least makes me happy."
  (interactive "r")
  (align-regexp start end "\\(\\s-*\\)(*(")
  (align-regexp start end "\\(\\s-*\\)," 1 1 t)
  (align-regexp start end "\\(\\s-*\\))*)")
  )

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

;;; funcs.el ends here
