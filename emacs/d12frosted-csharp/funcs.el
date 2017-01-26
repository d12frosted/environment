;;; funcs.el --- d12frosted-csharp layer funcs file for Spacemacs.
;;
;; Copyright (c) 2015-2017 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:
;;

;;; Code:
;;

(defun d12-csharp/go-to-definition-at-center ()
  (interactive)
  (progn
    (omnisharp-go-to-definition)
    (recenter)))

(defun d12-csharp/comment-to-doc ()
  (interactive)
  (save-excursion
    (let ((comment-regexp "^\\([\s\t]+\\)//\\(.*\\)$")
          (indent-str "")
          (match-comment
           (lambda ()
             (beginning-of-line)
             (search-forward-regexp comment-regexp (line-end-position) t)))
          (format-beginning
           (lambda ()
             (format "%s/// <summary>\n" indent-str)))
          (format-ending
           (lambda ()
             (format "%s/// </summary>\n" indent-str)))
          (format-line
           (lambda ()
             (format "%s//%s" indent-str (match-string 2)))))

      ;; check if we are inside of comment
      (when (funcall match-comment)
        ;; find beginning of multiline comment
        (previous-line)
        (while (funcall match-comment)
          (previous-line))
        (next-line)

        ;; now convert it to proper format
        (when (funcall match-comment)
          (setq indent-str (match-string 1))
          (replace-match
           (concat (funcall format-beginning)
                   (funcall format-line))
           t nil)
          (next-line)
          (while (funcall match-comment)
            (replace-match (funcall format-line) t nil)
            (next-line))
          (beginning-of-line)
          (insert (funcall format-ending)))))))

;;; funcs.el ends here
