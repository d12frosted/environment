;;; lang/vulpea/autoload/refile.el -*- lexical-binding: t; -*-

(defvar +refile-ignore-tags '("JOURNAL" "REFILE")
  "List of tags to ignore during refile.")

;;;###autoload
(defun +refile-verify-target ()
  "Exclude todo keywords with a done state from refile targets."
  (let ((tags-at (org-get-tags)))
    (and
     ;; doesn't have done keyword
     (not (member (nth 2 (org-heading-components)) org-done-keywords))

     ;; doesn't have blacklisted tag
     (or (null tags-at)
         (cl-member-if-not
          (lambda (x)
            (member (if (listp x) (car x) x) +refile-ignore-tags))
          tags-at)))))
