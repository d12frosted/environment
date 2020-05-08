;;; lang/vulpea/autoload/link.el -*- lexical-binding: t; -*-

;;;###autoload
(defvar +org-id-link-regexp
  (concat
   ;; outer bracket
   "\\["

   ;; link
   "\\["
   ;; type
   "\\([[a-zA-Z0-9\\-]+\\)"
   ":"
   ;; uuid
   "\\("
   "[a-zA-Z0-9]\\{8\\}"
   "-"
   "[a-zA-Z0-9]\\{4\\}"
   "-"
   "[a-zA-Z0-9]\\{4\\}"
   "-"
   "[a-zA-Z0-9]\\{4\\}"
   "-"
   "[a-zA-Z0-9]\\{12\\}"
   "\\)"
   "\\]"

   ;; title
   "\\[" "\\(.*?\\)" "\\]"

   ;; outer bracket
   "\\]"))

;;;###autoload
(defun +org-extract-id-from-link (link)
  "Extract headline id from LINK."
  (+string-match-n 2 +org-id-link-regexp link))
