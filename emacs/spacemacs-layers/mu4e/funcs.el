;;; funcs.el --- mu4e Layer funcs File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Boris Buliga
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


(defun d12frosted/mu4e-set-account ()
  "Set the account for composing a message."
  (let* ((account
          (if mu4e-compose-parent-message
              (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                (string-match "/\\(.*?\\)/" maildir)
                (match-string 1 maildir))
            (completing-read (format "Compose with account: (%s) "
                                     (mapconcat #'(lambda (var) (car var))
                                                d12frosted/mu4e-account-alist "/"))
                             (mapcar #'(lambda (var) (car var)) d12frosted/mu4e-account-alist)
                             nil t nil nil (caar d12frosted/mu4e-account-alist))))
         (account-vars (cdr (assoc account d12frosted/mu4e-account-alist))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars)
      (error "No email account found"))))

(defun mu4e-msgv-action-view-in-browser (msg)
  "View the body of the message in a web browser."
  (interactive)
  (let ((html (mu4e-msg-field (mu4e-message-at-point t) :body-html))
        (tmpfile (format "%s/%d.html" temporary-file-directory (random))))
    (unless html (error "No html part for this message"))
    (with-temp-file tmpfile
      (insert
       "<html>"
       "<head><meta http-equiv=\"content-type\""
       "content=\"text/html;charset=UTF-8\">"
       html))
    (browse-url (concat "file://" tmpfile))))
