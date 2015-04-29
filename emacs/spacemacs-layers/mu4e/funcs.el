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

(defun mu4e/set-account-vars (account)
  "Set the variables for ACCOUNT from 'mu4e/accounts-alist."
  (setq mu4e/current-account account)
  (let* ((account-vars (cdr (assoc account mu4e/accounts-alist))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars)
      (error "Couln't find variables for <%s>" account))))

;; todo - use some dash functions to make it easy to read
(defun mu4e/set-account-for-composing ()
  "Set the account for composing a message."
  (let* ((account
          (if mu4e-compose-parent-message
              (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                (string-match "/\\(.*?\\)/" maildir)
                (match-string 1 maildir))
            (completing-read (format "Compose with account: (%s) "
                                     (mapconcat #'(lambda (var) (car var))
                                                mu4e/accounts-alist "/"))
                             (mapcar #'(lambda (var) (car var)) mu4e/accounts-alist)
                             nil t nil nil (caar mu4e/accounts-alist)))))
    (mu4e/set-account-vars account)))

;;; folders

(defun mu4e/get-folder (type msg)
  "Returns the folder of TYPE based on msg.

   If MSG is nil then returns the folder of
   TYPE based on 'mu4e/current-account."
  (let* ((account (if msg
                      (mu4e/get-account-from-maildir
                       (mu4e-message-field msg :maildir))
                    mu4e/current-account)))
    (mu4e/get-prop-for-account-in-alist account
                                        type
                                        mu4e/folders-alist)))

(defun mu4e/trash-folder-fn (msg)
  "Returns trash folder for MSG."
  (mu4e/get-folder 'mu4e-trash-folder msg))

(defun mu4e/refile-folder-fn (msg)
  "Returns refile folder for MSG."
  (mu4e/get-folder 'mu4e-refile-folder msg))

(defun mu4e/drafts-folder-fn (msg)
  "Returns drafts folder for MSG."
  (mu4e/get-folder 'mu4e-drafts-folder msg))

(defun mu4e/sent-folder-fn (msg)
  "Returns sent folder for MSG."
  (mu4e/get-folder 'mu4e-sent-folder msg))

;;; helper functions

(defun mu4e/get-account-from-maildir (maildir)
  "Return account name for maildir.

   For example, (mu4e/get-account-from-maildir \"/some-name/Trash\")
   returns \"some-name\"."
  (string-match "/\\(.*?\\)/" maildir)
  (match-string 1 maildir))

(defun mu4e/get-prop-for-account-in-alist (account prop alist)
  (let* ((props (cdr (assoc account alist)))
         (value (car (cdr (assoc prop props)))))
    (if value
        value
      (error "Couldn't find '%s' property for '%s' in %s"
             prop
             account
             alist))))
