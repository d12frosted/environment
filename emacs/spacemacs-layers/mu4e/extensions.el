;;; extensions.el --- mu4e Layer extensions File for Spacemacs
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

(defvar mu4e-pre-extensions
  '(mu4e)
  "List of all extensions to load before the packages.")

(defvar mu4e-post-extensions
  '()
  "List of all extensions to load after the packages.")

(defun mu4e/init-mu4e ()
  "Initialize my extension"
  (use-package mu4e
    :defer 1
    :init
    :config
    (setq mu4e-use-fancy-chars t ; should be executed only for GUI
          message-send-mail-function 'message-send-mail-with-sendmail
          message-sendmail-extra-arguments '("--read-envelope-from")
          message-sendmail-f-is-evil 't

          mu4e-maildir "~/.mail" ; tell mu4e where my Maildir is
          ;; mu4e-get-mail-command "mbsync -a" ; tell mu4e how to sync email
          mu4e-get-mail-command "" ; i am using daemon that checks it for me
          sendmail-program "msmtp"
          ;; mu4e-html2text-command "w3m -T text/html" ; tell mu4e to use w3m for html rendering
          mu4e-html2text-command "html2text -utf8 -width 80"

          ;; default (main) account settings
          mu4e-drafts-folder "/d12frosted/Drafts"
          mu4e-sent-folder "/d12frosted/Sent Mail"
          mu4e-trash-folder "/d12frosted/Trash"
          mu4e-refile-folder "/d12frosted/Archive"
          user-mail-address "d12frosted@icloud.com"
          mu4e-compose-signature "Cheers, Boris.")

    (defvar d12frosted/mu4e-account-alist
      '(("d12frosted"
         (mu4e-drafts-folder "/d12frosted/Drafts")
         (mu4e-sent-folder "/d12frosted/Sent Mail")
         (mu4e-trash-folder "/d12frosted/Trash")
         (mu4e-refile-folder "/d12frosted/Archive")
         (user-mail-address "d12frosted@icloud.com")
         (mu4e-compose-signature "Cheers, Boris."))
        ("boris"
         (mu4e-drafts-folder "/boris/Drafts")
         (mu4e-sent-folder "/boris/Sent Mail")
         (mu4e-trash-folder "/boris/Trash")
         (mu4e-refile-folder "/boris/Archive")
         (user-mail-address "boris.buliga@icloud.com")
         (mu4e-compose-signature "Cheers, Boris."))
        ("timecode"
         (mu4e-drafts-folder "/timecode/Drafts")
         (mu4e-sent-folder "/timecode/Sent Mail")
         (mu4e-trash-folder "/timecode/Trash")
         (mu4e-refile-folder "/timecode/Archive")
         (user-mail-address "boris.buliga@timecode.co")
         (mu4e-compose-signature "Cheers, Boris."))))

    (defun d12frosted/mu4e-set-account ()
      "Set the account for composing a message."
      (let* ((account
              (if mu4e-compose-parent-message
                  (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                    (string-match "/\\(.*?\\)/" maildir)
                    (match-string 1 maildir))
                (completing-read (format "Compose with account: (%s) "
                                         (mapconcat #'(lambda (var) (car var))
                                                    my-mu4e-account-alist "/"))
                                 (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                                 nil t nil nil (caar my-mu4e-account-alist))))
             (account-vars (cdr (assoc account d12frosted/mu4e-account-alist))))
        (if account-vars
            (mapc #'(lambda (var)
                      (set (car var) (cadr var)))
                  account-vars)
          (error "No email account found"))))

    (add-hook 'mu4e-compose-pre-hook 'd12frosted/mu4e-set-account)

    (evil-leader/set-key
      "am" 'mu4e)))
