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

    (when (fboundp 'imagemagick-register-types)
      (imagemagick-register-types))

    (setq mu4e-maildir "~/.mail" ; tell mu4e where my Maildir is
          mu4e-use-fancy-chars t ; should be executed only for GUI
          mu4e-get-mail-command "sh ~/.environment/email/gendalf.sh mu4e 1"
          ;; mu4e-html2text-command "html2text -utf8 -width 80"
          ;; mu4e-html2text-command "textutil -stdin -format html -convert txt -stdout"
          mu4e-html2text-command "w3m -dump -cols 80 -T text/html"
          mu4e-view-show-images t
          mu4e-update-interval 60
          mu4e-bookmarks
          '(("flag:unread AND NOT flag:trashed"     "Unread messages"               ?u)
            ("date:today..now AND NOT flag:trashed" "Today's messages"              ?t)
            ("date:today..now"                      "Today's messages (with Trash)" ?T)
            ("date:7d..now AND NOT flag:trashed"    "Last 7 days"                   ?w)
            ("date:7d..now"                         "Last 7 days (with Trash)"      ?W))

          message-send-mail-function 'message-send-mail-with-sendmail
          message-sendmail-extra-arguments '("--read-envelope-from")
          message-sendmail-f-is-evil 't

          sendmail-program "msmtp"
          mail-user-agent 'mu4e-user-agent)

    (mu4e-set-account-vars mu4e-default-account)

    (add-to-list 'mu4e-view-actions
                 '("View in browser" . mu4e-msgv-action-view-in-browser) t)

    (add-hook 'mu4e-compose-pre-hook 'mu4e-set-account)

    (evil-leader/set-key
      "am" 'mu4e)))
