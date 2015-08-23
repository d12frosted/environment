;;; extensions.el --- d12frosted Layer extensions File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Boris Buliga & Contributors
;;
;; Author: Boris Buliga <d12frosted@icloud.com>
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq d12frosted-pre-extensions
      '(
        ;; pre extension d12frosteds go here
        mu4e
        ))

(setq d12frosted-post-extensions
      '(
        ;; post extension d12frosteds go here
        ))

(defun d12frosted/init-mu4e ()
  "Initialize mu4e extension."
  (use-package mu4e
    :load-path d12-mu4e/installation-path
    :commands mu4e
    :defer t
    :init
    (progn
      (load-file d12-mu4e/private-config-path)
      (evil-leader/set-key "a M" 'mu4e)
      (global-set-key (kbd "C-x m") 'mu4e-compose-new))
    :config
    (progn
      (spacemacs|evilify-map mu4e-main-mode-map
        :mode mu4e-main-mode
        :bindings
        (kbd "j") 'mu4e~headers-jump-to-maildir)
      (spacemacs|evilify-map mu4e-headers-mode-map :mode mu4e-headers-mode)
      (spacemacs|evilify-map mu4e-view-mode-map :mode mu4e-view-mode)

      ;; (require 'mu4e-contrib)
      (setq mu4e-completing-read-function 'helm--completing-read-default

            ;; mu4e essentials
            mu4e-maildir d12-mu4e/maildir-path
            mu4e-trash-folder 'd12-mu4e/trash-folder-fn
            mu4e-refile-folder 'd12-mu4e/refile-folder-fn
            mu4e-drafts-folder 'd12-mu4e/drafts-folder-fn
            mu4e-sent-folder 'd12-mu4e/sent-folder-fn
            d12-mu4e/current-account (car (car d12-mu4e/accounts-alist))

            ;; sendmail essentials
            sendmail-program "msmtp"
            message-send-mail-function 'message-send-mail-with-sendmail
            message-sendmail-extra-arguments '("--read-envelope-from")
            message-sendmail-f-is-evil 't

            ;; customizations
            ;; This shouldn't be necessary, but see https://github.com/djcb/mu/issues/399.
            ;; mu4e-user-mail-address-list (list user-mail-address)
            message-kill-buffer-on-exit t
            mu4e-compose-dont-reply-to-self t
            ;; mu4e-bookmarks
            ;; '(("flag:unread AND NOT flag:trashed"            "Unread messages"               ?u)
            ;;   ("date:;TODO: oday..now AND NOT flag:trashed"  "Today's messages"              ?t)
            ;;   ("date:today..now"                             "Today's messages (with Trash)" ?T)
            ;;   ("date:7d..now AND NOT flag:trashed"           "Last 7 days"                   ?w)
            ;;   ("date:7d..now"                                "Last 7 days (with Trash)"      ?W))
            ;; mu4e-html2text-command 'mu4e-shr2text
            ;; mu4e-html2text-command 'html2text
            mu4e-html2text-command "pandoc -s -r html -t markdown_github"
            ;; mu4e-view-fields '(:from :to :cc :subject :date :tags :attachments)
            ;; mu4e-headers-visible-lines 16
            ;; mu4e-get-mail-command "sh ~/.environment/email/gendalf.sh mu4e 1" ;todo-usethevalueofupdateinterval
            mu4e-view-show-images t
            ;; mu4e-attachment-dir "~/Downloads/p"
            mu4e-view-image-max-width 700
            mail-user-agent 'mu4e-user-agent
            )

      (d12-mu4e/set-account-vars d12-mu4e/current-account)

      (add-to-list 'mu4e-view-actions
                   '("View in browser" . d12-mu4e/msgv-action-view-in-browser) t)
      (add-hook 'mu4e-compose-pre-hook 'd12-mu4e/set-account)
      (add-hook 'message-sent-hook 'd12-mu4e/mail-account-reset)
      )))

;; For each extension, define a function d12frosted/init-<extension-d12frosted>
;;
;; (defun d12frosted/init-my-extension ()
;;   "Initialize my extension"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
