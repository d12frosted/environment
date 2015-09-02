;;; mu4e-configs.el --- configs file of mu4e configurations
;;
;; Copyright (c) 2014-2015 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@icloud.com>
;; Maintainer: Boris Buliga <d12frosted@icloud.com>
;; Created: 02 May 2015
;;
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

;;; Code

;;; Configurable variables
;; ========================

;; todo - consider removing this variable
(defvar mu4e/update-interval 600)

(defvar mu4e/folders-alist
  '(("example"
     (mu4e-drafts-folder "/example/Drafts")
     (mu4e-sent-folder "/example/Sent")
     (mu4e-trash-folder "/example/Trash")
     (mu4e-refile-folder "/example/Archive"))))

(defvar mu4e/accounts-alist
  '(("example"
     (mu4e-sent-messages-behavior delete)
     (user-mail-address "example@example.com")
     (user-full-name  "John Doe")
     (mu4e-compose-signature "Sent with love using emacs and mu4e."))))

;;; mu4e configurations
;; =====================

(setq-default mu4e/default-account "d12frosted"
              mu4e/update-interval 60
              mu4e/folders-alist
              '(("d12frosted"
                 (mu4e-drafts-folder "/d12frosted/Drafts")
                 (mu4e-sent-folder "/d12frosted/Sent")
                 (mu4e-trash-folder "/d12frosted/Trash")
                 (mu4e-refile-folder "/d12frosted/Archive"))
                ("boris"
                 (mu4e-drafts-folder "/boris/Drafts")
                 (mu4e-sent-folder "/boris/Sent")
                 (mu4e-trash-folder "/boris/Trash")
                 (mu4e-refile-folder "/boris/Archive"))
                ("timecode"
                 (mu4e-drafts-folder "/timecode/Drafts")
                 (mu4e-sent-folder "/timecode/Sent Mail")
                 (mu4e-trash-folder "/timecode/Trash")
                 (mu4e-refile-folder "/timecode/Archive")))
              mu4e/accounts-alist
              '(("d12frosted"
                 (mu4e-sent-messages-behavior sent)
                 (user-mail-address "d12frosted@icloud.com")
                 (user-full-name  "Boris")
                 (mu4e-compose-signature "Cheers, Boris."))
                ("boris"
                 (mu4e-sent-messages-behavior sent)
                 (user-mail-address "boris.buliga@icloud.com")
                 (user-full-name  "Boris Buliga")
                 (mu4e-compose-signature "Cheers, Boris."))
                ("timecode"
                 (mu4e-sent-messages-behavior delete)
                 (user-mail-address "boris.buliga@timecode.co")
                 (user-full-name  "Boris Buliga")
                 (mu4e-compose-signature "Cheers, Boris.")))
              )

(use-package mu4e
  :load-path "packages/mu4e/"
  :defer 1
  :bind ("C-c a m" . mu4e)
  :config

  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  (setq mu4e/current-account (car (car mu4e/accounts-alist))
        mu4e-update-interval mu4e/update-interval
        mu4e-trash-folder 'mu4e/trash-folder-fn
        mu4e-refile-folder 'mu4e/refile-folder-fn
        mu4e-drafts-folder 'mu4e/drafts-folder-fn
        mu4e-sent-folder 'mu4e/sent-folder-fn
        ;; This shouldn't be necessary, but see https://github.com/djcb/mu/issues/399.
        mu4e-user-mail-address-list (list user-mail-address)
        message-kill-buffer-on-exit t
        mu4e-compose-dont-reply-to-self t
         mu4e-bookmarks
        '(("flag:unread AND NOT flag:trashed"            "Unread messages"               ?u)
          ("date:;TODO: oday..now AND NOT flag:trashed"  "Today's messages"              ?t)
          ("date:today..now"                             "Today's messages (with Trash)" ?T)
          ("date:7d..now AND NOT flag:trashed"           "Last 7 days"                   ?w)
          ("date:7d..now"                                "Last 7 days (with Trash)"      ?W))
        ;; mu4e-html2text-command "html2text -utf8 -width 80"
        ;; mu4e-html2text-command "textutil -stdin -format html -convert txt -stdout"
        ;; mu4e-html2text-command            "w3m -dump -cols 80 -T text/html"
        mu4e-html2text-command            "w3m -dump -cols 110 -T text/html"
        mu4e-view-fields '(:from :to :cc :subject :date :tags :attachments)
        mu4e-maildir "~/.mail"
        mu4e-headers-visible-lines 16
        mu4e-use-fancy-chars t                                            ;shouldbeexecutedonlyforGUI
        mu4e-headers-draft-mark '("D" . "D")                              ;draft
        mu4e-headers-seen-mark '("S" . "○")                               ;seen
        mu4e-headers-unseen-mark '("u" . "●")                             ;unseen
        mu4e-headers-flagged-mark '("F" . "⭐︎")                            ;flagged
        mu4e-headers-new-mark '("N" . "❀")                                ;new
        mu4e-headers-replied-mark '("R" . "↵")                            ;replied
        mu4e-headers-passed-mark '("P" . "⇉")                             ;passed
        mu4e-headers-encrypted-mark '("x" . "♯")                          ;encrypted
        mu4e-headers-signed-mark '("s" . "✍")                             ;signed
        mu4e-headers-attach-mark '("a" . "⚓︎")
        mu4e-get-mail-command "sh ~/.environment/email/gendalf.sh mu4e 1" ;todo-usethevalueofupdateinterval
        mu4e-view-show-images t
        mu4e-attachment-dir "~/Downloads/p"
        mu4e-view-image-max-width 700
        message-send-mail-function 'message-send-mail-with-sendmail
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-sendmail-f-is-evil 't
        sendmail-program "msmtp"
        mail-user-agent 'mu4e-user-agent)

  (mu4e/set-account-vars mu4e/current-account)

  (add-to-list 'mu4e-view-actions
               '("View In Browser" . mu4e-action-view-in-browser) t)

  (add-hook 'mu4e-compose-pre-hook 'mu4e/set-account-for-composing))

;;; mu4e maildirs extension
;; =========================

(use-package mu4e-maildirs-extension
  :ensure t
  :defer 1
  :config
  (mu4e-maildirs-extension)
  (add-hook 'mu4e-index-updated-hook 'mu4e-maildirs-extension-index-updated-handler)
  (add-hook 'mu4e-main-mode-hook 'mu4e-maildirs-extension-index-updated-handler)
  (setq mu4e-maildirs-extension-maildir-separator "*"
        mu4e-maildirs-extension-submaildir-separator "»"))
