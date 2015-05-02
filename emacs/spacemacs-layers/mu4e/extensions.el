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

    (setq mu4e/current-account (car (car mu4e/accounts-alist))
          mu4e-update-interval mu4e/update-interval
          mu4e-trash-folder 'mu4e/trash-folder-fn
          mu4e-refile-folder 'mu4e/refile-folder-fn
          mu4e-drafts-folder 'mu4e/drafts-folder-fn
          mu4e-sent-folder 'mu4e/sent-folder-fn
          ;; This shouldn't be necessary, but see https://github.com/djcb/mu/issues/399.
          mu4e-user-mail-address-list (list user-mail-address)
          message-kill-buffer-on-exit t
          mu4e-compose-dont-reply-to-self t)

    ;; don't forget to
    ;; $ mu mkdir ~/Maildir/queue
    ;; $ touch ~/Maildir/queue/.noindex

    (mu4e/set-account-vars mu4e/current-account)

    (add-to-list 'mu4e-view-actions
                 '("View In Browser" . mu4e-action-view-in-browser) t)

    (add-hook 'mu4e-compose-pre-hook 'mu4e/set-account-for-composing)

    (evil-leader/set-key
      "am" 'mu4e)))
