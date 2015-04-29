;;; config.el --- mu4e Layer config File for Spacemacs
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
