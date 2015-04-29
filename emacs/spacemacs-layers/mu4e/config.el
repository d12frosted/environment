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

(defvar mu4e-default-account "d12frosted")

(defvar mu4e-account-alist
  '(("d12frosted"
     (mu4e-drafts-folder "/d12frosted/Drafts")
     (mu4e-sent-folder "/d12frosted/Sent")
     (mu4e-trash-folder "/d12frosted/Trash")
     (mu4e-refile-folder "/d12frosted/Archive")
     (mu4e-sent-messages-behavior sent)
     (user-mail-address "d12frosted@icloud.com")
     (user-full-name  "Boris")
     (mu4e-compose-signature "Cheers, Boris."))
    ("boris"
     (mu4e-drafts-folder "/boris/Drafts")
     (mu4e-sent-folder "/boris/Sent")
     (mu4e-trash-folder "/boris/Trash")
     (mu4e-refile-folder "/boris/Archive")
     (mu4e-sent-messages-behavior sent)
     (user-mail-address "boris.buliga@icloud.com")
     (user-full-name  "Boris Buliga")
     (mu4e-compose-signature "Cheers, Boris."))
    ("timecode"
     (mu4e-drafts-folder "/timecode/Drafts")
     (mu4e-sent-folder "/timecode/Sent Mail")
     (mu4e-trash-folder "/timecode/Trash")
     (mu4e-refile-folder "/timecode/Archive")
     (mu4e-sent-messages-behavior delete)
     (user-mail-address "boris.buliga@timecode.co")
     (user-full-name  "Boris Buliga")
     (mu4e-compose-signature "Cheers, Boris."))))
