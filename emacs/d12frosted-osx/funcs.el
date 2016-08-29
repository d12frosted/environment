;;; funcs.el --- d12frosted-osx layer funcs file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Boris <boris@d12frosted.io>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defun d12-osx/get-version ()
  "Get OS version."
  (interactive)
  (string-trim (shell-command-to-string
                "sw_vers  -productVersion")))

(defun d12-osx/run-osascript (string)
  "Run a STRING as osascipt."
  (interactive "sContent of osascript: ")
  (let  ((file (make-temp-file "d12-osx-" nil ".scpt")))
    (with-temp-file file
      (insert string)
      ;;delete the script after execution
      (insert "\ndo shell script \"rm -rf \" & the quoted form of POSIX path of (path to me)"))
    (d12-osx/run-osascript-file file)))

(defun d12-osx/run-osascript-region (p1 p2)
  "Run region as osascipt."
  (interactive "r")
  (d12-osx/run-osascript (buffer-substring p1 p2)))

(defun d12-osx/run-osascript-file (file)
  "Run an osascipt FILE."
  (interactive "f")
  (start-process "OsaScript" "*OsaScript*" "osascript" file)
  (switch-to-buffer-other-window "*OsaScript*")
  (local-set-key (kbd "q") 'kill-this-buffer))

;;; funcs.el ends here
