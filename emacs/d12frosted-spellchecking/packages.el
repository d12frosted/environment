;;; packages.el --- d12frosted-spellchecking layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Boris Buliga <d12frosted@d12frosted.local>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defconst d12frosted-spellchecking-packages
  '(writegood-mode
    ;; langtool
    flyspell-correct
    osx-dictionary))

(defun d12frosted-spellchecking/init-writegood-mode ()
  (use-package writegood-mode
      :commands (writegood-mode)))

(defun d12frosted-spellchecking/post-init-flyspell-correct ()
  (setq
   ;; ispell-program-name (executable-find "hunspell")
   flyspell-issue-message-flag nil))

(defun d12frosted-spellchecking/init-osx-dictionary ()
  (use-package osx-dictionary
    :commands (osx-dictionary-search-pointer
               osx-dictionary-search-input
               osx-dictionary-cli-find-or-recompile)
    :init
    (progn
      (spacemacs/set-leader-keys "xwd" 'osx-dictionary-search-pointer)
      (spacemacs/set-leader-keys "xwD" 'osx-dictionary-search-input))
    :config
    (progn
      (evilified-state-evilify-map osx-dictionary-mode-map
        :mode osx-dictionary-mode
        :bindings
        "q" 'osx-dictionary-quit
        "r" 'osx-dictionary-read-word
        "s" 'osx-dictionary-search-input
        "o" 'osx-dictionary-open-dictionary.app))))

;;; packages.el ends here
