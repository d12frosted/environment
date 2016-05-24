;;; config.el --- d12frosted-core layer config file for Spacemacs.
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

;; require all modules
(require 'd12-files)
(when (configuration-layer/layer-usedp 'helm)
  (require 'd12-helm))
(require 'd12-dir-settings)
(require 'd12-copy-paste)
(require 'd12-comment)
(require 'd12-eww)

(when (configuration-layer/layer-usedp 'helm)
  (add-to-list 'd12-helm/sources 'd12-helm/config-source))

;;; Auto modes

(add-to-list 'auto-mode-alist '("SConstruct" . python-mode))
(add-to-list 'auto-mode-alist '("SConscript" . python-mode))

;;; Hooks

(add-hook 'find-file-hook 'd12-dir-settings/load)
(add-hook 'company-mode-hook 'company-quickhelp-mode)
(add-hook 'prog-mode-hook 'vimish-fold-mode)
(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'after-save-hook 'delete-trailing-whitespace)

;;; Configs

(defvar d12-env-shell-type (d12/get-env-shell-type)
  "Type of shell. Available types are `fish', `bash' and `zsh'.
  If you wish to add support for more types checkout
  `d12/setup-env-shell-type'")

(setq-default
 ;; Miscellaneous
 vc-follow-symlinks t
 require-final-newline t

 ;; Whitespace mode
 whitespace-style '(face tabs tab-mark)
 whitespace-display-mappings
 '((newline-mark 10 [172 10])
   (tab-mark 9 [9655 9]))

 ;; Shell
 sh-basic-offset 2
 sh-indentation 2)

(delete-selection-mode 1)

;; setup scrolling
(setq scroll-margin 0                   ; Drag the point along while scrolling
      scroll-conservatively 1000        ; Never recenter the screen while scrolling
      scroll-error-top-bottom t         ; Move to beg/end of buffer before
                                        ; signalling an error
      ;; These settings make trackpad scrolling on OS X much more predictable
      ;; and smooth
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(1))

;; OS X
(if (spacemacs/system-is-mac)
    (setq mac-command-modifier 'meta
          mac-option-modifier  'none))

;; python
(defun pyenv-mode-versions ()
  "List installed python versions."
  (let ((versions (shell-command-to-string "vf ls")))
    (delete-dups (cons "system" (split-string versions)))))

;; d12-ivy
(defvar d12-ivy--files '()
  "List of interesting files to discover via ivy interface.")

(setq d12-ivy--config-files `(,(concat d12-path/emacs-layers "init.el")
                              ,(concat d12-path/emacs-private "private.el")
                              ,(concat d12-path/fish-public "config.fish")
                              ,(concat d12-path/fish-private "preconfig.fish")
                              ,(concat d12-path/fish-private "postconfig.fish")
                              ,(concat d12-path/emacs-home "init.el")))

;; Langs
(quail-define-package
 "d12-ukrainian" "UTF-8" "UK" t "Input method for Ukrainian based
 on OS X Ukrainian keyboard layout with some custom
 modifications." nil t t t t nil nil nil nil nil t)

(quail-define-rules
 ("`" ?`)
 ("1" ?1)
 ("2" ?2)
 ("3" ?3)
 ("4" ?4)
 ("5" ?5)
 ("6" ?6)
 ("7" ?7)
 ("8" ?8)
 ("9" ?9)
 ("0" ?0)
 ("-" ?-)
 ("=" ?=)
 ("q" ?й)
 ("w" ?ц)
 ("e" ?у)
 ("r" ?к)
 ("t" ?е)
 ("y" ?н)
 ("u" ?г)
 ("i" ?ш)
 ("o" ?щ)
 ("p" ?з)
 ("[" ?х)
 ("]" ?ї)
 ("\\" ?ґ)
 ("a" ?ф)
 ("s" ?и)
 ("d" ?в)
 ("f" ?а)
 ("g" ?п)
 ("h" ?р)
 ("j" ?о)
 ("k" ?л)
 ("l" ?д)
 (";" ?ж)
 ("'" ?є)
 ("z" ?я)
 ("x" ?ч)
 ("c" ?с)
 ("v" ?м)
 ("b" ?і)
 ("n" ?т)
 ("m" ?ь)
 ("," ?б)
 ("." ?ю)
 ("/" ?/)

 ("~" ?~)
 ("!" ?!)
 ("@" ?\")
 ("#" ?#)
 ("$" ?\%)
 ("%" ?:)
 ("^" ?,)
 ("&" ?\.)
 ("*" ?\;)
 ("(" ?\()
 (")" ?\))
 ("_" ?_)
 ("+" ?\+)
 ("Q" ?Й)
 ("W" ?Ц)
 ("E" ?У)
 ("R" ?К)
 ("T" ?Е)
 ("Y" ?Н)
 ("U" ?Г)
 ("I" ?Ш)
 ("O" ?Щ)
 ("P" ?З)
 ("{" ?Х)
 ("}" ?Ї)
 ("|" ?Ґ)
 ("A" ?Ф)
 ("S" ?И)
 ("D" ?В)
 ("F" ?А)
 ("G" ?П)
 ("H" ?Р)
 ("J" ?О)
 ("K" ?Л)
 ("L" ?Д)
 (":" ?Ж)
 ("\"" ?Є)
 ("Z" ?Я)
 ("X" ?Ч)
 ("C" ?С)
 ("V" ?М)
 ("B" ?І)
 ("N" ?Т)
 ("M" ?Ь)
 ("<" ?Б)
 (">" ?Ю)
 ("?" ?\?)

 ("¢" ?$)

 ("ß" ?ы)
 ("‘" ?ъ)
 ("æ" ?э)
 ("Æ" ?Э)
 ("«" ?ё)
 ("’" ?Ъ)
 ("»" ?Ё))

;;; config.el ends here
