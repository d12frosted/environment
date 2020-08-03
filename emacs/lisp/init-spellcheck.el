;;; init-spellcheck.el --- spell checking -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 23 Oct 2019
;;
;; URL:
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(require 'init-package)
(require 'init-selection)
(require 'lib-hook)

(use-package ispell
  :config
  (setq ispell-dictionary "english"
        ispell-program-name "aspell")
  (when (equal (file-name-base ispell-program-name) "aspell")
    (add-to-list 'ispell-extra-args "--sug-mode=ultra")))

(use-package flyspell
  :defer t
  :diminish flyspell-mode
  :init
  (+hook 'text-mode-hook #'flyspell-mode)
  (+hook 'prog-mode-hook #'flyspell-prog-mode))

;; remove sometimes later
(with-no-warnings
  (require 'cl))

(use-package flyspell-lazy
  :after flyspell
  :commands (flyspell-lazy-mode)
  :defines (flyspell-lazy-idle-seconds
            flyspell-lazy-window-idle-seconds)
  :config
  (setq flyspell-lazy-idle-seconds 1
        flyspell-lazy-window-idle-seconds 3)
  (flyspell-lazy-mode +1))

(use-package flyspell-correct-ivy
  :if (eq +selection-system 'ivy)
  :defer t
  :commands (flyspell-correct-ivy)
  :init
  (setq flyspell-correct-interface #'flyspell-correct-ivy))

(use-package flyspell-correct
  :if (eq +selection-system 'selectrum)
  :defer t)

(use-package writegood-mode
  :disabled
  :hook (org-mode markdown-mode rst-mode asciidoc-mode latex-mode))

(provide 'init-spellcheck)
;;; init-spellcheck.el ends here
