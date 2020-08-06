;;; init-selection.el --- selecting candidates -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 03 Aug 2020
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

(require 'init-project)
(require 'init-keybindings)
(require 'init-package)
(require 'subr-x)

(defvar +selection-system 'ivy
  "Selection system to use: ivy or selectrum.")

(use-package counsel
  :if (eq +selection-system 'ivy)
  :diminish ivy-mode counsel-mode
  :commands (counsel-find-file
             counsel-file-jump)
  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode))
  :bind (("M-x" . counsel-M-x))
  :init
  (setq
   enable-recursive-minibuffers t
   ivy-use-selectable-prompt t
   ivy-use-virtual-buffers t
   ivy-height 10
   ivy-count-format "(%d/%d) "
   ivy-on-del-error-function nil)
  :config
  (setq
   ivy-initial-inputs-alist
   '((counsel-minor . "^+")
		 (counsel-package . "^+")
		 (counsel-org-capture . "^")
		 (counsel-M-x . "^+?")
		 (counsel-describe-function . "^+?")
		 (counsel-describe-variable . "^+?"))))

(use-package ivy-prescient
  :if (eq +selection-system 'ivy)
  :hook (ivy-mode . ivy-prescient-mode)
  :defines (ivy-prescient-retain-classic-highlighting
            ivy-prescient-sort-commands)
  :config
  (setq ivy-prescient-sort-commands
        '(:not swiper swiper-isearch ivy-switch-buffer counsel-grep
               counsel-git-grep counsel-ag counsel-rg counsel-imenu
               counsel-yank-pop counsel-recentf counsel-buffer-or-recentf)
        ivy-prescient-retain-classic-highlighting t))

(use-package ivy-rich
  :if (eq +selection-system 'ivy)
  :after ivy
  :commands (ivy-rich-mode)
  :init
  (ivy-rich-mode))

(use-package selectrum
  :if (eq +selection-system 'selectrum)
  :hook ((after-init . selectrum-mode)))

(use-package selectrum-prescient
  :if (eq +selection-system 'selectrum)
  :hook (selectrum-mode . selectrum-prescient-mode))

(use-package ctrlf
  :if (eq +selection-system 'selectrum)
  :hook (selectrum-mode . ctrlf-mode))

(use-package prescient
  :defer t
  :commands (prescient-persist-mode
             prescient--with-group
             prescient--initials-regexp
             prescient--fuzzy-regexp
             prescient-split-query)
  :defines (prescient-filter-method prescient-save-file)
  :init
  (setq prescient-filter-method '(literal regexp initialism))
  :config
  (setq prescient-save-file (concat +path-cache-dir "prescient-save.el"))
  (prescient-persist-mode +1)

  ;; remove once https://github.com/raxod502/prescient.el/pull/66 is merged
  (defun prescient-filter-regexps (query &optional with-groups)
    "Convert QUERY to list of regexps.
Each regexp must match the candidate in order for a candidate to
match the QUERY.

If WITH-GROUPS is non-nil, enclose the initials in initialisms
with capture groups. If it is the symbol `all', additionally
enclose literal substrings with capture groups."
    (mapcar
     (lambda (subquery)
       (string-join
        (cl-remove
         nil
         (mapcar
          (lambda (method)
            (pcase method
              (`literal
               (prescient--with-group
                (if (eq search-default-mode #'char-fold-to-regexp)
                    (char-fold-to-regexp (regexp-quote subquery))
                  (regexp-quote subquery))
                (eq with-groups 'all)))
              (`initialism
               (prescient--initials-regexp subquery with-groups))
              (`regexp
               (ignore-errors
                 ;; Ignore regexp if it's malformed.
                 (string-match-p subquery "")
                 subquery))
              (`fuzzy
               (prescient--fuzzy-regexp subquery with-groups))))
          (pcase prescient-filter-method
            ;; We support `literal+initialism' for backwards
            ;; compatibility.
            (`literal+initialism '(literal initialism))
            ((and (pred listp) x) x)
            (x (list x))))
         :test #'eq)
        "\\|"))
     (prescient-split-query query))))

(provide 'init-selection)
;;; init-selection.el ends here
