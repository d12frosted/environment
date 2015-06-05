;;; completion-configs.el --- configs file of completion configurations
;;
;; Copyright (c) 2014-2015 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@icloud.com>
;; Maintainer: Boris Buliga <d12frosted@icloud.com>
;; Created: 04 May 2015
;;
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

;;; Code

;;; Configurable variables
;; ========================

;; not used for now
(defvar auto-completion-enable-company-yasnippet nil
  "If non nil enable yasnippet for all company backends.
Not used for now.")

(defvar auto-completion-enable-company-help-tooltip t
  "If non nil the docstring appears in a tooltip.")

(defvar company-mode-completion-cancel-keywords
  '("do"
    "then"
    "begin"
    "case")
  "Keywords on which to cancel completion so that you can use RET
to complet without blocking common line endings.")

;;; Toggle
;; ========

(d12|add-toggle auto-completion
                :status
                (if (eq 'company auto-completion-front-end)
                    company-mode
                  auto-complete-mode)
                :on
                (progn
                  (if (eq 'company auto-completion-front-end)
                      (company-mode)
                    (auto-complete-mode))
                  (message "Enabled auto-completion (using %S)."
                           auto-completion-front-end))
                :off
                (progn
                  (if (eq 'company auto-completion-front-end)
                      (company-mode -1)
                    (auto-complete-mode -1))
                  (message "Disabled auto-completion."))
                :documentation "Activate auto-completion."
                :bind-global "C-c t a")

;;; Packages configurations
;; =========================

(use-package ac-ispell
  :ensure t
  :defer t
  :init
  (setq ac-ispell-requires 4)
  (eval-after-load 'auto-complete
    '(ac-ispell-setup)))

(use-package auto-complete
  :ensure t
  :defer t
  :init
  (setq ac-auto-start 0
        ac-delay 0.2
        ac-quick-help-delay 1.
        ac-use-fuzzy t
        ac-fuzzy-enable t
        ac-comphist-file (concat d12/cache-directory "ac-comphist.dat")
        ;; use 'complete when auto-complete is disabled
        tab-always-indent 'complete
        ac-dwim t)
  :config
  (require 'auto-complete-config)
  (setq-default ac-sources '(ac-source-abbrev
                             ac-source-dictionary
                             ac-source-words-in-same-mode-buffers))
  ;; todo - when yasnippet
  ;; (push 'ac-source-yasnippet ac-sources)
  (add-to-list 'completion-styles 'initials t)
  (define-key ac-completing-map (kbd "C-j") 'ac-next)
  (define-key ac-completing-map (kbd "C-k") 'ac-previous)
  (define-key ac-completing-map (kbd "<S-tab>") 'ac-previous)
  (d12|diminish auto-complete-mode " (A)"))

(use-package company
  :ensure t
  :defer t
  :init
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-tooltip-flip-when-above t
        company-frontends '(company-pseudo-tooltip-frontend)
        company-clang-prefix-guesser 'company-mode/more-than-prefix-guesser)
  :config
  (d12|diminish company-mode " (A)")
  (bind-keys
   :map company-active-map
   ("<tab>" . company-complete-common-or-cycle)
   ("C-/" . company-search-candidates)
   ("C-M-/" . company-filter-candidates))
  ;; Nicer looking faces
  (custom-set-faces
   '(company-tooltip-common
     ((t (:inherit company-tooltip :weight bold :underline nil))))
   '(company-tooltip-common-selection
     ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
  ;; Transformers
  (defun company-transformer-cancel (candidates)
    "Cancel completion if prefix is in the list
      `company-mode-completion-cancel-keywords'"
    (unless (and (member company-prefix company-mode-completion-cancel-keywords)
                 (not auto-completion-use-tab-instead-of-enter))
      candidates))
  (setq company-transformers '(company-transformer-cancel
                               company-sort-by-occurrence)))

(use-package company-quickhelp
  :if (and auto-completion-enable-company-help-tooltip
           (display-graphic-p))
  :ensure t
  :defer t
  :init (add-hook 'company-mode-hook 'company-quickhelp-mode))
