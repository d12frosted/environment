;;; syntax-checking-configs.el --- configs file of syntax checking configurations
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

(use-package flycheck
  :ensure t
  :defer t
  :bind (("C-c u e c" . flycheck-clear)
         ("C-c u e l" . flycheck-list-errors))
  :init
  (setq flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-standard-error-navigation nil)
  (d12|add-toggle syntax-checking
                  :status flycheck-mode
                  :on (flycheck-mode)
                  :off (flycheck-mode -1)
                  :documentation "Enable error and syntax checking."
                  :bind-global "C-c t f")
  :config
  (d12|diminish flycheck-mode " Ⓕ")
  ;; color mode line faces
  (defun d12/defface-flycheck-mode-line-color (state)
    "Define a face for the given Flycheck STATE."
    (let* ((fname (intern (format "d12/mode-line-flycheck-%s-face"
                                  (symbol-name state))))
           (foreground (face-foreground
                        (intern (format "flycheck-fringe-%s" state)))))
      (eval `(defface ,fname '((t ()))
               ,(format "Color for Flycheck %s feedback in mode line."
                        (symbol-name state))
               :group 'd12))
      (set-face-attribute fname nil
                          :foreground foreground
                          :box (face-attribute 'mode-line :box))))

  (defun d12/set-flycheck-mode-line-faces ()
    "Define or set the flycheck info mode-line faces."
    (mapcar 'd12/defface-flycheck-mode-line-color
            '(error warning info)))

  (d12/set-flycheck-mode-line-faces)

  (defmacro d12|custom-flycheck-lighter (error)
    "Return a formatted string for the given ERROR (error, warning, info)."
    `(let* ((error-counts (flycheck-count-errors
                           flycheck-current-errors))
            (errorp (flycheck-has-current-errors-p ',error))
            (err (or (cdr (assq ',error error-counts)) "?"))
            (running (eq 'running flycheck-last-status-change)))
       (if (or errorp running) (format "•%s " err))))

  ;; Custom fringe indicator
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'my-flycheck-fringe-indicator
      (vector #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00011100
              #b00111110
              #b00111110
              #b00111110
              #b00011100
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b01111111)))

  (flycheck-define-error-level 'error
                               :overlay-category 'flycheck-error-overlay
                               :fringe-bitmap 'my-flycheck-fringe-indicator
                               :fringe-face 'flycheck-fringe-error)

  (flycheck-define-error-level 'warning
                               :overlay-category 'flycheck-warning-overlay
                               :fringe-bitmap 'my-flycheck-fringe-indicator
                               :fringe-face 'flycheck-fringe-warning)

  (flycheck-define-error-level 'info
                               :overlay-category 'flycheck-info-overlay
                               :fringe-bitmap 'my-flycheck-fringe-indicator
                               :fringe-face 'flycheck-fringe-info))

(use-package flycheck-pos-tip
  :ensure t
  :defer t
  :init
  (setq flycheck-display-errors-function 'flycheck-pos-tip-error-messages))

(use-package flyspell
  :ensure t
  :defer t
  :bind (("C-c u s" . ispell-word)
         ("C-c u S b" . flyspell-buffer)
         ("C-c u S n" . flyspell-goto-next-error))
  :init
  (setq-default ispell-program-name "aspell")
  (setq-default ispell-dictionary "english")
  (add-hook 'markdown-mode-hook '(lambda () (flyspell-mode 1)))
  (add-hook 'text-mode-hook '(lambda () (flyspell-mode 1)))
  (d12|add-toggle spelling-checking
                  :status flyspell-mode
                  :on (flyspell-mode)
                  :off (flyspell-mode -1)
                  :documentation
                  "Enable flyspell for automatic spelling checking."
                  :bind-global "C-c t s")
  :config
  (flyspell-prog-mode)
  (d12|diminish flyspell-mode " Ⓢ"))
