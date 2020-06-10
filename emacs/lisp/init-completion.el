;;; init-completion.el --- auto completion feature -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019 Boris
;;
;; Author: Boris <boris@d12frosted.io>
;; Maintainer: Boris <boris@d12frosted.io>
;;
;; Created: 22 Oct 2019
;;
;; URL: https://github.com/d12frosted/environment/emacs
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(require 'lib-list)
(require 'init-path)
(require 'init-package)
(require 'init-ui)
(require '+company)

(put '+company-init-backends 'permanent-local-hook t)

(use-package company
  :defer 2
  :diminish
  :defines (company-backends)
  :commands (company-complete-common
             company-manual-begin
             company-grab-line
             global-company-mode)
  :init
  (add-hook 'post-self-insert-hook #'+company-mode-maybe)
  (setq-default
   company-minimum-prefix-length 2
   company-tooltip-limit 14
   company-tooltip-align-annotations t
   company-require-match 'never
   company-global-modes
   '(not erc-mode message-mode help-mode gud-mode eshell-mode)
   company-backends '(company-capf)
   company-frontends
   '(company-pseudo-tooltip-frontend
     company-echo-metadata-frontend))
  :config
  (add-hook 'company-mode-hook #'+company-init-backends)
  (global-company-mode +1))

(use-package company-prescient
  :hook (company-mode . company-prescient-mode)
  :defines (prescient-save-file)
  :commands (prescient-persist-mode)
  :config
  (setq prescient-save-file (concat +path-cache-dir "prescient-save.el"))
  (prescient-persist-mode +1))

(use-package company-box
  :hook (company-mode . company-box-mode)
  :init
  (setq company-box-show-single-candidate t
        company-box-backends-colors nil
        company-box-max-candidates 50
        company-box-icons-alist 'company-box-icons-all-the-icons
        company-box-icons-functions
        '(+company-box-icons--yasnippet-fn
          company-box-icons--lsp
          +company-box-icons--elisp-fn
          company-box-icons--acphp)
        company-box-icons-all-the-icons
        `((Unknown       . ,(all-the-icons-material "find_in_page"             :height 0.8 :face 'all-the-icons-purple))
          (Text          . ,(all-the-icons-material "text_fields"              :height 0.8 :face 'all-the-icons-green))
          (Method        . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))
          (Function      . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))
          (Constructor   . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))
          (Field         . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))
          (Variable      . ,(all-the-icons-material "adjust"                   :height 0.8 :face 'all-the-icons-blue))
          (Class         . ,(all-the-icons-material "class"                    :height 0.8 :face 'all-the-icons-red))
          (Interface     . ,(all-the-icons-material "settings_input_component" :height 0.8 :face 'all-the-icons-red))
          (Module        . ,(all-the-icons-material "view_module"              :height 0.8 :face 'all-the-icons-red))
          (Property      . ,(all-the-icons-material "settings"                 :height 0.8 :face 'all-the-icons-red))
          (Unit          . ,(all-the-icons-material "straighten"               :height 0.8 :face 'all-the-icons-red))
          (Value         . ,(all-the-icons-material "filter_1"                 :height 0.8 :face 'all-the-icons-red))
          (Enum          . ,(all-the-icons-material "plus_one"                 :height 0.8 :face 'all-the-icons-red))
          (Keyword       . ,(all-the-icons-material "filter_center_focus"      :height 0.8 :face 'all-the-icons-red))
          (Snippet       . ,(all-the-icons-material "short_text"               :height 0.8 :face 'all-the-icons-red))
          (Color         . ,(all-the-icons-material "color_lens"               :height 0.8 :face 'all-the-icons-red))
          (File          . ,(all-the-icons-material "insert_drive_file"        :height 0.8 :face 'all-the-icons-red))
          (Reference     . ,(all-the-icons-material "collections_bookmark"     :height 0.8 :face 'all-the-icons-red))
          (Folder        . ,(all-the-icons-material "folder"                   :height 0.8 :face 'all-the-icons-red))
          (EnumMember    . ,(all-the-icons-material "people"                   :height 0.8 :face 'all-the-icons-red))
          (Constant      . ,(all-the-icons-material "pause_circle_filled"      :height 0.8 :face 'all-the-icons-red))
          (Struct        . ,(all-the-icons-material "streetview"               :height 0.8 :face 'all-the-icons-red))
          (Event         . ,(all-the-icons-material "event"                    :height 0.8 :face 'all-the-icons-red))
          (Operator      . ,(all-the-icons-material "control_point"            :height 0.8 :face 'all-the-icons-red))
          (TypeParameter . ,(all-the-icons-material "class"                    :height 0.8 :face 'all-the-icons-red))
          ;; (Template   . ,(company-box-icons-image "Template.png"))))
          (Yasnippet     . ,(all-the-icons-material "short_text"               :height 0.8 :face 'all-the-icons-green))
          (ElispFunction . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))
          (ElispVariable . ,(all-the-icons-material "check_circle"             :height 0.8 :face 'all-the-icons-blue))
          (ElispFeature  . ,(all-the-icons-material "stars"                    :height 0.8 :face 'all-the-icons-orange))
          (ElispFace     . ,(all-the-icons-material "format_paint"             :height 0.8 :face 'all-the-icons-pink))))

  (defun +company-box-icons--yasnippet-fn (candidate)
    (when (get-text-property 0 'yas-annotation candidate)
      'Yasnippet))

  (defun +company-box-icons--elisp-fn (candidate)
    (when (derived-mode-p 'emacs-lisp-mode)
      (let ((sym (intern candidate)))
        (cond ((fboundp sym)  'ElispFunction)
              ((boundp sym)   'ElispVariable)
              ((featurep sym) 'ElispFeature)
              ((facep sym)    'ElispFace))))))

(defun +company-mode-maybe ()
  "Enable company mode if it's not enabled."
  (unless global-company-mode
    (global-company-mode +1)))

(defun +company-set-backend (modes &rest backends)
  "Prepend BACKENDS (in order) to `company-backends' in MODES.

MODES should be one symbol or a list of them, representing major
or minor modes. This will overwrite backends for MODES on
consecutive uses.

If the car of BACKENDS is nil, unset the backends for MODES.

Examples:

  (+company-set-backend 'js2-mode
    'company-tide 'company-yasnippet)

  (+company-set-backend 'sh-mode
    '(company-shell :with company-yasnippet))

  (+company-set-backend '(c-mode c++-mode)
    '(:separate company-irony-c-headers company-irony))

  ; unsets backends for `sh-mode'
  (+company-set-backend 'sh-mode nil)"
  (declare (indent defun))
  (dolist (mode (+seq-enlist modes))
    (if (null (car backends))
        (setq +company-backend-alist
              (delq (assq mode +company-backend-alist)
                    +company-backend-alist))
      (setf (alist-get mode +company-backend-alist)
            backends))))

(defun +company--backends ()
  "Return the list of `company' backends."
  (let (backends)
    (let ((mode major-mode)
          (modes (list major-mode)))
      (while (setq mode (get mode 'derived-mode-parent))
        (push mode modes))
      (dolist (mode modes)
        (dolist (backend (append (cdr (assq mode +company-backend-alist))
                                 (default-value 'company-backends)))
          (push backend backends)))
      (delete-dups
       (append (cl-loop for (mode . backends) in +company-backend-alist
                        if (or (eq major-mode mode) ; major modes
                               (and (boundp mode)
                                    (symbol-value mode))) ; minor modes
                        append backends)
               (nreverse backends))))))

(provide 'init-completion)
;;; init-completion.el ends here
