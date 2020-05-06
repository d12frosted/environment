;;; ui/chisinau/config.el -*- lexical-binding: t; -*-

(setq doom-font (font-spec :family "Source Code Pro" :size 12)
      doom-variable-pitch-font (font-spec :family "Source Code Pro")
      doom-serif-font (font-spec :family "Source Code Pro")
      doom-theme 'leuven
      display-line-numbers-type nil)

(custom-theme-set-faces! 'leuven
  '(org-checkbox :background "#FAF7CC")
  '(doom-modeline-project-dir :foreground "SkyBlue3")
  '(doom-modeline-info :foreground "SkyBlue3")
  '(doom-modeline-buffer-modified :foreground "orange" :weight bold))

(custom-set-faces!
  '(mode-line :family "Source Code Pro" :height 1)
  '(mode-line-inactive :family "Source Code Pro" :height 1))

(when IS-MAC
  (setq-default line-spacing 1))

(after! org
  (set-popup-rule! "^CAPTURE"
    :slot -1 :vslot -1 :size #'+popup-shrink-to-fit :side 'right :ttl 0))

(after! helpful
  (set-popup-rule! "^\\*helpful"
    :slot -1 :vslot -1 :size #'+popup-shrink-to-fit :side 'right :ttl 0))
