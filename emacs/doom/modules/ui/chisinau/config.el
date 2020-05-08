;;; ui/chisinau/config.el -*- lexical-binding: t; -*-

(when IS-MAC
  (setq
   doom-font (font-spec :family "Source Code Pro" :size 12)
   doom-variable-pitch-font (font-spec :family "Source Code Pro")
   doom-serif-font (font-spec :family "Source Code Pro"))

  (custom-set-faces!
    '(mode-line :family "Source Code Pro" :height 1)
    '(mode-line-inactive :family "Source Code Pro" :height 1))

  (setq-default line-spacing 1))

(setq doom-theme 'leuven
      display-line-numbers-type nil)

(custom-theme-set-faces! 'leuven
  ;; org
  '(org-checkbox :background "#FAF7CC")
  '(org-scheduled-previously :foreground "black")
  '(org-scheduled-today :foreground "black")
  '(org-agenda-calendar-event :foreground "black")
  '(org-warning :foreground "black" :weight bold)
  '(org-upcoming-deadline :foreground "black" :slant italic)

  ;; modeline
  '(doom-modeline-project-dir :foreground "SkyBlue3")
  '(doom-modeline-info :foreground "SkyBlue3")
  '(doom-modeline-buffer-modified :foreground "orange" :weight bold))

(after! org
  (set-popup-rule! "^CAPTURE"
    :slot -1 :vslot -1 :size #'+popup-shrink-to-fit :side 'right :ttl 0))

(after! helpful
  (set-popup-rule! "^\\*helpful"
    :slot -1 :vslot -1 :size #'+popup-shrink-to-fit :side 'right :ttl 0))
