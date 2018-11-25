;;; ui/modeline/config.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;;         Henrik Lissner <henrik@lissner.net>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 24 Nov 2018
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

;;
;; Variables

(defgroup +modeline nil
  "+modeline"
  :group 'faces)

(defvar +modeline-width 3
  "How wide the mode-line bar should be (only respected in GUI
  emacs).")

(defvar +modeline-height 22
  "How tall the mode-line should be (only respected in GUI
  emacs).")

(defvar +modeline-bar-at-end nil
  "If non-nil, the bar is placed at the end, instead of at the
beginning of the modeline.")

(defvar +modeline-bar-invisible nil
  "If non-nil, the bar is transparent, and only used to police
the height of the mode-line.")

(defvar +modeline-buffer-path-function #'+modeline-file-path-with-project
  "A function that returns, in list form, components of the
buffer file name display in the mode-line.

Each item should either be a string or a a cons cell whose CAR is
the path component and CDR is the name of a face.

Currently available functions:

+ `+modeline-file-path-with-project': project/src/lib/file.c
+ `+modeline-file-path-from-project': src/lib/file.c
+ `+modeline-file-path-truncated-with-project': project/s/l/file.c
+ `+modeline-file-path-truncated-upto-project': ~/w/project/src/lib/file.c
+ `+modeline-file-path-truncated-upto-project-root': ~/w/p/s/lib/file.c
+ `+modeline-file-path-truncated': ~/w/p/s/l/file.c
+ `+modeline-file-name': file.c")

(defvar-local +modeline-format-left  () "TODO")
(defvar-local +modeline-format-right () "TODO")

(put '+modeline-format-left  'risky-local-variable t)
(put '+modeline-format-right 'risky-local-variable t)

(defvar +modeline--vspc (propertize " " 'face 'variable-pitch))

(defvar all-the-icons-scale-factor)
(defvar all-the-icons-default-adjust)

;;
;; Custom faces

(defface +modeline-buffer-path
  '((t (:inherit (mode-line-emphasis bold))))
  "Face used for the dirname part of the buffer path."
  :group '+modeline)

(defface +modeline-buffer-file
  '((t (:inherit (mode-line-buffer-id bold))))
  "Face used for the filename part of the mode-line buffer path."
  :group '+modeline)

(defface +modeline-buffer-project-root
  '((t (:inherit +modeline-buffer-path)))
  "Face used for the project root at the beginning of the
mode-line path."
  :group '+modeline)

(defface +modeline-buffer-modified
  '((t (:inherit (error bold) :background nil)))
  "Face used for the 'unsaved' symbol in the mode-line."
  :group '+modeline)

(defface +modeline-buffer-major-mode
  '((t (:inherit (mode-line-emphasis bold))))
  "Face used for the major-mode segment in the mode-line."
  :group '+modeline)

(defface +modeline-highlight
  '((t (:inherit mode-line-emphasis)))
  "Face for bright segments of the mode-line."
  :group '+modeline)

(defface +modeline-panel
  '((t (:inherit mode-line-highlight)))
  "Face for 'X out of Y' segments, such as`iedit'."
  :group '+modeline)

(defface +modeline-info
  `((t (:inherit (success bold))))
  "Face for info-level messages in the modeline. Used by `*vc'."
  :group '+modeline)

(defface +modeline-warning
  `((t (:inherit (warning bold))))
  "Face for warnings in the modeline. Used by `*flycheck'"
  :group '+modeline)

(defface +modeline-urgent
  `((t (:inherit (error bold))))
  "Face for errors in the modeline. Used by `*flycheck'"
  :group '+modeline)

(defface +modeline-bar
  '((t (:inherit highlight)))
  "The face used for the left-most bar on the mode-line of an active window."
  :group '+modeline)

;;
;; Aliases

(defvaralias 'mode-line-format-left '+modeline-format-left)
(defvaralias 'mode-line-format-right '+modeline-format-right)

;;
;; Make friends

;; Otherwise appended segments will produce *Invalid*
(setq global-mode-string '(""))

;; We handle this ourselves
(setq projectile-dynamic-mode-line nil)

;;
;; Hacks

;; Keep `+modeline-current-window' up-to-date
(defvar +modeline-current-window (frame-selected-window))

(defun +modeline|set-selected-window (&rest _)
  "Sets `+modeline-current-window' appropriately"
  (when-let* ((win (frame-selected-window)))
    (unless (minibuffer-window-active-p win)
      (setq +modeline-current-window win)
      (force-mode-line-update))))

(defun +modeline|unset-selected-window ()
  (setq +modeline-current-window nil)
  (force-mode-line-update))

(add-hook 'window-configuration-change-hook #'+modeline|set-selected-window)
(add-hook 'nucleus-enter-window-hook #'+modeline|set-selected-window)
(add-hook 'nucleus-exit-window-hook #'+modeline|unset-selected-window)
(if (not (boundp 'after-focus-change-function))
    (progn
      (add-hook 'focus-in-hook  #'+modeline|set-selected-window)
      (add-hook 'focus-out-hook #'+modeline|unset-selected-window))
  (defun +modeline|refresh-frame ()
    (setq +modeline-current-window nil)
    (cl-loop for frame in (frame-list)
             if (eq (frame-focus-state frame) t)
             return (setq +modeline-current-window (frame-selected-window frame)))
    (force-mode-line-update t))
  (add-function :after after-focus-change-function #'+modeline|refresh-frame))

(defsubst active ()
  (eq (selected-window) +modeline-current-window))

;; Ensure modeline is inactive when Emacs is unfocused (and active otherwise)
(defvar +modeline-remap-face-cookies nil)

(defun +modeline|focus-all-windows (&rest _)
  (cl-loop for (buffer . cookie) in +modeline-remap-face-cookies
           if (buffer-live-p buffer)
           do (with-current-buffer buffer
                (face-remap-remove-relative cookie))))

(defun +modeline|unfocus-all-windows (&rest _)
  (setq +modeline-remap-face-cookies
        (cl-loop for window in (window-list)
                 for buffer = (window-buffer window)
                 if (buffer-live-p buffer)
                 collect
                 (with-current-buffer buffer
                   (cons buffer
                         (face-remap-add-relative 'mode-line
                                                  'mode-line-inactive))))))

(add-hook 'focus-in-hook #'+modeline|focus-all-windows)
(add-hook 'focus-out-hook #'+modeline|unfocus-all-windows)
(advice-add #'posframe-hide :after #'+modeline|focus-all-windows)
(advice-add #'posframe-delete :after #'+modeline|focus-all-windows)

;;
;; Helpers

(defun +modeline--make-xpm (width height &optional color)
  "Create an XPM bitmap.

Inspired by `powerline''s `pl/make-xpm'."
  (propertize
   " " 'display
   (let ((data (make-list height (make-list width 1)))
         (color (or color "None")))
     (ignore-errors
       (create-image
        (concat
         (format "/* XPM */\nstatic char * percent[] = {\n\"%i %i 2 1\",\n\". c %s\",\n\"  c %s\","
                 (length (car data)) (length data) color color)
         (cl-loop with idx = 0
                  with len = (length data)
                  for dl in data
                  do (cl-incf idx)
                  concat "\""
                  concat (cl-loop for d in dl
                                  if (= d 0) collect (string-to-char " ")
                                  else collect (string-to-char "."))
                  concat (if (eq idx len) "\"};" "\",\n")))
        'xpm t :ascent 'center)))))

(defun +modeline-build-path (path)
  "Construct the file path for the `+modeline-buffer-id' segment
using `+mdoeline-buffer-path-function'. If the buffer has no
`buffer-file-name', just use `buffer-name'."
  (let ((buffer-file-name (or path buffer-file-name)))
    (if (or (eq major-mode 'dired-mode)
            (null buffer-file-name))
        (propertize "%s" 'face '+modeline-buffer-path)
      (cl-loop for spec in (funcall +modeline-buffer-path-function)
               if (stringp spec) concat spec
               else if (not (null spec))
               concat (propertize (car spec) 'face (cdr spec))))))

;;
;; Buffer file path styles

(defun +modeline-file-path-with-project ()
  "Returns the unaltered buffer file path relative to the project
root's parent.

e.g. project/src/lib/file.c"
  (let* ((base (buffer-base-buffer))
         (filename (file-truename (buffer-file-name base))))
    (append (if (+project-p)
                (let* ((project-root (+project-root))
                       (relative-dirs (file-relative-name (file-name-directory filename)
                                                          (file-truename project-root))))
                  (list (cons (concat (+project-name) "/")
                              '+modeline-buffer-project-root)
                        (unless (equal "./" relative-dirs)
                          (cons relative-dirs '+modeline-buffer-path))))
              (list nil (cons (abbreviate-file-name (file-name-directory filename))
                              '+modeline-buffer-path)))
            (list (cons (file-name-nondirectory filename)
                        '+modeline-buffer-file)))))

(defun +modeline-file-path-from-project ()
  "Returns file path relative to the project root.

e.g. src/lib/file.c

Meant for `+modeline-buffer-path-function'."
  (cdr (+modeline-file-path-with-project)))

(defun +modeline-file-path-truncated-with-project ()
  "Returns file path relative to (and including) project root,
with descendent folders truncated.

e.g. project/s/l/file.c

Meant for `+modeline-buffer-path-function'."
  (let* ((parts (+modeline-file-path-with-project))
         (dirs (car (nth 1 parts))))
    (setcar (nth 1 parts)
            (shrink-path--dirs-internal dirs t))
    parts))

(defun +modeline-file-path-truncated-upto-project ()
  "Returns file path, truncating segments prior to the project.

e.g. ~/w/project/src/lib/file.c

Meant for `+modeline-buffer-path-function'."
  (pcase-let
      ((`(,root-parent ,root ,dir, file)
        (let ((buffer-file-name (or buffer-file-name (buffer-file-name (buffer-base-buffer)))))
          (shrink-path-file-mixed (or (+project-root) default-directory)
                                  (file-name-directory buffer-file-name)
                                  buffer-file-name))))
    (list (cons root-parent 'font-lock-comment-face)
          (cons root '+modeline-buffer-project-root)
          (cons (concat "/" dir) '+modeline-buffer-path)
          (cons file '+modeline-buffer-file))))

(defun +modeline-file-path-truncated-upto-project-root ()
  "Return file path, truncating segemnts prior to (and including)
the project root.

e.g. ~/w/p/src/lib/file.c

Meant for `+modeline-buffer-path-function'."
  (let* ((parts (+modeline-file-path-truncated-upto-project))
         (root (car (nth 1 parts))))
    (setcar (nth 1 parts)
            (let ((first (substring root 0 1)))
              (if (equal first ".")
                  (substring root 0 2)
                first)))
    parts))

(defun +modeline-file-path-truncated ()
  "Return absolute file path with all directories truncated.

e.g. ~/w/p/s/l/file.c

Meant for `+modeline-buffer-path-function'."
  (pcase-let ((`(,dir . ,file)
               (shrink-path-prompt (buffer-file-name (buffer-base-buffer)))))
    (list (cons dir '+modeline-buffer-path)
          (cons file '+modeline-buffer-file))))

(defun +modeline-file-name ()
  "Return buffer name.

e.g. file.c

Meant for `+modeline-buffer-path-function'."
  (list (cons "%b" '+modeline-buffer-path)))

;;
;; Bars

(defvar +modeline-bar-start nil "TODO")
(put '+modeline-bar-start 'risky-local-variable t)
(defvar +modeline-bar-end nil "TODO")
(put '+modeline-bar-end 'risky-local-variable t)

(defvar +modeline-bar-active nil "TODO")
(defvar +modeline-bar-inactive nil "TODO")
(defun +modeline|setup-bars ()
  (setq +modeline-bar-active
        (+modeline--make-xpm +modeline-width +modeline-height
                             (unless +modeline-bar-invisible
                               (face-background '+modeline-bar nil t)))
        +modeline-bar-inactive
        (+modeline--make-xpm +modeline-width +modeline-height))
  (setq +modeline-bar-start nil
        +modeline-bar-end nil)
  (if +modeline-bar-at-end
      (setq +modeline-bar-end '+modeline-bar)
    (setq +modeline-bar-start '+modeline-bar)))
(add-hook 'nucleus-load-theme-hook #'+modeline|setup-bars)

(defun +modeline|setup-bars-after-change (sym val op _where)
  (when (eq op 'set)
    (set sym val)
    (+modeline|setup-bars)))
(add-variable-watcher '+modeline-width  #'+modeline|setup-bars-after-change)
(add-variable-watcher '+modeline-height #'+modeline|setup-bars-after-change)
(add-variable-watcher '+modeline-bar-at-end #'+modeline|setup-bars-after-change)
(add-variable-watcher '+modeline-bar-invisible #'+modeline|setup-bars-after-change)

(def-modeline-segment! +modeline-bar
  (if (active) +modeline-bar-active +modeline-bar-inactive))

;;
;; Segments

(def-modeline-segment! +modeline-buffer-state
  (let* ((base (buffer-base-buffer))
         (icon (cond (buffer-read-only
                      (all-the-icons-octicon
                       "lock"
                       :face '+modeline-warning
                       :v-adjust -0.05))
                     ((buffer-modified-p base)
                      (all-the-icons-faicon
                       "floppy-o"
                       :face '+modeline-buffer-modified
                       :v-adjust -0.05))
                     ((and (buffer-file-name base)
                           (not (file-exists-p (buffer-file-name base))))
                      (all-the-icons-octicon
                       "circle-slash"
                       :face '+modeline-urgent
                       :v-adjust -0.05)))))
    (if icon (concat icon " "))))

(def-modeline-segment! +modeline-buffer-id
  :on-hooks (find-file-hook after-save-hook after-revert-hook)
  :init (propertize "%b" 'face '+modeline-buffer-file)
  :faces t
  (let ((file-path (buffer-file-name (buffer-base-buffer))))
    (propertize (+modeline-build-path file-path)
                'help-echo file-path)))

(def-modeline-segment! +modeline-buffer-directory
  (let ((face (if (active) '+modeline-buffer-path)))
    (propertize
     (concat (if (display-graphic-p) " ")
             (all-the-icons-octicon
              "file-directory"
              :face face
              :v-adjust -0.1
              :height 1.25)
             " "
             (propertize (abbreviate-file-name default-directory)
                         'face face))
     'help-echo default-directory)))

(def-modeline-segment! +modeline-vcs
  ;; TODO vc-mode is not activated in files that are not registered in VCS which
  ;; means there is no state for this file.
  :on-set (vc-mode)
  :on-hooks (nucleus-enter-buffer-hook
             nucleus-enter-window-hook
             nucleus-exit-window-hook)
  (when (and vc-mode buffer-file-name)
    (let* ((backend (vc-backend buffer-file-name))
           (state   (vc-state-refresh buffer-file-name backend)))
      (let ((face    'mode-line-inactive)
            (active  (active))
            (all-the-icons-default-adjust -0.1))
        (concat (cond ((memq state '(edited added))
                       (if active (setq face '+modeline-warning))
                       (all-the-icons-octicon
                        "git-compare"
                        :face face
                        :v-adjust -0.05))
                      ((eq state 'needs-merge)
                       (if active (setq face '+modeline-info))
                       (all-the-icons-octicon "git-merge" :face face))
                      ((eq state 'needs-update)
                       (if active (setq face '+modeline-warning))
                       (all-the-icons-octicon "arrow-down" :face face))
                      ((memq state '(removed conflict unregistered))
                       (if active (setq face '+modeline-urgent))
                       (all-the-icons-octicon "alert" :face face))
                      (t
                       (if active (setq face '+modeline-info))
                       (all-the-icons-octicon
                        "git-compare"
                        :face face
                        :v-adjust -0.05)))
                +modeline--vspc
                (propertize (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
                            'face (if active face)))))))

(def-modeline-segment! +modeline-indent-style
  :on-hooks (after-revert-hook after-save-hook find-file-hook)
  :on-set (indent-tabs-mode tab-width)
  (propertize (format "%s%d  "
                      (if indent-tabs-mode "⭾" "␣")
                      tab-width)
              'help-echo
              (format "Indentation: %d %s wide"
                      tab-width
                      (if indent-tabs-mode "tabs" "spaces"))))

(def-modeline-segment! +modeline-encoding
  :on-hooks (after-revert-hook after-save-hook find-file-hook)
  :on-set (buffer-file-coding-system)
  (concat (pcase (coding-system-eol-type buffer-file-coding-system)
            (0 (propertize "LF" 'help-echo "EOL convention: \\n (Unix)"))
            (1 (propertize "CRLF" 'help-echo "EOL convention: \\r\\n (Windows, Symbian OS, etc)"))
            (2 (propertize "CR" 'help-echo "EOL convention: \\r (pre-OSX MacOS)")))
          "  "
          (let* ((sys (coding-system-plist buffer-file-coding-system))
                 (category (plist-get sys :category)))
            (propertize
             (cond ((eq category 'coding-category-undecided)
                    "")
                   ((or (eq category 'coding-category-utf-8)
                        (eq (plist-get sys :name) 'prefer-utf-8))
                    "UTF-8  ")
                   ((concat (upcase (symbol-name (plist-get sys :name)))
                            "  ")))
             'help-echo (plist-get (coding-system-plist buffer-file-coding-system) :docstring)))))

(def-modeline-segment! +modeline-major-mode
  (propertize (format-mode-line mode-name)
              'face (if (active) '+modeline-buffer-major-mode)))

(defun +modeline--macro-recording ()
  "Display current Emacs macro being recorded."
  (when (and (active) (or defining-kbd-macro executing-kbd-macro))
    (let ((sep (propertize " " 'face '+modeline-panel)))
      (concat sep
              (propertize "Macro"
                          'face
                          '+modeline-panel)
              sep
              (all-the-icons-octicon "triangle-right"
                                     :face '+modeline-panel
                                     :v-adjust -0.05)
              sep))))

(defun +modeline-themes--overlay-sort (a b)
  (< (overlay-start a) (overlay-start b)))

(defsubst +modeline--iedit ()
  "Show the number of iedit regions matches + what match you're
on."
  (when (and iedit-mode iedit-occurrences-overlays)
    (propertize
     (let ((this-oc (or (let ((inhibit-message t))
                          (iedit-find-current-occurrence-overlay))
                        (progn (iedit-prev-occurrence)
                               (iedit-find-current-occurrence-overlay))))
           (length (length iedit-occurrences-overlays)))
       (format " %s/%d "
               (if this-oc
                   (- length
                      (length (memq this-oc (sort (append iedit-occurrences-overlays nil)
                                                  #'+modeline-themes--overlay-sort)))
                      -1)
                 "-")
               length))
     'face (if (active) '+modeline-panel))))

(def-modeline-segment! +modeline-matches
  "Displays:

1. the currently recording macro,
2. nothing more"
  (let ((meta (concat (+modeline--macro-recording)
                      " ")))
     (or (and (not (equal meta " ")) meta)
         (if buffer-file-name " %I "))))

;;
(defsubst +modeline-column (pos)
  (save-excursion (goto-char pos)
                  (current-column)))

(defvar-local +modeline-enable-word-count nil
  "If non-nil, a word count will be added to the selection-info
modeline segment.")

(defun +modeline|enable-word-count ()
  (setq +modeline-enable-word-count t))
(add-hook 'text-mode-hook #'+modeline|enable-word-count)

(def-modeline-segment! +modeline-selection-info
  (let ((beg (region-beginning))
        (end (region-end)))
    (propertize
     (let ((lines (count-lines beg (min end (point-max)))))
       (concat (cond ((bound-and-true-p rectangle-mark-mode)
                      (let ((cols (abs (- (+modeline-column end)
                                          (+modeline-column beg)))))
                        (format "%dx%dB" lines cols)))
                     ((> lines 1)
                      (format "%dC %dL" (- end beg) lines))
                     ((format "%dC" (- end beg))))
               (when +modeline-enable-word-count
                 (format " %dW" (count-words beg end)))))
     'face '+modeline-highlight)))

(defun +modeline|enable-selection-info ()
  (add-to-list '+modeline-format-left '+modeline-selection-info t #'eq))
(defun +modeline|disable-selection-info ()
  (setq +modeline-format-left (delq '+modeline-selection-info +modeline-format-left)))
(add-hook 'activate-mark-hook #'+modeline|enable-selection-info)
(add-hook 'deactivate-mark-hook #'+modeline|disable-selection-info)

;; flycheck
(defun +modeline-ml-icon (icon &optional text face voffset)
  "Displays an octicon ICON with FACE, followed by TEXT. Uses
`all-the-icons-octicon' to fetch the icon."
  (concat (when icon
            (concat
             (all-the-icons-material icon :face face :height 1.1 :v-adjust (or voffset -0.2))
             (if text +modeline--vspc)))
          (if text (propertize text 'face face))))

(defun +modeline-flycheck-status (status)
  (pcase status
    (`finished (if flycheck-current-errors
                   (let-alist (flycheck-count-errors flycheck-current-errors)
                     (let ((sum (+ (or .error 0) (or .warning 0))))
                       (+modeline-ml-icon "do_not_disturb_alt"
                                      (number-to-string sum)
                                      (if .error '+modeline-urgent '+modeline-warning)
                                      -0.25)))
                 (+modeline-ml-icon "check" nil '+modeline-info)))
    (`running     (+modeline-ml-icon "access_time" nil 'font-lock-doc-face -0.25))
    ;; (`no-checker  (+modeline-ml-icon "sim_card_alert" "-" 'font-lock-doc-face))
    (`errored     (+modeline-ml-icon "sim_card_alert" "Error" '+modeline-urgent))
    (`interrupted (+modeline-ml-icon "pause" "Interrupted" 'font-lock-doc-face))))

(defun +modeline|update-flycheck-segment (&optional status)
  (setq +modeline-flycheck
        (when-let* ((status-str (+modeline-flycheck-status status)))
          (concat +modeline--vspc status-str " "))))
(add-hook 'flycheck-mode-hook #'+modeline|update-flycheck-segment)
(add-hook 'flycheck-status-changed-functions #'+modeline|update-flycheck-segment)

(def-modeline-segment! +modeline-flycheck
  "Displays color-coded flycheck error status in the current
buffer with pretty icons."
  :init nil)

;;
;; Preset modeline formats

(def-modeline-format! :main
  '(+modeline-matches " "
    +modeline-buffer-state
    +modeline-buffer-id
    "  %2l:%c %p  ")
  `(mode-line-misc-info
    +modeline-indent-style
    +modeline-encoding
    +modeline-major-mode " "
    (vc-mode (" " +modeline-vcs " "))
    mode-line-process
    +modeline-flycheck))

(def-modeline-format! :minimal
  '(+modeline-matches " "
    +modeline-buffer-state
    +modeline-buffer-id)
  '(+modeline-major-mode))

(def-modeline-format! :special
  '(+modeline-matches +modeline-buffer-state " %b " +modeline-buffer-position)
  '(+modeline-encoding +modeline-major-mode mode-line-process))

(def-modeline-format! :project
  '(+modeline-buffer-directory)
  '(+modeline-major-mode))


;;
(def-modeline-segment! +modeline--rest
  (let ((rhs-str (format-mode-line +modeline-format-right)))
    (list (propertize
           " " 'display
           `((space :align-to (- (+ right right-fringe right-margin)
                                 ,(1+ (string-width rhs-str))))))
          rhs-str)))

(setq-default mode-line-format '("" +modeline-bar-start +modeline-format-left +modeline--rest +modeline-bar-end))

;;
(set-modeline! :main t)
