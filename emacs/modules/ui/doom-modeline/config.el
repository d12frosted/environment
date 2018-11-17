;;; ui/nucleus-modeline/config.el -*- lexical-binding: t; -*-

;; We handle this ourselves
(setq projectile-dynamic-mode-line nil)


;;
;; Modeline library

(defvar nucleus--modeline-fn-alist ())
(defvar nucleus--modeline-var-alist ())

(defmacro def-modeline-segment! (name &rest body)
  "Defines a modeline segment and byte compiles it."
  (declare (indent defun) (doc-string 2))
  (let ((sym (intern (format "nucleus-modeline-segment--%s" name)))
        (docstring (if (stringp (car body))
                       (pop body)
                     (format "%s modeline segment" name))))
    (cond ((and (symbolp (car body))
                (not (cdr body)))
           (add-to-list 'nucleus--modeline-var-alist (cons name (car body)))
           `(add-to-list 'nucleus--modeline-var-alist (cons ',name ',(car body))))
          (t
           (add-to-list 'nucleus--modeline-fn-alist (cons name sym))
           `(progn
              (fset ',sym (lambda () ,docstring ,@body))
              (add-to-list 'nucleus--modeline-fn-alist (cons ',name ',sym))
              ,(unless (bound-and-true-p byte-compile-current-file)
                 `(let (byte-compile-warnings)
                    (byte-compile #',sym))))))))

(defun nucleus--prepare-modeline-segments (segments)
  (let (forms it)
    (dolist (seg segments)
      (cond ((stringp seg)
             (push seg forms))
            ((symbolp seg)
             (cond ((setq it (cdr (assq seg nucleus--modeline-fn-alist)))
                    (push (list it) forms))
                   ((setq it (cdr (assq seg nucleus--modeline-var-alist)))
                    (push it forms))
                   ((error "%s is not a defined segment" seg))))
            ((error "%s is not a valid segment" seg))))
    (nreverse forms)))

(defun def-modeline! (name lhs &optional rhs)
  "Defines a modeline format and byte-compiles it. NAME is a symbol to identify
it (used by `nucleus-modeline' for retrieval). LHS and RHS are lists of symbols of
modeline segments defined with `def-modeline-segment!'.

Example:
  (def-modeline! 'minimal
    '(bar matches \" \" buffer-info)
    '(media-info major-mode))
  (nucleus-set-modeline 'minimal t)"
  (let ((sym (intern (format "nucleus-modeline-format--%s" name)))
        (lhs-forms (nucleus--prepare-modeline-segments lhs))
        (rhs-forms (nucleus--prepare-modeline-segments rhs)))
    (defalias sym
      (lambda ()
        (let ((lhs (eval `(list ,@lhs-forms) t))
              (rhs (eval `(list ,@rhs-forms) t)))
          (let ((rhs-str (format-mode-line rhs)))
            (list lhs
                  (propertize
                   " " 'display
                   `((space :align-to (- (+ right right-fringe right-margin)
                                         ,(+ 1 (string-width rhs-str))))))
                  rhs-str))))
      (concat "Modeline:\n"
              (format "  %s\n  %s"
                      (prin1-to-string lhs)
                      (prin1-to-string rhs))))))

(defun nucleus-modeline (key)
  "Returns a mode-line configuration associated with KEY (a symbol). Throws an
error if it doesn't exist."
  (let ((fn (intern-soft (format "nucleus-modeline-format--%s" key))))
    (when (functionp fn)
      `(:eval (,fn)))))

(defun nucleus-set-modeline (key &optional default)
  "Set the modeline format. Does nothing if the modeline KEY doesn't exist. If
DEFAULT is non-nil, set the default mode-line for all buffers."
  (when-let* ((modeline (nucleus-modeline key)))
    (setf (if default
              (default-value 'mode-line-format)
            (buffer-local-value 'mode-line-format (current-buffer)))
          (list "%e" modeline))))


;;
;; Custom faces

(defgroup +nucleus-modeline nil
  "TODO"
  :group 'faces)

(defface nucleus-modeline-buffer-path
  '((t (:inherit (mode-line-emphasis bold))))
  "Face used for the dirname part of the buffer path."
  :group '+nucleus-modeline)

(defface nucleus-modeline-buffer-file
  '((t (:inherit (mode-line-buffer-id bold))))
  "Face used for the filename part of the mode-line buffer path."
  :group '+nucleus-modeline)

(defface nucleus-modeline-buffer-modified
  '((t (:inherit (error bold) :background nil)))
  "Face used for the 'unsaved' symbol in the mode-line."
  :group '+nucleus-modeline)

(defface nucleus-modeline-buffer-major-mode
  '((t (:inherit (mode-line-emphasis bold))))
  "Face used for the major-mode segment in the mode-line."
  :group '+nucleus-modeline)

(defface nucleus-modeline-highlight
  '((t (:inherit mode-line-emphasis)))
  "Face for bright segments of the mode-line."
  :group '+nucleus-modeline)

(defface nucleus-modeline-panel
  '((t (:inherit mode-line-highlight)))
  "Face for 'X out of Y' segments, such as `+nucleus-modeline--anzu', `+nucleus-modeline--evil-substitute' and
`iedit'"
  :group '+nucleus-modeline)

(defface nucleus-modeline-info
  `((t (:inherit (success bold))))
  "Face for info-level messages in the modeline. Used by `*vc'."
  :group '+nucleus-modeline)

(defface nucleus-modeline-warning
  `((t (:inherit (warning bold))))
  "Face for warnings in the modeline. Used by `*flycheck'"
  :group '+nucleus-modeline)

(defface nucleus-modeline-urgent
  `((t (:inherit (error bold))))
  "Face for errors in the modeline. Used by `*flycheck'"
  :group '+nucleus-modeline)

;; Bar
(defface nucleus-modeline-bar '((t (:inherit highlight)))
  "The face used for the left-most bar on the mode-line of an active window."
  :group '+nucleus-modeline)

(defface nucleus-modeline-eldoc-bar '((t (:inherit shadow)))
  "The face used for the left-most bar on the mode-line when eldoc-eval is
active."
  :group '+nucleus-modeline)

(defface nucleus-modeline-inactive-bar '((t (:inherit warning :inverse-video t)))
  "The face used for the left-most bar on the mode-line of an inactive window."
  :group '+nucleus-modeline)


;;
;; Packages

;; anzu and evil-anzu expose current/total state that can be displayed in the
;; mode-line.
(def-package! anzu
  :after-call isearch-mode
  :config
  (setq anzu-cons-mode-line-p nil
        anzu-minimum-input-length 1
        anzu-search-threshold 250)
  (global-anzu-mode +1)

  (defun +nucleus-modeline*fix-anzu-count (positions here)
    (cl-loop for (start . end) in positions
             collect t into before
             when (and (>= here start) (<= here end))
             return (length before)
             finally return 0))
  (advice-add #'anzu--where-is-here :override #'+nucleus-modeline*fix-anzu-count)

  ;; Avoid anzu conflicts across buffers
  (mapc #'make-variable-buffer-local
        '(anzu--total-matched anzu--current-position anzu--state
          anzu--cached-count anzu--cached-positions anzu--last-command
          anzu--last-isearch-string anzu--overflow-p))
  ;; Ensure anzu state is cleared when searches & iedit are done
  (add-hook 'isearch-mode-end-hook #'anzu--reset-status t)
  (add-hook 'nucleus-escape-hook #'anzu--reset-status t)
  (add-hook 'iedit-mode-end-hook #'anzu--reset-status))


(def-package! evil-anzu
  :when (featurep! :feature evil)
  :after-call (evil-ex-start-search evil-ex-start-word-search))


;; fish-style modeline
(def-package! shrink-path
  :commands (shrink-path-prompt shrink-path-file-mixed))


;; Keep `+nucleus-modeline-current-window' up-to-date
(defvar +nucleus-modeline-current-window (frame-selected-window))
(defun +nucleus-modeline|set-selected-window (&rest _)
  "Sets `+nucleus-modeline-current-window' appropriately"
  (when-let* ((win (frame-selected-window)))
    (unless (minibuffer-window-active-p win)
      (setq +nucleus-modeline-current-window win)
      (force-mode-line-update))))

(defun +nucleus-modeline|unset-selected-window ()
  (setq +nucleus-modeline-current-window nil)
  (force-mode-line-update))

(add-hook 'window-configuration-change-hook #'+nucleus-modeline|set-selected-window)
(add-hook 'nucleus-enter-window-hook #'+nucleus-modeline|set-selected-window)
(with-no-warnings
  (cond ((not (boundp 'after-focus-change-function))
         (add-hook 'focus-in-hook  #'+nucleus-modeline|set-selected-window)
         (add-hook 'focus-out-hook #'+nucleus-modeline|unset-selected-window))
        ((defun +nucleus-modeline|refresh-frame ()
           (setq +nucleus-modeline-current-window nil)
           (cl-loop for frame in (frame-list)
                    if (eq (frame-focus-state frame) t)
                    return (setq +nucleus-modeline-current-window (frame-selected-window frame)))
           (force-mode-line-update))
         (add-function :after after-focus-change-function #'+nucleus-modeline|refresh-frame))))


;;
;; Variables

(defvar +nucleus-modeline-height 23
  "How tall the mode-line should be (only respected in GUI emacs).")

(defvar +nucleus-modeline-bar-width 3
  "How wide the mode-line bar should be (only respected in GUI emacs).")

(defvar +nucleus-modeline-buffer-file-name-style 'truncate-upto-project
  "Determines the style used by `+nucleus-modeline-buffer-file-name'.

Given ~/Projects/FOSS/emacs/lisp/comint.el
truncate-upto-project => ~/P/F/emacs/lisp/comint.el
truncate-upto-root => ~/P/F/e/lisp/comint.el
truncate-all => ~/P/F/e/l/comint.el
relative-from-project => emacs/lisp/comint.el
relative-to-project => lisp/comint.el
file-name => comint.el")

;; externs
(defvar anzu--state nil)
(defvar evil-mode nil)
(defvar evil-state nil)
(defvar evil-visual-selection nil)
(defvar iedit-mode nil)
(defvar all-the-icons-scale-factor)
(defvar all-the-icons-default-adjust)


;;
;; Modeline helpers

(defun active ()
  (eq (selected-window) +nucleus-modeline-current-window))

(defun +nucleus-modeline--make-xpm (face width height)
  "Create an XPM bitmap. Inspired by `powerline''s `pl/make-xpm'."
  (propertize
   " " 'display
   (let ((data (make-list height (make-list width 1)))
         (color (or (face-background face nil t) "None")))
     (ignore-errors
       (create-image
        (concat
         (format "/* XPM */\nstatic char * percent[] = {\n\"%i %i 2 1\",\n\". c %s\",\n\"  c %s\","
                 (length (car data))
                 (length data)
                 color
                 color)
         (apply #'concat
                (cl-loop with idx = 0
                         with len = (length data)
                         for dl in data
                         do (cl-incf idx)
                         collect
                         (concat "\""
                                 (cl-loop for d in dl
                                          if (= d 0) collect (string-to-char " ")
                                          else collect (string-to-char "."))
                                 (if (eq idx len) "\"};" "\",\n")))))
        'xpm t :ascent 'center)))))

(defun +nucleus-modeline-buffer-file-name ()
  "Propertized `buffer-file-name' based on `+nucleus-modeline-buffer-file-name-style'."
  (let ((buffer-file-name (or (buffer-file-name (buffer-base-buffer)) "")))
    (unless buffer-file-truename
      (setq buffer-file-truename (file-truename buffer-file-name)))
    (propertize
     (pcase +nucleus-modeline-buffer-file-name-style
       (`truncate-upto-project
        (+nucleus-modeline--buffer-file-name buffer-file-name buffer-file-truename 'shrink))
       (`truncate-upto-root
        (+nucleus-modeline--buffer-file-name-truncate buffer-file-name buffer-file-truename))
       (`truncate-all
        (+nucleus-modeline--buffer-file-name-truncate buffer-file-name buffer-file-truename t))
       (`relative-to-project
        (+nucleus-modeline--buffer-file-name-relative buffer-file-name buffer-file-truename))
       (`relative-from-project
        (+nucleus-modeline--buffer-file-name-relative buffer-file-name buffer-file-truename 'include-project))
       (`file-name
        (propertize (file-name-nondirectory buffer-file-name)
                    'face
                    (let ((face (or (and (buffer-modified-p)
                                         'nucleus-modeline-buffer-modified)
                                    (and (active)
                                         'nucleus-modeline-buffer-file))))
                      (when face `(:inherit ,face))))))
     'help-echo buffer-file-truename)))

(defun +nucleus-modeline--buffer-file-name-truncate (file-path true-file-path &optional truncate-tail)
  "Propertized `buffer-file-name' that truncates every dir along path.
If TRUNCATE-TAIL is t also truncate the parent directory of the file."
  (let ((dirs (shrink-path-prompt (file-name-directory true-file-path)))
        (active (active)))
    (if (null dirs)
        (propertize "%b" 'face (if active 'nucleus-modeline-buffer-file))
      (let ((modified-faces (if (buffer-modified-p) 'nucleus-modeline-buffer-modified)))
        (let ((dirname (car dirs))
              (basename (cdr dirs))
              (dir-faces (or modified-faces (if active 'nucleus-modeline-project-root-dir)))
              (file-faces (or modified-faces (if active 'nucleus-modeline-buffer-file))))
          (concat (propertize (concat dirname
                                      (if truncate-tail (substring basename 0 1) basename)
                                      "/")
                              'face (if dir-faces `(:inherit ,dir-faces)))
                  (propertize (file-name-nondirectory file-path)
                              'face (if file-faces `(:inherit ,file-faces)))))))))

(defun +nucleus-modeline--buffer-file-name-relative (_file-path true-file-path &optional include-project)
  "Propertized `buffer-file-name' showing directories relative to project's root only."
  (let ((root (or (nucleus-project-root) default-directory))
        (active (active)))
    (if (null root)
        (propertize "%b" 'face (if active 'nucleus-modeline-buffer-file))
      (let* ((modified-faces (if (buffer-modified-p) 'nucleus-modeline-buffer-modified))
             (relative-dirs (file-relative-name (file-name-directory true-file-path)
                                                (if include-project (concat root "../") root)))
             (relative-faces (or modified-faces (if active 'nucleus-modeline-buffer-path)))
             (file-faces (or modified-faces (if active 'nucleus-modeline-buffer-file))))
        (if (equal "./" relative-dirs) (setq relative-dirs ""))
        (concat (propertize relative-dirs 'face (if relative-faces `(:inherit ,relative-faces)))
                (propertize (file-name-nondirectory true-file-path)
                            'face (if file-faces `(:inherit ,file-faces))))))))

(defun +nucleus-modeline--buffer-file-name (file-path _true-file-path &optional truncate-project-root-parent)
  "Propertized `buffer-file-name'.
If TRUNCATE-PROJECT-ROOT-PARENT is t space will be saved by truncating it down
fish-shell style.

Example:
~/Projects/FOSS/emacs/lisp/comint.el => ~/P/F/emacs/lisp/comint.el"
  (let* ((project-root (or (nucleus-project-root) default-directory))
         (file-name-split (shrink-path-file-mixed project-root
                                                  (file-name-directory file-path)
                                                  file-path))
         (active (active)))
    (if (null file-name-split)
        (propertize "%b" 'face (if active 'nucleus-modeline-buffer-file))
      (pcase-let ((`(,root-path-parent ,project ,relative-path ,file-path) file-name-split))
        (let ((modified-faces (if (buffer-modified-p) 'nucleus-modeline-buffer-modified)))
          (let ((sp-faces       (or modified-faces (if active 'font-lock-comment-face)))
                (project-faces  (or modified-faces (if active 'font-lock-string-face)))
                (relative-faces (or modified-faces (if active 'nucleus-modeline-buffer-path)))
                (file-faces     (or modified-faces (if active 'nucleus-modeline-buffer-file))))
            (let ((sp-props       `(,@(if sp-faces       `(:inherit ,sp-faces))      ,@(if active '(:weight bold))))
                  (project-props  `(,@(if project-faces  `(:inherit ,project-faces)) ,@(if active '(:weight bold))))
                  (relative-props `(,@(if relative-faces `(:inherit ,relative-faces))))
                  (file-props     `(,@(if file-faces     `(:inherit ,file-faces)))))
              (concat (propertize (if truncate-project-root-parent
                                      root-path-parent
                                    (abbreviate-file-name project-root))
                                  'face sp-props)
                      (propertize (concat project "/") 'face project-props)
                      (if relative-path (propertize relative-path 'face relative-props))
                      (propertize file-path 'face file-props)))))))))


;;
;; buffer information

(def-modeline-segment! buffer-default-directory
  "Displays `default-directory'. This is for special buffers like the scratch
buffer where knowing the current project directory is important."
  (let ((face (if (active) 'nucleus-modeline-buffer-path)))
    (concat (if (display-graphic-p) " ")
            (all-the-icons-octicon
             "file-directory"
             :face face
             :v-adjust -0.05
             :height 1.25)
            (propertize (concat " " (abbreviate-file-name default-directory))
                        'face face))))

(def-modeline-segment! buffer-info
  "Combined information about the current buffer, including the current working
directory, the file name, and its state (modified, read-only or non-existent)."
  (concat (cond (buffer-read-only
                 (concat (all-the-icons-octicon
                          "lock"
                          :face 'nucleus-modeline-warning
                          :v-adjust -0.05)
                         " "))
                ((buffer-modified-p)
                 (concat (all-the-icons-faicon
                          "floppy-o"
                          :face 'nucleus-modeline-buffer-modified
                          :v-adjust -0.0575)
                         " "))
                ((and buffer-file-name
                      (not (file-exists-p buffer-file-name)))
                 (concat (all-the-icons-octicon
                          "circle-slash"
                          :face 'nucleus-modeline-urgent
                          :v-adjust -0.05)
                         " "))
                ((buffer-narrowed-p)
                 (concat (all-the-icons-octicon
                          "fold"
                          :face 'nucleus-modeline-warning
                          :v-adjust -0.05)
                         " ")))
          (if buffer-file-name
              (+nucleus-modeline-buffer-file-name)
            "%b")))

(def-modeline-segment! buffer-info-simple
  "Display only the current buffer's name, but with fontification."
  (propertize
   "%b"
   'face (cond ((and buffer-file-name (buffer-modified-p))
                'nucleus-modeline-buffer-modified)
               ((active) 'nucleus-modeline-buffer-file))))

;; (defvar +nucleus-modeline--encoding nil)
;; (def-modeline-segment! buffer-encoding
;;   "TODO"
;;   +nucleus-modeline--encoding)

;; (add-variable-watcher
;;  'buffer-file-coding-system
;;  (lambda (_sym val op _where)
;;    (when (eq op 'set)
;;      (setq +nucleus-modeline--encoding
;;            (concat (pcase (coding-system-eol-type val)
;;                      (0 "LF  ")
;;                      (1 "CRLF  ")
;;                      (2 "CR  "))
;;                    (let ((sys (coding-system-plist val)))
;;                      (if (memq (plist-get sys :category) '(coding-category-undecided coding-category-utf-8))
;;                          "UTF-8"
;;                        (upcase (symbol-name (plist-get sys :name)))))
;;                    "  ")))))

(def-modeline-segment! buffer-encoding
  "Displays the encoding and eol style of the buffer the same way Atom does."
  (concat (pcase (coding-system-eol-type buffer-file-coding-system)
            (0 "LF  ")
            (1 "CRLF  ")
            (2 "CR  "))
          (let ((sys (coding-system-plist buffer-file-coding-system)))
            (cond ((memq (plist-get sys :category) '(coding-category-undecided coding-category-utf-8))
                   "UTF-8")
                  (t (upcase (symbol-name (plist-get sys :name))))))
          "  "))


;;
;; major-mode

(def-modeline-segment! major-mode
  "The major mode, including process, environment and text-scale info."
  (propertize
   (concat (format-mode-line mode-name)
           (when (stringp mode-line-process)
             mode-line-process)
           (and (boundp 'text-scale-mode-amount)
                (/= text-scale-mode-amount 0)
                (format " (%+d)" text-scale-mode-amount)))
   'face (if (active) 'nucleus-modeline-buffer-major-mode)))


;;
;; vcs

(defvar-local +nucleus-modeline--vcs nil)
(defun +nucleus-modeline--update-vcs ()
  (setq +nucleus-modeline--vcs
        (when (and vc-mode buffer-file-name)
          (let* ((backend (vc-backend buffer-file-name))
                 (state   (vc-state buffer-file-name backend)))
            (let ((face    'mode-line-inactive)
                  (active  (active))
                  (all-the-icons-default-adjust -0.1))
              (concat "  "
                      (cond ((memq state '(edited added))
                             (if active (setq face 'nucleus-modeline-info))
                             (all-the-icons-octicon
                              "git-compare"
                              :face face
                              :v-adjust -0.05))
                            ((eq state 'needs-merge)
                             (if active (setq face 'nucleus-modeline-info))
                             (all-the-icons-octicon "git-merge" :face face))
                            ((eq state 'needs-update)
                             (if active (setq face 'nucleus-modeline-warning))
                             (all-the-icons-octicon "arrow-down" :face face))
                            ((memq state '(removed conflict unregistered))
                             (if active (setq face 'nucleus-modeline-urgent))
                             (all-the-icons-octicon "alert" :face face))
                            (t
                             (if active (setq face 'font-lock-doc-face))
                             (all-the-icons-octicon
                              "git-compare"
                              :face face
                              :v-adjust -0.05)))
                      " "
                      (propertize (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
                                  'face (if active face))
                      " "))))))
(add-hook 'after-revert-hook #'+nucleus-modeline--update-vcs)
(add-hook 'after-save-hook #'+nucleus-modeline--update-vcs)
(add-hook 'find-file-hook #'+nucleus-modeline--update-vcs t)
(advice-add #'vc-refresh-state :after #'+nucleus-modeline--update-vcs)

(def-modeline-segment! vcs
  "Displays the current branch, colored based on its state."
  +nucleus-modeline--vcs)


;;
;; flycheck

(defvar +nucleus-modeline-vspc
  (propertize " " 'face 'variable-pitch)
  "TODO")

(defun +nucleus-ml-icon (icon &optional text face voffset)
  "Displays an octicon ICON with FACE, followed by TEXT. Uses
`all-the-icons-octicon' to fetch the icon."
  (concat (if vc-mode " " "  ")
          (when icon
            (concat
             (all-the-icons-material icon :face face :height 1.1 :v-adjust (or voffset -0.2))
             (if text +nucleus-modeline-vspc)))
          (if text (propertize text 'face face))
          (if vc-mode "  " " ")))

(defvar-local +nucleus-modeline--flycheck nil)
(add-hook 'flycheck-status-changed-functions #'+nucleus-modeline|update-flycheck-segment)
(add-hook 'flycheck-mode-hook #'+nucleus-modeline|update-flycheck-segment)

(defun +nucleus-modeline|update-flycheck-segment (&optional status)
  (setq +nucleus-modeline--flycheck
        (pcase status
          ('finished (if flycheck-current-errors
                         (let-alist (flycheck-count-errors flycheck-current-errors)
                           (let ((sum (+ (or .error 0) (or .warning 0))))
                             (+nucleus-ml-icon "do_not_disturb_alt"
                                            (number-to-string sum)
                                            (if .error 'nucleus-modeline-urgent 'nucleus-modeline-warning)
                                            -0.25)))
                       (+nucleus-ml-icon "check" nil 'nucleus-modeline-info)))
          ('running     (+nucleus-ml-icon "access_time" nil 'font-lock-doc-face -0.25))
          ('no-checker  (+nucleus-ml-icon "sim_card_alert" "-" 'font-lock-doc-face))
          ('errored     (+nucleus-ml-icon "sim_card_alert" "Error" 'nucleus-modeline-urgent))
          ('interrupted (+nucleus-ml-icon "pause" "Interrupted" 'font-lock-doc-face)))))

(def-modeline-segment! flycheck
  "Displays color-coded flycheck error status in the current buffer with pretty
icons."
  +nucleus-modeline--flycheck)


;;
;; selection-info

(defsubst nucleus-column (pos)
  (save-excursion (goto-char pos)
                  (current-column)))

(defvar-local +nucleus-modeline-enable-word-count nil
  "If non-nil, a word count will be added to the selection-info modeline
segment.")

(defun +nucleus-modeline|enable-word-count () (setq +nucleus-modeline-enable-word-count t))
(add-hook 'text-mode-hook #'+nucleus-modeline|enable-word-count)

(def-modeline-segment! selection-info
  "Information about the current selection, such as how many characters and
lines are selected, or the NxM dimensions of a block selection."
  (when (and (active) (or mark-active (eq evil-state 'visual)))
    (cl-destructuring-bind (beg . end)
        (if (eq evil-state 'visual)
            (cons evil-visual-beginning evil-visual-end)
          (cons (region-beginning) (region-end)))
      (propertize
       (let ((lines (count-lines beg (min end (point-max)))))
         (concat (cond ((or (bound-and-true-p rectangle-mark-mode)
                            (eq 'block evil-visual-selection))
                        (let ((cols (abs (- (nucleus-column end)
                                            (nucleus-column beg)))))
                          (format "%dx%dB" lines cols)))
                       ((eq evil-visual-selection 'line)
                        (format "%dL" lines))
                       ((> lines 1)
                        (format "%dC %dL" (- end beg) lines))
                       ((format "%dC" (- end beg))))
                 (when +nucleus-modeline-enable-word-count
                   (format " %dW" (count-words beg end)))))
       'face 'nucleus-modeline-highlight))))


;;
;; matches (anzu, evil-substitute, iedit, macro)

(defun +nucleus-modeline--macro-recording ()
  "Display current Emacs or evil macro being recorded."
  (when (and (active) (or defining-kbd-macro executing-kbd-macro))
    (let ((sep (propertize " " 'face 'nucleus-modeline-panel)))
      (concat sep
              (propertize (if (bound-and-true-p evil-this-macro)
                              (char-to-string evil-this-macro)
                            "Macro")
                          'face 'nucleus-modeline-panel)
              sep
              (all-the-icons-octicon "triangle-right"
                                     :face 'nucleus-modeline-panel
                                     :v-adjust -0.05)
              sep))))

(defsubst +nucleus-modeline--anzu ()
  "Show the match index and total number thereof. Requires `anzu', also
`evil-anzu' if using `evil-mode' for compatibility with `evil-search'."
  (when (and anzu--state (not iedit-mode))
    (propertize
     (let ((here anzu--current-position)
           (total anzu--total-matched))
       (cond ((eq anzu--state 'replace-query)
              (format " %d replace " total))
             ((eq anzu--state 'replace)
              (format " %d/%d " here total))
             (anzu--overflow-p
              (format " %s+ " total))
             (t
              (format " %s/%d " here total))))
     'face (if (active) 'nucleus-modeline-panel))))

(defsubst +nucleus-modeline--evil-substitute ()
  "Show number of matches for evil-ex substitutions and highlights in real time."
  (when (and evil-mode
             (or (assq 'evil-ex-substitute evil-ex-active-highlights-alist)
                 (assq 'evil-ex-global-match evil-ex-active-highlights-alist)
                 (assq 'evil-ex-buffer-match evil-ex-active-highlights-alist)))
    (propertize
     (let ((range (if evil-ex-range
                      (cons (car evil-ex-range) (cadr evil-ex-range))
                    (cons (line-beginning-position) (line-end-position))))
           (pattern (car-safe (evil-delimited-arguments evil-ex-argument 2))))
       (if pattern
           (format " %s matches " (how-many pattern (car range) (cdr range)))
         " - "))
     'face (if (active) 'nucleus-modeline-panel))))

(defun nucleus-themes--overlay-sort (a b)
  (< (overlay-start a) (overlay-start b)))

(defsubst +nucleus-modeline--iedit ()
  "Show the number of iedit regions matches + what match you're on."
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
                                                  #'nucleus-themes--overlay-sort)))
                      -1)
                 "-")
               length))
     'face (if (active) 'nucleus-modeline-panel))))

(def-modeline-segment! matches
  "Displays: 1. the currently recording macro, 2. A current/total for the
current search term (with anzu), 3. The number of substitutions being conducted
with `evil-ex-substitute', and/or 4. The number of active `iedit' regions."
  (let ((meta (concat (+nucleus-modeline--macro-recording)
                      (+nucleus-modeline--anzu)
                      (+nucleus-modeline--evil-substitute)
                      (+nucleus-modeline--iedit))))
     (or (and (not (equal meta "")) meta)
         (if buffer-file-name " %I "))))


;;
;; media-info

(def-modeline-segment! media-info
  "Metadata regarding the current file, such as dimensions for images."
  ;; TODO Include other information
  (cond ((eq major-mode 'image-mode)
         (cl-destructuring-bind (width . height)
             (image-size (image-get-display-property) :pixels)
           (format "  %dx%d  " width height)))))


;;
;; bar

(defvar +nucleus-modeline--bar-active nil)
(defvar +nucleus-modeline--bar-inactive nil)
(def-modeline-segment! bar
  "The bar regulates the height of the mode-line in GUI Emacs.
Returns \"\" to not break --no-window-system."
  (if window-system
      (if (active)
          +nucleus-modeline--bar-active
        +nucleus-modeline--bar-inactive)
    ""))

(when EMACS26+
  (add-variable-watcher
   '+nucleus-modeline-height
   (lambda (_sym val op _where)
     (when (and (eq op 'set) (integerp val))
       (+nucleus-modeline|refresh-bars +nucleus-modeline-bar-width val))))

  (add-variable-watcher
   '+nucleus-modeline-bar-width
   (lambda (_sym val op _where)
     (when (and (eq op 'set) (integerp val))
       (+nucleus-modeline|refresh-bars val +nucleus-modeline-height))))

  (add-hook 'nucleus-big-font-mode-hook #'+nucleus-modeline|resize-for-big-font))


;;
;; Mode lines

(def-modeline! 'main
  '(bar matches " " buffer-info "  %l:%c %p  " selection-info)
  '(buffer-encoding major-mode vcs flycheck))

(def-modeline! 'minimal
  '(bar matches " " buffer-info)
  '(media-info major-mode))

(def-modeline! 'special
  '(bar matches " " buffer-info-simple "  %l:%c %p  " selection-info)
  '(buffer-encoding major-mode flycheck))

(def-modeline! 'project
  '(bar buffer-default-directory)
  '(major-mode))

(def-modeline! 'media
  '(bar " %b  ")
  '(media-info major-mode))


;;
;; Hooks

(defun +nucleus-modeline|refresh-bars (&optional width height)
  (setq +nucleus-modeline--bar-active
        (+nucleus-modeline--make-xpm 'nucleus-modeline-bar
                                  (or width +nucleus-modeline-bar-width)
                                  (or height +nucleus-modeline-height))
        +nucleus-modeline--bar-inactive
        (+nucleus-modeline--make-xpm 'nucleus-modeline-inactive-bar
                                  (or width +nucleus-modeline-bar-width)
                                  (or height +nucleus-modeline-height))))

(defun +nucleus-modeline|init ()
  ;; Create bars
  (+nucleus-modeline|refresh-bars)
  (unless after-init-time
    ;; These buffers are already created and don't get modelines. For the love
    ;; of Emacs, someone give the man a modeline!
    (dolist (bname '("*scratch*" "*Messages*"))
      (with-current-buffer bname
        (nucleus-set-modeline 'main)))))

(defun +nucleus-modeline|set-special-modeline ()
  (nucleus-set-modeline 'special))

(defun +nucleus-modeline|set-media-modeline ()
  (nucleus-set-modeline 'media))

(defun +nucleus-modeline|set-project-modeline ()
  (nucleus-set-modeline 'project))


;;
;; Bootstrap

(nucleus-set-modeline 'main t) ; set default modeline

(add-hook 'nucleus-load-theme-hook #'+nucleus-modeline|init)
(add-hook 'nucleus-scratch-buffer-hook #'+nucleus-modeline|set-special-modeline)
(add-hook '+nucleus-dashboard-mode-hook #'+nucleus-modeline|set-project-modeline)

(add-hook 'image-mode-hook #'+nucleus-modeline|set-media-modeline)
(add-hook 'circe-mode-hook #'+nucleus-modeline|set-special-modeline)

;; Ensure modeline is inactive when Emacs is unfocused (and active otherwise)
(defvar +nucleus-modeline-remap-face-cookie nil)
(defun +nucleus-modeline|focus ()
  (when +nucleus-modeline-remap-face-cookie
    (require 'face-remap)
    (face-remap-remove-relative +nucleus-modeline-remap-face-cookie)))
(defun +nucleus-modeline|unfocus ()
  (setq +nucleus-modeline-remap-face-cookie (face-remap-add-relative 'mode-line 'mode-line-inactive)))

(add-hook 'focus-in-hook #'+nucleus-modeline|focus)
(add-hook 'focus-out-hook #'+nucleus-modeline|unfocus)
