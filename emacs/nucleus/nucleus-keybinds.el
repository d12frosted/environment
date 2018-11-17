;;; nucleus-keybinds.el -*- lexical-binding: t; -*-

;; A centralized keybinds system, integrated with `which-key' to preview
;; available keybindings. All built into one powerful macro: `map!'. If evil is
;; never loaded, then evil bindings set with `map!' will be ignored.

(defvar nucleus-leader-key "SPC"
  "The leader prefix key, for global commands.")

(defvar nucleus-localleader-key "SPC m"
  "The localleader prefix key, for major-mode specific commands.")

(defvar nucleus-evil-state-alist
  '((?n . normal)
    (?v . visual)
    (?i . insert)
    (?e . emacs)
    (?o . operator)
    (?m . motion)
    (?r . replace)
    (?g . global))
  "A list of cons cells that map a letter to a evil state symbol.")

;;
(defvar nucleus-escape-hook nil
  "A hook run after C-g is pressed (or ESC in normal mode, for evil users). Both
trigger `nucleus/escape'.

If any hook returns non-nil, all hooks after it are ignored.")

(defun nucleus/escape ()
  "Run the `nucleus-escape-hook'."
  (interactive)
  (cond ((minibuffer-window-active-p (minibuffer-window))
         ;; quit the minibuffer if open.
         (abort-recursive-edit))
        ;; Run all escape hooks. If any returns non-nil, then stop there.
        ((cl-find-if #'funcall nucleus-escape-hook))
        ;; don't abort macros
        ((or defining-kbd-macro executing-kbd-macro) nil)
        ;; Back to the default
        ((keyboard-quit))))

(global-set-key [remap keyboard-quit] #'nucleus/escape)


;;
(def-package! which-key
  :defer 1
  :after-call pre-command-hook
  :init
  (setq which-key-sort-order #'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10)
  :config
  ;; general improvements to which-key readability
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
  (which-key-setup-side-window-bottom)
  (setq-hook! 'which-key-init-buffer-hook line-spacing 3)
  (which-key-mode +1))


;; `hydra'
(setq lv-use-seperator t)


;;
(defun nucleus--keybind-register (key desc &optional modes)
  "Register a description for KEY with `which-key' in MODES.

  KEYS should be a string in kbd format.
  DESC should be a string describing what KEY does.
  MODES should be a list of major mode symbols."
  (after! which-key
    (if modes
        (dolist (mode modes)
          (which-key-add-major-mode-key-based-replacements mode key desc))
      (which-key-add-key-based-replacements key desc))))


(defun nucleus--keyword-to-states (keyword)
  "Convert a KEYWORD into a list of evil state symbols.

For example, :nvi will map to (list 'normal 'visual 'insert). See
`nucleus-evil-state-alist' to customize this."
  (cl-loop for l across (substring (symbol-name keyword) 1)
           if (cdr (assq l nucleus-evil-state-alist)) collect it
           else do (error "not a valid state: %s" l)))


;; Register keywords for proper indentation (see `map!')
(put :after        'lisp-indent-function 'defun)
(put :desc         'lisp-indent-function 'defun)
(put :leader       'lisp-indent-function 'defun)
(put :local        'lisp-indent-function 'defun)
(put :localleader  'lisp-indent-function 'defun)
(put :map          'lisp-indent-function 'defun)
(put :map*         'lisp-indent-function 'defun)
(put :mode         'lisp-indent-function 'defun)
(put :prefix       'lisp-indent-function 'defun)
(put :textobj      'lisp-indent-function 'defun)
(put :unless       'lisp-indent-function 'defun)
(put :when         'lisp-indent-function 'defun)

;; specials
(defvar nucleus--keymaps nil)
(defvar nucleus--prefix  nil)
(defvar nucleus--defer   nil)
(defvar nucleus--local   nil)

(defmacro map! (&rest rest)
  "A nightmare of a key-binding macro that will use `evil-define-key*',
`define-key', `local-set-key' and `global-set-key' depending on context and
plist key flags (and whether evil is loaded or not). It was designed to make
binding multiple keys more concise, like in vim.

If evil isn't loaded, it will ignore evil-specific bindings.

States
    :n  normal
    :v  visual
    :i  insert
    :e  emacs
    :o  operator
    :m  motion
    :r  replace

    These can be combined (order doesn't matter), e.g. :nvi will apply to
    normal, visual and insert mode. The state resets after the following
    key=>def pair.

    If states are omitted the keybind will be global.

    This can be customized with `nucleus-evil-state-alist'.

    :textobj is a special state that takes a key and two commands, one for the
    inner binding, another for the outer.

Flags
    (:leader [...])            an alias for (:prefix nucleus-leader-key ...)
    (:localleader [...])       an alias for (:prefix nucleus-localleader-key ...)
    (:mode [MODE(s)] [...])    inner keybinds are applied to major MODE(s)
    (:map [KEYMAP(s)] [...])   inner keybinds are applied to KEYMAP(S)
    (:map* [KEYMAP(s)] [...])  same as :map, but deferred
    (:prefix [PREFIX] [...])   assign prefix to all inner keybindings
    (:after [FEATURE] [...])   apply keybinds when [FEATURE] loads
    (:local [...])             make bindings buffer local; incompatible with keymaps!

Conditional keybinds
    (:when [CONDITION] [...])
    (:unless [CONDITION] [...])

Example
    (map! :map magit-mode-map
          :m \"C-r\" 'do-something           ; assign C-r in motion state
          :nv \"q\" 'magit-mode-quit-window  ; assign to 'q' in normal and visual states
          \"C-x C-r\" 'a-global-keybind

          (:when IS-MAC
           :n \"M-s\" 'some-fn
           :i \"M-o\" (lambda (interactive) (message \"Hi\"))))"
  (let ((nucleus--keymaps nucleus--keymaps)
        (nucleus--prefix  nucleus--prefix)
        (nucleus--defer   nucleus--defer)
        (nucleus--local   nucleus--local)
        key def states forms desc modes)
    (while rest
      (setq key (pop rest))
      (cond
       ;; it's a sub expr
       ((listp key)
        (push (macroexpand `(map! ,@key)) forms))

       ;; it's a flag
       ((keywordp key)
        (cond ((eq key :leader)
               (push 'nucleus-leader-key rest)
               (setq key :prefix
                     desc "<leader>"))
              ((eq key :localleader)
               (push 'nucleus-localleader-key rest)
               (setq key :prefix
                     desc "<localleader>")))
        (pcase key
          (:when    (push `(if ,(pop rest)       ,(macroexpand `(map! ,@rest))) forms) (setq rest '()))
          (:unless  (push `(if (not ,(pop rest)) ,(macroexpand `(map! ,@rest))) forms) (setq rest '()))
          (:after   (push `(after! ,(pop rest)   ,(macroexpand `(map! ,@rest))) forms) (setq rest '()))
          (:desc    (setq desc (pop rest)))
          ((or :map :map*)
            (setq nucleus--keymaps (nucleus-enlist (pop rest))
                  nucleus--defer (eq key :map*)))
          (:mode
            (setq modes (nucleus-enlist (pop rest)))
            (unless nucleus--keymaps
              (setq nucleus--keymaps
                    (cl-loop for m in modes
                             collect (intern (format "%s-map" (symbol-name m)))))))
          (:textobj
            (let* ((key (pop rest))
                   (inner (pop rest))
                   (outer (pop rest)))
              (push (macroexpand `(map! (:map evil-inner-text-objects-map ,key ,inner)
                                        (:map evil-outer-text-objects-map ,key ,outer)))
                    forms)))
          (:prefix
            (let ((def (pop rest)))
              (setq nucleus--prefix
                    `(vconcat ,nucleus--prefix
                              ,(if (or (stringp def)
                                       (and (symbolp def)
                                            (stringp (symbol-value def))))
                                   `(kbd ,def)
                                 def)))
              (when desc
                (push `(nucleus--keybind-register ,(key-description (eval nucleus--prefix))
                                               ,desc ',modes)
                      forms)
                (setq desc nil))))
          (:local
           (setq nucleus--local t))
          (_ ; might be a state nucleus--prefix
           (setq states (nucleus--keyword-to-states key)))))

       ;; It's a key-def pair
       ((or (stringp key)
            (characterp key)
            (vectorp key)
            (symbolp key))
        (unwind-protect
            (catch 'skip
              (when (symbolp key)
                (setq key `(kbd ,key)))
              (when (stringp key)
                (setq key (kbd key)))
              (when nucleus--prefix
                (setq key (append nucleus--prefix (list key))))
              (unless (> (length rest) 0)
                (user-error "map! has no definition for %s key" key))
              (setq def (pop rest))
              (when (or (vectorp def)
                        (stringp def))
                (setq def
                      `(lambda () (interactive)
                         (setq unread-command-events
                               (nconc (mapcar (lambda (ev) (cons t ev))
                                              (listify-key-sequence
                                               ,(cond ((vectorp def) def)
                                                      ((stringp def) (kbd def)))))
                                      unread-command-events)))))
              (when desc
                (push `(nucleus--keybind-register ,(key-description (eval key))
                                               ,desc ',modes)
                      forms))
              (cond ((and nucleus--local nucleus--keymaps)
                     (push `(lwarn 'nucleus-map :warning
                                   "Can't local bind '%s' key to a keymap; skipped"
                                   ,key)
                           forms)
                     (throw 'skip 'local))
                    ((and nucleus--keymaps states)
                     (dolist (keymap nucleus--keymaps)
                       (when (memq 'global states)
                         (push `(define-key ,keymap ,key ,def) forms))
                       (when (featurep 'evil)
                         (when-let* ((states (delq 'global states)))
                           (push `(,(if nucleus--defer #'evil-define-key #'evil-define-key*)
                                   ',states ,keymap ,key ,def)
                                 forms)))))
                    (states
                     (dolist (state states)
                       (if (eq state 'global)
                           (push `(global-set-key ,key ,def) forms)
                         (when (featurep 'evil)
                           (push (if nucleus--local
                                     `(evil-local-set-key ',state ,key ,def)
                                   `(evil-define-key* ',state 'global ,key ,def))
                                 forms)))))
                    (nucleus--keymaps
                     (dolist (keymap nucleus--keymaps)
                       (push `(define-key ,keymap ,key ,def) forms)))
                    (t
                     (push `(,(if nucleus--local #'local-set-key #'global-set-key)
                             ,key ,def)
                           forms))))
          (setq states '()
                nucleus--local nil
                desc nil)))

       (t (user-error "Invalid key %s" key))))
    `(progn ,@(nreverse forms))))

(provide 'nucleus-keybinds)
;;; nucleus-keybinds.el ends here
