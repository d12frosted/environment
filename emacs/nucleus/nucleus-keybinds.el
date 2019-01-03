;;; nucleus-keybinds.el --- the heart of every cell -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;;         Henrik Lissner <henrik@lissner.net>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 23 Nov 2018
;;
;; URL: https://github.com/d12frosted/environment/emacs
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;; Most of the code was borrowed from hlissner/doom-emacs.
;;
;;; Commentary:
;;
;;; Code:

(defvar nucleus-leader-key "SPC"
  "The leader prefix key for Evil users.")

(defvar nucleus-leader-alt-key "M-m"
  "An alternative leader prefix key, used for Insert and Emacs
states, and for non-evil users.")

(defvar nucleus-localleader-key "SPC m"
  "The localleader prefix key, for major-mode specific
  commands.")

(defvar nucleus-localleader-alt-key "M-m m"
  "The localleader prefix key, for major-mode specific
  commands.")

(defvar nucleus-leader-map (make-sparse-keymap)
  "An overriding keymap for <leader> keys.")

;;
;; Quitter

(defvar nucleus-escape-hook nil
  "A hook run after C-g is pressed.

Triggers `nucleus/escape'.

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
;; General

(require 'general)

;; Convenience aliases
(defalias 'define-key! #'general-def)
(defalias 'unmap! #'general-unbind)

;; leader/localleader keys
(define-prefix-command 'nucleus-leader 'nucleus-leader-map)
(defvar nucleus-leader-alist `((t . ,nucleus-leader-map)))
(add-to-list 'emulation-mode-map-alists 'nucleus-leader-alist)

;; We avoid `general-create-definer' to ensure that :states, :prefix and
;; :keymaps cannot be overwritten.
(defmacro define-leader-key! (&rest args)
  `(general-define-key
    :states nil
    :keymaps 'nucleus-leader-map
    :prefix nucleus-leader-alt-key
    ,@args))

(general-create-definer define-localleader-key!
  :major-modes t
  :wk-full-keys nil
  :prefix nucleus-localleader-alt-key)

;; Because :non-normal-prefix doesn't work for non-evil sessions (only evil's
;; emacs state), we must redefine `define-localleader-key!' once evil is loaded
(after! evil
  (defmacro define-leader-key! (&rest args)
    `(general-define-key
      :states '(normal visual motion insert)
      :keymaps 'nucleus-leader-map
      :prefix nucleus-leader-key
      :non-normal-prefix nucleus-leader-alt-key
      ,@args))

  (general-create-definer define-localleader-key!
    :states (cdr general-describe-evil-states)
    :major-modes t
    :wk-full-keys nil
    :prefix nucleus-localleader-key
    :non-normal-prefix nucleus-localleader-alt-key))

;;
;; Packages

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
(put :localleader  'lisp-indent-function 'defun)
(put :map          'lisp-indent-function 'defun)
(put :keymap       'lisp-indent-function 'defun)
(put :mode         'lisp-indent-function 'defun)
(put :prefix       'lisp-indent-function 'defun)
(put :unless       'lisp-indent-function 'defun)
(put :if           'lisp-indent-function 'defun)
(put :when         'lisp-indent-function 'defun)

;; specials
(defvar nucleus--map-forms nil)
(defvar nucleus--map-fn nil)
(defvar nucleus--map-batch-forms nil)
(defvar nucleus--map-state '(:dummy t))
(defvar nucleus--map-parent-state nil)
(defvar nucleus--map-evil-p nil)
(after! evil (setq nucleus--map-evil-p t))

(defun nucleus--map-process (rest)
  (let ((nucleus--map-fn nucleus--map-fn)
        nucleus--map-state
        nucleus--map-forms
        desc)
    (while rest
      (let ((key (pop rest)))
        (cond ((listp key)
               (nucleus--map-nested nil key))

              ((keywordp key)
               (pcase key
                 (:leader
                  (nucleus--map-commit)
                  (setq nucleus--map-fn 'define-leader-key!))
                 (:localleader
                  (nucleus--map-commit)
                  (setq nucleus--map-fn 'define-localleader-key!))
                 (:after
                  (nucleus--map-nested (list 'after! (pop rest)) rest)
                  (setq rest nil))
                 (:desc
                  (setq desc (pop rest)))
                 ((or :map :map* :keymap)
                  (nucleus--map-set :keymaps `(quote ,(nucleus-enlist (pop rest)))))
                 (:mode
                  (push (cl-loop for m in (nucleus-enlist (pop rest))
                                 collect (intern (concat (symbol-name m) "-map")))
                        rest)
                  (push :map rest))
                 ((or :if :when :unless)
                  (nucleus--map-nested (list (intern (nucleus-keyword-name key)) (pop rest)) rest)
                  (setq rest nil))
                 (:prefix
                  (cl-destructuring-bind (prefix . desc) (nucleus-enlist (pop rest))
                    (nucleus--map-set (if nucleus--map-fn :infix :prefix)
                                   prefix)
                    (when (stringp desc)
                      (setq rest (append (list :desc desc "" nil) rest)))))
                 (:textobj
                  (let* ((key (pop rest))
                         (inner (pop rest))
                         (outer (pop rest)))
                    (push `(map! (:map evil-inner-text-objects-map ,key ,inner)
                                 (:map evil-outer-text-objects-map ,key ,outer))
                          nucleus--map-forms)))
                 (_
                  (condition-case e
                      (nucleus--map-def (pop rest) (pop rest) (nucleus--keyword-to-states key) desc)
                    (error
                     (error "Not a valid `map!' property: %s" key)))
                  (setq desc nil))))

              ((nucleus--map-def key (pop rest) nil desc)
               (setq desc nil)))))

    (nucleus--map-commit)
    (macroexp-progn (nreverse (delq nil nucleus--map-forms)))))

(defun nucleus--map-append-keys (prop)
  (let ((a (plist-get nucleus--map-parent-state prop))
        (b (plist-get nucleus--map-state prop)))
    (if (and a b)
        `(general--concat nil ,a ,b)
      (or a b))))

(defun nucleus--map-nested (wrapper rest)
  (nucleus--map-commit)
  (let ((nucleus--map-parent-state (nucleus--map-state)))
    (push (if wrapper
              (append wrapper (list (nucleus--map-process rest)))
            (nucleus--map-process rest))
          nucleus--map-forms)))

(defun nucleus--map-set (prop &optional value)
  (unless (equal (plist-get nucleus--map-state prop) value)
    (nucleus--map-commit))
  (setq nucleus--map-state (plist-put nucleus--map-state prop value)))

(defun nucleus--map-def (key def &optional states desc)
  (when (or (memq 'global states)
            (null states))
    (setq states (cons 'nil (delq 'global states))))
  (when desc
    (let (unquoted)
      (cond ((and (listp def)
                  (keywordp (car-safe (setq unquoted (nucleus-unquote def)))))
             (setq def (list 'quote (plist-put unquoted :which-key desc))))
            ((setq def (cons 'list
                             (if (and (equal key "")
                                      (null def))
                                 `(nil :which-key ,desc)
                               (plist-put (general--normalize-extended-def def)
                                          :which-key desc))))))))
  (dolist (state states)
    (push (list key def)
          (alist-get state nucleus--map-batch-forms)))
  t)

(defun nucleus--map-commit ()
  (when nucleus--map-batch-forms
    (cl-loop with attrs = (nucleus--map-state)
             for (state . defs) in nucleus--map-batch-forms
             if (or nucleus--map-evil-p (not state))
             collect `(,(or nucleus--map-fn 'general-define-key)
                       ,@(if state `(:states ',state)) ,@attrs
                       ,@(mapcan #'identity (nreverse defs)))
             into forms
             finally do (push (macroexp-progn forms) nucleus--map-forms))
    (setq nucleus--map-batch-forms nil)))

(defun nucleus--map-state ()
  (let ((plist
         (append (list :prefix (nucleus--map-append-keys :prefix)
                       :infix  (nucleus--map-append-keys :infix)
                       :keymaps
                       (append (plist-get nucleus--map-parent-state :keymaps)
                               (plist-get nucleus--map-state :keymaps)))
                 nucleus--map-state
                 nil))
        newplist)
    (while plist
      (let ((key (pop plist))
            (val (pop plist)))
        (when (and val (not (plist-member newplist key)))
          (push val newplist)
          (push key newplist))))
    newplist))

;;
(defmacro map! (&rest rest)
  "A convenience macro for defining keybinds, powered by `general'.

If evil isn't loaded, evil-specific bindings are ignored.

States
  :n  normal
  :v  visual
  :i  insert
  :e  emacs
  :o  operator
  :m  motion
  :r  replace
  :g  global  (binds the key without evil `current-global-map')

  These can be combined in any order, e.g. :nvi will apply to normal, visual and
  insert mode. The state resets after the following key=>def pair. If states are
  omitted the keybind will be global (no emacs state; this is different from
  evil's Emacs state and will work in the absence of `evil-mode').

Properties
  :leader [...]                   an alias for (:prefix nucleus-leader-key ...)
  :localleader [...]              bind to localleader; requires a keymap
  :mode [MODE(s)] [...]           inner keybinds are applied to major MODE(s)
  :map [KEYMAP(s)] [...]          inner keybinds are applied to KEYMAP(S)
  :keymap [KEYMAP(s)] [...]       same as :map
  :prefix [PREFIX] [...]          set keybind prefix for following keys
  :after [FEATURE] [...]          apply keybinds when [FEATURE] loads
  :textobj KEY INNER-FN OUTER-FN  define a text object keybind pair
  :if [CONDITION] [...]
  :when [CONDITION] [...]
  :unless [CONDITION] [...]

  Any of the above properties may be nested, so that they only apply to a
  certain group of keybinds.

Example
  (map! :map magit-mode-map
        :m  \"C-r\" 'do-something           ; C-r in motion state
        :nv \"q\" 'magit-mode-quit-window   ; q in normal+visual states
        \"C-x C-r\" 'a-global-keybind
        :g \"C-x C-r\" 'another-global-keybind  ; same as above

        (:when IS-MAC
         :n \"M-s\" 'some-fn
         :i \"M-o\" (lambda (interactive) (message \"Hi\"))))"
  (nucleus--map-process rest))

(provide 'nucleus-keybinds)
;;; nucleus-keybinds.el ends here
