;;; smart-ops.el --- Elisp DSL for defining smart operators.  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 0.1
;; Package-Requires: ((s "1.9.0") (dash "2.10.0"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides a DSL for declaring operators with intelligent padding. When
;; `smart-ops-mode', any of the declared operators will automatically be
;; formatted after insertion.
;;
;; `smart-ops-mode' also defines a backspace command that deletes through
;; operator padding.
;;
;; Below is an example of setting up smart operators for a mode:
;;
;;   (define-smart-ops-for-mode 'c++-mode
;;     (smart-ops
;;      "+" "-" "/" "%" "^" "|" "~" "!" "=" "<<" ">>" "==" "!=" "&&" "||"
;;      "+=" "-=" "/=" "%=" "^=" "&" "|=" "*=" "<<=" ">>="
;;      :pad-unless 'cpp/after-operator-keyword?)
;;
;;     (smart-ops "." "::" "->" "->*"
;;                :pad-before nil :pad-after nil
;;                :action 'cpp/company-popup)
;;
;;     (smart-ops ":" ";" :pad-before nil)
;;
;;     (smart-ops "&" "*" ","
;;                :pad-before nil
;;                :pad-unless 'cpp/after-operator-keyword?)
;;
;;     ;; Position point inside template braces.
;;     (smart-op "<>"
;;               :pad-before nil :pad-after nil
;;               :action (lambda (&rest _) (search-backward ">")))
;;
;;     (smart-ops "--" "++"
;;                :pad-before nil :pad-after nil
;;                :pad-unless 'cpp/after-operator-keyword?))
;;
;;   (defun cpp/after-operator-keyword? (&rest _)
;;     (smart-ops-looking-at-thing-before-operator? (rx bow "operator" eow (* space))))
;;
;;   (defun cpp/company-popup (&rest _)
;;     (when (and (boundp 'company-mode) company-mode)
;;       (company-manual-begin)))
;;

;;; Code:

(require 'dash)
(require 's)
(autoload 'evil-define-key "evil-core")
(autoload 'sp-backward-delete-char "smartparens")
(autoload 'thing-at-point-looking-at "thingatpt")

(defgroup smart-ops nil
  "Configuration for smart operators."
  :group 'languages
  :prefix "smart-ops-")

(defcustom smart-ops-default-ops '("=" "<" ">" "%" "+" "-" "*" "/" "&" "|" "!" ":")
  "The smart ops to be configured when calling `smart-ops-apply-default-settings'."
  :group 'smart-ops
  :type '(repeat string))

(defcustom smart-ops-backspace-uses-smartparens-backward-delete-char? t
  "By default, backspace will use smartparens if it is installed to delete
matched pairs Set to nil to disable this behaviour."
  :group 'smart-ops
  :type 'boolean)

(defcustom smart-ops-debug? nil
  "If non-nil, print debugging messages."
  :group 'smart-ops
  :type 'boolean)

(defvar smart-ops--log-buffer-name "*Smart Ops Log*")



(defun smart-ops--bol-to-pt ()
  (buffer-substring (line-beginning-position) (point)))

(defun smart-ops--at-operator-section? ()
  (s-matches? (rx "(" (* space) eos) (smart-ops--bol-to-pt)))

(defun smart-ops--line-empty-up-to-point? ()
  (s-matches? (rx bos (* space) eos) (smart-ops--bol-to-pt)))

(defun smart-ops--inside-string-or-comment? ()
  (or (nth 8 (syntax-ppss))
      (-intersection (list font-lock-comment-face font-lock-doc-face font-lock-string-face)
                     (face-at-point nil t))))

(defun smart-ops--op-characters (rules)
  (let ((excluded (-map 'string-to-char '(" ")))
        (op-chars (-uniq (--mapcat (string-to-list (plist-get it :op)) rules))))
    (-difference op-chars excluded)))

(defun smart-ops--maybe-beginning-of-op (rules)
  (save-excursion
    (let ((cs (cons (string-to-char " ") (smart-ops--op-characters rules))))
      (while (and (not (bolp)) (-contains? cs (char-before)))
        (forward-char -1))
      (point))))

(defun smart-ops--maybe-end-of-op (rules)
  (save-excursion
    (let ((cs (cons (string-to-char " ") (smart-ops--op-characters rules))))
      (while (and (not (eolp)) (-contains? cs (char-after)))
        (forward-char 1))
      (point))))

(defun smart-ops--op-at-pt (rules)
  (-when-let* ((beg (smart-ops--maybe-beginning-of-op rules))
               (end (smart-ops--maybe-end-of-op rules))
               (substr (s-replace " " "" (buffer-substring beg end))))
    (unless (s-blank? substr)
      substr)))

(defun smart-ops--maybe-rule-for-op (op rules)
  (--first (equal op (plist-get it :op)) rules))

(defun smart-ops--maybe-rule-for-op-at-pt (rules)
  (-when-let (op-at-pt (smart-ops--op-at-pt rules))
    (smart-ops--maybe-rule-for-op op-at-pt rules)))

(defun smart-ops--apply-pre-padding? (rule op-start-pos)
  (-let [(&plist
          :pad-before pad-before
          :pad-before-if pad-before-if
          :pad-before-unless pad-before-unless
          :pad-if pad-if
          :pad-unless pad-unless)
         rule]
    (when pad-before
      (and (funcall pad-if (point))
           (funcall pad-before-if (point))
           (not (funcall pad-unless (point)))
           (not (funcall pad-before-unless (point)))))))

(defun smart-ops--apply-post-padding? (rule op-end-pos)
  (-let [(&plist
          :pad-after pad-after
          :pad-after-if pad-after-if
          :pad-after-unless pad-after-unless
          :pad-if pad-if
          :pad-unless pad-unless)
         rule]
    (when pad-after
      (and (funcall pad-if (point))
           (funcall pad-after-if (point))
           (not (funcall pad-unless (point)))
           (not (funcall pad-after-unless (point)))))))

(defun smart-ops--unknown-smart-op? (op rules)
  (not (-contains? (--map (plist-get it :op) rules) op)))

(defun smart-ops--apply-padding-rules (rules)
  (save-restriction
    (narrow-to-region (line-beginning-position) (line-end-position))
    (atomic-change-group
      (save-restriction

        (when (s-matches? (rx (* space) ";")
                          (buffer-substring (point) (line-end-position)))
          (narrow-to-region (line-beginning-position) (point)))

        (-if-let* ((_ (smart-ops--logged "In code?"
                        (not (smart-ops--inside-string-or-comment?))))
                   (_ (smart-ops--logged "char-before is an op character:"
                        (-contains? (smart-ops--op-characters rules) (char-before))))
                   (_ (smart-ops--logged "not bypassed?"
                        (not (plist-get (smart-ops--maybe-rule-for-op-at-pt rules) :bypass?))))
                   (_ (smart-ops--logged "deleting space?"
                        (unless (s-blank? (s-trim (buffer-substring (line-beginning-position) (point))))
                          (delete-horizontal-space) t)))
                   (indent (save-excursion (back-to-indentation) (point)))
                   (beg (smart-ops--logged "beg:"
                          (max indent (smart-ops--maybe-beginning-of-op rules))))
                   (end (smart-ops--logged "end:"
                          (smart-ops--maybe-end-of-op rules))))
            (progn
              (-let* (((rule &as &plist :op op :action action)
                       (smart-ops--logged "rule:"
                         (smart-ops--maybe-rule-for-op-at-pt rules)))
                      (op (smart-ops--logged "op:" (or op (smart-ops--op-at-pt rules))))
                      (pre-pad?
                       (save-excursion
                         (goto-char (smart-ops--maybe-beginning-of-op rules))
                         (smart-ops--log-debug "\nPre-pad")
                         (cond
                          ((smart-ops--logged "  - empty line before pt?"
                             (smart-ops--line-empty-up-to-point?))
                           nil)
                          ((smart-ops--logged "  - at operator section?"
                             (smart-ops--at-operator-section?))
                           nil)
                          ((smart-ops--logged "  - apply pre-padding?"
                             (smart-ops--apply-pre-padding? rule beg))
                           t)
                          ((smart-ops--logged "  - unknown op?"
                             (smart-ops--unknown-smart-op? op rules))
                           t))))
                      (post-pad?
                       (progn
                         (smart-ops--log-debug "\nPost-pad")
                         (cond ((smart-ops--logged "  - apply rules?"
                                  (smart-ops--apply-post-padding? rule end))
                                t)
                               ((smart-ops--logged "  - unknown op?"
                                  (smart-ops--unknown-smart-op? op rules))
                                t))))

                      ;; The part of the op inserted after point, so that point can
                      ;; be restored correctly.
                      (op-component-after-pt
                       (let ((str (buffer-substring (point) end)))
                         (smart-ops--log-debug "\nOp substring after point: \"%s\"" str)
                         str)))

                (smart-ops--log-debug "\nDeleting extent: %s" (buffer-substring beg end))
                (delete-region beg end)
                (when (smart-ops--logged "\nInsert pre-pad:" pre-pad?)
                  (smart-ops--log-debug "  - inserting pre-pad...")
                  (insert " ")
                  (smart-ops--log-debug "  - done"))
                (when (smart-ops--logged "\nInsert op:" op)
                  (smart-ops--log-debug "  - inserting op..." op)
                  (insert op)
                  (smart-ops--log-debug "  - done"))
                (when (smart-ops--logged "\nInsert post pad:" post-pad?)
                  (smart-ops--log-debug "  - inserting post-pad...")
                  (insert " ")
                  (smart-ops--log-debug "  - done"))
                (smart-ops--logged "Moving back to point:"
                  (search-backward op-component-after-pt nil t))
                (when action
                  (smart-ops--log-debug "\nCalling action...")
                  (funcall action)
                  (smart-ops--log-debug "  done"))

                (smart-ops--log-debug "\nInserted op.\n------")))

          (smart-ops--log-debug "Not at an operator\n------"))))))

(defmacro smart-ops--logged (note &rest forms)
  (declare (indent 1))
  `(let ((result (progn ,@forms)))
     (smart-ops--log-debug "%s %s" ,note result)
     result))

(font-lock-add-keywords 'emacs-lisp-mode
                        `((,(rx "(" (group (or "smart-ops--logged" "smart-ops--log-debug")))
                           1 font-lock-comment-face)))

(defmacro smart-ops--log-debug (format-string &rest args)
  `(when smart-ops-debug?
     (let ((buf (get-buffer-create smart-ops--log-buffer-name)))
       (with-current-buffer buf
         (goto-char (point-max))
         (newline)
         (insert (format ,format-string ,@args))
         (goto-char (point-max))
         (recenter))
       (display-buffer buf))))

(defvar smart-ops--alist nil
  "Alist of major mode symbols to a list of smart operator rules.
 Use `smart-ops-set-ops-for-mode' to configure this variable.")

;;;###autoload
(defun define-smart-ops-for-mode (mode &rest op-rules)
  "Define the smart ops to be used for MODE.

OPERATOR-RULES define operators in terms of formatting rules. Use
`smart-op' and `smart-ops' to define these."
  (let ((updated (cons (cons mode (-flatten-n 1 op-rules))
                       (assq-delete-all mode smart-ops--alist))))
    (setq smart-ops--alist updated)))

;;;###autoload
(defun smart-ops (&rest settings)
  "Declare settings for several smart ops at once.
See `smart-op' for the valid settings."
  (-let [(ops plist) (-split-with 'stringp settings)]
    (--mapcat (apply 'smart-op it plist) ops)))

;;;###autoload
(defun smart-ops-default-ops (&rest settings)
  "Declare settings for the default smart ops.

See `smart-ops-default-ops' for the default ops.

See `smart-op' for a description of valid SETTINGS."
  (--mapcat (apply 'smart-op it settings) smart-ops-default-ops))

(defsubst smart-ops--validate-args (op plist)
  (unless (stringp op)
    (user-error "smart-op: Invalid argument. OP '%s' was not a string" op))
  (unless (zerop (mod (length plist) 2))
    (user-error "smart-op: Invalid argument. PLIST had an uneven number of elements" op))
  (-when-let* ((plist-keys (-map 'car (-partition 2 plist)))
               (invalid-keys (-reject 'keywordp plist-keys)))
    (user-error "smart-op: Invalid argument. PLIST had keys that were not keywords: %s" invalid-keys)))

(defsubst smart-ops--plist-put-if-absent (k v plist)
  (if (plist-member plist k) plist (plist-put plist k v)))

(defconst smart-ops--nil-fn (lambda (&rest _) nil))
(defconst smart-ops--true-fn (lambda (&rest _) t))

(defsubst smart-ops--apply-defaults-for-absent-keys (plist)
  (->> (copy-sequence plist)
       (smart-ops--plist-put-if-absent :bypass? nil)
       (smart-ops--plist-put-if-absent :action #'ignore)
       (smart-ops--plist-put-if-absent :pad-before  t)
       (smart-ops--plist-put-if-absent :pad-after   t)
       (smart-ops--plist-put-if-absent :pad-before-if smart-ops--true-fn)
       (smart-ops--plist-put-if-absent :pad-before-unless smart-ops--nil-fn)
       (smart-ops--plist-put-if-absent :pad-after-if smart-ops--true-fn)
       (smart-ops--plist-put-if-absent :pad-after-unless smart-ops--nil-fn)
       (smart-ops--plist-put-if-absent :pad-if smart-ops--true-fn)
       (smart-ops--plist-put-if-absent :pad-unless smart-ops--nil-fn)))

;;;###autoload
(defun smart-op (op &rest plist)
  "Define padding behaviour for an operator.

OP is a string representing the operator. The remaining arguments are
keyword-value pairs specifying the behaviour of the generated command:

:bypass? (default nil)     Whether to completely ignore special treatment.

:pad-before (default t)    Specifies whether to insert padding before the
                           operator.

:pad-after (default t)     Specifies whether to insert padding after the
                           operator.

:pad-if                    A unary predicate called with the end position of the
                           operator before any padding is inserted. If this
                           predicate returns non-nil, insert padding in front of
                           or after the operator.

:pad-unless                A unary predicate called with the end position of the
                           operator before any padding is inserted. If this
                           predicate returns non-nil, do not insert padding in
                           front of or after the operator.

:pad-before-if             A unary predicate called with the start position of
                           the operator before any padding is inserted. If the
                           predicate returns non-nil, insert padding in front of
                           the operator.

:pad-before-unless         A unary predicate called with the start position of
                           the operator before any padding is inserted. If the
                           predicate returns non-nil, do not insert padding in
                           front of the operator.

:pad-after-if              A unary predicate called with the end position of the
                           operator before any padding is inserted. If the
                           predicate returns non-nil, insert padding after the
                           operator.

:pad-after-unless          A unary predicate called with the end position of the
                           operator before any padding is inserted. If the
                           predicate returns non-nil, do not insert padding
                           after the operator.

:action                    Action called after all padding actions are complete.

\(fn (op : string) &key [padding-settings...])"
  (smart-ops--validate-args op plist)
  (list (plist-put (smart-ops--apply-defaults-for-absent-keys plist) :op op)))

;;;###autoload
(defun smart-ops-before-match? (regexp)
  "Return a unary function to be used for a predicate in an operator definition.

It will be called with point just after the last inserted operator."
  (lambda (pos)
    (save-excursion
      (goto-char pos)
      (let ((op-to-eol (buffer-substring (point) (line-end-position))))
        (s-matches? regexp op-to-eol)))))

;;;###autoload
(defun smart-ops-after-match? (regexp)
  "Return a unary function to be used for a predicate in an operator definition.

It will be called with point just before the current op."
  (lambda (pos)
    (save-excursion
      (goto-char pos)
      (let ((bol-to-op (buffer-substring (line-beginning-position) (point))))
        (s-matches? regexp bol-to-op)))))



;;;###autoload
(defun turn-on-smart-ops-mode ()
  "Unconditionally enable `smart-ops-global-mode' for programming modes."
  (interactive)
  (when (derived-mode-p 'prog-mode)
    (smart-ops-mode +1)))

(defun smart-ops--after-op? (rules)
  (let ((chars (-map 'char-to-string (smart-ops--op-characters rules))))
    (when chars
      (s-matches? (rx-to-string `(and (regex ,(regexp-opt chars)) (* space) eos) t)
                  (smart-ops--bol-to-pt)))))

(defun smart-ops--skip-back-past-padded-char ()
  (skip-chars-backward " \t")
  (unless (bolp) (forward-char -1))
  (skip-chars-backward " \t")
  (point))

;;;###autoload
(defun smart-ops-delete-last-op ()
  (interactive)
  (let ((rules (smart-ops--rules-for-current-mode)))
    (when (smart-ops--after-op? rules)
      (let ((limit (save-excursion (smart-ops--skip-back-past-padded-char))))
        (save-restriction
          (narrow-to-region limit (point))
          (while (not (or (bobp)
                          (get-char-property (point) 'read-only)))
            (delete-char -1)))
        t))))

(defun smart-ops--detete-backward-char (&optional arg)
  (interactive "P")
  (if (and smart-ops-backspace-uses-smartparens-backward-delete-char?
           (boundp 'smartparens-mode)
           smartparens-mode)
      (call-interactively #'sp-backward-delete-char)
    (delete-char -1)))

;;;###autoload
(defun smart-ops-backspace (&optional arg)
  "Delete backwards, with special handling for operators.

 If point is before a smart ops, deletes the last character of that op and
 any preceding or subsequent whitespace."
  (interactive "P")
  (cond
   ((smart-ops--inside-string-or-comment?)
    (call-interactively #'smart-ops--detete-backward-char))
   ((smart-ops-delete-last-op)
    t)
   (t
    (call-interactively #'smart-ops--detete-backward-char))))

(put 'smart-ops-backspace 'delete-selection 'supersede)

;;;###autoload
(defvar smart-ops-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<backspace>") #'smart-ops-backspace)
    map))

;;;###autoload
(define-minor-mode smart-ops-mode
  "Minor mode for automatically inserting and deleting padding around operators.
Use `define-smart-ops-for-mode' to configure smart ops for a given major mode.

\\{smart-ops-mode-map}"
  :initial-value nil
  :lighter " SmartOp"
  :group 'smart-ops
  (if smart-ops-mode
      (add-hook 'post-self-insert-hook 'smart-ops--post-self-insert-hook-function)
    (remove-hook 'post-self-insert-hook 'smart-ops--post-self-insert-hook-function)))

(defun smart-ops--rules-for-current-mode ()
  (let ((rules (cdr (assoc major-mode smart-ops--alist))))
    (-map 'smart-ops--apply-defaults-for-absent-keys rules)))

(defun smart-ops--post-self-insert-hook-function ()
  (when smart-ops-mode
    (smart-ops--apply-padding-rules (smart-ops--rules-for-current-mode))))

;;;###autoload
(defun smart-ops-insert (op rules)
  "Insert OP using formatting RULES.

If RULES is not supplied, use the default for this major mode.

Useful for Elisp programs."
  (--each rules
    (smart-ops--validate-args op it))
  (insert op)
  (when smart-ops-mode
    (let ((rules (or (-map 'smart-ops--apply-defaults-for-absent-keys rules)
                     (smart-ops--rules-for-current-mode))))
      (smart-ops--apply-padding-rules rules))))

;;;###autoload
(define-globalized-minor-mode smart-ops-global-mode
  smart-ops-mode
  turn-on-smart-ops-mode)

(defun smart-ops-debug-clear-buffer ()
  "Erase the contents of the smart ops debug log buffer."
  (interactive)
  (-when-let (buf (get-buffer smart-ops--log-buffer-name))
    (with-current-buffer buf
      (erase-buffer))
    (when (called-interactively-p nil)
      (message "Log buffer cleared."))))

(defvar smart-ops-debug-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-k") 'smart-ops-debug-clear-buffer)
    map))

(define-minor-mode smart-ops-debug-mode
  "Minor mode for printing debugging information for smart ops."
  :init-value nil
  :keymap smart-ops-debug-mode-map
  :lighter " SODebug"
  (cond
   (smart-ops-debug-mode
    (smart-ops-debug-clear-buffer)
    (setq smart-ops-debug? t))
   (t
    (setq smart-ops-debug? nil))))

(defun smart-ops-init ()
  (with-eval-after-load 'evil
    (evil-define-key 'insert smart-ops-mode-map (kbd "<backspace>") nil)
    (evil-define-key 'normal smart-ops-mode-map (kbd "<backspace>") #'ignore))
  (smart-ops-global-mode))

(provide 'smart-ops)

;;; smart-ops.el ends here
