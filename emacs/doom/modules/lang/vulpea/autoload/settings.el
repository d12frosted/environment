;;; lang/vulpea/autoload/settings.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +org-get-buffer-setting (name)
  "Get a setting called NAME from buffer as a string."
  (save-excursion
    (widen)
    (goto-char (point-min))
    (when (re-search-forward (concat "^#\\+" name ": \\(.*\\)") (point-max) t)
      (buffer-substring-no-properties
       (match-beginning 1)
       (match-end 1)))))

;;;###autoload
(defun +org-get-buffer-settings (name &optional separators)
  "Get a setting NAME from buffer as a list using SEPARATORS."
  (split-string (+org-get-buffer-setting name) separators))

;;;###autoload
(defmacro def-org-buffer-setting (name val hook prop doc)
  "Define a buffer setting with NAME and default value VAL.

Value is loaded on HOOK from the Org buffer setting named PROP.

DOC is used as a documentation string."
  `(def-org-buffer-setting-generic
     ,name
     ,val
     ,hook
     (+org-get-buffer-setting ,prop)
     ,(format "%s

Can be set in the org-mode buffer by adding following line in the
top of the file:

#+%s: value"
              doc
              prop)))

;;;###autoload
(defmacro def-org-buffer-setting-list (name val sep hook prop doc)
  "Define a buffer setting with NAME and default value VAL.

Value is loaded on HOOK from the Org buffer setting named PROP
and then split by SEP to become a list.

DOC is used as a documentation string."
  `(def-org-buffer-setting-generic
     ,name
     ,val
     ,hook
     (split-string-and-unquote (+org-get-buffer-setting ,prop) ,sep)
     ,(format "%s

Can be set in the org-mode buffer by adding following line in the
top of the file:

#+%s: values

VALUES must be separated by '%s'."
              doc
              prop
              sep)))

;;;###autoload
(defmacro def-org-buffer-brain-entry-setting (name hook prop doc)
  "Define a buffer setting with NAME and default value VAL.

Value is loaded on HOOK from the Org buffer setting named PROP.

DOC is used as a documentation string."
  `(def-org-buffer-setting-generic
     ,name
     nil
     ,hook
     (+brain-as-entry (+org-get-buffer-setting ,prop))
     ,(format "%s

Can be set in the org-mode buffer by adding following line in the
top of the file:

#+%s: ID"
              doc
              prop)))

;;;###autoload
(defmacro def-org-buffer-setting-generic (name val hook getter doc)
  "Define a buffer setting with NAME and default value VAL.

Value is set on HOOK using a GETTER.

DOC is used as a documentation string."
  `(progn
     (defvar-local ,name ,val ,doc)
     (add-hook ,hook (lambda ()
                       (setq-local ,name ,getter)))))
