;;; funcs.el --- d12frosted-org layer funcs file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Boris Buliga <d12frosted@d12frosted.local>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defun d12-org/reload-agenda-files ()
  "Reload agenda files. Useful for situations when file is added
or removed from agenda files directory."
  (interactive)
  (setq d12-org/files-list (d12-files/query "*.org" d12-path/org-home 1))
  (setq org-agenda-files d12-org/files-list)
  (d12-interesting-files-add d12-org/files-list))

(when (configuration-layer/layer-usedp 'helm)
  (defun d12-helm/gtd-source ()
    "Construct helm source from `d12-org/files-list'."
    `((name . "org files")
      (candidates . d12-org/files-list)
      (candidate-number-limit)
      (action . (("Open file" . find-file))))))

(defun org-global-props (&optional property buffer)
  "Get the plists of global org properties of current
buffer."
  (unless property (setq property "PROPERTY"))
  (with-current-buffer (or buffer (current-buffer))
    (org-element-map (org-element-parse-buffer) 'keyword (lambda (el) (when (string-match property (org-element-property :key el)) el)))))

(defun org-global-prop-value (key)
  "Get global org property KEY of current buffer."
  (org-element-property :value (car (org-global-props key))))

(defun d12//org-mode-setup-title ()
  (when-let ((title (org-global-prop-value "title")))
    (rename-buffer title)))

(defun d12-org/insert-block-template ()
  "Insert block template at point."
  (interactive)
  (if (org-at-table-p)
      (call-interactively 'org-table-rotate-recalc-marks)
    (let* ((choices '(("s" . "SRC")
                      ("e" . "EXAMPLE")
                      ("h" . "HTML")
                      ("q" . "QUOTE")
                      ("c" . "CENTER")))
           (key
            (key-description
             (vector
              (read-key
               (concat (propertize "Template type: " 'face 'minibuffer-prompt)
                       (mapconcat (lambda (choice)
                                    (concat (propertize (car choice) 'face 'font-lock-type-face)
                                            ": "
                                            (cdr choice)))
                                  choices
                                  ", ")))))))
      (let ((result (assoc key choices)))
        (when result
          (let ((choice (cdr result)))
            (cond
             ((region-active-p)
              (let ((start (region-beginning))
                    (end (region-end)))
                (goto-char end)
                (insert "\n#+END_" choice)
                (goto-char start)
                (insert "#+BEGIN_" choice "\n")))
             (t
              (insert "#+BEGIN_" choice "\n")
              (save-excursion (insert "\n#+END_" choice))))))))))

(defun d12-org/sort-current-level ()
  "Sort current level by TODO."
  (interactive)
  (org-sort-entries nil ?o))

(defun d12-org/sort-upper-level ()
  "Go to upper level and sort it by TODO."
  (interactive)
  (progn (outline-up-heading 1)
         (d12/org-sort-current-level)))

(defun d12-org/visit-journal-entry ()
  (interactive)
  (setq current-prefix-arg '(t))
  (call-interactively 'org-journal-new-entry))

(defun d12-org/update-parent-cookie ()
  (when (equal major-mode 'org-mode)
    (save-excursion
      (ignore-errors
        (org-back-to-heading)
        (org-update-parent-todo-statistics)))))

(defadvice d12/delete-line-or-region (after fix-cookies activate)
  (d12-org/update-parent-cookie))

(defadvice d12/duplicate-line-or-region (after fix-cookies activate)
  (d12-org/update-parent-cookie))

(defadvice d12/cut-line-or-region (after fix-cookies activate)
  (d12-org/update-parent-cookie))

(defun org-clock-get-clock-string ()
  "Form a clock-string, that will be shown in the mode line.
If an effort estimate was defined for the current item, use
01:30/01:50 format (clocked/estimated).
If not, show simply the clocked time like 01:50."
  (let ((clocked-time (org-clock-get-clocked-time)))
    (if org-clock-effort
        (let* ((effort-in-minutes
                (org-duration-string-to-minutes org-clock-effort))
               (work-done-str
                (org-propertize
                 (org-minutes-to-clocksum-string clocked-time)
                 'face (if (and org-clock-task-overrun (not org-clock-task-overrun-text))
                           'org-mode-line-clock-overrun 'org-mode-line-clock)))
               (effort-str (org-minutes-to-clocksum-string effort-in-minutes))
               (clockstr (org-propertize (concat  " [%s/" effort-str "] ")
                                         'face 'org-mode-line-clock)))
          (format clockstr work-done-str))
      (org-propertize (concat "[" (org-minutes-to-clocksum-string clocked-time) "]")
                      'face 'org-mode-line-clock))))

;; http://lists.gnu.org/archive/html/emacs-orgmode/2010-11/msg00542.html
(defun d12/org-agenda-day-face-holidays-function (date)
  "Compute DATE face for holidays."
  (unless (org-agenda-todayp date)
    (dolist (file (org-agenda-files nil 'ifmode))
      (let ((face
             (dolist (entry (org-agenda-get-day-entries file date))
               (let ((category (with-temp-buffer
                                 (insert entry)
                                 (org-get-category (point-min)))))
                 (when (or (string= "Holidays" category)
                           (string= "Vacation" category))
                   (return 'org-agenda-date-weekend))))))
        (when face (return face))))))

(defun d12/toggle-org-html-export-on-save ()
  (interactive)
  (if (memq 'org-html-export-to-html after-save-hook)
      (progn
        (remove-hook 'after-save-hook 'org-html-export-to-html t)
        (message "Disabled org html export on save for current buffer..."))
    (add-hook 'after-save-hook 'org-html-export-to-html nil t)
    (message "Enabled org html export on save for current buffer...")))

(defun d12-org/insert-github-issue-link (repo number)
  (insert (format "[[https://github.com/%s/issues/%s][%s#%s]] "
                  repo number
                  repo number)))

(defun d12-org/insert-spacemacs-issue-link (number)
  (interactive "n")
  (d12-org/insert-github-issue-link "syl20bnr/spacemacs" number))

;; http://emacs.stackexchange.com/questions/16688/how-can-i-escape-the-in-org-mode-to-prevent-bold-fontification/16746#16746
(defun d12-org/entity-get-name (char)
  "Return the entity name for CHAR. For example, return \"ast\" for *."
  (let ((ll (append org-entities-user
                    org-entities))
        e name utf8)
    (catch 'break
      (while ll
        (setq e (pop ll))
        (when (not (stringp e))
          (setq utf8 (nth 6 e))
          (when (string= char utf8)
            (setq name (car e))
            (throw 'break name)))))))

;; http://emacs.stackexchange.com/questions/16688/how-can-i-escape-the-in-org-mode-to-prevent-bold-fontification/16746#16746
(defun d12-org/insert-org-entity-maybe (&rest args)
  "When the universal prefix C-u is used before entering any character,
    insert the character's `org-entity' name if available.

    If C-u prefix is not used and if `org-entity' name is not available, the
    returned value `entity-name' will be nil."
  ;; It would be fine to use just (this-command-keys) instead of
  ;; (substring (this-command-keys) -1) below in emacs 25+.
  ;; But if the user pressed "C-u *", then
  ;;  - in emacs 24.5, (this-command-keys) would return "^U*", and
  ;;  - in emacs 25.x, (this-command-keys) would return "*".
  ;; But in both versions, (substring (this-command-keys) -1) will return
  ;; "*", which is what we want.
  ;; http://thread.gmane.org/gmane.emacs.orgmode/106974/focus=106996
  (let ((pressed-key (substring (this-command-keys) -1))
        entity-name)
    (when (and (listp args) (eq 4 (car args)))
      (setq entity-name (d12-org/entity-get-name pressed-key))
      (when entity-name
        (setq entity-name (concat "\\" entity-name "{}"))
        (insert entity-name)
        (message (concat "Inserted `org-entity' "
                         (propertize entity-name
                                     'face 'font-lock-function-name-face)
                         " for the symbol "
                         (propertize pressed-key
                                     'face 'font-lock-function-name-face)
                         "."))))
    entity-name))

;;; funcs.el ends here
