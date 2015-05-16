;;; org-funcs.el --- funcs file of org configurations
;;
;; Copyright (c) 2014-2015 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@icloud.com>
;; Maintainer: Boris Buliga <d12frosted@icloud.com>
;; Created: 02 May 2015
;;
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

;;; Code

(defun gtd ()
   (interactive)
   (find-file (concat d12/org-home-path "gtd.org")))

(defun d12/reload-agenda-files ()
  (interactive)
  (let* ((d12/org-ignored-dirs (-flatten
                                (-non-nil
                                 (-map (lambda (dir)
                                         (d12/org-dir-and-subdirs dir))
                                       d12/org-agenda-ignore-dirs))))
         (d12/org-agenda-dirs (-difference (d12/org-dir-and-subdirs "") d12/org-ignored-dirs))
         (d12/org-agenda-files (-flatten (-map (lambda (dir)
                                                 (d12/org-files-in-folder dir))
                                               d12/org-agenda-dirs))))
    (setq org-agenda-files d12/org-agenda-files)))

;;; Files and driectories
;; =======================

(defun d12/org-dir-and-subdirs (dir)
      (let ((org-dir (concat d12/org-home-path dir)))
        (when (file-directory-p org-dir)
          (-insert-at 0 (s-chop-suffix "/" org-dir) (d12/directory-dirs-r org-dir)))))

(defun d12/org-files-in-folder (folder)
  (directory-files folder t ".*\.org$\\|[0-9]+-[0-9]+-[0-9]+$"))

;;; Templates
;; ===========

(defun d12/org-insert-block-template ()
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

(defun d12/org-new-file-template ()
  "Create template for new org file."
  (let ((option-key-width 16)
        (title (d12/org-guess-title))
        (author (s-format "$0 <$1>" 'elt `(,d12/org-author-name ,d12/org-author-email)))
        (email d12/org-author-email)
        (date (format "<%s>" (format-time-string "%Y-%m-%d"))))
    (s-join "\n" (list (d12/org-option option-key-width "TITLE" title)
                       (d12/org-option option-key-width "AUTHOR" author)
                       (d12/org-option option-key-width "EMAIL" email)
                       (d12/org-option option-key-width "DATE" date)
                       (d12/org-option option-key-width "STARTUP" "showeverything")
                       (d12/org-option option-key-width "OPTIONS" "toc:nil")))))

(defun d12/org-journal-date-header ()
  (concat org-journal-date-prefix
          (format-time-string org-journal-date-format)))

(defun d12/org-auto-insert-template ()
  "Insert template for the newly created buffer."
  (interactive)
  (unless (d12/org-buffer-contains-header?)
    (goto-char (point-min))
    (insert (d12/org-new-file-template))
    (insert "\n\n")
    (unless (and (d12/org-journal-buffer-contains-date-header?)
                 (eq major-mode "Journal"))
      (insert (d12/org-journal-date-header))
      (insert "\n"))
    (goto-char (point-max))))

;;; Predicates
;; ============

(defun d12/org-buffer-contains-header? ()
  "Does current buffer contain org header?"
  (interactive)
  (let ((empty (= (point-min)
                  (point-max)))
        (titled (or t (s-contains? "#+TITLE:" (buffer-string) t))))
    (and titled (not empty))))

(defun d12/org-journal-buffer-contains-date-header? ()
  "Does current buffer contain date header?"
  (interactive)
  (buffer-contains-substring? (d12/org-journal-date-header)))

;;; Various functions
;; ===================

(defun d12/org-guess-title ()
  "Try to guess title for org file.
In case of failure it will use value of d12/org-default-title."
  (let ((bname (buffer-name)))
    (if (s-present? bname)
        (if (s-suffix? ".org" bname)
            (substring bname 0 -4)
          bname)
      d12/org-default-title)))

(defun d12/org-option (width key value)
  "Create an option string for org file."
  (s-append value (s-pad-right width " " (s-concat "#+" key ":"))))

(defun d12/org-sort-current-level ()
  "Sort current level by TODO."
  (interactive)
  (org-sort-entries nil ?o))

(defun d12/org-sort-upper-level ()
  "Go to upper level and sort it by TODO."
  (interactive)
  (progn (outline-up-heading 1)
         (d12/org-sort-current-level)))

(defun d12/org-create-new-org-file (name)
  (interactive "sEnter the name of new file: ")

  (let ((existing-files (d12/directory-dirs d12/org-home-path))
        (new-file-dir (s-concat d12/org-home-path name) ))
    (if (-contains? existing-files new-file-dir)
        (message "Sorry, but there is already file named '%s'." name)
      (progn (dired-create-directory new-file-dir)
             (dired-create-directory (s-concat new-file-dir "exports"))
             (dired-create-directory (s-concat new-file-dir "assets"))
             (with-temp-buffer (write-file (s-concat new-file-dir (s-append ".org" name))))))))

(defun d12/org-insert-date ()
  "Insert timestamp formated by value of d12/org-date-format"
  (interactive)
  (insert (format-time-string d12/org-date-format)))

(defun d12/org-insert-time ()
  "Insert timestamp formated by value of d12/org-time-format"
  (interactive)
  (insert (format-time-string d12/org-time-format)))

(defun d12/org-insert-full-date ()
  "Insert date and timestamp. Uses 'd12/org-insert-date
  and 'd12/org-insert-time."
  (interactive)
  (insert (format-time-string (concat d12/org-date-format " " d12/org-time-format))))

(defun org-journal-visit-entry ()
  (interactive)
  (setq current-prefix-arg '(t))
  (call-interactively 'org-journal-new-entry))

(defun d12/org-update-parent-cookie ()
  (when (equal major-mode 'org-mode)
    (save-excursion
      (ignore-errors
        (org-back-to-heading)
        (org-update-parent-todo-statistics)))))

(defadvice d12/delete-line-or-region (after fix-cookies activate)
  (d12/org-update-parent-cookie))

(defadvice d12/duplicate-line-or-region (after fix-cookies activate)
  (d12/org-update-parent-cookie))

(defadvice d12/cut-line-or-region (after fix-cookies activate)
  (d12/org-update-parent-cookie))
