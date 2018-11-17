;;; d12-auto-id.el --- Automatically set CUSTOM_ID in org-mode files -*- lexical-binding: t; -*-
;;
;;; Copyright (c) 2015-2018 Boris Buliga
;;
;;; Author: Boris Buliga <boris@d12frosted.io>
;;
;;; URL: https://github.com/d12frosted/environment/emacs
;;
;; Package-X-Original-Version: 0.0.1
;; Package-Requires: ((org "9.0.0"))
;;
;; This file is not part of GNU Emacs.
;;; License: GPLv3
;;
;;; Commentary:
;;
;; based on https://writequit.org/articles/emacs-org-mode-generate-ids.html
;;; Code:

(require 'org-id)

;;;###autoload
(defun d12-auto-id-enable ()
  "Enable and configure auto-id feature."
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  (add-hook 'before-save-hook #'d12-auto-id-add-to-headlines-in-file)
  (add-hook 'org-capture-prepare-finalize-hook
            (lambda () (d12-auto-id-get (point) 'create))))

(defun d12-auto-id-get (&optional pom create prefix)
  "Get the CUSTOM_ID property of the entry at point-or-marker POM.
   If POM is nil, refer to the entry at point. If the entry does
   not have an CUSTOM_ID, the function returns nil. However, when
   CREATE is non nil, create a CUSTOM_ID if none is present
   already. PREFIX will be passed through to `org-id-new'. In any
   case, the CUSTOM_ID of the entry is returned."
  (interactive)
  (org-with-point-at pom
    (org-id-get-create)
    (let ((id (org-entry-get nil "CUSTOM_ID")))
      (cond
       ((and id (stringp id) (string-match "\\S-" id))
        id)
       (create
        (setq id (org-id-new (concat prefix "h")))
        (org-entry-put pom "CUSTOM_ID" id)
        (org-id-add-location id (buffer-file-name (buffer-base-buffer)))
        id)))))

(defun d12-auto-id-add-to-headlines-in-file ()
  "Add CUSTOM_ID properties to all headlines in the current
   file which do not already have one. Only adds ids if the
   `auto-id' option is set to `t' in the file somewhere. ie,
   #+OPTIONS: auto-id:t"
  (interactive)
  (when (and (eq major-mode 'org-mode)
             (eq buffer-read-only nil))
    (save-excursion
      (widen)
      (goto-char (point-min))
      (when (re-search-forward "^#\\+OPTIONS:.*auto-id:t" (point-max) t)
        (org-map-entries (lambda ()
                           (d12-auto-id-get (point) 'create)
                           (org-id-get-create)
                           ))))))

(provide 'd12-auto-id)

;;; d12-auto-id.el ends here
