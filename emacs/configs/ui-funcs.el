;;; ui-funcs.el --- funcs file of ui configurations
;;
;; Copyright (c) 2014-2015 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@icloud.com>
;; Maintainer: Boris Buliga <d12frosted@icloud.com>
;; Created: 11 May 2015
;;
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

;;; Code

(defun d12/update-header-line ()
  "Setup and update header line."
  (when eldoc-mode
    (let ((line-format (concat (unless (= (length d12/eldoc-msg-format) 0)
                                 (concat " [" d12/eldoc-msg-format "] "))))
          (line-args d12/eldoc-msg-args))
      (setq header-line-format (apply 'format line-format line-args))
      (force-mode-line-update))))

(defun highlight-TODO-words ()
  "Highlight keywords for  "
  (interactive)
  (font-lock-add-keywords
   nil '(("\\<\\(\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):\\)"
          1 font-lock-warning-face t))))
