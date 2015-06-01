;;; omnisharp-funcs.el --- funcs file of omnisharp configurations
;;
;; Copyright (c) 2014-2015 Boris Buliga
;;
;; Author: Boris Buliga <d12@icloud.com>
;; Maintainer: Boris Buliga <d12@icloud.com>
;; Created: 04 May 2015
;;
;; URL: https://github.com/d12/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

;;; Code

;;; Setup
;; =======

(defun d12/omnisharp-setup ()
  (progn   (setq-local indent-tabs-mode t)
           (setq-local c-default-style "k&r")
           (setq-local c-basic-offset 4)
           (setq-local tab-width 4)
           (setq-local hs-isearch-open t)
           (c-set-offset 'case-label '+)
           (c-set-offset 'cpp-macro 'csharp-lineup-region)))

;;; Hide-show
;; ===========

(defun csharp-hs-forward-sexp (&optional arg)
    "Stolen from emacswiki"
    (message "csharp-hs-forward-sexp, (arg %d) (point %d)..."
             (if (numberp arg) arg -1)
             (point))

    (let ((nestlevel 0)
          (mark1 (point))
          (done nil))

      (if (and arg (< arg 0))
          (message "negative arg (%d) is not supported..." arg)

        ;; else, we have a positive argument, hence move forward.
        ;; simple case is just move forward one brace
        (if (looking-at "{")
            (forward-sexp arg)

          ;; The more complex case is dealing with a "region/endregion" block.
          ;; We have to deal with nested regions!
          (and
           (while (not done)
             (re-search-forward "^[ \\t]*#[ \\t]*\\(region\\|endregion\\)\\b"
                                (point-max) 'move)
             (cond

              ;; do nothing if at end of buffer
              ((eobp))

              ((and
                (match-beginning 1)
                ;; if the match is longer than 6 chars, we know it is "endregion"
                (if (> (- (match-end 1) (match-beginning 1)) 6)
                    (setq nestlevel (1- nestlevel))
                  (setq nestlevel (1+ nestlevel))))))

             (setq done (not (and (> nestlevel 0) (not (eobp))))))

           (if (= nest 0)
               (goto-char (match-end 2))))))))

;;; Faces
;; =======

(defun d12/csharp-highlight-directives ()
  (interactive)

  ;; if directive
  (make-face 'font-lock-csharp-if-directive-face)
  (set-face-attribute 'font-lock-csharp-if-directive-face nil
                      :height 1.0
                      :weight 'bold
                      :overline "#A7A7A7"
                      :foreground "#3C3C3C"
                      :background "#F0F0F0")
  (setq csharp-if-directive-font-lock `(("\\([ ]*#\\(if .*\\|else\\|endif\\)\\)" 1 'font-lock-csharp-if-directive-face t)))

  ;; region directive
  (make-face 'font-lock-csharp-region-directive-face)
  (set-face-attribute 'font-lock-csharp-region-directive-face nil
                      :height 1.0
                      :weight 'bold
                      :overline "#123555"
                      :foreground "#123555"
                      :background "#E5F4FB")
  (setq csharp-region-directive-font-lock `(("\\([ ]*#\\(region .*\\|endregion\\)\\)" 1 'font-lock-csharp-region-directive-face t)))

  ;; update font-lock
  (font-lock-add-keywords 'csharp-mode csharp-if-directive-font-lock)
  (font-lock-add-keywords 'csharp-mode csharp-region-directive-font-lock))

;;; Other stuff
;; =============

(defun d12/omnisharp-go-to-definition-at-center ()
  (interactive)
  (progn
    (omnisharp-go-to-definition)
    (recenter)))

(defun d12/omnisharp-comment-to-doc ()
  "Convert regular comment at point int to documentation comment."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (search-forward-regexp "\\([ 	]+\\)//\\(.*\\)" nil t)
      (replace-match (concat (match-string 1)
                             "/// <summary>\n"
                             (match-string 1)
                             "/// "
                             (match-string 2)
                             "\n"
                             (match-string 1)
                             "/// </summary>") t nil))))
