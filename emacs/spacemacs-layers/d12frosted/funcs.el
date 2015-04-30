;;; funcs.el --- d12frosted Layer funcs File for Spacemacs
;;
;; Copyright (c) 2014-2015 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@icloud.com>
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

;;; directoriy and file manipulations

(defun d12frosted/directory-dirs (dir)
  "Find all directories in DIR."
  (unless (file-directory-p dir)
    (error "Not a directory `%s'" dir))
  (-filter (lambda (f) (file-directory-p f))
           (-map (lambda (f) (s-concat dir f))
                 (-filter (lambda (f) (not (-contains? '("." "..") f))) (directory-files dir)))))

(defun d12frosted/directory-dirs-r (dir)
  "Find all directories in DIR (recursive)."
  (unless (file-directory-p dir)
    (error "Not a directory `%s'" dir))
  (let ((dir (directory-file-name dir))
        (dirs '())
        (files (directory-files dir nil nil t)))
    (dolist (file files)
      (unless (member file '("." ".."))
        (let ((file (concat dir "/" file)))
          (when (file-directory-p file)
            (setq dirs (append (cons file
                                     (d12frosted/directory-dirs-r file))
                               dirs))))))
    dirs))

(defun rename-current-file (new-name)
  "Change the name of current file and buffer to NAME."
  (interactive "sEnter new name for this file: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun d12frosted/invalidate-cache ()
  "Invalidate projectile and recentf cache."
  (interactive)
  (progn (projectile-invalidate-cache nil)
         (recentf-cleanup)))

;;; text manipulations

(defun comment-dwim-line (&optional arg)
  "Do-what-I-mean commenting the current line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(defun copy-line-or-region ()
  "Copy current line (with newline character) or region. When `universal-argument' is called first, copy whole buffer (but respect `narrow-to-region')."
  (interactive)
  (kill-ring-save (line-or-region-point-min)
                  (line-or-region-point-max))
  (message "copied"))

(defun cut-line-or-region ()
  "Cut current line or region. When `universal-argument' is called first, cut whole buffer (but respect `narrow-to-region')."
  (interactive)
  (kill-region (line-or-region-point-min)
               (line-or-region-point-max))
  (message "cut"))

(defun duplicate-line-or-region ()
  "Duplicates current line or region. When `universal-argument' is called first, duplicate whole buffer (but respect `narrow-to-region')."
  (interactive)
  (kill-ring-save (line-or-region-point-min)
                  (line-or-region-point-max))
  (move-beginning-of-line 1)
  (yank)
  (message "duplicated"))

(defun delete-line-or-region ()
  "Delete current line or region without putting it to kill-ring. When `universal-argument' is called first, duplicate whole buffer (but respect `narrow-to-region')."
  (interactive)
  (delete-region (line-or-region-point-min)
                 (line-or-region-point-max))
  (message "deleted"))

(defun line-or-region-point-min ()
  "Return min point of line or region. When `universal-argument' is called first, returns min point of whole buffer (but respect `narrow-to-region')."
  (if (null current-prefix-arg)
      (if (use-region-p)
          (region-beginning)
        (line-beginning-position))
    (point-min)))

(defun line-or-region-point-max ()
  "Return max point of line or region. When `universal-argument' is called first, returns max point of whole buffer (but respect `narrow-to-region')."
  (if (null current-prefix-arg)
      (if (use-region-p)
          (region-end)
        (line-beginning-position 2))
    (point-max)))

;;; omnisharp config

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

(defun d12frosted/omnisharp-go-to-definition-at-center ()
  (interactive)
  (progn
    (omnisharp-go-to-definition)
    (recenter)))

(defun d12frosted/omnisharp-comment-to-doc ()
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

(defun d12frosted/omnisharp-config ()
  "Function that should be called when omnisharp mode is enabled."

  (spacemacs|diminish omnisharp-mode " ♯" " #")

  (setq indent-tabs-mode t
        c-default-style "k&r"
        c-basic-offset 4
        tab-width 4
        hs-isearch-open t)

  (c-set-offset 'case-label '+)
  (c-set-offset 'cpp-macro 'csharp-lineup-if-and-region)

  (local-set-key (kbd "C-c <") 'hs-hide-block)
  (local-set-key (kbd "C-c >") 'hs-show-block)

  (local-unset-key (kbd "{"))
  (local-unset-key (kbd "C-c C-d"))

  (evil-leader/set-key-for-mode 'csharp-mode
    "mgg" 'd12frosted/omnisharp-go-to-definition-at-center)

  (local-set-key (kbd "C-.") 'omnisharp-auto-complete))

;;; stuff

(defun d12/goto-line-and-center ()
  (interactive)
  (call-interactively 'goto-line)
  (call-interactively 'recenter-top-bottom))
