;;; funcs.el --- d12frosted-omnisharp Layer funcs File for Spacemacs
;;
;; Copyright (c) 2014-2015 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@icloud.com>
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

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

(defun omnisharp/go-to-definition-at-center ()
  (interactive)
  (omnisharp-go-to-definition)
  (recenter))

(defun omnisharp/on-load-fn ()
  "Function that should be called when omnisharp mode is enabled."

  (setq indent-tabs-mode t
            c-default-style "k&r"
            c-basic-offset 2
            hs-isearch-open t)

  (c-set-offset 'case-label '+)
  (c-set-offset 'cpp-macro 'csharp-lineup-if-and-region)

  (local-set-key (kbd "C-c <") 'hs-hide-block)
  (local-set-key (kbd "C-c >") 'hs-show-block)

  (local-unset-key (kbd "{"))
  (local-unset-key (kbd "/"))
  (local-unset-key (kbd "C-c C-d"))

  (local-set-key (kbd "M-.") 'omnisharp-auto-complete)

  ;; todo - menu integration!
  (local-set-key (kbd "C-c o y") 'go-to-definition-at-center)
  (local-set-key (kbd "C-c o u") 'omnisharp-find-usages)
  (local-set-key (kbd "C-c o r") 'omnisharp-rename)
  (local-set-key (kbd "C-c o i") 'omnisharp-current-type-information)
  (local-set-key (kbd "C-c o f") 'omnisharp-navigate-to-solution-file)
  (local-set-key (kbd "C-c o g") 'omnisharp-navigate-to-current-file-member)
  (local-set-key (kbd "C-c o o") 'omnisharp-auto-complete-overrides))
