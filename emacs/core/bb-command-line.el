;;; bb-command-line.el --- command-line file for personal configurations -*- lexical-binding: t; -*-
;;
;;; Copyright (c) 2015-2018 Boris Buliga
;;
;;; Author: Boris Buliga <boris@d12frosted.io>
;;
;;; URL: https://github.com/d12frosted/environment/emacs
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(defun bb:parse-command-line (args)
  "Handle some command line arguments."
  (let ((i 0) new-args)
    (while (< i (length args))
      (let ((arg (nth i args))
            (next-arg-digit
             (when (< (1+ i) (length args))
               (string-to-number (nth (1+ i) args)))))
        (when (or (null next-arg-digit) (= 0 next-arg-digit))
          (setq next-arg-digit nil))
        (pcase arg
          ("--debug"
           (setq bb-debug-mode t))
          (_ (push arg new-args))))
      (setq i (1+ i)))
    (nreverse new-args)))

(setq command-line-args (bb:parse-command-line command-line-args))

(provide 'bb-command-line)

;;; bb-command-line.el ends here
