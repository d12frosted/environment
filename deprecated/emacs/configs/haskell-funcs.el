;;; haskell-funcs.el --- funcs file of haskell configurations
;;
;; Copyright (c) 2014-2015 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@icloud.com>
;; Maintainer: Boris Buliga <d12frosted@icloud.com>
;; Created: 16 May 2015
;;
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

(defun shm-contextual-space ()
  "Do contextual space first, and run shm/space if no change in
the cursor position happened."
  (interactive)
  (if (looking-back "import")
      (call-interactively 'haskell-mode-contextual-space)
    (progn
      (let ((ident (haskell-ident-at-point)))
        (when ident
          (and interactive-haskell-mode
               (haskell-process-do-try-type ident))))
      (call-interactively 'shm/space))))

(defun haskell-move-right ()
  (interactive)
  (haskell-move-nested 1))

(defun haskell-move-left ()
  (interactive)
  (haskell-move-nested -1))
