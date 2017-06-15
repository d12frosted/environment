;;; haskell-flycheck.el --- Flychecker using the GHCi process

;; Copyright (c) 2014 Chris Done. All rights reserved.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'haskell-process)
(require 'flycheck)

(defun flycheck-haskell-process-start (checker callback)
  "Start a GHCi load with CHECKER.

CALLBACK is the status callback passed by Flycheck."
  (let ((session (haskell-session)))
    (haskell-session-current-dir session)
    (let ((process (haskell-process)))
      (haskell-process-queue-command
       process
       (make-haskell-command
        :state
        (list :process process
              :session session
              :filename (buffer-file-name)
              :callback callback
              :buffer (current-buffer)
              :original (buffer-string))
        :go
        (lambda (state)
          (with-current-buffer (plist-get state :buffer)
            (let* ((filename (plist-get state :filename)))
              (write-region (point-min) (point-max) filename)
              (clear-visited-file-modtime)
              (haskell-process-send-string
               (plist-get state :process)
               (format ":load \"%s\""
                       (replace-regexp-in-string
                        "\""
                        "\\\\\""
                        filename))))))
        :live (lambda (state _)
                (when (plist-get state :original)
                  (with-temp-buffer
                    (insert (plist-get state :original))
                    (write-region (point-min) (point-max) (plist-get state :filename))
                    (plist-put state :original nil))))
        :complete
        (lambda (state response)
          (let ((session (plist-get state :session))
                (process (plist-get state :process)))
            (haskell-process-set-response-cursor process 0)
            (let ((errors (list))
                  (next-error t))
              (while next-error
                (setq next-error
                      (haskell-process-errors-warnings
                       session
                       process
                       response
                       t))
                (when (consp next-error)
                  (add-to-list 'errors
                               (flycheck-error-new-at
                                (plist-get next-error :line)
                                (plist-get next-error :col)
                                (plist-get next-error :type)
                                (plist-get next-error :msg)
                                :checker 'haskell-process
                                :buffer (plist-get state :buffer)))))
              (funcall (plist-get state :callback)
                       'finished
                       errors)))))))))


(flycheck-define-generic-checker 'haskell-process
  "A syntax and type checker for Haskell using GHCi (via the
haskell-process Emacs module)."
  :start 'flycheck-haskell-process-start
  :modes '(haskell-mode)
  :next-checkers '((warning . haskell-hlint)))

(add-to-list 'flycheck-checkers 'haskell-process) ; Register as an auto-selectable checker

(provide 'haskell-flycheck)
