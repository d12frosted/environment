(defun show-conversion-1 (int)
  (interactive (list (or (number-at-point)
                         (symbol-at-point))))
  ;; handle 0x and \u cases
  (when (symbolp int)
    (let ((name (symbol-name int)))
      (cond ((string-prefix-p "0x" name)
             (setq int (string-to-number (substring name 2) 16)))
            ((string-match-p "^u[[:xdigit:]]+$" name)
             (setq int (string-to-number (substring name 1) 16)))
            (t (user-error "No valid integer at point")))))
  (let* ((dec (format "%d" int))
         (hex (format "%X" int))
         (oct (format "%o" int))
         (bin (int-to-binary-string int))
         (uni (cpp-escape-unicode hex)))
    (popup-menu
     `(,(format "%s (Click format to copy)" (symbol-at-point))
       [,(format "Decimal \t\t= %s" dec) (kill-new dec)]
       [,(format "Hex \t\t\t= %s" hex) (kill-new hex)]
       [,(format "Octal \t\t\t= %s" oct) (kill-new oct)]
       [,(format "Binary \t\t\t= %s" bin) (kill-new bin)]
       [,(format "Unicode (%s)%s\t= %s"
                 (char-to-string int)
                 (if (< (char-width int) 2)
                     "\t"
                   "")
                 uni)
        (kill-new uni)]))))

(defun show-conversion (event)
  "Pops up a conversion menu for the integer at point."
  (interactive "e")
  (mouse-set-point event)
  ;; (call-interactively #'show-conversion-1)
  (show-conversion-1 (or (number-at-point)
                         (symbol-at-point))))

;;; Helpers

(defun cpp-escape-unicode (hex)
  "Convert a string HEX into a valid escaped \\u format for C++."
  (let ((len (length hex)))
    (concat "\\u"
            (when (< len 4)
              (make-string (- 4 len) ?0))
            hex)))

(defun int-to-binary-string (i)
  "Convert an integer I into its binary representation in string format.
Stolen from http://stackoverflow.com/a/20577329."
  (let ((res ""))
    (while (not (= i 0))
      (setq res (concat (if (= 1 (logand i 1)) "1" "0") res))
      (setq i (lsh i -1)))
    (if (string= res "")
        (setq res "0"))
    res))

(provide 'number-conversion-tooltip)
