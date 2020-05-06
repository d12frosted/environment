;;; core/navigation/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line. If
point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil, move forward ARG lines first. If point reaches
the beginning or end of the buffer, stop there."
  (interactive "P")
  (when (numberp arg)
    (let ((line-move-visual nil))
      (forward-line arg)))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))
