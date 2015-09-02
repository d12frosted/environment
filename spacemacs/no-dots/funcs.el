(defun no-dots/helm-ff-filter-candidate-one-by-one (fcn file)
  (unless (string-match "\\(?:/\\|\\`\\)\\.\\{1,2\\}\\'" file)
    (funcall fcn file)))
