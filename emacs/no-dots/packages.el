(setq no-dots-packages '(helm))
(setq no-dots-excluded-packages '())

(defun no-dots/post-init-helm ()
  (with-eval-after-load 'helm-files
    (advice-add 'helm-ff-filter-candidate-one-by-one
                :around 'no-dots/helm-ff-filter-candidate-one-by-one)
    (advice-add 'helm-find-files-up-one-level
                :around 'no-dots/helm-find-files-up-one-level)))
