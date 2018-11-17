;; -*- no-byte-compile: t; -*-
;;; ../core/test/test-core-projects.el

(require 'core-projects)
(require 'projectile)

(describe "core/projects"
  (before-each (projectile-mode +1))
  (after-each  (projectile-mode -1))

  (describe "project-p"
    (it "Should detect when in a valid project"
      (expect (nucleus-project-p nucleus-emacs-dir)))
    (it "Should detect when not in a valid project"
      (expect (nucleus-project-p (expand-file-name "~")) :to-be nil)))

  (describe "project-root"
    (it "should resolve to the project's root"
      (expect (nucleus-project-root nucleus-core-dir) :to-equal nucleus-emacs-dir))
    (it "should return nil if not in a project"
      (expect (nucleus-project-root (expand-file-name "~")) :to-be nil)))

  (describe "project-expand"
    (it "expands to a path relative to the project root"
      (expect (nucleus-project-expand "init.el" nucleus-core-dir)
              :to-equal (expand-file-name "init.el" (nucleus-project-root nucleus-core-dir)))))

  (describe "project-file-exists-p!"
    (let ((default-directory nucleus-core-dir))
      ;; Resolve from project root
      (expect (project-file-exists-p! "init.el"))
      ;; Chained file checks
      (expect (project-file-exists-p! (and "init.el" "LICENSE")))
      (expect (project-file-exists-p! (or "init.el" "does-not-exist")))
      (expect (project-file-exists-p! (and "init.el" (or "LICENSE" "does-not-exist")))))))
