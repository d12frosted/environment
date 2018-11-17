;; -*- no-byte-compile: t; -*-
;;; ui/nucleus-dashboard/test/test-nucleus-dashboard.el

(require 'core-projects)
(require 'projectile)
(require! :ui nucleus-dashboard)

(describe "ui/nucleus-dashboard"
  :var (default-directory projectile-enable-caching)
  (before-all (setq projectile-enable-caching nil))

  (before-each (projectile-mode +1))
  (after-each  (projectile-mode -1))

  (describe "get-pwd"
    :var (+nucleus-dashboard--last-cwd)
    (before-each
      (setq +nucleus-dashboard--last-cwd nucleus-core-dir
            default-directory nucleus-core-dir))
    (it "returns the current directory when policy is nil"
      (let (+nucleus-dashboard-pwd-policy)
        (expect (+nucleus-dashboard--get-pwd) :to-equal default-directory)))
    (it "returns a path if policy is a path"
      (let ((+nucleus-dashboard-pwd-policy "~"))
        (expect (+nucleus-dashboard--get-pwd) :to-equal (expand-file-name "~"))))
    (it "returns return value of policy as a function"
      (let ((+nucleus-dashboard-pwd-policy (lambda (x) "x")))
        (expect (+nucleus-dashboard--get-pwd) :to-equal "x")))
    (it "returns last cwd if policy is 'last"
      (let ((+nucleus-dashboard-pwd-policy 'last))
        (expect (+nucleus-dashboard--get-pwd) :to-equal nucleus-core-dir)))
    (it "returns last project if policy is 'last-project"
      (let ((+nucleus-dashboard-pwd-policy 'last-project))
        (expect (+nucleus-dashboard--get-pwd) :to-equal nucleus-emacs-dir))))

  (describe "dashboard-p"
    (it "changes the fallback buffer to the dashboard buffer"
      (expect (+nucleus-dashboard-p (nucleus-fallback-buffer))))))
