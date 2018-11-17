;; -*- no-byte-compile: t; -*-
;;; core/test/test-autoload-files.el
;;;
(require 'core-projects)
(require 'projectile)

(describe "core/autoload/files"
  :var (src dest projectile-projects-cache-time projectile-projects-cache)
  (before-each
    (setq src      (make-temp-file "test-src")
          existing (make-temp-file "test-existing")
          dest     (expand-file-name "test-dest" temporary-file-directory))
    (quiet! (find-file-literally src))
    (spy-on 'y-or-n-p :and-return-value nil)
    (projectile-mode +1))

  (after-each
    (projectile-mode -1)
    (switch-to-buffer (nucleus-fallback-buffer))
    (ignore-errors (delete-file src))
    (ignore-errors (delete-file existing))
    (ignore-errors (delete-file dest)))

  (describe "move-this-file"
    (it "won't move to itself"
      (expect (quiet! (nucleus/move-this-file src)) :to-throw))
    (it "will move to another file"
      (expect (quiet! (nucleus/move-this-file dest t)))
      (expect (file-exists-p dest))
      (expect (file-exists-p src) :to-be nil))
    (it "will prompt if overwriting a file"
      (quiet! (nucleus/move-this-file existing))
      (expect 'y-or-n-p :to-have-been-called-times 1)
      (expect (file-exists-p src))))

  (describe "copy-this-file"
    (it "refuses to copy to itself"
      (expect (quiet! (nucleus/copy-this-file src)) :to-throw))
    (it "copies to another file"
      (expect (quiet! (nucleus/copy-this-file dest t)))
      (expect (file-exists-p! src dest)))
    (it "prompts if overwriting a file"
      (quiet! (nucleus/copy-this-file existing))
      (expect 'y-or-n-p :to-have-been-called-times 1)))

  (describe "delete-this-file"
    (it "fails gracefully on non-existent files"
      (expect (quiet! (nucleus/delete-this-file dest)) :to-throw))
    (it "deletes existing files"
      (quiet! (nucleus/delete-this-file existing t))
      (expect (file-exists-p existing) :to-be nil))
    (it "prompts to delete any existing file"
      (quiet! (nucleus/delete-this-file existing))
      (expect 'y-or-n-p :to-have-been-called-times 1))))
