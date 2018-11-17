;; -*- no-byte-compile: t; -*-
;;; core/test/test-autoload-package.el

(describe "core/autoload/packages"
  :var (package-alist
        package-archive-contents
        package-selected-packages
        nucleus-packages
        quelpa-cache
        quelpa-initialized-p
        nucleus-packages-dir
        nucleus-core-packages
        package-user-dir
        quelpa-dir
        pkg)

  (before-all
    (fset 'pkg
          (lambda (name version &optional reqs)
            (package-desc-create
             :name name :version version :reqs reqs
             :dir (expand-file-name (format "%s/" name) package-user-dir))))
    (require 'package)
    (require 'quelpa)
    (setq nucleus-packages-dir (expand-file-name "packages/" (file-name-directory load-file-name))
          package-user-dir (expand-file-name "elpa" nucleus-packages-dir)
          quelpa-dir (expand-file-name "quelpa" nucleus-packages-dir)
          quelpa-initialized-p t
          nucleus-core-packages nil)
    (spy-on #'package--user-installed-p :and-call-fake (lambda (_p) t))
    (spy-on #'nucleus-initialize-packages :and-call-fake (lambda (&optional _)))
    (spy-on #'package-refresh-contents :and-call-fake (lambda (&optional _)))
    (spy-on #'quelpa-checkout :and-call-fake
            (lambda (rcp _dir)
              (when (eq (car rcp) 'nucleus-quelpa-dummy)
                "20170405.1234"))))

  (after-all
    (unload-feature 'package t)
    (unload-feature 'quelpa t))

  (before-each
    (setq package-alist
          `((nucleus-dummy ,(pkg 'nucleus-dummy '(20160405 1234)))
            (nucleus-uptodate-dummy ,(pkg 'nucleus-uptodate-dummy '(20160605 1234)))
            (nucleus-unwanted-dummy ,(pkg 'nucleus-unwanted-dummy '(20160605 1234)))
            (nucleus-quelpa-dummy ,(pkg 'nucleus-quelpa-dummy '(20160405 1234)))
            (nucleus-noquelpa-dummy ,(pkg 'nucleus-noquelpa-dummy '(20160405 1234))))
          package-archive-contents
          `((nucleus-dummy ,(pkg 'nucleus-dummy '(20170405 1234)))
            (nucleus-uptodate-dummy ,(pkg 'nucleus-uptodate-dummy '(20160605 1234))))
          nucleus-packages
          '((nucleus-dummy)
            (nucleus-uptodate-dummy)
            (nucleus-missing-dummy)
            (nucleus-noquelpa-dummy)
            (nucleus-disabled-dummy :disable t)
            (nucleus-private-dummy :private t)
            (nucleus-disabled-private-dummy :private t :disable t)
            (nucleus-quelpa-dummy :recipe (nucleus-quelpa-dummy :fetcher github :repo "hlissner/does-not-exist")))
          quelpa-cache
          '((nucleus-quelpa-dummy :fetcher github :repo "hlissner/does-not-exist")
            (nucleus-noquelpa-dummy :fetcher github :repo "hlissner/does-not-exist-3")
            (nucleus-new-quelpa-dummy :fetcher github :repo "hlissner/does-not-exist-2"))
          package-selected-packages (mapcar #'car nucleus-packages)))

  (describe "package-backend"
    (it "determines the correct backend of a package"
      (expect (nucleus-package-backend 'nucleus-dummy) :to-be 'elpa)
      (expect (nucleus-package-backend 'nucleus-quelpa-dummy) :to-be 'quelpa)
      (expect (nucleus-package-backend 'org) :to-be 'emacs))
    (it "errors out if package isn't installed"
      (expect (nucleus-package-backend 'xyz) :to-throw)))

  (describe "package-outdated-p (elpa)"
    (it "detects outdated ELPA packages and returns both versions"
      (expect (nucleus-package-outdated-p 'nucleus-dummy)
              :to-equal '(nucleus-dummy (20160405 1234) (20170405 1234))))
    (it "ignores up-to-date ELPA packages"
      (expect (nucleus-package-outdated-p 'nucleus-uptodate-dummy) :to-be nil))

    (it "detects outdated QUELPA packages and returns both versions"
      (expect (nucleus-package-outdated-p 'nucleus-quelpa-dummy)
              :to-equal '(nucleus-quelpa-dummy (20160405 1234) (20170405 1234))))
    (it "ignores up-to-date QUELPA packages"
      (expect (nucleus-package-outdated-p 'nucleus-uptodate-dummy) :to-be nil))

    (it "returns nil if package isn't installed"
      (expect (nucleus-package-outdated-p 'xyz) :to-be nil)))

  (describe "get-packages"
    (before-all
      ;; In addition to `package-installed-p', `nucleus-package-installed-p' does
      ;; file existence checks which won't work here, so we simplify it
      (spy-on #'nucleus-package-installed-p :and-call-fake #'package-installed-p))

    (it "returns all packages"
      (expect (mapcar #'car (nucleus-get-packages))
              :to-have-same-items-as
              (mapcar #'car nucleus-packages)))
    (it "returns only disabled packages"
      (expect (mapcar #'car (nucleus-get-packages :disabled t))
              :to-have-same-items-as
              '(nucleus-disabled-dummy nucleus-disabled-private-dummy)))
    (it "returns only non-disabled packages"
      (expect (mapcar #'car (nucleus-get-packages :disabled nil))
              :to-have-same-items-as
              '(nucleus-dummy nucleus-uptodate-dummy nucleus-quelpa-dummy nucleus-missing-dummy nucleus-noquelpa-dummy nucleus-private-dummy)))
    (it "returns only installed packages"
      (expect (mapcar #'car (nucleus-get-packages :disabled nil :installed t))
              :to-have-same-items-as
              '(nucleus-dummy nucleus-uptodate-dummy nucleus-quelpa-dummy nucleus-noquelpa-dummy)))
    (it "returns only non-installed packages"
      (expect (mapcar #'car (nucleus-get-packages :disabled nil :installed nil))
              :to-have-same-items-as
              '(nucleus-missing-dummy nucleus-private-dummy)))
    (it "returns only private packages"
      (expect (mapcar #'car (nucleus-get-packages :private t))
              :to-have-same-items-as
              '(nucleus-private-dummy nucleus-disabled-private-dummy)))
    (it "returns only disabled and private packages"
      (expect (mapcar #'car (nucleus-get-packages :disabled t :private t))
              :to-have-same-items-as
              '(nucleus-disabled-private-dummy))))

  (describe "get-orphaned-packages"
    (it "returns orphaned packages"
      (expect (nucleus-get-orphaned-packages) :to-contain 'nucleus-unwanted-dummy))
    (it "returns packages that have changed backends"
      (expect (nucleus-get-orphaned-packages) :to-contain 'nucleus-noquelpa-dummy)))

  (describe "get-missing-packages"
    (it "returns packages that haven't been installed"
      (expect (mapcar #'car (nucleus-get-missing-packages))
              :to-contain 'nucleus-missing-dummy))
    (it "returns packages that have changed backends"
      (expect (mapcar #'car (nucleus-get-missing-packages))
              :to-contain 'nucleus-noquelpa-dummy))))
