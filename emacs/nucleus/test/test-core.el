;; -*- no-byte-compile: t; -*-
;;; core/test/test-core.el

(describe "core"
  (xdescribe "initialize"
    :var (nucleus-init-p nucleus-init-modules-p nucleus-private-dir)
    (before-each
      (setq nucleus-init-p nil
            nucleus-init-modules-p nil
            nucleus-private-dir nucleus-emacs-dir)

      (spy-on 'require)
      (spy-on 'load)
      (spy-on 'nucleus-reload-nucleus-autoloads)
      (spy-on 'nucleus-reload-package-autoloads)
      (spy-on 'nucleus-initialize-autoloads)
      (spy-on 'nucleus-ensure-core-directories)
      (spy-on 'nucleus-ensure-core-packages)
      (spy-on 'nucleus-ensure-packages-initialized)
      (spy-on 'nucleus-ensure-same-emacs-version-p))

    (describe "in interactive session"
      :var (noninteractive)
      (before-each (setq noninteractive t))

      (it "initializes once, unless forced")
      (it "does not initialize on consecutive invokations")
      (it "loads all core libraries" )
      (it "loads autoloads file" )
      (it "does not load autoloads file if forced" )
      (it "regenerates missing autoloads" ))

    (describe "in non-interactive session"
      :var (noninteractive)
      (before-each (setq noninteractive nil))

      (it "initializes once, unless forced")
      (it "does not initialize on consecutive invokations")
      (it "does not load all core libraries" )
      (it "loads autoloads file" )
      (it "does not load autoloads file if forced" )
      (it "does not regenerate missing autoloads" )))

  (xdescribe "initialize-packages"
    (before-each (spy-on 'quelpa-setup-p))

    (it "initializes package.el once, unless forced" )
    (it "initializes quelpa once, unless forced" )
    (it "initializes nucleus-packages once, unless forced" ))

  (xdescribe "initialize-modules"
    (it "loads private init.el once, unless forced" ))

  (xdescribe "initialize-autoloads"
    (it "loads autoloads file" )
    (it "ignores autoloads file if cleared" ))

  (describe "custom hooks"
    (describe "switch hooks"
      :var (before-hook after-hook a b)
      (before-each
        (setq a (switch-to-buffer (get-buffer-create "a"))
              b (get-buffer-create "b"))
        (spy-on 'before-hook)
        (spy-on 'after-hook)
        (nucleus|init-switch-hooks))
      (after-each
        (nucleus|init-switch-hooks 'disable)
        (kill-buffer a)
        (kill-buffer b))

      (describe "switch-buffer"
        :var (nucleus-exit-buffer-hook
              nucleus-enter-buffer-hook)
        (before-each
          (setq nucleus-exit-buffer-hook '(before-hook)
                nucleus-enter-buffer-hook '(after-hook)))
        (after-each
          (setq nucleus-exit-buffer-hook nil
                nucleus-enter-buffer-hook nil))

        (it "should trigger when switching buffers"
          (switch-to-buffer b)
          (switch-to-buffer a)
          (switch-to-buffer b)
          (expect 'before-hook :to-have-been-called-times 3)
          (expect 'after-hook :to-have-been-called-times 3))

        (it "should trigger only once on the same buffer"
          (switch-to-buffer b)
          (switch-to-buffer b)
          (switch-to-buffer a)
          (expect 'before-hook :to-have-been-called-times 2)
          (expect 'after-hook :to-have-been-called-times 2)))


      (describe "switch-window"
        :var (nucleus-exit-window-hook
              nucleus-enter-window-hook
              x y)
        (before-each
          (delete-other-windows)
          (setq x (get-buffer-window a)
                y (save-selected-window (split-window)))
          (with-selected-window y
            (switch-to-buffer b))
          (select-window x)
          (spy-calls-reset 'before-hook)
          (spy-calls-reset 'after-hook)
          (setq nucleus-exit-window-hook '(before-hook)
                nucleus-enter-window-hook '(after-hook)))

        (it "should trigger when switching windows"
          (select-window y)
          (select-window x)
          (select-window y)
          (expect 'before-hook :to-have-been-called-times 3)
          (expect 'after-hook :to-have-been-called-times 3))

        (it "should trigger only once on the same window"
          (select-window y)
          (select-window y)
          (select-window x)
          (expect 'before-hook :to-have-been-called-times 2)
          (expect 'after-hook :to-have-been-called-times 2))))))
