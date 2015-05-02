;; -*- mode: dotspacemacs -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (ie. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/.environment/emacs/spacemacs-layers/")
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(better-defaults
     org
     auto-completion
     syntax-checking
     colors
     git
     xkcd
     erc

     ;; langs
     csharp
     markdown
     ;; shell-scripts

     ;; private layers
     mu4e
     d12frosted
     d12frosted-org
     d12frosted-haskell)
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; Either `vim' or `emacs'. Evil is always enabled but if the variable
   ;; is `emacs' then the `holy-mode' is enabled at startup.
   dotspacemacs-editing-style 'emacs
   ;; If non nil output loading progess in `*Messages*' buffer.
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to a .PNG file.
   ;; If the value is nil then no banner is displayed.
   ;; dotspacemacs-startup-banner 'official
   dotspacemacs-startup-banner 'official
   ;; t if you always want to see the changelog at startup
   dotspacemacs-always-show-changelog t
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'."
   dotspacemacs-startup-lists '(recents projects)
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(leuven
                         solarized-light
                         solarized-dark
                         zenburn)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Menlo"
                               :size 12
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it.
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil the paste micro-state is enabled. While enabled pressing `p`
   ;; several times cycle between the kill ring content.
   dotspacemacs-enable-paste-micro-state t
   ;; Guide-key delay in seconds. The Guide-key is the popup buffer listing
   ;; the commands bound to the current keystrokes.
   dotspacemacs-guide-key-delay 0.4
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil ;; to boost the loading time.
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up.
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup t
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX."
   dotspacemacs-fullscreen-use-non-native t
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line.
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen.
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   dotspacemacs-smartparens-strict-mode nil
   ;; If non nil advises quit functions to keep server open when quitting.
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now.
   dotspacemacs-default-package-repository nil
   )

  ;; User initialization goes here
  (setq-default git-enable-github-support t
                git-magit-status-fullscreen t
                colors-enable-nyan-cat-progress-bar t

                omnisharp-server-executable-path "~/.omnisharp/OmniSharp/bin/Debug/OmniSharp.exe"
                ;; omnisharp-server-executable-path "~/Developer/omnisharp-roslyn/scripts/Omnisharp"

                ;; d12frosted-org layer
                d12frosted/org-home-path "~/Dropbox/org/"
                d12frosted/org-author-name "Boris Buliga"
                d12frosted/org-author-email "d12frosted@icloud.com"

                ;; mu4e layer
                mu4e/default-account "d12frosted"
                mu4e/update-interval 60
                mu4e/folders-alist
                '(("d12frosted"
                   (mu4e-drafts-folder "/d12frosted/Drafts")
                   (mu4e-sent-folder "/d12frosted/Sent")
                   (mu4e-trash-folder "/d12frosted/Trash")
                   (mu4e-refile-folder "/d12frosted/Archive"))
                  ("boris"
                   (mu4e-drafts-folder "/boris/Drafts")
                   (mu4e-sent-folder "/boris/Sent")
                   (mu4e-trash-folder "/boris/Trash")
                   (mu4e-refile-folder "/boris/Archive"))
                  ("timecode"
                   (mu4e-drafts-folder "/timecode/Drafts")
                   (mu4e-sent-folder "/timecode/Sent Mail")
                   (mu4e-trash-folder "/timecode/Trash")
                   (mu4e-refile-folder "/timecode/Archive")))
                mu4e/accounts-alist
                '(("d12frosted"
                   (mu4e-sent-messages-behavior sent)
                   (user-mail-address "d12frosted@icloud.com")
                   (user-full-name  "Boris")
                   (mu4e-compose-signature "Cheers, Boris."))
                  ("boris"
                   (mu4e-sent-messages-behavior sent)
                   (user-mail-address "boris.buliga@icloud.com")
                   (user-full-name  "Boris Buliga")
                   (mu4e-compose-signature "Cheers, Boris."))
                  ("timecode"
                   (mu4e-sent-messages-behavior delete)
                   (user-mail-address "boris.buliga@timecode.co")
                   (user-full-name  "Boris Buliga")
                   (mu4e-compose-signature "Cheers, Boris.")))
                ))

(defun dotspacemacs/config ()
  "Configuration function.
 This function is called at the very end of Spacemacs initialization after
layers configuration."

  ;; variables
  (setq powerline-default-separator       'arrow                                       ; set arrow as a separator for powerline

        helm-candidate-number-limit 36                                                 ; to help fuzzy match
        helm-M-x-fuzzy-match              t                                            ; enable fuzzy match for M-x
        helm-lisp-fuzzy-completion        t                                            ; enable fuzzy match for lisp
                                                                                       ; functions completion list

        projectile-enable-caching         nil                                          ; disable caching
                                                                                       ; because of SSD

        magit-repo-dirs                   '("~/Developer/"
                                            "~/.environment/")                         ; help magit to search for git repos

        nyan-wavy-trail                   nil                                          ; wavy trail bothers me, so I disable it

        ;; mu4e layer
        mu4e-bookmarks
        '(("flag:unread AND NOT flag:trashed"            "Unread messages"               ?u)
          ("date:;TODO: oday..now AND NOT flag:trashed"  "Today's messages"              ?t)
          ("date:today..now"                             "Today's messages (with Trash)" ?T)
          ("date:7d..now AND NOT flag:trashed"           "Last 7 days"                   ?w)
          ("date:7d..now"                                "Last 7 days (with Trash)"      ?W))
        ;; mu4e-html2text-command "html2text -utf8 -width 80"
        ;; mu4e-html2text-command "textutil -stdin -format html -convert txt -stdout"
        ;; mu4e-html2text-command            "w3m -dump -cols 80 -T text/html"
        mu4e-html2text-command            "w3m -dump -cols 110 -T text/html"
        mu4e-view-fields                  '(:from
                                            :to
                                            :cc
                                            :subject
                                            :date
                                            :tags
                                            :attachments)
        mu4e-maildir                      "~/.mail"
        mu4e-headers-visible-lines        16
        mu4e-use-fancy-chars              t                                            ; should be executed only for GUI
        mu4e-headers-draft-mark           '("D" . "⚒ ")                                ; draft
        mu4e-headers-seen-mark            '("S" . "☑ ")                                ; seen
        mu4e-headers-unseen-mark          '("u" . "☐ ")                                ; unseen
        mu4e-headers-flagged-mark         '("F" .  "⚑ ")                               ; flagged
        mu4e-headers-new-mark             '("N" .  "✉ ")                               ; new
        mu4e-headers-replied-mark         '("R" . "↵ ")                                ; replied
        mu4e-headers-passed-mark          '("P" . "⇉ ")                                ; passed
        mu4e-headers-encrypted-mark       '("x" . "♯ ")                                ; encrypted
        mu4e-headers-signed-mark          '("s" . "✍ ")                                ; signed
        mu4e-headers-attach-mark          '("a" . "⚓︎ ")
        mu4e-get-mail-command             "sh ~/.environment/email/gendalf.sh mu4e 1"  ; todo - use the value of update interval
        mu4e-view-show-images             t
        mu4e-attachment-dir               "~/Downloads"
        mu4e-view-image-max-width         700
        message-send-mail-function        'message-send-mail-with-sendmail
        message-sendmail-extra-arguments  '("--read-envelope-from")
        message-sendmail-f-is-evil        't
        sendmail-program                  "msmtp"
        mail-user-agent                   'mu4e-user-agent)

  ;; hooks
  (add-hook 'csharp-mode-hook 'd12frosted/omnisharp-config t)

  ;; key bindings
  (evil-leader/set-key
    "ff" 'helm-multi-files))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
