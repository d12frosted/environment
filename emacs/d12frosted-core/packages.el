;;; packages.el --- d12frosted-core layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Boris Buliga <d12frosted@d12frosted.local>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defconst d12frosted-core-packages
  '(
    ;; utils
    ranger
    google-translate
    projectile
    magit
    git-messenger
    comment-dwim-2
    bpr
    zoom-frm
    god-mode
    ace-window
    persp-mode
    move-text
    alert
    (composable :location (recipe
                           :fetcher github
                           :repo "paldepind/composable.el"))
    (counsel-osx-app :location (recipe
                                :fetcher github
                                :repo "d12frosted/counsel-osx-app"))

    ;; completion
    helm
    ivy

    ;; visual
    beacon
    spaceline

    ;; langs
    glsl-mode
    lua-mode

    ;; flyspell
    flycheck-package
    (flyspell-correct :location (recipe
                                 :fetcher github
                                 :repo "d12frosted/flyspell-correct"))

    ;; other
    mu4e
    elfeed
    elfeed-goodies))

;;; packages.el ends here
