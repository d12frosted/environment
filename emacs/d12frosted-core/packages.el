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
  '(beacon
    ranger
    google-translate
    projectile
    spaceline
    magit
    git-messenger
    helm
    ivy
    glsl-mode
    lua-mode
    comment-dwim-2
    elfeed
    elfeed-goodies
    bpr
    zoom-frm
    move-text
    mu4e
    god-mode
    spaceline
    ace-window
    flycheck-package
    (composable :location (recipe
                           :fetcher github
                           :repo "paldepind/composable.el"))
    (counsel-osx-app :location (recipe
                                :fetcher github
                                :repo "d12frosted/counsel-osx-app"))
    (flyspell-correct :location (recipe
                               :fetcher github
                               :repo "d12frosted/flyspell-correct"))))

;;; packages.el ends here
