;;; layers.el --- d12-shell layer layers file for Spacemacs. -*- lexical-binding: t; -*-
;;
;;; Copyright (c) 2015-2018 Boris Buliga
;;
;;; Author: Boris Buliga <boris@d12frosted.io>
;;
;;; URL: https://github.com/d12frosted/environment/emacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(configuration-layer/declare-layers
 '((shell
    :variables
    shell-default-shell 'multi-term)

   (shell-scripts
    :variables
    sh-basic-offset 2)))

;;; layers.el ends here
