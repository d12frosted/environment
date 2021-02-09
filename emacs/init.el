;;; init.el --- Custom configurations -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2021 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;;
;; Created: 08 Jan 2021
;;
;; URL: https://github.com/d12frosted/environment/emacs
;;
;; License: GPLv3
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; These are my personal Emacs configurations. Please refer to the
;; README for information on how to run and modify them.
;;
;;; Code:

;; Add Lisp directory to `load-path'. Since we might be running in CI
;; or other environments, stick to XDG_CONFIG_HOME value if possible.
(let ((emacs-dir (if-let ((xdg (getenv "XDG_CONFIG_HOME")))
                     (expand-file-name "emacs/" xdg)
                   user-emacs-directory)))
  (add-to-list 'load-path (expand-file-name "lisp" emacs-dir))

  ;; Load `init-autoloads' if it was generated.
  (load-file (expand-file-name "lisp/init-autoloads.el"
                               emacs-dir)))

;; Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold
                             normal-gc-cons-threshold))))

;; bootstrap
(require 'init-path)
(require 'init-elpa)

;; popular packages
(use-package s)
(use-package dash)
(use-package async)

;; core
(require 'init-env)
(require 'init-editor)
(require 'init-ui)
(require 'init-buffer)
(require 'init-window)
(require 'init-kbd)

;; utilities
(require 'init-selection)
(require 'init-vcs)
(provide 'init)
;;; init.el ends here
