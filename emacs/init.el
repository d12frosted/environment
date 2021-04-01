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
;; URL: https://github.com/d12frosted/environment/tree/master/emacs
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

;; Since we might be running in CI or other environments, stick to
;; XDG_CONFIG_HOME value if possible.
(let ((emacs-home (if-let ((xdg (getenv "XDG_CONFIG_HOME")))
                      (expand-file-name "emacs/" xdg)
                    user-emacs-directory)))
  ;; Add Lisp directory to `load-path'.
  (add-to-list 'load-path (expand-file-name "lisp" emacs-home)))

;; Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold
                             normal-gc-cons-threshold))))

;; bootstrap
(require 'config-path)
(require 'init-elpa)

;; Setup `custom-file`.
(setq custom-file (concat path-local-dir "custom.el"))

;; load autoloads file
(unless elpa-bootstrap-p
  (unless (file-exists-p path-autoloads-file)
    (error "Autoloads file doesn't exist, please run '%s'"
           "eru install emacs"))
  (load path-autoloads-file nil 'nomessage))

;; core
(require 'init-env)
(require 'init-kbd)
(require 'init-editor)
(require 'init-ui)
(require 'init-buffer)
(require 'init-window)

;; utilities
(require 'init-selection)
(require 'init-project)
(require 'init-vcs)
(require 'init-ide)
(require 'init-vulpea)
(require 'init-vino)
(require 'init-file-templates)
(require 'init-dired)
(require 'init-spell)
(require 'init-pdf)
(require 'init-telega)

;; languages
(require 'init-elisp)
(require 'init-haskell)
(require 'init-sh)
(require 'init-json)
(require 'init-yaml)
(require 'init-js)

;; I don't use `customize' interface, but .dir-locals.el put 'safe'
;; variables into `custom-file'. And to be honest, I hate to allow
;; them every time I restart Emacs.
(when (file-exists-p custom-file)
  (load custom-file nil 'nomessage))

(provide 'init)
;;; init.el ends here
