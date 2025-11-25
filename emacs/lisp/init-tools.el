;;; init-tools.el --- various tools configurations -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 30 Mar 2022
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
;; Miscellaneous tools: REST client for API testing, jq for JSON processing,
;; and gptel for AI chat integration with Claude and other models.
;;
;;; Code:

(require 'config-vulpea)

(use-package agent-shell
  :ensure t)

(use-package restclient
  :ensure (:host github :repo "pashky/restclient.el" :files ("restclient.el" "restclient-jq.el"))
  :defer t
  :config
  (require 'restclient-jq))

(use-package jq-mode
  :ensure t
  :defer t)

(use-package gptel
  :ensure t
  :defer t
  :config
  (gptel-make-anthropic "Claude"
    :stream t
    :key (auth-source-pick-first-password :host "api.anthropic.com"))
  (setq-default
   gptel-model 'o1
   gptel-directives
   (let ((file (expand-file-name "gptel-directives.el" vulpea-directory)))
     (when (file-exists-p file)
       (with-temp-buffer
         (condition-case nil
	           (progn
	             (insert-file-contents file)
               (read (current-buffer)))
	         (error
	          (message "Could not read data from %s" file))))))))

(provide 'init-tools)
;;; init-tools.el ends here
