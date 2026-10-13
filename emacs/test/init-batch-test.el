;;; init-batch-test.el --- Tests for batch startup -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2026, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Version: 0.1
;; Package-Requires: ((emacs "28.1"))
;;
;; Created: 13 Oct 2026
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
;; This file contains tests for startup in batch mode. They rely on
;; init.el being loaded before the tests run (which is what the test
;; hook in Eldev file does).
;;
;;; Code:

(require 'buttercup)

(defconst init-batch-test-interactive-features
  '(agent-shell
    apheleia
    combobulate-rules
    corfu
    difftastic-bindings
    jinx
    marginalia
    minions
    modus-themes
    orderless
    pulsar
    savehist
    spacious-padding
    ukrainian-input-method
    vertico
    which-key)
  "Features that make sense only in an interactive session.")

(describe "batch startup"
  (it "loads init.el"
    (expect noninteractive :to-be-truthy)
    (expect (featurep 'init) :to-be-truthy))

  (it "does not load features needed only for interactive use"
    (expect (seq-filter #'featurep init-batch-test-interactive-features)
            :to-equal nil))

  (it "does not open elpaca log buffer"
    (expect (get-buffer "*elpaca-log*") :to-be nil)))

(provide 'init-batch-test)
;;; init-batch-test.el ends here
