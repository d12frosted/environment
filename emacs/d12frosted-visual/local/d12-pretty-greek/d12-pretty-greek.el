;;; d12-pretty-greek.el --- display Greek letters in place of their names

;; Copyright (c) 2015-2017 Boris Buliga

;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Created: 13 Jun 2016

;; Keywords:
;; Homepage: https://github.com/d12frosted/environment

;; Package-Version: 0.0.1
;; Package-Requires: ()

;; This file is not part of GNU Emacs.
;;; License: GPLv3

;;; Commentary:
;;
;; Display Greek letters in place of their names. Supports names in lower case,
;; capital case and upper case. This is generalisation of PrettyGreek by
;; BenignoUria. To use, just add a hook to any related mode.
;;
;; (add-hook 'prog-mode-hook #'d12-pretty-greek)
;;

;;; Code:
;;

(defun d12-pretty-greek--lock (greek code0)
  (loop for word in greek
        for code = code0 then (+ 1 code)
        do  (unless (eq 82 code)        ; workaround
              (let ((greek-char (make-char 'greek-iso8859-7 code)))
                (font-lock-add-keywords nil
                                        `((,(concatenate 'string "\\(^\\|[^a-zA-Z0-9]\\)\\(" word "\\)[a-zA-Z]")
                                           (0 (progn (decompose-region (match-beginning 2) (match-end 2))
                                                     nil)))))
                (font-lock-add-keywords nil
                                        `((,(concatenate 'string "\\(^\\|[^a-zA-Z0-9]\\)\\(" word "\\)[^a-zA-Z]")
                                           (0 (progn (compose-region (match-beginning 2) (match-end 2)
                                                                     ,greek-char)
                                                     nil)))))))))

(defun d12-pretty-greek-lower ()
  (d12-pretty-greek--lock '("alpha" "beta" "gamma" "delta" "epsilon" "zeta" "eta" "theta" "iota" "kappa" "lambda" "mu" "nu" "xi" "omicron" "pi" "rho" "sigma_final" "sigma" "tau" "upsilon" "phi" "chi" "psi" "omega") 97))

(defun d12-pretty-greek-capital ()
  (d12-pretty-greek--lock '("Alpha" "Beta" "Gamma" "Delta" "Epsilon" "Zeta" "Eta" "Theta" "Iota" "Kappa" "Lambda" "Mu" "Nu" "Xi" "Omicron" "Pi" "Rho" "Sigma_final" "Sigma" "Tau" "Upsilon" "Phi" "Chi" "Psi" "Omega") 65))

(defun d12-pretty-greek-upper ()
  (d12-pretty-greek--lock '("ALPHA" "BETA" "GAMMA" "DELTA" "EPSILON" "ZETA" "ETA" "THETA" "IOTA" "KAPPA" "LAMBDA" "MU" "NU" "XI" "OMICRON" "PI" "RHO" "SIGMA_FINAL" "SIGMA" "TAU" "UPSILON" "PHI" "CHI" "PSI" "OMEGA") 65))

;;;###autoload
(defun d12-pretty-greek ()
  (d12-pretty-greek-lower)
  (d12-pretty-greek-capital)
  (d12-pretty-greek-upper))

(provide 'd12-pretty-greek)

;;; d12-pretty-greek.el ends here
