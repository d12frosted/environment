;;; cache.el --- the heart of every cell -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;;         Henrik Lissner <henrik@lissner.net>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; URL: https://github.com/d12frosted/environment/emacs
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;; Most of the code was borrowed from hlissner/doom-emacs.
;;
;;; Commentary:
;;
;; This little library thinly wraps around persistent-soft (which is a pcache
;; wrapper, how about that). It has three purposes:
;;
;; - To encapsulate the cache backend (persistent-soft/pcache in this case), in
;;   case it needs to change.
;; - To provide `nucleus-cache-persist': a mechanism for easily persisting
;;   variables across Emacs sessions.
;; - To lazy-load persistent-soft until it is really needed.
;;
;; Like persistent-soft, caches assume a 2-tier structure, where all caches are
;; namespaced by location.
;;
;;; Code:

(defvar nucleus-cache-alists '(t)
  "An alist of alists, containing lists of variables for the
nucleus cache library to persist across Emacs sessions.")

(defvar nucleus-cache-location 'nucleus
  "The default location for cache files. This symbol is
translated into a file name under `pcache-directory' (by default
a subdirectory under `nucleus-cache-dir'). One file may contain
multiple cache entries.")

(defun nucleus|save-persistent-cache ()
  "Hook to run when an Emacs session is killed. Saves all
persisted variables listed in `nucleus-cache-alists' to files."
  (dolist (alist (butlast nucleus-cache-alists 1))
    (cl-loop with key = (car alist)
             for var in (cdr alist)
             if (symbol-value var)
             do (nucleus-cache-set var it nil key))))
(add-hook 'kill-emacs-hook #'nucleus|save-persistent-cache)

;;
;; Library

;;;###autoload
(defmacro with-cache! (location &rest body)
  "Runs BODY with a different default `nucleus-cache-location'."
  (declare (indent defun))
  `(let ((nucleus-cache-location ',location))
     ,@body))

;;;###autoload
(defun nucleus-cache-persist (location variables)
  "Persist VARIABLES (list of symbols) in LOCATION (symbol).

This populates these variables with cached values, if one exists,
and saves them to file when Emacs quits.

Warning: this is incompatible with buffer-local variables."
  (dolist (var variables)
    (when (nucleus-cache-exists var location)
      (set var (nucleus-cache-get var location))))
  (setf (alist-get location nucleus-cache-alists)
        (append variables (cdr (assq location nucleus-cache-alists)))))

;;;###autoload
(defun nucleus-cache-desist (location &optional variables)
  "Unregisters VARIABLES (list of symbols) in LOCATION (symbol)
from `nucleus-cache-alists', thus preventing them from being
saved between sessions.  Does not affect the actual variables
themselves or their values."
  (if variables
      (setf (alist-get location nucleus-cache-alists)
            (cl-set-difference (cdr (assq location nucleus-cache-alists))
                               variables))
    (delq (assq location nucleus-cache-alists)
          nucleus-cache-alists)))

;;;###autoload
(defun nucleus-cache-get (key &optional location)
  "Retrieve KEY from LOCATION (defaults to
`nucleus-cache-location'), if it exists and hasn't expired."
  (persistent-soft-fetch
   key (symbol-name (or location nucleus-cache-location))))

;;;###autoload
(defun nucleus-cache-set (key value &optional ttl location)
  "Set KEY to VALUE in the cache. TTL is the time (in seconds)
until this cache entry expires. LOCATION is the super-key to
store this cache item under; the default is
`nucleus-cache-location'. "
  (persistent-soft-store
   key value
   (symbol-name (or location nucleus-cache-location)) ttl))

;;;###autoload
(defun nucleus-cache-exists (key &optional location)
  "Returns t if KEY exists at LOCATION (defaults to
`nucleus-cache-location')."
  (persistent-soft-exists-p key (or location nucleus-cache-location)))

;;;###autoload
(defun nucleus-cache-clear (&optional location)
  "Clear a cache LOCATION (defaults to
`nucleus-cache-location')."
  (persistent-soft-flush (or location nucleus-cache-location)))
