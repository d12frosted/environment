;;; funcs.el --- d12frosted-core layer funcs file for Spacemacs.
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

(defun d12/buffer-contains-substring? (string)
  "Check if current buffer contains substring."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (search-forward string nil t))))

(defun d12/goto-line-and-center ()
  "Same as `goto-line', but recenter after it."
  (interactive)
  (call-interactively 'goto-line)
  (call-interactively 'recenter-top-bottom))

(defmacro d12|rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after d12|rename-modeline-hack activate)
        (setq mode-name ,new-name))))

(defun d12/string-equal (s1 s2)
  "Wrapper for `string-equal' that checks that s1 and s2 are
string before that."
  (and (stringp s1)
       (stringp s2)
       (string-equal s1 s2)))

(defun d12/string-to-number (string default)
  "Wrapper for `string-to-number' that checks that STRING is
string before that."
  (if (and (stringp string)
           (not (string-empty-p string)))
      (string-to-number string)
    default))

(defun d12/get-env-shell-type ()
  (interactive)
  (let* ((path (getenv "SHELL"))
         (shell (file-name-nondirectory path)))
    (pcase shell
      ("fish" 'fish)
      ("bash" 'bash)
      ("zsh" 'zsh)
      (t 'unknown))))

(defun configuration-layer/get-owner (pkg &optional print)
  (interactive "SEnter package name: \np")
  (let ((owner (cdr (assoc pkg
                           (mapcar (lambda (pkg)
                                     (cons (oref pkg :name)
                                           (oref pkg :owner)))
                                   configuration-layer--packages)))))
    (when print
      (message "Owner of %S is %S" pkg owner))
    owner))

(defun d12/setup-M-h ()
  "Setup M-h key binding on OS X in GUI."
  (when (and (spacemacs/system-is-mac)
             (display-graphic-p))
    (bind-key "M-h" 'ns-do-hide-emacs)
    (-map (lambda (mode)
            (add-hook (intern (concat (symbol-name mode) "-hook"))
                      `(lambda ()
                         (define-key
                           (symbol-value (intern ,(concat (symbol-name mode) "-map")))
                           (kbd "M-h")
                           nil))))
          '(org-mode))))

(defun d12/align-repeat (start end regexp)
  "Repeat alignment with respect to the given regular
expression."
  (interactive "r\nsAlign regexp: ")
  (align-regexp start end
                (concat "\\(\\s-*\\)" regexp) 1 1 t))

(defun d12/refill-paragraphs-to-be-one-line (&optional start end)
  "Fill individual paragraphs with large fill column"
  (interactive "r")
  (let* ((fill-column most-positive-fixnum)
         (has-region (/= start end))
         (p1 (if has-region start (point-min)))
         (p2 (if has-region end (point-max))))
    (fill-individual-paragraphs p1 p2)))

(defun d12-github//get-remote-relative-url (remote)
  "Return relative url in form $user/$repo for a given remote. It
works only for github urls (http[s] and ssh)."
  (let ((url (vc-git--run-command-string nil "config" (format "remote.%s.url" remote))))
    (unless url (error "Not in a GitHub repo"))
    (when (and url (string-match "github.com:?/?\\(.*\\)" url))
      (replace-regexp-in-string "\\.git$" "" (match-string 1 url)))))

(defun d12-github//get-remote-branch (remote)
  "Return the name of remote branch current branch is tracking.
If there is none return 'master'."
  (let* ((ref (replace-regexp-in-string
               "\n" ""
               (vc-git--run-command-string nil "symbolic-ref" "-q" "HEAD")))
         (origin-branch (replace-regexp-in-string
                         "\n" ""
                         (vc-git--run-command-string
                          nil "for-each-ref" "--format=%(upstream:short)" ref)))
         (branch-name (mapconcat 'identity
                                 (cdr (split-string origin-branch "/"))
                                 "/")))
    (if (eq branch-name "") "master" branch-name)))

(defun d12-github/browse-remote ()
  "Browse remote repository."
  (interactive)
  (let* ((remotes (magit-list-remotes))
         (remote (if (eq (length remotes) 1)
                     (first remotes);
                   (completing-read "Choose remote: " remotes)))
         (relative-url (d12-github//get-remote-relative-url remote))
         (branch (d12-github//get-remote-branch remote))
         (url (format "https://github.com/%s/tree/%s" relative-url branch)))
    (browse-url url)))

(when (configuration-layer/package-usedp 'projectile)
  (defun d12/projectile-replace-regexp ()
    "Replace a string in the project using `tags-query-replace'.
Less efficient than `projectile-replace' but at least allows
usage of regular expressions. See
https://github.com/bbatsov/projectile/issues/576 for more details
on `projectile-replace' issue with regexps."
    (interactive "P")
    (let* ((old-text (read-string
                      (projectile-prepend-project-name "Replace: ")
                      (projectile-symbol-or-selection-at-point)))
           (new-text (read-string
                      (projectile-prepend-project-name
                       (format "Replace %s with: " old-text))))
           (files (-map (lambda (f) (concat (projectile-project-root) f)) (projectile-current-project-files))))
      (tags-query-replace old-text new-text nil (cons 'list files)))))

(defun d12/find-file-in-project ()
  "Find file in current project.

If it's possible, this function uses `counsel-git' to find file.
Otherwise `projectile-find-file' is used."
  (interactive)
  (if (and (configuration-layer/layer-usedp 'ivy)
           (locate-dominating-file default-directory ".git"))
      (counsel-git)
    (projectile-find-file)))

(defun d12/calc-eval-and-replace (p1 p2)
  "Calculate selection and replace it with result."
  (interactive "r")
  (let ((res (calc-eval (buffer-substring p1 p2))))
    (delete-region p1 p2)
    (insert res)))

(defun d12/rename-frame (format)
  (interactive "sFormat: ")
  (setq frame-title-format format))

(defun d12/sync-spacemacs ()
  "Sync Spacemacs."
  (if (configuration-layer/package-usedp 'bpr)
      (bpr-spawn "sync_spacemacs")
    (async-shell-command "sync_spacemacs")))

(defun d12/disable-final-newline ()
  "Set `require-final-newline' to nil locally."
  (interactive)
  (set (make-local-variable 'require-final-newline) nil))

;;; funcs.el ends here
