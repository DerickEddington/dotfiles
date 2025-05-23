;; -*- lexical-binding: t; coding: utf-8; -*-

(defgroup my nil
  "My own things."
  :group 'emacs)


(defcustom my-follow-symlinks vc-follow-symlinks
  "What to do if visiting a symbolic link to another file.

If this variable is `t', follow the link and visit the real file,
telling you about it in the echo area.  If it is `ask', ask for
confirmation whether it should follow the link.  If `nil', the link is
visited and a warning displayed."
  :type '(choice (const :tag "Ask for confirmation" ask)
                 (const :tag "Visit link without following and warn" nil)
                 (const :tag "Follow link without confirmation" t))
  :group 'my)

(defcustom my-dont-ask-follow-symlinks nil
  "Functions for which non-nil `my-follow-symlinks' will not ask.

Can be used like:
  (defun your-lang-mode-hook ()
    (setq-local my-dont-ask-follow-symlinks \\='(xref-pop-to-location)))"
  :type '(set (const xref-pop-to-location)) ;; Could be extended with more items.
  :group 'my)

(defun my-follow-link ()
  (vc-follow-link)
  (message "Followed sym-link to %s" buffer-file-name))

(defun my-maybe-follow-link ()
  "Somewhat similar to `vc-refresh-state' but reduced for my purposes."
  (interactive)
  (let* ((truename (and buffer-file-truename
                        (expand-file-name buffer-file-truename)))
         (is-link (and truename
                       (not (equal buffer-file-name truename)))))
    (cond ((not is-link) nil)   ;Nothing to do.
          ((eq my-follow-symlinks nil)
           (message "Warning: visiting sym-link without following it."))
          ((or (not (eq my-follow-symlinks 'ask))
               ;; Assume we cannot ask, default to yes.
               noninteractive
               ;; Copied from server-start.  Seems like there should
               ;; be a better way to ask "can we get user input?"...
               (and (daemonp)
                    (null (cdr (frame-list)))
                    (eq (selected-frame) terminal-frame))
               ;; If we already visited this file by following
               ;; the link, don't ask again if we try to visit
               ;; it again.  GUD does that, and repeated questions
               ;; are painful.
               (get-file-buffer
                (abbreviate-file-name
                 (file-chase-links buffer-file-name))))
           (my-follow-link))
          (t
           (if (yes-or-no-p (format "Sym-link to %s; follow? " buffer-file-truename))
               (my-follow-link)
             (message "Warning: editing through the sym-link bypasses any version control.")
             )))))


(defun my-disable-vc-mode ()
  (remove-hook 'find-file-hook #'vc-refresh-state)
  (remove-hook 'kill-buffer-hook #'vc-kill-buffer-hook)
  (setopt vc-handled-backends nil
          vc-ignore-dir-regexp ""
          auto-revert-check-vc-info nil)
  (add-hook 'find-file-hook #'my-maybe-follow-link))


(defun my--xref-pop-to-location--maybe-dont-ask-follow-symlinks (func &rest rest)
  (let ((my-follow-symlinks
         (or (and (not vc-handled-backends)  ;; If VC is disabled.
                  my-follow-symlinks
                  (memq 'xref-pop-to-location my-dont-ask-follow-symlinks)
                  t)
             my-follow-symlinks)))
    (apply func rest)))


(cond
 ((version<= "30" emacs-version)
  (defun my--custom-save-all--pretty-print-old-way (func &rest rest)
    (let ((pp-default-function #'pp-28))
      (apply func rest))))
 ((version<= "29" emacs-version)
  (with-suppressed-warnings ((obsolete pp-use-max-width))
    (defun my--custom-save-all--pretty-print-old-way (func &rest rest)
      (let ((pp-use-max-width nil))
        (apply func rest))))))


(unless (version< emacs-version "28")
  (defun my--string-truncate-left (func string length)
    "My modification of `string-truncate-left' to use \"…\" instead."
    (declare (pure t) (side-effect-free t))
    (if (char-displayable-p ?…)
        (let ((strlen (length string)))
          (if (<= strlen length)
              string
            (setq length (max 1 (- length 1)))
            (concat "…" (substring string (- strlen length)))))
      (apply func string length))))


(provide 'my)
