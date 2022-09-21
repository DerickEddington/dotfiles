(require 'vc-hooks)
(require 'custom)

(defgroup my nil
  "My own things.")

(defun my-disable-vc-mode ()
  (remove-hook 'find-file-hook #'vc-refresh-state)
  (remove-hook 'kill-buffer-hook #'vc-kill-buffer-hook)
  (custom-set-variables
   '(vc-handled-backends nil)
   '(vc-ignore-dir-regexp "")
   '(auto-revert-check-vc-info nil))
  (add-hook 'find-file-hook #'my-maybe-follow-link))


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


(require 'xref)

(defcustom my-dont-ask-follow-symlinks nil
  "Functions for which non-nil `my-follow-symlinks' will not ask.

Can be used like:
  (defun your-lang-mode-hook ()
    (setq-local my-dont-ask-follow-symlinks '(xref-pop-to-location)))"
  :type '(set (const xref-pop-to-location)) ;; Could be extended with more items.
  :local t
  :group 'my)

(defun my--xref-pop-to-location--around-advice--dont-ask-follow-symlinks (func &rest rest)
  (let ((my-follow-symlinks
         (or (and my-follow-symlinks
                  (memq 'xref-pop-to-location my-dont-ask-follow-symlinks)
                  t)
             my-follow-symlinks)))
    (apply func rest)))

(advice-add 'xref-pop-to-location :around
            #'my--xref-pop-to-location--around-advice--dont-ask-follow-symlinks)
