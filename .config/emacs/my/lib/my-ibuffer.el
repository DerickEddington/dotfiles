;; -*- lexical-binding: t; -*-

(eval-when-compile (require 'use-package))


(use-package ibuffer :ensure nil
  ;; Needed by my functions below.
  :commands (ibuffer-update)
  :autoload (ibuffer-buffer-file-name
             ibuffer-make-column-filename-and-process))
(use-package ibuf-ext :ensure nil)
(use-package ibuffer-project
  ;; Needed by my functions below.
  :commands (ibuffer-project-clear-cache)
  :autoload (ibuffer-project-root ibuffer-project-generate-filter-groups))

(if (version<= "28" emacs-version)
    (use-package ibuffer :ensure nil
      ;; Needed by my functions below.
      :autoload (ibuffer--abbreviate-file-name))
  (defun ibuffer--abbreviate-file-name (filename)
    "Abbreviate FILENAME using `ibuffer-directory-abbrev-alist'."
    (let ((directory-abbrev-alist ibuffer-directory-abbrev-alist))
      (abbreviate-file-name filename))))


(defun my-ibuffer (&optional clear-cache)
  "Better than relying on a hook added to `ibuffer-hook'.

Causes `ibuffer' to use our filter-groups initially, which avoids
seeing stale state that otherwise would require an extra
`ibuffer-update' call to see properly."
  (interactive "P")
  (when clear-cache
    (ibuffer-project-clear-cache))
  (let ((filter-groups (ibuffer-project-generate-filter-groups))
        (other-window-p nil) (name nil) (qualifiers nil)
        (noselect nil) (shrink nil) (formats nil))
    (ibuffer
     other-window-p name qualifiers noselect shrink filter-groups formats)))


(defun my-ibuffer-update (arg &optional silent)
  "Regenerate filter-groups according to `ibuffer-project', when appropriate.

Causes `ibuffer-update' to use updated filter-groups, which
avoids seeing new buffers placed in the \"Default\" group that
otherwise would require an extra `ibuffer' call to see properly."
  (interactive "P")
  (when (and ibuffer-filter-groups  ; When nil, has been intentionally modified.
             (seq-every-p (lambda (g) (pcase g (`(,_group-label (project-root . ,_qualifier)) t)))
                          ibuffer-filter-groups))
    (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups)))
  (ibuffer-update arg silent))


(defun my-ibuffer-column--size--body (_buffer _mark)
  (let ((size (buffer-size)))
    (propertize (file-size-human-readable size) 'my-ibuffer-size size)))

(defun my-ibuffer-column--size--summarizer (column-strings)
  (let ((total 0))
    (dolist (string column-strings)
      ;; Because the original string could've been (probably was) aligned with
      ;; space characters, the position of our text property is not fixed, so we
      ;; must search for it.
      (let* ((pos (string-match "[^ ]" string))
             (size (get-text-property pos 'my-ibuffer-size string)))
        (setq total (+ total size))))
    (file-size-human-readable total)))

(defun my-ibuffer-column--proc-&-rel-fn--body (buffer mark)
  (let* ((abbrevname-and-process
          (ibuffer-make-column-filename-and-process buffer mark))
         (filename
          (or (ibuffer-buffer-file-name) ""))
         (process
          (if (and (get-buffer-process buffer)
                   (string-match "^\\((.* .*)\\)" abbrevname-and-process))
              (match-string 1 abbrevname-and-process)))
         (project-root
          (and (> (length filename) 0) (ibuffer-project-root buffer))))
    (setq filename
          (if-let* (project-root
                    (root-dir (car project-root)))
              (file-relative-name filename root-dir)
            (ibuffer--abbreviate-file-name filename)))
    (if process
        (concat (propertize process 'font-lock-face 'font-lock-comment-face)
                (if (> (length filename) 0) (format " %s" filename) ""))
      filename)))


(provide 'my-ibuffer)
