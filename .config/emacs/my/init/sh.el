;; -*- lexical-binding: t; -*-

(use-package flycheck)

(use-package lsp-mode
  :hook (sh-mode . lsp))


(defconst my-use-bash-ts-mode
  (and my-use-treesitter-modes
       (treesit-language-available-p 'bash)))

(when my-use-bash-ts-mode                          ;; (Keep below in-sync with above.)
  (use-package sh-script :ensure nil
    :hook (bash-ts-mode . my-bash-ts-mode-hook))

  ;; Set this here so this is only done when our conditional is true.
  (setopt major-mode-remap-alist (append '((sh-mode . bash-ts-mode)) major-mode-remap-alist))

  (use-package lsp-mode
    :hook (bash-ts-mode . lsp))

  (defun my-bash-ts-mode-hook ()
    ;; Prevent highlighting shell command expressions' command name, by removing the `command'
    ;; feature from what TreeSitter does for these buffers.
    (treesit-font-lock-recompute-features nil '(command))))
