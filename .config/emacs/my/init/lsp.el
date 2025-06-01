;; -*- lexical-binding: t; -*-

(use-package my-lsp :load-path "my/lib"
  :autoload (my-lsp-ui-doc-toggle-moused
             my-lsp-stdio-connection--wait-for-stty-to-take-effect
             my-lsp-resolve-final-command--wait-for-stty-to-take-effect))


(use-package lsp-mode

  :bind (:map lsp-mode-map
              ("C-h ." . lsp-describe-thing-at-point))

  :custom
  ;; This variable is not customizable, so we set it like this here.
  (read-process-output-max (* 1 1024 1024)
   "Helps performance, especially for `lsp-mode' with `rust-analyzer'.")

  :config
  ;; Work-around a bug in lsp-mode+tramp where there seems to be a race condition
  ;; between the remote side needing to execute "stty raw" before the local side
  ;; sends any input.  If any input were to be sent before the "stty raw" takes
  ;; effect, the remote tty would convert carriage-return to line-feed (`icrnl')
  ;; but that would break the LSP protocol and could cause the LSP server to fail
  ;; to parse the input.  See:
  ;; https://github.com/emacs-lsp/lsp-mode/issues/2709#issuecomment-864498751
  ;; https://github.com/emacs-lsp/lsp-mode/issues/2375
  ;; https://github.com/emacs-lsp/lsp-mode/issues/1845
  (advice-add 'lsp-stdio-connection :filter-return
              #'my-lsp-stdio-connection--wait-for-stty-to-take-effect)
  (advice-add 'lsp-resolve-final-command :filter-return
              #'my-lsp-resolve-final-command--wait-for-stty-to-take-effect))

(use-package lsp-ivy)
(use-package lsp-ui)

(use-package lsp-ui-doc :ensure nil  ;; (It's installed by `lsp-ui').
  :bind
  (:map lsp-mode-map
        ("s-<mouse-1>" . my-lsp-ui-doc-toggle-moused)
        ("C-h d" . lsp-ui-doc-toggle)
   :map lsp-command-map
        ("hd" . lsp-ui-doc-toggle)))
