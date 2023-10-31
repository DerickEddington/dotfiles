;; -*- lexical-binding: t; -*-

(use-package my-lsp :load-path "my/lib"
  :autoload my-lsp-tramp-connection--wait-for-stty-to-take-effect)


(use-package lsp-mode

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
  (advice-add 'lsp-tramp-connection :around
              #'my-lsp-tramp-connection--wait-for-stty-to-take-effect))

(use-package lsp-ivy)
(use-package lsp-ui)
