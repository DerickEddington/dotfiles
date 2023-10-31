;; -*- lexical-binding: t; -*-

(use-package flycheck)

(use-package lsp-mode

  :hook (sh-mode . lsp)

  :autoload (lsp-register-client make-lsp-client lsp-tramp-connection)  ;; Needed by below.

  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection '("bash-language-server" "start"))
                    :major-modes '(sh-mode bash-ts-mode)
                    :remote? t
                    :server-id 'bash-language-server-remote)))
