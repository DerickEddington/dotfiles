(use-package flycheck)

(use-package lsp-mode
  :hook (sh-mode . lsp)
  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection '("bash-language-server" "start"))
                    :major-modes '(sh-mode bash-ts-mode)
                    :remote? t
                    :server-id 'bash-language-server-remote)))
