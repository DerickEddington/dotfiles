(require 'flycheck)

(require 'lsp-mode)
(add-hook 'sh-mode-hook #'lsp)
(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-tramp-connection #'(lambda () '("bash-language-server" "start")))
  :major-modes '(sh-mode bash-ts-mode)
  :remote? t
  :server-id 'bash-language-server-remote))
