(require 'cc-mode)

(require 'eldoc)

(require 'flycheck)

(require 'lsp-mode)
(add-hook 'c-mode-hook #'lsp)
(lsp-register-client
 (make-lsp-client :new-connection (lsp-tramp-connection "clangd")
                  :major-modes '(c-mode)  ; Could also include: c++-mode
                  :remote? t
                  :server-id 'clangd-remote))

(require 'smartparens)
(define-key c-mode-map [remap sp-forward-slurp-sexp] #'sp-slurp-hybrid-sexp)
(define-key c-mode-map [remap sp-transpose-sexp] #'sp-transpose-hybrid-sexp)

(require 'cmake-mode)

(add-hook 'c-mode-hook #'imenu-add-menubar-index)
(add-hook 'c-mode-hook #'hs-minor-mode)
