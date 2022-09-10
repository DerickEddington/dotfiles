(require 'rust-mode)

(require 'company)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)

(require 'eldoc)

(require 'cargo)
(add-hook 'rust-mode-hook #'cargo-minor-mode)

(require 'flycheck)

(require 'lsp-mode)
(add-hook 'rust-mode-hook #'lsp)
(lsp-register-client
 (make-lsp-client :new-connection (lsp-tramp-connection "rust-analyzer")
                  :major-modes '(rust-mode)
                  :remote? t
                  :server-id 'rust-analyzer-remote))

(require 'smartparens)
(define-key rust-mode-map [remap sp-forward-slurp-sexp] #'sp-slurp-hybrid-sexp)
(define-key rust-mode-map [remap sp-transpose-sexp] #'sp-transpose-hybrid-sexp)

(add-hook 'rust-mode-hook #'(lambda () (setq fill-column 98)))
(add-hook 'rust-mode-hook #'imenu-add-menubar-index)
(add-hook 'rust-mode-hook #'hs-minor-mode)
