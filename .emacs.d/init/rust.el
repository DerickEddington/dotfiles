(require 'rust-mode)

(require 'company)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)

(require 'eldoc)

(require 'cargo)
(add-hook 'rust-mode-hook #'cargo-minor-mode)

(require 'flycheck)
(require 'flycheck-rust)

(require 'lsp-mode)
(add-hook 'rust-mode-hook #'lsp)

(require 'smartparens)
(define-key rust-mode-map [remap sp-forward-slurp-sexp] #'sp-slurp-hybrid-sexp)
(define-key rust-mode-map [remap sp-transpose-sexp] #'sp-transpose-hybrid-sexp)
