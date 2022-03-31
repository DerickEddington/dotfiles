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
;; The rust-analyzer LSP lens for "Run Test|Debug" displays with the glyphless
;; VARIATION SELECTOR-15 character (#xFE0E) but we don't want to see the
;; hex-code box for that.
(aset glyphless-char-display #xFE0E 'zero-width)

(require 'smartparens)
(define-key rust-mode-map [remap sp-forward-slurp-sexp] #'sp-slurp-hybrid-sexp)
(define-key rust-mode-map [remap sp-transpose-sexp] #'sp-transpose-hybrid-sexp)

(add-hook 'rust-mode-hook #'(lambda () (setq fill-column 98)))
(add-hook 'rust-mode-hook #'imenu-add-menubar-index)
(add-hook 'rust-mode-hook #'hs-minor-mode)
