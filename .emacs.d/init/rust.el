(use-package rust-mode
  :bind
  (:map rust-mode-map
        ("TAB" . company-indent-or-complete-common)
        ([remap sp-forward-slurp-sexp] . sp-slurp-hybrid-sexp)
        ([remap sp-transpose-sexp]     . sp-transpose-hybrid-sexp))
  :hook
  ((rust-mode . hs-minor-mode)
   (rust-mode . imenu-add-menubar-index)))

(use-package eldoc)

(use-package flycheck)

(use-package lsp-mode
  :hook (rust-mode . lsp)
  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "rust-analyzer")
                    :major-modes '(rust-mode)
                    :remote? t
                    :server-id 'rust-analyzer-remote)))
