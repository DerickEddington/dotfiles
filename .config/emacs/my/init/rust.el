;; -*- lexical-binding: t; -*-

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
  :hook (rust-mode . lsp))
