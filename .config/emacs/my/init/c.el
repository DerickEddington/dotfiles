;; -*- lexical-binding: t; -*-

(use-package cc-mode
  :bind
  (:map c-mode-map
        ([remap sp-forward-slurp-sexp] . sp-slurp-hybrid-sexp)
        ([remap sp-transpose-sexp]     . sp-transpose-hybrid-sexp))
  :hook
  (((c-mode c++-mode) . hs-minor-mode)
   ((c-mode c++-mode) . imenu-add-menubar-index)))

(use-package eldoc)

(use-package flycheck)

(use-package lsp-mode
  :hook ((c-mode c++-mode) . lsp))

(use-package cmake-mode)
