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

  :hook ((c-mode c++-mode) . lsp)

  :autoload (lsp-register-client make-lsp-client lsp-tramp-connection)  ;; Needed by below.

  :config

  (use-package lsp-clangd :ensure nil)  ;; Needed by below.

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection
                                     (lambda () (cons "clangd" lsp-clients-clangd-args)))
                    :major-modes '(c-mode c++-mode)
                    :remote? t
                    :server-id 'clangd-remote)))

(use-package cmake-mode)
