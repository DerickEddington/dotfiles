;; -*- lexical-binding: t; -*-

(use-package rust-mode
  :bind
  (:map rust-mode-map
        ("TAB" . company-indent-or-complete-common)
        ([remap sp-forward-slurp-sexp] . sp-slurp-hybrid-sexp)
        ([remap sp-transpose-sexp]     . sp-transpose-hybrid-sexp))
  :hook
  ((rust-mode . hs-minor-mode)
   (rust-mode . imenu-add-menubar-index)
   (rust-mode . (lambda ()
                  (when (file-remote-p default-directory)
                    (setq-local  ;; Note: Dir- or file-local vars can override, if desired.
                     rust-format-on-save nil  ;; Disable, because it always runs local `rustfmt'.
                     rust-rustfmt-switches '()))))))

(use-package eldoc)

(use-package flycheck)

(use-package lsp-mode
  :hook ((rust-mode . (lambda ()
                        (when (file-remote-p default-directory)
                          (setq-local  ;; Note: Dir- or file-local vars can override, if desired.
                           lsp-rust-analyzer-rustfmt-extra-args []))  ;; Assume nothing.
                        ;; Wait to invoke `lsp-mode' until after local vars were processed.
                        (add-hook 'hack-local-variables-hook #'lsp 100 t)))
         (lsp-mode . (lambda ()
                       (when (file-remote-p default-directory)
                         (if lsp-mode  ;; `rustfmt' via `rust-analyzer'. Both run in the remote.
                             (add-hook 'before-save-hook #'lsp-format-buffer -100 t)
                           (remove-hook 'before-save-hook #'lsp-format-buffer t)))))))
