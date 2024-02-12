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


;; Hack to pass the "init options" to rust-analyzer that are needed for rust-analyzer to
;; understand the project structure of the source-code of The Rust Programming Language itself
;; (e.g. a checkout of the rust-lang/rust.git repository).  This is especially useful for browsing
;; and navigating the source-code of the Rust Standard Libraries (and/or Compiler), in Emacs via
;; rust-analyzer.

(use-package my-rust-lang-proj :load-path "my/lib"
  :autoload (my--lsp-rust-analyzer--make-init-options--extend-for-rust-lang-proj))

(use-package lsp-rust
  :config
  (advice-add 'lsp-rust-analyzer--make-init-options :filter-return
              #'my--lsp-rust-analyzer--make-init-options--extend-for-rust-lang-proj))

;; TODO: Could also have some more advice that'd extend with some always-used (i.e. not
;; conditional) `my-rust-analyzer-extra-settings' that'd be useful for any/all projects
;; (i.e. other than rust-lang) to use further configuration options of rust-analyzer that
;; `lsp-rust.el' doesn't provide support for.  See: `../lib/my-rust-analyzer.el'.
