;; This helps performance, especially for lsp-mode with rust-analyzer.
;; This variable is not customizable, so we set it like this here.
(setq read-process-output-max (* 2 1024 1024))

(require 'lsp-mode)
