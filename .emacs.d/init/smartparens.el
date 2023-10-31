(use-package smartparens
  :config

  ;; Set here, after loading `smartparens', because these need variables from that.
  (setopt sp-lisp-modes (append '(lisp-data-mode) sp-lisp-modes))

  (use-package smartparens-config :demand t))
