;; -*- lexical-binding: t; -*-

(use-package my-c :load-path "my/lib"
  :autoload (my-c-mode-hook))


(use-package cc-mode :ensure nil
  :bind
  (:map c-mode-base-map
        ([remap sp-forward-slurp-sexp] . sp-slurp-hybrid-sexp)
        ([remap sp-transpose-sexp]     . sp-transpose-hybrid-sexp))
  :hook
  (((c-mode c++-mode) . hs-minor-mode)
   ((c-mode c++-mode) . imenu-add-menubar-index)
   (c-mode-common . my-c-mode-hook)))

(use-package eldoc :ensure nil)

(use-package flycheck)

(use-package lsp-mode
  :hook ((c-mode c++-mode) . lsp))

(use-package cmake-mode)


(defconst my-use-c-ts-mode
  (and my-use-treesitter-modes
       (treesit-language-available-p 'c)))

(defconst my-use-c++-ts-mode
  (and my-use-c-ts-mode
       (treesit-language-available-p 'cpp)))

(when my-use-c-ts-mode                          ;; (Keep below in-sync with above.)
  (use-package c-ts-mode :ensure nil
    :bind
    (:map c-ts-base-mode-map
          ([remap sp-forward-slurp-sexp] . sp-slurp-hybrid-sexp)
          ([remap sp-transpose-sexp]     . sp-transpose-hybrid-sexp))
    :hook
    ((c-ts-mode . hs-minor-mode)
     (c-ts-mode . imenu-add-menubar-index)
     (c-ts-base-mode . my-c-mode-hook)))

  ;; Set this here so this is only done when our conditional is true.
  (setopt major-mode-remap-alist (append '((c-mode . c-ts-mode)) major-mode-remap-alist))

  (use-package lsp-mode
    :hook (c-ts-mode . lsp)))

(when my-use-c++-ts-mode                        ;; (Keep below in-sync with above.)
  (use-package c-ts-mode :ensure nil
    :hook
    ((c++-ts-mode . hs-minor-mode)
     (c++-ts-mode . imenu-add-menubar-index)))

  ;; Set this here so this is only done when our conditional is true.
  (setopt major-mode-remap-alist (append '((c++-mode . c++-ts-mode)
                                           (c-or-c++-mode . c-or-c++-ts-mode))
                                         major-mode-remap-alist))

  (use-package lsp-mode
    :hook (c++-ts-mode . lsp)))
