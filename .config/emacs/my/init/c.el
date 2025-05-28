;; -*- lexical-binding: t; -*-

(use-package cc-mode :ensure nil
  :bind
  (:map c-mode-base-map
        ([remap sp-forward-slurp-sexp] . sp-slurp-hybrid-sexp)
        ([remap sp-transpose-sexp]     . sp-transpose-hybrid-sexp))
  :hook
  (((c-mode c++-mode) . hs-minor-mode)
   ((c-mode c++-mode) . imenu-add-menubar-index)
   (c-mode-common . my-c-mode-hook)))

(defun my-c-mode-hook ()
  (face-remap-set-base 'button nil)
  (font-lock-add-keywords nil
    '(("\\(?:\\s-\\|\\s.\\|\\s(\\|\\s)\\|^\\)\\(\\(?:\\(?:[[:digit:]]*[.][[:digit:]]+\\|[[:digit:]]+[.]\\)\\(?:[eE][+-]?[[:digit:]]+\\)?\\|[[:digit:]]+[eE][+-]?[[:digit:]]+\\|0[xX]\\(?:[[:xdigit:]]*[.][[:xdigit:]]+\\|[[:xdigit:]]+[.]?\\)[pP][+-]?[[:digit:]]+\\)[flFL]?\\)\\(?:\\s-\\|\\s.\\|\\s(\\|\\s)\\|$\\)"
       1 'font-lock-constant-face)
      ("\\_<\\(?:[1-9][[:digit:]]*\\|[0-7]+\\|\\(?:0[xX]\\)[[:xdigit:]]+\\)\\(?:[uU]\\(?:[lL]\\|ll\\|LL\\)?\\|\\(?:[lL]\\|ll\\|LL\\)[uU]?\\)?\\_>"
       . 'font-lock-constant-face))))

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
