;; -*- lexical-binding: t; -*-

(use-package my :load-path "my/lib"
  :autoload (my--xref-pop-to-location--maybe-dont-ask-follow-symlinks
             my--custom-save-all--pretty-print-old-way
             my--string-truncate-left))


(use-package xref :ensure nil
  :config
  (advice-add 'xref-pop-to-location :around
              #'my--xref-pop-to-location--maybe-dont-ask-follow-symlinks))


(unless (version< emacs-version "29")
  ;; Make newer Emacs versions use the exact-same pretty-printing algorithm as previous versions,
  ;; to avoid unnecessary superficial formatting changes in my `custom.el' that is already tracked
  ;; in my `~/.dotfiles/' repository where such changes would be undesirable uncommitted noise.
  (use-package cus-edit :ensure nil
    :config
    (advice-add 'custom-save-all :around
                #'my--custom-save-all--pretty-print-old-way))

  (defface my-negated-operator-face
    '((t (:inherit (font-lock-negation-char-face font-lock-operator-face))))
    "Merge these two faces for use as one name."))


(unless (version< emacs-version "28")
  (use-package subr-x :ensure nil
    :config
    (advice-add 'string-truncate-left :around
                #'my--string-truncate-left)))


(defconst my-use-treesitter-modes
  (and nil  ;; Remove this `nil' if want to use Tree-sitter modes.
       (version<= "30" emacs-version)
       (treesit-available-p)))
