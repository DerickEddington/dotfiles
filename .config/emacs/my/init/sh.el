;; -*- lexical-binding: t; -*-

(use-package flycheck)

(use-package lsp-mode
  :hook (sh-mode . lsp))
