;; -*- lexical-binding: t; -*-

(use-package smerge-mode :ensure nil)

(use-package my-smerge :load-path "my/lib"
  :after smerge-mode
  :hook (smerge-mode . my-smerge-mode-hook))
