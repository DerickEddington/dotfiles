;; -*- lexical-binding: t; -*-

(use-package my :load-path "my/lib"
  :autoload my--xref-pop-to-location--maybe-dont-ask-follow-symlinks)


(use-package xref
  :config
  (advice-add 'xref-pop-to-location :around
              #'my--xref-pop-to-location--maybe-dont-ask-follow-symlinks))
