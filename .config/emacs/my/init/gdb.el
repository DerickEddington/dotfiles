;; -*- lexical-binding: t; -*-

(use-package my-gdb :load-path "my/lib"
  :autoload my-gdb--setup-windows--customize)


(use-package gdb-mi :ensure nil

  ;; :hook (gdb-mode . my-gdb-mode-hook)

  :config
  ;; This advice is needed because `gdb-many-windows-hook' is insufficient because
  ;; `gdb-setup-windows' can be run again after that (e.g. via the "Restore Window Layout"
  ;; function of the Gud menu (i.e. `gdb-restore-windows')) and it causes the window to be reset
  ;; to dedicated again, which would prevent being able to switch it, so we must undo that each
  ;; time.  This is also run when the `gdb-many-windows' minor mode is toggled on and so adding it
  ;; to the hook is unnecessary.
  (advice-add 'gdb-setup-windows :after #'my-gdb--setup-windows--customize))
