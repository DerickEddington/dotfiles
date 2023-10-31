;; -*- lexical-binding: t; -*-

(use-package my-magit :load-path "my/lib"
  :autoload my-magit-process-git-arguments--tramp-files-local-names)


(use-package magit

  :bind (("C-c g" . magit-file-dispatch)
         :map magit-status-mode-map
              ("x" . nil))

  :hook ((after-save . magit-after-save-refresh-status))

  :config

  (setopt magit-wip-mode t)  ;; Doing this here is faster than elsewhere.

  (put 'magit-reset-quickly 'disabled t)

  (advice-add 'magit-process-git-arguments :filter-return
              #'my-magit-process-git-arguments--tramp-files-local-names))
