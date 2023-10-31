(use-package magit

  :bind (("C-c g" . magit-file-dispatch)
         :map magit-status-mode-map
              ("x" . nil))

  :config

  (put 'magit-reset-quickly 'disabled t)

  (advice-add 'magit-process-git-arguments
              :filter-return
              #'(lambda (args)
                  (if (fboundp 'tramp-tramp-file-p)
                      (mapcar #'(lambda (a) (if (tramp-tramp-file-p a) (file-local-name a) a))
                              args)
                    args))))
