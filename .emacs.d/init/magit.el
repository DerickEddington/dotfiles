(require 'magit)

(put 'magit-reset-quickly 'disabled t)
(add-hook 'magit-status-mode-hook
          #'(lambda ()
              (define-key magit-status-mode-map (kbd "x") nil)))

(advice-add 'magit-process-git-arguments
            :filter-return
            #'(lambda (args)
                (mapcar
                 #'(lambda (a) (if (tramp-tramp-file-p a) (file-local-name a) a))
                 args)))
