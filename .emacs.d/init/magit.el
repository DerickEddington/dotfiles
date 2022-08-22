(require 'magit)

(put 'magit-reset-quickly 'disabled t)
(add-hook 'magit-status-mode-hook
          #'(lambda ()
              (define-key magit-status-mode-map (kbd "x") nil)))
