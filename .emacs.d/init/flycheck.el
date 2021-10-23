(require 'flycheck)
(add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck errors*" eos)
               display-buffer-use-some-frame
               (inhibit-same-window . t)
               (reusable-frames . visible)
               (inhibit-switch-frame . t)
               ;(window-height   . 1.0)
               ))
