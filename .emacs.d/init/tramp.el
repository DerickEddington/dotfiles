(require 'custom)
(require 'tramp)
(require 'vagrant-tramp nil t)
(require 'docker-tramp nil t)


(defun my--tramp-send-command--workaround-stty-icanon-bug (conn-vec orig-command &rest args)
  "See: https://github.com/magit/magit/issues/4720"
  (let ((command
         (if (string= "stty -icrnl -icanon min 1 time 0" orig-command)
             "stty -icrnl"
           orig-command)))
    (append (list conn-vec command) args)))

(defun my--tramp-send-command--workaround-stty-icanon-bug--filter-args (args)
  (apply #'my--tramp-send-command--workaround-stty-icanon-bug args))

(advice-add 'tramp-send-command :filter-args
            #'my--tramp-send-command--workaround-stty-icanon-bug--filter-args)
