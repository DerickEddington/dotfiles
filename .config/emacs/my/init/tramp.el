;; -*- lexical-binding: t; -*-

(use-package my-utils :load-path "my/lib"
  :autoload when-have-library)
(use-package my-tramp :load-path "my/lib"
  :autoload
  (my--tramp-send-command--workaround-stty-icanon-bug--filter-args
   my-setup-tramp-ssh-multiplexing))


(when-have-library "vagrant-tramp"
  ;; Must be conditional at compile-time, otherwise `use-package' tries to load the missing
  ;; library at compile-time even if `:if' inside the form or `when' around the form were used.
  (use-package vagrant-tramp
    :after tramp))


(use-package tramp
  :config

  ;; Set here, after loading `tramp', because these need variables from that.
  (setopt
   tramp-connection-properties (append '((nil         "remote-shell"       "bash")
                                         ("/vagrant:" "remote-shell-login" ("-l")))
                                       tramp-connection-properties)
   tramp-remote-path (cons 'tramp-own-remote-path tramp-remote-path))

  (advice-add 'tramp-send-command :filter-args
              #'my--tramp-send-command--workaround-stty-icanon-bug--filter-args))


(use-package tramp-sh :ensure nil
  :after tramp

  :config

  ;; Configure the TRAMP methods that use SSH to use multiplexed SSH connections, because this
  ;; usually improves performance.  The configuration is independent for each method and for each
  ;; emacs process, to not affect other SSH connections (e.g. my interactive sessions in external
  ;; terminals, or other emacs processes') that are not for the method or not for the same emacs
  ;; process.  (Note: The length of a control-socket's pathname must be minimized, due to the size
  ;; limit of `sockaddr_un.sun_path', which is why this uses short code-names for
  ;; sub-directories.)
  (my-setup-tramp-ssh-multiplexing))
