(use-package vagrant-tramp
  :if (locate-library "vagrant-tramp")
  :after tramp)

(use-package docker-tramp
  :if (locate-library "docker-tramp")
  :after tramp)


(use-package tramp

  :config

  ;; Set here, after loading `tramp', because these need variables from that.
  (setopt
   tramp-connection-properties (append '((nil         "remote-shell"       "bash")
                                         ("/vagrant:" "remote-shell-login" ("-l")))
                                       tramp-connection-properties)
   tramp-remote-path (cons 'tramp-own-remote-path tramp-remote-path))

  (advice-add 'tramp-send-command :filter-args
              #'my--tramp-send-command--workaround-stty-icanon-bug--filter-args)

  ;; Configure the TRAMP methods that use SSH to use multiplexed SSH connections, because this
  ;; usually improves performance.  The configuration is independent for each method and for each
  ;; emacs process, to not affect other SSH connections (e.g. my interactive sessions in external
  ;; terminals, or other emacs processes') that are not for the method or not for the same emacs
  ;; process.  (Note: The length of a control-socket's pathname must be minimized, due to the size
  ;; limit of `sockaddr_un.sun_path', which is why this uses short code-names for
  ;; sub-directories.)
  ;;
  (if-let (((bound-and-true-p my--ssh-control-sockets-dir))
           (tramp-dir (concat my--ssh-control-sockets-dir "/t"))
           (docker-codename  "d")
           (ssh-codename     "s")
           (vagrant-codename "v"))
      (progn
        ;; Ensure these directories exist if not already.
        (dolist (method-subdir (list docker-codename ssh-codename vagrant-codename))
          (make-directory (concat tramp-dir "/" method-subdir) t))

        ;; Configure the `/ssh:' method.
        (let* ((method-dir (concat tramp-dir "/" ssh-codename))
               (sock-path (concat method-dir "/%%C")))  ;; (`%C' expands to a constant-length.)
          (setq tramp-ssh-controlmaster-options  ;; (The double-`%' are needed for this.)
                (string-join
                 `("-o ControlMaster=auto"
                   ;; The following two are similar to what `~/.ssh/config' does.
                   ,(concat "-o ControlPath=" sock-path)
                   "-o ControlPersist=30m")  ;; Prevent orphaning `ssh' processes for too long.
                 " ")))

        ;; Configure the `/vagrant:' method.  This requires a hack around the limitations of
        ;; `vagrant-tramp/bin/vagrant-tramp-ssh' (which doesn't support passing options for `ssh'
        ;; through `vagrant ssh').  This coordinates with the corresponding special `Match exec'
        ;; block in `~/.ssh/config'.
        (let ((method-dir (concat tramp-dir "/" vagrant-codename)))
          (setq
           tramp-methods
           (mapcar
            #'(lambda (method)
                (let ((name (car method)) (params (cdr method)))
                  (if (equal "vagrant" name)
                      (let ((login-program (cadr (assoc 'tramp-login-program params))))
                        (cons name
                              (mapcar #'(lambda (p)
                                          (let ((key (car  p)) (val (cadr p)))
                                            (cond
                                             ;; Change the login program to `env', to enable
                                             ;; setting our special environment variable.
                                             ((equal 'tramp-login-program key)
                                              (list key "env"))
                                             ;; Add arguments that do the setting of our special
                                             ;; environment variable, for the execution of the
                                             ;; original login program.
                                             ((equal 'tramp-login-args key)
                                              (list key
                                                    (cons
                                                     (list (concat "MY_SSH_CONTROL_SOCKETS_DIR="
                                                                   method-dir)
                                                           ;; Preserve the original program.
                                                           login-program)
                                                     ;; Preserve the original arguments.
                                                     val)))
                                             ;; Irrelevant. Preserve whatever it is.
                                             (t
                                              p))))
                                      params)))
                    ;; Irrelevant. Preserve whatever it is.
                    method)))
            tramp-methods))))))


(defun my--tramp-send-command--workaround-stty-icanon-bug (conn-vec orig-command &rest args)
  "See: https://github.com/magit/magit/issues/4720"
  (let ((command
         (if (string= "stty -icrnl -icanon min 1 time 0" orig-command)
             "stty -icrnl"
           orig-command)))
    (append (list conn-vec command) args)))

(defun my--tramp-send-command--workaround-stty-icanon-bug--filter-args (args)
  (apply #'my--tramp-send-command--workaround-stty-icanon-bug args))
