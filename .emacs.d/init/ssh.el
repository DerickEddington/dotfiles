(if-let ((runtime-dir (getenv "XDG_RUNTIME_DIR")))
    (progn
      (defconst my--ssh-control-sockets-dir
        (format "%s/my/emacs/%d" runtime-dir (emacs-pid))
        "The length of this must be minimized,
due to the size limit of `sockaddr_un.sun_path'.")

      (make-directory my--ssh-control-sockets-dir t)

      (add-hook 'kill-emacs-hook #'(lambda () (delete-directory my--ssh-control-sockets-dir t)))))
