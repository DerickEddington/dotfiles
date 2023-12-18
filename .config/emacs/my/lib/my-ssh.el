;; -*- lexical-binding: t; -*-

(defconst my--ssh-control-sockets-dir
  (if-let ((runtime-dir (getenv "XDG_RUNTIME_DIR")))
      (concat (file-name-as-directory runtime-dir)
              "my/emacs/"
              (format "%d/" (emacs-pid))))
  "The length of this must be minimized,
due to the size limit of `sockaddr_un.sun_path'.")

(when my--ssh-control-sockets-dir

  (make-directory my--ssh-control-sockets-dir t)

  (add-hook 'kill-emacs-hook (lambda () (delete-directory my--ssh-control-sockets-dir t))))


(provide 'my-ssh)
