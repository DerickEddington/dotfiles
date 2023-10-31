;; -*- lexical-binding: t; -*-

(defun my-magit-process-git-arguments--tramp-files-local-names (args)
  (if (fboundp 'tramp-tramp-file-p)
      (mapcar (lambda (a) (if (tramp-tramp-file-p a) (file-local-name a) a))
              args)
    args))


(provide 'my-magit)
