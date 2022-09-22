;; Must be before `(require 'projectile)' is ever done, to have effect for
;; non-file non-project buffers.
(custom-set-variables
 '(projectile-mode-line-prefix ""))

(require 'projectile)
(require 'ibuffer-projectile)

(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)

(defun my-projectile-mode-line ()
  "Report project name, but not type, only if there is a current project."
  (let ((project-name (projectile-project-name)))
    (if (and project-name (not (string= "-" project-name)))
        (format " Proj[%s]" project-name)
      "")))

(defun my-ibuffer-projectile-group-name (project-name root-dir)
  ;(format "%s %s" project-name root-dir)
  root-dir)
