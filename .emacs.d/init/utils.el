(defun my-alist-remove (al exclude)
  (map-filter
   (lambda (key val) (not (member key exclude)))
   al))


(require 'vc-hooks)
(require 'custom)

(defun my-disable-vc-mode ()
  (remove-hook 'find-file-hook #'vc-refresh-state)
  (remove-hook 'kill-buffer-hook #'vc-kill-buffer-hook)
  (custom-set-variables
   '(vc-handled-backends nil)
   '(vc-ignore-dir-regexp "")
   '(auto-revert-check-vc-info nil)))
