;; -*- lexical-binding: t; -*-  ;; To capture `orig-command' in the closure below.

(use-package lsp-mode
  :autoload (lsp-resolve-final-command lsp-warn))


(defun my-lsp-stdio-connection--wait-for-stty-to-take-effect (result)
  (if (file-remote-p default-directory)
      (let ((connect-func (plist-get result :connect)))
        (add-function :around (var connect-func) #'my-wait-for-stty-to-take-effect)
        ;; For some reason, this must be done, instead of just returning `result'.
        (plist-put result :connect connect-func))
    result))

(defun my-lsp-resolve-final-command--wait-for-stty-to-take-effect (result)
  (if (listp result)
      (mapcar (lambda (arg)
                ;; This added "echo after-stty-token" command depends on the assumption that
                ;; `lsp-resolve-final-command' will prepend the "stty raw" command immediately
                ;; preceding its argument command, for remote files/dirs.
                (string-replace "stty raw > /dev/null;"
                                "stty raw > /dev/null; echo after-stty-token;"
                                arg))
              result)
    result))

(defun my-wait-for-stty-to-take-effect (func &rest args)
  (let* ((timeout 10)
         (result
          ;; Call the `:connect' function of `lsp-stdio-connection'.
          (apply func args))
         (proc (car result))
         (orig-filter (process-filter proc)))
    ;; Temporarily change the process filter, for the following `accept-process-output'.
    (set-process-filter proc (lambda (_proc output)
                               (unless (string= "after-stty-token\n" output)
                                 (lsp-warn "Unexpected stty-hack message: %s" output))))
    (unless (accept-process-output proc timeout)
      (lsp-warn "Failed to receive stty-hack message"))
    ;; Restore its original state.
    (set-process-filter proc orig-filter)
    ;; Return the result of the `:connect' function.
    result))


(provide 'my-lsp)
