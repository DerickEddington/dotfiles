;; -*- lexical-binding: t; -*-  ;; To capture `orig-command' in the closure below.

(use-package lsp-mode
  :autoload (lsp-resolve-final-function lsp-warn))


(defun my-lsp-tramp-connection--wait-for-stty-to-take-effect (func orig-command &rest rest)
  (let* ((my-command
          ;; This depends on the assumption that the `:connect' function of `lsp-tramp-connection'
          ;; will prepend the "stty raw ;" command immediately preceding this echo command.
          (lambda ()
            (append '("echo" "after-stty-token" ";")
                    ;; Ensure this is a list of strings.
                    (lsp-resolve-final-function orig-command))))
         (result
          ;; Call `lsp-tramp-connection' with our extended command.
          (apply func my-command rest))
         (connect-func (plist-get result :connect)))
    (add-function :around (var connect-func) #'my-wait-for-stty-to-take-effect)
    ;; For some reason, this must be done, instead of just returning `result'.
    (plist-put result :connect connect-func)))

(defun my-wait-for-stty-to-take-effect (func &rest args)
  (let* ((timeout 10)
         (result
          ;; Call the `:connect' function of `lsp-tramp-connection'.
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
