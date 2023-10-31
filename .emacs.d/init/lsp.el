;; -*- lexical-binding: t; -*-


(use-package lsp-mode
  :custom
  ;; This variable is not customizable, so we set it like this here.
  (read-process-output-max (* 1 1024 1024)
   "Helps performance, especially for `lsp-mode' with `rust-analyzer'."))

(defun my-wait-for-stty-to-take-effect (func &rest args)
  (let* ((timeout 10)
         (result
          ;; Call the `:connect' function of `lsp-tramp-connection'.
          (apply func args))
         (proc (car result))
         (orig-filter (process-filter proc)))
    ;; Temporarily change the process filter, for the following
    ;; `accept-process-output'.
    (set-process-filter proc #'(lambda (proc output)
                                 (unless (string= "after-stty-token\n" output)
                                   (lsp-warn "Unexpected stty-hack message: %s" output))))
    (unless (accept-process-output proc timeout)
      (lsp-warn "Failed to receive stty-hack message"))
    ;; Restore its original state.
    (set-process-filter proc orig-filter)
    ;; Return the result of the `:connect' function.
    result))

;; Work-around a bug in lsp-mode+tramp where there seems to be a race condition
;; between the remote side needing to execute "stty raw" before the local side
;; sends any input.  If any input were to be sent before the "stty raw" takes
;; effect, the remote tty would convert carriage-return to line-feed (`icrnl')
;; but that would break the LSP protocol and could cause the LSP server to fail
;; to parse the input.  See:
;; https://github.com/emacs-lsp/lsp-mode/issues/2709#issuecomment-864498751
;; https://github.com/emacs-lsp/lsp-mode/issues/2375
;; https://github.com/emacs-lsp/lsp-mode/issues/1845
(advice-add 'lsp-tramp-connection
            :around
            #'(lambda (func orig-command &rest rest)
                (let* ((my-command
                        ;; This depends on the assumption that the `:connect'
                        ;; function of `lsp-tramp-connection' will prepend the
                        ;; "stty raw ;" command immediately preceding this echo
                        ;; command.
                        #'(lambda ()
                            (append '("echo" "after-stty-token" ";")
                                    ;; Ensure this is a list of strings.
                                    (lsp-resolve-final-function orig-command))))
                       (result
                        ;; Call `lsp-tramp-connection' with our extended command.
                        (apply func my-command rest))
                       (connect-func (plist-get result :connect)))
                  (add-function :around (var connect-func) #'my-wait-for-stty-to-take-effect)
                  ;; For some reason, this must be done, instead of just
                  ;; returning `result'.
                  (plist-put result :connect connect-func))))
