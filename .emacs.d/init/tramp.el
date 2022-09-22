(require 'custom)
(require 'tramp)

(custom-set-variables
 '(tramp-remote-path (cons 'tramp-own-remote-path tramp-remote-path))
 ;; Per Tramp Info doc section 7 Frequently Asked Questions:
 '(debug-ignored-errors (cons 'remote-file-error debug-ignored-errors)))
