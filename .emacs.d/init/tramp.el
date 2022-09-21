(require 'custom)
(require 'tramp)

;; Per Tramp Info doc section 7 Frequently Asked Questions:
(custom-set-variables
 '(debug-ignored-errors (cons 'remote-file-error debug-ignored-errors)))
