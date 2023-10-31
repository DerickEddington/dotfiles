;; -*- lexical-binding: t; -*-

(use-package my-modeline :load-path "my/lib"
  :autoload my-hide-select-minor-modes-from-modeline)

;; Do it after each change of major mode to ensure our selection to exclude is effective, because
;; the dynamic loading of required libraries can add new elements to minor-mode-alist at later
;; different times.
(add-hook 'after-change-major-mode-hook #'my-hide-select-minor-modes-from-modeline 90)
