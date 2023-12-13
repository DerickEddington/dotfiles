;; -*- lexical-binding: t; -*-

(use-package my-modeline :load-path "my/lib"
  ;; Adding this hook is not done via `:hook' because that wouldn't allow controlling the priority
  ;; of this hook in the hook-list, whereas `add-hook' does allow that.
  :autoload my-hide-select-minor-modes-from-modeline
  :init
  ;; Do it after each change of major mode to ensure our selection to exclude is effective,
  ;; because the dynamic loading of required libraries can add new elements to `minor-mode-alist'
  ;; at later different times.  Our hook is added near the end of the hook-list (by giving the
  ;; `90' argument), so that it runs after other hooks which might change `minor-mode-alist', so
  ;; that it applies to whatever the other hooks did.
  (add-hook 'after-change-major-mode-hook #'my-hide-select-minor-modes-from-modeline 90))
