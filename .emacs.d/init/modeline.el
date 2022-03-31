(defun my-hide-select-minor-modes-from-modeline ()
  (let* ((exclude-minor-modes
          '(
            auto-revert-mode
            cargo-minor-mode
            company-mode
            company-search-mode
            counsel-mode
            eldoc-mode
            global-auto-revert-mode
            ivy-mode
            lsp-lens-mode
            lsp-mode
            magit-wip-mode
            smartparens-mode
            visual-line-mode
            )))
    (setq minor-mode-alist
          (my-alist-remove minor-mode-alist exclude-minor-modes))))

;; Do it after each change of major mode to ensure our selection to exclude is
;; effective, because the dynamic loading of required libraries can add new
;; elements to minor-mode-alist at later different times.
(add-hook 'after-change-major-mode-hook
          #'my-hide-select-minor-modes-from-modeline
          90)
