;; -*- lexical-binding: t; -*-

(eval-when-compile (require 'use-package))


(use-package my-utils :load-path "my/lib"
  :autoload my-alist-remove)


(defcustom my-exclude-minor-modes '(
                                    auto-revert-mode
                                    cargo-minor-mode
                                    company-mode
                                    company-search-mode
                                    counsel-mode
                                    eldoc-mode
                                    global-auto-revert-mode
                                    hs-minor-mode
                                    ivy-mode
                                    lsp-lens-mode
                                    lsp-mode
                                    magit-wip-mode
                                    smartparens-mode
                                    visual-line-mode
                                    which-key-mode
                                    )
  "Minor modes to not show in the modeline."
  :type '(repeat symbol)
  :group 'my)


(defun my-hide-select-minor-modes-from-modeline ()
  (setq minor-mode-alist
        (my-alist-remove minor-mode-alist my-exclude-minor-modes)))


(provide 'my-modeline)
