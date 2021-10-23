(dolist (subinit '(
                   "smartparens"
                   "multiple-cursors"
                   "ivy"
                   "magit"
                   "flycheck"
                   "lsp"
                   "rust"
                   ))
  (load (format "%s/init/%s" user-emacs-directory subinit)))

(setq custom-file (format "%s/custom.el" user-emacs-directory))
(load custom-file)

;; Ibuffer (instead of default Buffer Menu)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(require 'ibuffer-project)

(require 'home-end)
(global-set-key [home] 'home-end-home)
(global-set-key [end]  'home-end-end)

(global-set-key (kbd "C-c g") 'magit-file-dispatch)

(global-set-key (kbd "C-M-<delete>") 'backward-kill-sexp)
(global-set-key (kbd "C-\\") (lambda () (interactive) (insert ?λ)))
(global-set-key (kbd "C-.") (lambda () (interactive) (insert ?▷)))

;; Protect myself from dangerous mistypes
(global-set-key (kbd "C-x C-c") nil)
(global-set-key (kbd "C-x 5 0") nil)

(setq default-major-mode 'text-mode)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Work around bug in message-insert-formatted-citation-line
(setq gnus-extract-address-components 'gnus-extract-address-components)
