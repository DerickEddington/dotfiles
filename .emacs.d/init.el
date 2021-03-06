(dolist (subinit '(
                   ;; Do not include a .el extension on these, to allow the
                   ;; possibility of loading .elc files instead.
                   "utils"  ;; Should be kept as first
                   "smartparens"
                   "multiple-cursors"
                   "ivy"
                   "magit"
                   "flycheck"
                   "lsp"
                   "gdb"
                   "rust"
                   "modeline"  ;; Should be kept as last
                   ))
  (load (expand-file-name (concat "init/" subinit) user-emacs-directory)))

;; Include a .el extension because custom-file is used in various ways where
;; some probably require it.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Ibuffer (instead of default Buffer Menu)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(require 'ibuffer-project)

;; Load this so it's immediately available (to have `dired-jump') before a
;; `dired' buffer is created.
(require 'dired-x)

(require 'company)
(global-set-key (kbd "TAB") #'company-indent-or-complete-common)

(require 'home-end)
(global-set-key [home] 'home-end-home)
(global-set-key [end]  'home-end-end)

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(global-set-key (kbd "C-c g") 'magit-file-dispatch)

(global-set-key [remap just-one-space] #'cycle-spacing)

(global-set-key (kbd "C-M-<delete>") 'backward-kill-sexp)
(global-set-key (kbd "C-\\") (lambda () (interactive) (insert ?λ)))
(global-set-key (kbd "C-.") (lambda () (interactive) (insert ?▷)))

;; Protect myself from dangerous mistypes
(global-set-key (kbd "C-x C-c") nil)
(global-set-key (kbd "C-x 5 0") nil)

(setq default-major-mode 'text-mode)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; It'd be nice to have this, but Adaptive-Wrap-Prefix mode usually causes too
;; much slow-down for moving around buffers.  Instead, I selectively enable it.
;(add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)

;; Work around bug in message-insert-formatted-citation-line
(setq gnus-extract-address-components 'gnus-extract-address-components)
