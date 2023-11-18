;; -*- lexical-binding: t; -*-

(when (featurep 'native-compile)
  ;; Prepend this again (after my `./early-init.el' already added this) to ensure
  ;; `my-eln-cache-dir' is at the front of the list so it takes precedence over whatever the
  ;; `site-start.el' might've added to this list.  This is needed, e.g., with Nixpkgs'
  ;; `site-start' which adds subdirectories from `NIX_PROFILES', and this might be needed in other
  ;; platforms for similar reasons.
  (push my-eln-cache-dir native-comp-eln-load-path))

(setq custom-file
      ;; (Include a .el extension because custom-file is used in various ways
      ;; where some probably require it.)
      (locate-user-emacs-file "my/init/custom.el"))

;; Initialize things that must already be, before evaluating `custom-file' and
;; my "subinit" files.
(require 'use-package)  ;; Also makes my `scroll-bar-mode' setting work, for some reason.

;; Load `custom-file' before my "subinit" files, because doing so is faster, for
;; some reason, and because some of the "subinit" files depend on my customized
;; values.
(load (substring custom-file 0 -3))  ;; Try w/o the .el, to allow .elc.

;; ASAP, to avoid seeing it, but must be after above, for some reason.
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(dolist (subinit '(
                   ;; Do not include a .el extension on these, to allow the
                   ;; possibility of loading .elc files instead.
                   "xdg-bds"  ;; Should be kept as first.
                   "my"       ;; Should be kept as second.
                   "tramp"
                   "ibuffer"
                  ;"projectile"  ;; Unused currently.
                   "smartparens"
                   "multiple-cursors"
                   "ivy"
                   "magit"
                   "flycheck"
                   "lsp"
                   "gdb"
                   "rust"
                   "nix"
                   "c"
                   "sh"
                   "modeline"  ;; Should be kept as last
                   ))
  (load (locate-user-emacs-file (concat "my/init/" subinit))))

;; Setting my desired modes here is a little faster than in `custom-file', for
;; some reason.
(column-number-mode)
(use-package company :commands global-company-mode :init (global-company-mode))
(use-package flycheck :commands global-flycheck-mode :init (global-flycheck-mode))
(global-visual-line-mode)
(use-package ivy :commands ivy-mode :init (ivy-mode))
(use-package counsel :commands counsel-mode :init (counsel-mode))
(minibuffer-depth-indicate-mode)
(savehist-mode)
(use-package smartparens
  :commands (show-smartparens-global-mode smartparens-global-strict-mode)
  :init (show-smartparens-global-mode) (smartparens-global-strict-mode))
(size-indication-mode)
(tooltip-mode -1)
(use-package which-key :commands which-key-mode :init (which-key-mode))

(use-package dired :ensure nil
  :bind ("C-x j" . dired-jump)
  :config (use-package dired-x :demand t :ensure nil))

(use-package company
  :bind ("TAB" . company-indent-or-complete-common))

(use-package define-word
  :bind (("M-s d"   . define-word-at-point)
         ("M-s M-d" . define-word)))

(use-package home-end
  :bind (("<home>" . home-end-home)
         ("<end>"  . home-end-end)))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package grep
  ;; Doing this here is a little faster than elsewhere, because this avoids
  ;; loading the `grep' package until needed.
  :config (setopt grep-command "egrep --color -nH -e "))

(use-package open-junk-file
  :bind ("C-x C-j" . open-junk-file))

(use-package adaptive-wrap)
(use-package all-the-icons)
(use-package charmap)
(use-package fd-dired)
(use-package git-modes)
(use-package lua-mode)
(use-package markdown-mode)
(use-package rg)
(use-package toml-mode)
(use-package yaml-mode)


(global-set-key [remap just-one-space] #'cycle-spacing)

(global-set-key (kbd "C-M-<delete>") 'backward-kill-sexp)
(global-set-key (kbd "C-\\") (lambda () (interactive) (insert ?λ)))
(global-set-key (kbd "C-.") (lambda () (interactive) (insert ?▷)))

;; Protect myself from dangerous mistypes
(put 'kill-emacs 'disabled t)
(global-set-key (kbd "C-x 5 0") nil)

;; It'd be nice to have this, but Adaptive-Wrap-Prefix mode usually causes too
;; much slow-down for moving around buffers.  Instead, I selectively enable it.
;(add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)

;; Enable some functions
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)


;; Kill the *Compile-Log* when nothing was byte-compiled, which is the usual case when my `.elc's
;; are already up-to-date.  This is needed because my `./early-init.el' always does
;; `byte-recompile-directory' which causes this buffer to always be created even when nothing
;; happened.  When something was byte-compiled without issue, this buffer won't be killed and
;; won't be shown but remains as a note that byte-compilation happened.  When there was an issue,
;; this buffer will be shown (which is automatically arranged outside my logic).
(let ((comp-log-buf (get-buffer "*Compile-Log*")))
  (when (and comp-log-buf (= 0 (buffer-size comp-log-buf)))
    (kill-buffer comp-log-buf)))

(when (featurep 'native-compile)
  ;; Kill the *Async-native-compile-log* and related buffers which are usually always created due
  ;; to native-compilation activity.  Repeatedly kill them because they sometimes repeatedly are
  ;; recreated, but do not kill them if any one of them is currently being displayed.
  (let ((acomp-log-bufs '("*Async-native-compile-log*" "*Native-compile-Log*" "*Warnings*"))
        (freq-sec 180))
    (run-with-timer freq-sec freq-sec
      (lambda ()
        (let* ((bufs (seq-filter #'identity (seq-map #'get-buffer acomp-log-bufs)))
               (is-displayed (seq-some (lambda (b) (get-buffer-window b t)) bufs)))
          (unless is-displayed
            (seq-do #'kill-buffer bufs)))))))