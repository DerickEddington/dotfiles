(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(adaptive-wrap-extra-indent 2)
 '(after-save-hook '(magit-after-save-refresh-status))
 '(auth-source-save-behavior nil)
 '(auto-revert-buffer-list-filter 'magit-auto-revert-repository-buffer-p)
 '(auto-revert-check-vc-info nil)
 '(auto-revert-mode-text "")
 '(auto-revert-remote-files t)
 '(blink-cursor-blinks -1)
 '(blink-cursor-delay 0.7)
 '(blink-cursor-interval 0.5)
 '(blink-matching-paren-dont-ignore-comments t)
 '(cargo-process--command-fmt "+nightly fmt -- --unstable-features")
 '(cargo-process--enable-rust-backtrace t)
 '(cargo-process--open-file-after-new t)
 '(color-theme-history-max-length 3)
 '(column-number-indicator-zero-based nil)
 '(column-number-mode t)
 '(comint-buffer-maximum-size 200000)
 '(comint-input-ignoredups t)
 '(comint-move-point-for-output 'this)
 '(comint-scroll-show-maximum-output nil)
 '(comment-auto-fill-only-comments t)
 '(comment-empty-lines t)
 '(company-abort-manual-when-too-short t)
 '(company-tooltip-align-annotations t)
 '(company-tooltip-flip-when-above t)
 '(company-tooltip-limit 15)
 '(compilation-message-face 'bold)
 '(confirm-kill-emacs 'y-or-n-p)
 '(counsel-find-file-at-point t)
 '(counsel-mode t)
 '(ctl-arrow nil)
 '(debug-ignored-errors (cons 'remote-file-error debug-ignored-errors))
 '(default-frame-alist
    '((menu-bar-lines . 1)
      (width . 93)
      (height . 54)
      (tool-bar-lines . 0)))
 '(diff-default-read-only t)
 '(dired-create-destination-dirs 'ask)
 '(dired-listing-switches "-l -A -h -F")
 '(dired-vc-rename-file t)
 '(eldoc-echo-area-use-multiline-p t)
 '(eldoc-idle-delay 1)
 '(eldoc-minor-mode-string "")
 '(eldoc-print-after-edit t)
 '(emacs-lisp-mode-hook '(eldoc-mode imenu-add-menubar-index hs-minor-mode))
 '(enable-local-eval nil)
 '(enable-local-variables :safe)
 '(enable-recursive-minibuffers t)
 '(face-font-family-alternatives nil)
 '(face-font-registry-alternatives nil)
 '(fill-column 80)
 '(find-file-visit-truename nil)
 '(flycheck-check-syntax-automatically '(save idle-buffer-switch mode-enabled))
 '(flycheck-display-errors-delay 0.5)
 '(flycheck-global-modes '(rust-mode))
 '(flycheck-idle-change-delay 5)
 '(gc-cons-threshold 100000000)
 '(gdb-display-io-nopopup t)
 '(gdb-switch-when-another-stopped nil)
 '(git-commit-style-convention-checks '(non-empty-second-line overlong-summary-line))
 '(global-company-mode t)
 '(global-flycheck-mode t)
 '(global-visual-line-mode t)
 '(grep-command "egrep --color -nH -e ")
 '(gud-tooltip-mode t)
 '(hs-isearch-open nil)
 '(ibuffer-compressed-file-name-regexp
   "\\.\\(arj\\|bgz\\|bz2\\|gz\\|lzh\\|taz\\|tgz\\|xz\\|zip\\|z\\|zst\\)$")
 '(ibuffer-default-sorting-mode 'alphabetic)
 '(ibuffer-projectile-group-name-function 'my-ibuffer-projectile-group-name)
 '(ibuffer-projectile-prefix "Proj: ")
 '(ibuffer-projectile-skip-if-remote nil)
 '(ibuffer-show-empty-filter-groups nil)
 '(ibuffer-truncate-lines nil)
 '(indent-tabs-mode nil)
 '(indicate-buffer-boundaries 'left)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(initial-major-mode 'text-mode)
 '(initial-scratch-message nil)
 '(ivy-count-format "(%d/%d) ")
 '(ivy-extra-directories nil)
 '(ivy-mode t)
 '(ivy-use-selectable-prompt t)
 '(lsp-before-save-edits nil)
 '(lsp-clients-clangd-args
   '("--header-insertion-decorators=0" "--header-insertion=never"))
 '(lsp-diagnostic-clean-after-change t)
 '(lsp-diagnostics-attributes
   '((unnecessary :foreground "gray30")
     (deprecated :strike-through t)))
 '(lsp-diagnostics-flycheck-default-level 'info)
 '(lsp-document-sync-method nil)
 '(lsp-eldoc-enable-hover nil)
 '(lsp-eldoc-render-all t)
 '(lsp-enable-dap-auto-configure nil)
 '(lsp-enable-snippet nil)
 '(lsp-file-watch-threshold 2000)
 '(lsp-headerline-breadcrumb-icons-enable nil)
 '(lsp-headerline-breadcrumb-segments '(project path-up-to-project file symbols))
 '(lsp-keep-workspace-alive nil)
 '(lsp-lens-enable t)
 '(lsp-modeline-code-actions-segments '(count))
 '(lsp-response-timeout 20)
 '(lsp-rust-analyzer-cargo-watch-command "clippy")
 '(lsp-rust-analyzer-diagnostics-enable t)
 '(lsp-rust-analyzer-diagnostics-enable-experimental t)
 '(lsp-rust-analyzer-display-chaining-hints t)
 '(lsp-rust-analyzer-display-parameter-hints t)
 '(lsp-rust-analyzer-experimental-proc-attr-macros t)
 '(lsp-rust-analyzer-max-inlay-hint-length 40)
 '(lsp-rust-analyzer-proc-macro-enable t)
 '(lsp-rust-analyzer-rustfmt-extra-args ["+nightly" "--unstable-features"])
 '(lsp-rust-analyzer-server-display-inlay-hints t)
 '(lsp-rust-build-on-save t)
 '(lsp-ui-doc-delay 2)
 '(lsp-ui-doc-include-signature t)
 '(lsp-ui-doc-max-height 17)
 '(lsp-ui-doc-max-width 100)
 '(lsp-ui-doc-show-with-cursor nil)
 '(lsp-ui-doc-show-with-mouse t)
 '(lsp-ui-doc-use-webkit nil)
 '(lsp-ui-imenu-colors '("#161CCC" "#006E10"))
 '(lsp-ui-peek-always-show t)
 '(lsp-ui-sideline-actions-kind-regex "quickfix.*")
 '(lsp-ui-sideline-diagnostic-max-lines 2)
 '(lsp-ui-sideline-ignore-duplicate t)
 '(lsp-ui-sideline-show-code-actions t)
 '(lsp-ui-sideline-show-diagnostics nil)
 '(lsp-ui-sideline-update-mode 'line)
 '(magit-blame-styles
   '((headings
      (heading-format . "%C %.7H %A %a %s
"))
     (margin
      (margin-format "%s%f" "%C %a%f" "%H%f")
      (margin-width . 68)
      (margin-face . magit-blame-margin)
      (margin-body-face . magit-blame-dimmed))
     (lines
      (show-lines . t)
      (show-message . t))))
 '(magit-blame-time-format "%F_%H:%M")
 '(magit-bury-buffer-function 'magit-restore-window-configuration)
 '(magit-cherry-margin '(t age-abbreviated magit-log-margin-width t 16))
 '(magit-diff-highlight-keywords nil)
 '(magit-diff-paint-whitespace 'uncommitted)
 '(magit-diff-refine-hunk t)
 '(magit-log-highlight-keywords nil)
 '(magit-log-margin '(t age-abbreviated magit-log-margin-width t 16))
 '(magit-log-select-margin '(t age-abbreviated magit-log-margin-width t 16))
 '(magit-no-confirm nil)
 '(magit-process-popup-time 5)
 '(magit-published-branches
   '("origin/master" "origin/main" "origin/default" "origin/primary"))
 '(magit-reflog-margin '(t age-abbreviated magit-log-margin-width nil 16))
 '(magit-refs-margin '(t age-abbreviated magit-log-margin-width nil 16))
 '(magit-refs-margin-for-tags t)
 '(magit-section-initial-visibility-alist '((stashes . hide) (untracked . hide)))
 '(magit-slow-confirm '(discard drop-stashes set-and-push))
 '(magit-stashes-margin '(t age-abbreviated magit-log-margin-width nil 16))
 '(magit-status-goto-file-position t)
 '(magit-status-margin '(t age-abbreviated magit-log-margin-width t 16))
 '(magit-view-git-manual-method 'man)
 '(magit-wip-mode t)
 '(make-backup-files nil)
 '(markdown-code-lang-modes
   '(("ocaml" . tuareg-mode)
     ("elisp" . emacs-lisp-mode)
     ("ditaa" . artist-mode)
     ("asymptote" . asy-mode)
     ("dot" . fundamental-mode)
     ("sqlite" . sql-mode)
     ("calc" . fundamental-mode)
     ("C" . c-mode)
     ("cpp" . c++-mode)
     ("C++" . c++-mode)
     ("screen" . shell-script-mode)
     ("shell" . sh-mode)
     ("bash" . sh-mode)
     ("rust" . rust-mode)))
 '(markdown-fontify-code-blocks-natively t)
 '(max-mini-window-height 0.5)
 '(message-citation-line-format
   "



------------------------------------------------------------
%N (%Y-%m-%d %H:%M %Z) wrote:
")
 '(message-citation-line-function 'message-insert-formatted-citation-line)
 '(message-confirm-send t)
 '(message-kill-buffer-on-exit t)
 '(message-log-max 1000000)
 '(message-send-mail-function 'smtpmail-send-it)
 '(minibuffer-depth-indicate-mode t)
 '(mode-line-format
   '("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote " " mode-line-position " "
     (:eval
      (propertized-buffer-identification "%b"))
     " " mode-line-modes mode-line-misc-info mode-line-end-spaces))
 '(mode-line-percent-position '(6 "%q"))
 '(next-error-recenter '(4))
 '(package-archives nil)
 '(parse-sexp-ignore-comments t)
 '(projectile-enable-cmake-presets t)
 '(projectile-mode-line-function 'my-projectile-mode-line)
 '(projectile-mode-line-prefix "")
 '(projectile-per-project-compilation-buffer t)
 '(python-shell-interpreter "python3")
 '(racer-complete-insert-argument-placeholders nil)
 '(racer-eldoc-timeout 5)
 '(remote-file-name-inhibit-locks t)
 '(require-final-newline 'ask)
 '(rg-command-line-flags '("--search-zip" "--follow"))
 '(rust-format-on-save t)
 '(rust-indent-method-chain t)
 '(rust-indent-return-type-to-arguments nil)
 '(rust-indent-where-clause nil)
 '(rust-rustfmt-switches '("+nightly" "--unstable-features"))
 '(savehist-mode t)
 '(scroll-bar-mode nil)
 '(shell-command-prompt-show-cwd t)
 '(show-paren-delay 0.25)
 '(show-paren-style 'mixed)
 '(show-paren-when-point-inside-paren t)
 '(show-smartparens-global-mode t)
 '(show-trailing-whitespace t)
 '(size-indication-mode t)
 '(smartparens-global-strict-mode t)
 '(smtpmail-stream-type 'ssl)
 '(sp-base-key-bindings 'sp)
 '(sp-highlight-pair-overlay nil)
 '(sp-override-key-bindings
   '(("<C-right>")
     ("<C-left>")
     ("<M-right>" . sp-forward-slurp-sexp)
     ("<M-left>" . sp-forward-barf-sexp)
     ("C-M-a")
     ("C-M-e")
     ("C-M-t" . sp-transpose-sexp)))
 '(sp-show-pair-delay 0.25)
 '(swiper-action-recenter t)
 '(swiper-goto-start-of-match t)
 '(swiper-include-line-number-in-search t)
 '(tab-always-indent 'complete)
 '(tab-first-completion 'word)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(tramp-default-method "ssh")
 '(tramp-histfile-override t)
 '(tramp-remote-path (cons 'tramp-own-remote-path tramp-remote-path))
 '(tramp-use-ssh-controlmaster-options nil)
 '(vc-handled-backends nil)
 '(vc-ignore-dir-regexp "")
 '(view-read-only t)
 '(visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
 '(wdired-allow-to-change-permissions t)
 '(wdired-use-dired-vertical-movement 'sometimes)
 '(what-cursor-show-names t)
 '(whitespace-line-column nil)
 '(whitespace-style '(face tabs lines-tail))
 '(woman-fill-frame t)
 '(x-stretch-cursor t)
 '(xref-marker-ring-length 128))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "#BDB8A0" :foreground "black" :height 145 :family "Ubuntu Mono"))))
 '(bold ((t (:weight bold))))
 '(bold-italic ((t (:inherit bold :slant italic))))
 '(breakpoint-disabled ((t (:foreground "grey35"))))
 '(cargo-process--errno-face ((t (:inherit warning :underline t))))
 '(cargo-process--error-face ((t (:inherit error))))
 '(cargo-process--ok-face ((t (:inherit font-lock-string-face))))
 '(cargo-process--pointer-face ((t (:inherit link))))
 '(cargo-process--standard-face ((t (:inherit font-lock-keyword-face))))
 '(cargo-process--warning-face ((t (:inherit warning))))
 '(comint-highlight-input ((t nil)))
 '(comint-highlight-prompt ((((min-colors 88) (background dark)) (:background "black" :foreground "red"))))
 '(company-echo-common ((t (:background "peru"))))
 '(company-tooltip-common ((t (:foreground "saddle brown"))))
 '(compilation-mode-line-fail ((t (:inherit compilation-error :weight bold))))
 '(cursor ((t (:inverse-video t))))
 '(custom-button-pressed-unraised ((t (:inherit custom-button-unraised :foreground "dark magenta"))))
 '(custom-face-tag ((t (:foreground "medium blue" :weight bold))))
 '(custom-state ((t (:inherit success))))
 '(diff-added ((t (:foreground "#006E10"))))
 '(diff-file-header ((t (:background "grey80" :weight bold))))
 '(diff-function ((t (:inherit diff-hunk-header))))
 '(diff-header ((t (:background "grey70"))))
 '(diff-hunk-header ((t (:inherit diff-header :foreground "grey25"))))
 '(diff-refine-changed ((((class color) (min-colors 88) (background dark)) (:background "grey60" :foreground "black"))))
 '(diff-removed ((t (:foreground "#820000"))))
 '(dired-directory ((t (:inherit font-lock-function-name-face :underline nil))))
 '(dired-header ((t (:foreground "DarkGoldenrod4" :underline t :weight bold :height 1.1))))
 '(dired-mark ((t (:inherit dired-marked))))
 '(dired-perm-write ((t (:inherit warning :weight normal))))
 '(dired-set-id ((t (:inherit dired-perm-write))))
 '(dired-special ((t (:inherit font-lock-builtin-face))))
 '(dired-symlink ((t (:inherit font-lock-string-face))))
 '(error ((t (:background "red2" :foreground "white" :weight bold))))
 '(escape-glyph ((t (:inherit warning))))
 '(fixed-pitch ((t nil)))
 '(fixed-pitch-serif ((t nil)))
 '(flycheck-error ((t (:background "#FF7777" :box (:line-width 2 :color "red2")))))
 '(flycheck-error-list-checker-name ((t (:inherit font-lock-variable-name-face))))
 '(flycheck-error-list-error ((t (:inherit flycheck-error))))
 '(flycheck-error-list-filename ((t (:inherit font-lock-function-name-face))))
 '(flycheck-info ((t (:inherit success))))
 '(flycheck-warning ((t (:underline (:color "yellow2" :style wave)))))
 '(font-lock-builtin-face ((t (:inherit font-lock-keyword-face))))
 '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
 '(font-lock-comment-face ((t (:inherit shadow :slant italic))))
 '(font-lock-constant-face ((t (:foreground "#820000"))))
 '(font-lock-doc-face ((t (:inherit font-lock-comment-face :underline t))))
 '(font-lock-function-name-face ((t (:inherit font-lock-variable-name-face :underline t))))
 '(font-lock-keyword-face ((t (:foreground "#74007A"))))
 '(font-lock-preprocessor-face ((t (:inherit font-lock-builtin-face))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground "gold"))))
 '(font-lock-regexp-grouping-construct ((t (:foreground "gold"))))
 '(font-lock-string-face ((t (:foreground "#006E10"))))
 '(font-lock-type-face ((t (:inherit font-lock-constant-face))))
 '(font-lock-variable-name-face ((t (:inherit link :underline nil))))
 '(font-lock-warning-face ((t (:inherit warning :underline t))))
 '(fringe ((t nil)))
 '(header-line ((t (:background "gray70" :foreground "black" :box (:line-width -1 :style released-button)))))
 '(header-line-highlight ((t (:background "#CFC9AC"))))
 '(help-key-binding ((t (:foreground "DarkBlue"))))
 '(highlight ((t (:background "yellow3"))))
 '(info-menu-star ((t nil)))
 '(isearch ((t (:inherit highlight))))
 '(italic ((t (:slant italic))))
 '(ivy-current-match ((t (:inherit region :underline t))))
 '(ivy-minibuffer-match-face-1 ((t nil)))
 '(ivy-minibuffer-match-face-2 ((t (:inherit header-line-highlight :weight bold))))
 '(ivy-minibuffer-match-face-3 ((t (:inherit ivy-minibuffer-match-face-2))))
 '(ivy-minibuffer-match-face-4 ((t (:inherit ivy-minibuffer-match-face-2))))
 '(lazy-highlight ((t (:background "yellow4" :foreground "black"))))
 '(link ((t (:foreground "#161CCC" :underline t))))
 '(link-visited ((t (:inherit link :foreground "#74007A"))))
 '(lsp-face-highlight-read ((t (:inherit lsp-face-highlight-textual))))
 '(lsp-face-highlight-write ((t (:inherit lsp-face-highlight-textual))))
 '(lsp-headerline-breadcrumb-path-error-face ((t (:inherit lsp-headerline-breadcrumb-path-face :underline (:color "Red2" :style wave)))))
 '(lsp-headerline-breadcrumb-path-warning-face ((t (:inherit lsp-headerline-breadcrumb-path-face :underline (:color "yellow2" :style wave)))))
 '(lsp-headerline-breadcrumb-symbols-error-face ((t (:inherit lsp-headerline-breadcrumb-symbols-face :underline (:color "Red2" :style wave)))))
 '(lsp-headerline-breadcrumb-symbols-face ((t (:foreground "grey21" :weight bold))))
 '(lsp-headerline-breadcrumb-symbols-warning-face ((t (:inherit lsp-headerline-breadcrumb-symbols-face :underline (:color "yellow2" :style wave)))))
 '(lsp-lsp-flycheck-info-unnecessary-face ((t (:inherit font-lock-comment-face :strike-through t))) t)
 '(lsp-lsp-flycheck-warning-unnecessary-face ((t (:underline (:color "yellow2" :style wave)))) t)
 '(lsp-modeline-code-actions-face ((t (:inherit lsp-ui-sideline-code-action))))
 '(lsp-ui-sideline-code-action ((t (:foreground "DarkOrange3"))))
 '(lsp-ui-sideline-current-symbol ((t (:inherit highlight))))
 '(lsp-ui-sideline-global ((t (:box (:line-width 1 :style pressed-button) :slant italic :weight normal :height 0.8))))
 '(lsp-ui-sideline-symbol ((t (:foreground "grey30" :box (:line-width -1 :color "grey30")))))
 '(magit-blame-dimmed ((t (:inherit magit-dimmed :background "#BDB8A0" :underline nil :slant normal :weight normal))))
 '(magit-blame-hash ((t (:inherit magit-hash))) t)
 '(magit-blame-heading ((t (:inherit magit-blame-highlight :extend t :box (:line-width 7 :color "gray75") :height 0.9))))
 '(magit-blame-highlight ((t (:extend t :background "gray75" :foreground "black" :underline nil :slant normal :weight normal))))
 '(magit-blame-margin ((t (:inherit magit-blame-highlight))))
 '(magit-blame-name ((t (:inherit magit-log-author))) t)
 '(magit-branch-current ((t (:inherit magit-branch-local :box 2))))
 '(magit-branch-local ((t (:inherit font-lock-string-face :weight bold))))
 '(magit-branch-remote ((t (:inherit font-lock-constant-face :weight bold))))
 '(magit-branch-remote-head ((t (:inherit magit-branch-remote :box 2))))
 '(magit-branch-upstream ((t (:inherit magit-branch-remote :slant italic))))
 '(magit-diff-added ((t (:inherit diff-added))))
 '(magit-diff-added-highlight ((t (:inherit (magit-diff-added magit-section-highlight)))))
 '(magit-diff-context ((t nil)))
 '(magit-diff-context-highlight ((t (:inherit (magit-diff-context magit-section-highlight)))))
 '(magit-diff-file-heading ((t (:inherit diff-file-header :height 1.1))))
 '(magit-diff-hunk-heading ((t (:inherit diff-hunk-header))))
 '(magit-diff-hunk-heading-highlight ((t (:inherit magit-diff-hunk-heading))))
 '(magit-diff-removed ((t (:inherit diff-removed))))
 '(magit-diff-removed-highlight ((t (:inherit (magit-diff-removed magit-section-highlight)))))
 '(magit-diffstat-added ((t (:inherit diff-indicator-added))))
 '(magit-diffstat-removed ((t (:inherit diff-indicator-removed))))
 '(magit-dimmed ((t (:inherit shadow))))
 '(magit-hash ((t (:inherit font-lock-comment-face))))
 '(magit-keyword ((t (:inherit font-lock-keyword-face))))
 '(magit-refname ((t (:inherit font-lock-variable-name-face))))
 '(magit-section-heading ((t (:extend t :foreground "DarkGoldenrod4" :underline t :weight bold :height 1.1))))
 '(magit-section-highlight ((t (:inherit header-line-highlight :extend t))))
 '(magit-signature-good ((t (:inherit success :weight normal))))
 '(magit-signature-revoked ((t (:inherit error :weight normal))))
 '(magit-signature-untrusted ((t (:inherit warning :weight normal))))
 '(magit-tag ((t (:inherit font-lock-builtin-face))))
 '(match ((t (:background "yellow2"))))
 '(mc/cursor-face ((t (:inherit cursor))))
 '(minibuffer-prompt ((t (:inherit font-lock-keyword-face))))
 '(mode-line ((t (:background "gray79" :foreground "black" :box (:line-width -1 :style released-button)))))
 '(mode-line-inactive ((t (:background "#ABA690" :box (:line-width -1 :style released-button)))))
 '(nxml-attribute-local-name ((t (:inherit font-lock-variable-name-face))))
 '(region ((t (:background "#E6DFBF"))))
 '(rust-builtin-formatting-macro ((t (:inherit font-lock-preprocessor-face))))
 '(rust-question-mark ((t (:inherit font-lock-builtin-face :underline t :weight bold))))
 '(secondary-selection ((t (:background "yellow2"))))
 '(sh-heredoc ((t (:inherit font-lock-string-face))))
 '(sh-quoted-exec ((t (:inherit font-lock-constant-face))))
 '(shadow ((t (:foreground "grey40"))))
 '(show-paren-match ((t (:background "gold3"))))
 '(show-paren-mismatch ((t (:background "white" :foreground "purple"))))
 '(speedbar-file-face ((((class color) (background dark)) (:foreground "white smoke"))))
 '(success ((t (:foreground "#006E10" :weight bold))))
 '(swiper-line-face ((t nil)))
 '(tool-bar ((t (:background "black" :foreground "#92b6f4" :box (:line-width 1 :style released-button)))))
 '(tooltip ((((class color)) (:inherit variable-pitch :background "lightyellow" :foreground "black" :height 80))))
 '(trailing-whitespace ((t (:underline (:color "red" :style wave)))))
 '(underline ((t (:underline t))))
 '(warning ((t (:foreground "yellow2" :weight bold))))
 '(widget-field ((t (:background "gray75"))))
 '(widget-single-line-field ((t (:inherit widget-field :background "gray85"))))
 '(woman-bold ((((background dark)) (:foreground "red"))))
 '(woman-italic ((((background dark)) (:foreground "green2"))))
 '(woman-unknown ((t (:inherit font-lock-string-face)))))
