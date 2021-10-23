(require 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C->") 'mc/mark-more-like-this-extended)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-M->") 'mc/skip-to-next-like-this)
(global-set-key (kbd "C-M-<") 'mc/skip-to-previous-like-this)
(global-set-key (kbd "C-c C-M->") 'mc/unmark-next-like-this)
(global-set-key (kbd "C-c C-M-<") 'mc/unmark-previous-like-this)
(global-set-key (kbd "C-c C-SPC") 'mc/mark-pop)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

;; make <return> insert a newline; multiple-cursors-mode can still be disabled
;; with C-g
(define-key mc/keymap (kbd "<return>") nil)
