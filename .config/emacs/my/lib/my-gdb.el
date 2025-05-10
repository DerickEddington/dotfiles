;; -*- lexical-binding: t; -*-

(eval-when-compile (require 'use-package))


(use-package gdb-mi
  ;; Needed by my functions below.
  :autoload (gdb-get-buffer gdb-set-window-buffer gdb-get-buffer-create))


(defun my-gdb-set-window-dedicated (buffer-type flag)
  (when-let ((buf (gdb-get-buffer buffer-type))
             (win (get-buffer-window buf t)))
    (set-window-dedicated-p win flag)))


;; Disallow the "*gud*" window from being switched.
;; Must be done this way instead, because `(dedicated . t)' wouldn't work for the
;; `(gdb-buffer-type . command)' in the `gdb-default-window-configuration-file'.
(defun my-gdb-disallow-command-win-switch ()
  (my-gdb-set-window-dedicated 'gdbmi t))


;; Hackaround `gdb-save-window-configuration' and `gdb-load-window-configuration' not creating the
;; `'gdb-locals-buffer' (unlike the default layout of `gdb-setup-windows').
(defun my-gdb-setup-locals-buffer-window ()
  (when-let ((buf (gdb-get-buffer 'gdb-locals-values-buffer))
             (win (get-buffer-window buf t)))
    (gdb-set-window-buffer (gdb-get-buffer-create 'gdb-locals-buffer) t win)))


;; `gdb-setup-windows' hardcodes creating a Buffer Menu buffer, but I don't want that.
(defun my-gdb-kill-buffer-menu ()
  (when-let ((unwanted (get-buffer "*Buffer List*")))
    (kill-buffer unwanted)))


(defun my-gdb--setup-windows--customize ()
  (my-gdb-disallow-command-win-switch)
  (when gdb-default-window-configuration-file
    (my-gdb-setup-locals-buffer-window))
  (my-gdb-kill-buffer-menu))


(defun my-gdb--gud-find-file--read-only (buf)
  (with-current-buffer buf (read-only-mode 1))
  buf)


(provide 'my-gdb)
