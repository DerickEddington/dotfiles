;; -*- lexical-binding: t; -*-

(eval-when-compile (require 'use-package))


(use-package gdb-mi
  :autoload gdb-get-buffer)  ;; Needed by my function below.


;; Allow the "*input/output of" window of the "Display Other Windows" function of the Gud menu
;; (i.e. `gdb-many-windows') to not be dedicated so that it can be switched.  E.g. I sometimes
;; like to switch this window to display the "*disassembly of" buffer instead.
(defun my-gdb-allow-io-win-switch ()
  (let* ((io-buf (gdb-get-buffer 'gdb-inferior-io))
         (io-win (when io-buf (get-buffer-window io-buf t))))
    (when io-win
      (set-window-dedicated-p io-win nil))))


(provide 'my-gdb)
