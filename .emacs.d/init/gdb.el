;; Allow the "*input/output of" window of the "Display Other Windows" function
;; of the Gud menu (i.e. `gdb-many-windows') to not be dedicated so that it can
;; be switched.  E.g. I sometimes like to switch this window to display the
;; "*disassembly of" buffer instead.
(defun my-gdb-allow-io-win-switch ()
  (let* ((io-buf (gdb-get-buffer 'gdb-inferior-io))
         (io-win (when io-buf (get-buffer-window io-buf t))))
    (when io-win
      (set-window-dedicated-p io-win nil))))

;; This advice is needed because `gdb-many-windows-hook' is insufficient because
;; `gdb-setup-windows' can be run again after that (e.g. via the "Restore Window
;; Layout" function of the Gud menu (i.e. `gdb-restore-windows')) and it causes
;; the window to be reset to dedicated again, which would prevent being able to
;; switch it, so we must undo that each time.  This is also run when the
;; `gdb-many-windows' minor mode is toggled on and so adding it to the hook is
;; unnecessary.
(advice-add 'gdb-setup-windows :after #'my-gdb-allow-io-win-switch)

;; Using the hook would be like:
; (add-hook 'gdb-many-windows-hook #'my-gdb-allow-io-win-switch)
