(use-package multiple-cursors
  :bind
  (("C-S-c C-S-c"   . mc/edit-lines)
   ("C->"           . mc/mark-next-like-this)
   ("C-<"           . mc/mark-previous-like-this)
   ("C-c C->"       . mc/mark-more-like-this-extended)
   ("C-c C-<"       . mc/mark-all-like-this)
   ("C-M->"         . mc/skip-to-next-like-this)
   ("C-M-<"         . mc/skip-to-previous-like-this)
   ("C-c C-M->"     . mc/unmark-next-like-this)
   ("C-c C-M-<"     . mc/unmark-previous-like-this)
   ("C-c C-SPC"     . mc/mark-pop)
   ("C-S-<mouse-1>" . mc/add-cursor-on-click)

   ;; Make <return> insert a newline.  multiple-cursors-mode can still be disabled with C-g.
   :map mc/keymap
   ("<return>"      . nil)))
