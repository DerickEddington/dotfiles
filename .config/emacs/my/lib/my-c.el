;; -*- lexical-binding: t; -*-

(defun my-c-mode-hook ()
  (face-remap-set-base 'button nil)
  (font-lock-add-keywords nil
    (append
     '(("\\(?:\\s-\\|\\s.\\|\\s(\\|\\s)\\|^\\)\\(\\(?:\\(?:[[:digit:]]*[.][[:digit:]]+\\|[[:digit:]]+[.]\\)\\(?:[eE][+-]?[[:digit:]]+\\)?\\|[[:digit:]]+[eE][+-]?[[:digit:]]+\\|0[xX]\\(?:[[:xdigit:]]*[.][[:xdigit:]]+\\|[[:xdigit:]]+[.]?\\)[pP][+-]?[[:digit:]]+\\)[flFL]?\\)\\(?:\\s-\\|\\s.\\|\\s(\\|\\s)\\|$\\)"
        1 'font-lock-constant-face)
       ("\\_<\\(?:[1-9][[:digit:]]*\\|[0-7]+\\|\\(?:0[xX]\\)[[:xdigit:]]+\\)\\(?:[uU]\\(?:[lL]\\|ll\\|LL\\)?\\|\\(?:[lL]\\|ll\\|LL\\)[uU]?\\)?\\_>"
        . 'font-lock-constant-face))

     (unless (version< emacs-version "29")
       '(("[?]\\{2\\}[=/()'!<>-]" 0 'font-lock-escape-face prepend)  ;; The trigraph sequences.
         ("[][()=.*&~^|<>?:#/%+-]" . 'font-lock-operator-face)
         ("[!]" . 'my-negated-operator-face)
         ("[{}]" . 'font-lock-bracket-face)
         ("[,;]" . 'font-lock-delimiter-face)
         ("[\\]\\(?:\\s-*$\\|[abfnrtv'\"?\\]\\|[0-7]\\{1,3\\}\\|x[[:xdigit:]]+\\|u[[:xdigit:]]\\{4\\}\\|U[[:xdigit:]]\\{8\\}\\)"
          0 'font-lock-escape-face prepend))))))


(provide 'my-c)
