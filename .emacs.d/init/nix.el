(require 'nix-mode)

(add-hook
 'nix-mode-hook
 #'(lambda ()
     (setq fill-column 98)
     (whitespace-mode)
     ))
