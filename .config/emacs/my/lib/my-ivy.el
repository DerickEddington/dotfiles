;; -*- lexical-binding: t; -*-

(use-package ivy
  :autoload ivy-string<)


(defun my-ivy-sort-matches-fall-thru (name cands)
  (if (or (string= "" name) (string= "^" name))  ;; Nothing given to match yet.
      cands  ;; Keep the preexisting order.  This preserves having directories before files, e.g.
    (sort (copy-sequence cands) #'ivy-string<)))  ;; Something was given to match, so do my way.


(provide 'my-ivy)
