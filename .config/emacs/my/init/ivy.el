;; -*- lexical-binding: t; -*-

(use-package my-ivy :load-path "my/lib"
  :autoload my-ivy-sort-matches-fall-thru)


(use-package ivy
  :after counsel  ;; Needed for below to not interfere with what `counsel' configures for `ivy'.
  :functions ivy-string<
  :config
  ;; Set here, after loading `ivy', because these need the variables from that.
  (setopt
   ivy-sort-functions-alist (append `((counsel-M-x . ,#'ivy-string<)
                                      (counsel-describe-function . ,#'ivy-string<)
                                      (counsel-describe-variable . ,#'ivy-string<)
                                      (customize-face     . ,#'ivy-string<)
                                      (customize-group    . ,#'ivy-string<)
                                      (customize-icon     . ,#'ivy-string<)
                                      (customize-variable . ,#'ivy-string<))
                                    ivy-sort-functions-alist)
   ivy-sort-matches-functions-alist (mapcar (lambda (pair)
                                              (if (eq t (car pair))
                                                  `(t . ,#'my-ivy-sort-matches-fall-thru)
                                                pair))
                                            ivy-sort-matches-functions-alist)))

(use-package ivy-hydra)
(use-package counsel)
(use-package counsel-fd)
(use-package counsel-tramp)
