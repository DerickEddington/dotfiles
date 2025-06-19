;; -*- lexical-binding: t; -*-

(eval-when-compile (require 'use-package))

;; Needed by my below.
;;
(use-package smerge-mode :demand t :ensure nil
  :defines (smerge-font-lock-keywords)
  :autoload (smerge-find-conflict))

;; Needed by my customization of `smerge-markers' face that inherits `magit-blame-heading'.
;;
(use-package magit-blame :demand t :ensure nil)


(defface my-smerge-markers-upper
  `((t (:foreground ,(face-attribute 'smerge-upper :background nil t)
      ; :weight bold
        :inherit smerge-markers)))
  "Use background of `smerge-upper' face as foreground for upper marker."
  :group 'my)

(defface my-smerge-markers-base
  `((t (:foreground ,(face-attribute 'smerge-base :background nil t)
      ; :weight bold
        :inherit smerge-markers)))
  "Use background of `smerge-base' face as foreground for base marker."
  :group 'my)

(defface my-smerge-markers-lower
  `((t (:foreground ,(face-attribute 'smerge-lower :background nil t)
      ; :weight bold
        :inherit smerge-markers)))
  "Use background of `smerge-lower' face as foreground for lower marker."
  :group 'my)

(defconst my-expected-default-smerge-font-lock-keywords
  '((smerge-find-conflict
     (1 smerge-upper-face prepend t)
     (2 smerge-base-face prepend t)
     (3 smerge-lower-face prepend t)
     (0 smerge-markers-face keep)
     (4 nil t t)
     (5 nil t t))))

(defconst my-smerge-font-lock-keywords
  '((my-smerge-find-conflict
     (1 'my-smerge-markers-upper t)
     (2 smerge-upper-face prepend t)
     (3 'my-smerge-markers-base t)
     (4 smerge-base-face prepend t)
     (5 'my-smerge-markers-lower t)
     (6 smerge-lower-face prepend t)
     (7 'my-smerge-markers-lower t)
     (8 nil t t)
     (9 nil t t))))

(defun my-smerge-mode-hook ()
  (when font-lock-mode
    (if smerge-mode
        (if (equal smerge-font-lock-keywords my-expected-default-smerge-font-lock-keywords)
            (progn
              ;; Remove SMerge's default, so we can replace it.
              (font-lock-remove-keywords nil smerge-font-lock-keywords)
              ;; Replace how the SMerge markers are matched for the fontification it does.
              (font-lock-add-keywords nil my-smerge-font-lock-keywords 'append))
          (warn "Unexpected `smerge-font-lock-keywords'. Need to update my hack."))
      (font-lock-remove-keywords nil my-smerge-font-lock-keywords))))

(defun my-smerge-find-conflict (limit)
  (when-let* ((found (smerge-find-conflict limit)))
    (pcase (match-data)
      (`(,start               ,end
         ,upper-start         ,upper-end
         ,base-start          ,base-end
         ,lower-start         ,lower-end
         ,base-start-minus-1  ,base-start
         ,lower-start-minus-1 ,lower-start)
       (store-match-data
        (list start               end           ;; #0 (the entire match)
              start               upper-start   ;; #1 smerge-markers-face
              upper-start         upper-end     ;; #2 smerge-upper-face
              upper-end           base-start    ;; #3 smerge-markers-face
              base-start          base-end      ;; #4 smerge-base-face
              base-end            lower-start   ;; #5 smerge-markers-face
              lower-start         lower-end     ;; #6 smerge-lower-face
              lower-end           end           ;; #7 smerge-markers-face
              base-start-minus-1  base-start    ;; #8 nil
              lower-start-minus-1 lower-start)) ;; #9 nil
       found)
      (unexpected
       (warn "Unexpected match data after calling `smerge-find-conflict': %s" unexpected)
       nil))))


(provide 'my-smerge)
