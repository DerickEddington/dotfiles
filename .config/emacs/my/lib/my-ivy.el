;; -*- lexical-binding: t; -*-

(eval-when-compile (require 'use-package))


(use-package ivy
  :autoload ivy-string<)


(defconst my-ivy-word-seps-rx
  (rx-let ((my-word-seps (any "-_+=:.,;/")) ;; Could add more (or otherwise change).
           (regexp-syntax (any "^$")))      ;; Could add more.
    (rx (or my-word-seps (regexp split-string-default-separators) regexp-syntax))))

(defun my-ivy-lowercase (cand)
  "To lowercase.  Use CARs when given pairs, like Ivy does."
  (let* ((str (if (consp cand) (car cand) cand))
         (lowercase-str (downcase (string-to-multibyte str))))
    (if (consp cand)
        (cons lowercase-str (cdr cand))
      lowercase-str)))

(defun my-ivy-split-on-words (cand)
  (let* ((str (if (consp cand) (car cand) cand))  ;; Use CARs when given pairs, like Ivy does.
         (str (my-ivy-lowercase str))  ;; To have case-insensitive comparisons.
         (words (split-string str my-ivy-word-seps-rx t my-ivy-word-seps-rx)))
    `(,cand . ,words)))

(defun my-ivy-sorter (given)
  "Return a sorting predicate function that matches against the `given' Ivy input."
  (let ((given-words (cdr (my-ivy-split-on-words given))))
    (lambda (a b)
      "Decide if `a' is a better match against `given' than `b' is."
      (let ((a-words (cdr a)) (b-words (cdr b)) (g-words given-words)
            (is-undecided t) (is-a-better-than-b nil))
        (while (and is-undecided (consp a-words) (consp b-words) (consp g-words))
          (let ((aw (car a-words)) (bw (car b-words)) (gw (car g-words)))
            ;; Compare word prefixes case-insensitively.
            (let ((a-is-p (string-prefix-p gw aw)) (b-is-p (string-prefix-p gw bw)))
              (if a-is-p
                  (if b-is-p
                      ;; Same so far. Advance all. Keep going.
                      (setq a-words (cdr a-words)
                            b-words (cdr b-words)
                            g-words (cdr g-words))
                    ;; `a' matches better. Done.
                    (setq is-undecided nil
                          is-a-better-than-b t))
                (if b-is-p
                    ;; `b' matches better. Done.
                    (setq is-undecided nil
                          is-a-better-than-b nil)
                  ;; Neither match. Advance the pair but not the given. Keep going.
                  (setq a-words (cdr a-words)
                        b-words (cdr b-words)))))))
        (if is-undecided
            (let ((a-words (cdr a)) (b-words (cdr b)))
              (let ((a-is-e (equal a-words given-words)) (b-is-e (equal b-words given-words)))
                (if (eq a-is-e b-is-e)
                    ;; Either both are unequal, or both are equal, to the given, by full words.
                    (let ((a-cand (car a)) (b-cand (car b)))
                      ;; Compare lexicographically case-insensitively.
                      (ivy-string< (my-ivy-lowercase a-cand) (my-ivy-lowercase b-cand)))
                  a-is-e)))  ;; Whichever one is equal to the given is what matches better.
          ;; Already decided which is better, by word prefixes above.
          is-a-better-than-b)))))

(defun my-ivy-sort-matches-fall-thru (given cands)
  (if (or (or (string= "^" given) (string= "" given)) ;; Nothing given to match yet.
          (nthcdr ivy-sort-max-size cands))           ;; Too many.
      ;; Keep the prior order.  Preserves having directories before files, e.g.
      cands
    ;; Something was given to match, so do my way.
    (mapcar #'car (sort (mapcar #'my-ivy-split-on-words cands)
                        (my-ivy-sorter given)))))


(defun my-ivy--recompute-index--unless-0 (oldfun &rest args)
  "Don't keep the previous selection when it was the first of those candidates.
Allow the index to remain as `0' so updating candidates selects the new first
of the new sorting.  This prevents undesirably keeping the previous as selected
when it was involuntarily forced to be something by Ivy automatically, and this
causes selecting the first of the updated sorting, which is desirable with
`my-ivy-sorter'."
  (unless (= 0 ivy--index)
    (apply oldfun args)))


(provide 'my-ivy)
