;; -*- lexical-binding: t; -*-

(eval-when-compile (require 'use-package))

(use-package map
  :autoload (map-apply map-into map-merge-with map-remove))


;; TODO: Could have some `my-rust-analyzer-extra-settings', similar to
;; `my-rust-lang-proj--rust-analyzer-settings'.


(defun my-merge-plists-with-overriding (v1 v2)
  "Merge plists, recursively doing so for properties' values. `v2' overrides `v1'."
  (if (and (listp v1) (listp v2))
      ;; When both are (sub-)plists, combine them by recursing.
      (map-merge-with 'plist #'my-merge-plists-with-overriding v1 v2)
    ;; When either or both are not plists, the 2nd value overrides the 1st.
    v2))

(defun my-filter-plist-remove-marked (pl)
  "Remove any properties whose value is `:remove-this-prop', recursively."
  (map-into (map-apply
             (lambda (key val)
               ;; When a value is a (sub-)plist, recurse to remove any marked in it.
               (cons key (if (listp val)
                             (my-filter-plist-remove-marked val)
                           val)))
             ;; Remove any properties that were marked to be removed.
             (map-remove (lambda (_key val) (eq :remove-this-prop val))
                         pl))
            'plist))

(defun my--lsp-rust-analyzer--make-init-options--extend (original-init-options extra-init-options)
  "Merge keyword-plists that represent JSON settings for rust-analyzer.

Non-list (i.e. non-JSON-object) values are not merged, so that
the value from `extra-init-options' overrides that from
`original-init-options'."
  (my-filter-plist-remove-marked
   (my-merge-plists-with-overriding
    original-init-options
    extra-init-options)))


;; Note: For debugging this stuff, it's very useful to have `export RA_LOG=rust_analyzer=info` in
;; the environment of the `rust-analyzer` process.
