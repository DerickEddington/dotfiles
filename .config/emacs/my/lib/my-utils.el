;; -*- lexical-binding: t; -*-

(require 'map)


(defun my-alist-remove (al exclude)
  (map-filter
   (lambda (key _val) (not (member key exclude)))
   al))

(defun my-read-all (stream)
  "Read all Lisp forms (but doesn't work from a string) and return as a list."
  (let (datums)
    (while (ignore-error end-of-file (push (read stream) datums)))
    (nreverse datums)))

(defun my-read-all-from-file (filename)
  "Read FILENAME and return a list of its Lisp forms."
  (with-temp-buffer
    (insert-file-contents filename)
    (my-read-all (current-buffer))))

(defmacro my-include (filename)
  (let ((filename (eval filename)))
    `(progn ,@(my-read-all-from-file filename))))

(defmacro my-include-multi (&rest filenames)
  `(progn ,@(mapcar (lambda (filename) `(my-include ,filename))
                    filenames)))

(defmacro my-include-multi-rel (basedir &rest filenames)
  (declare (indent 1))
  (let ((basedir (eval basedir)))
    `(my-include-multi ,@(mapcar (lambda (filename) (concat basedir "/" filename))
                                 filenames))))

(defmacro when-have-library (library-name &rest body)
  (declare (indent 1))
  (when (locate-library (eval library-name))
    `(progn ,@body)))


(provide 'my-utils)
