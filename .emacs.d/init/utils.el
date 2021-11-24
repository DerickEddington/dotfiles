(defun alist-remove (al exclude)
  (map-filter
   (lambda (key val) (not (member key exclude)))
   al))
