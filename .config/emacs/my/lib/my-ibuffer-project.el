;; -*- lexical-binding: t; -*-

(defcustom my-ibuffer-project-root-regexps '()
  "Regexps whose entire match will be used as a project root directory.

Each is automatically prefixed with
`tramp-initial-file-name-regexp' as optional, so that they also
apply to remote directories."
  :type '(repeat (choice regexp (cons :tag "Rx-form sequence" (const rx) (repeat sexp))))
  :group 'my)

(defun my-ibuffer-project-root-functions-from-my-regexps ()
  `((ibuffer-project-project-root . "")
    ,@(mapcar
       (lambda (r)
         (cons (rx-to-string
                `(seq string-start
                      (group (? (regex ,tramp-initial-file-name-regexp))
                             ,(pcase r
                                (`(rx . ,sexps)  `(seq ,@sexps))
                                ((pred stringp) `(regex ,r))
                                (_ (error
                                    "Bogus `my-ibuffer-project-root-regexps' element: %s" r))))
                      "/"))
               ""))
       my-ibuffer-project-root-regexps)
    (identity . "")))


(defun my-dir-name-w/o-slash-suffix (dir-name)
  "Don't want to see trailing slashes on group names."
  (if (and (> (length (file-local-name dir-name)) 1)
           (string-suffix-p "/" dir-name))
      (substring dir-name 0 -1)
    dir-name))


(defun my-ibuffer-project--filter-group-qualifier-root-dir (filter-group)
  "Extract the project root from an element of a filter-groups alist."
  (pcase filter-group
    (`(,_group-label (project-root . ,qualifier) . ,_)
     (pcase qualifier
       (`(,root-dir . ,_group-type-title)
        root-dir)))))

(defun my-ibuffer-project--sort-filter-groups-by-local-vs-remote (filter-groups)
  "Further sort the list of groups by whether they are for local or remote."
  (sort filter-groups
        (lambda (a b)
          (let ((root-dir-a (my-ibuffer-project--filter-group-qualifier-root-dir a))
                (root-dir-b (my-ibuffer-project--filter-group-qualifier-root-dir b)))
            (and (not (file-remote-p root-dir-a))
                 (file-remote-p root-dir-b))))))


;; Not used currently.  If ever desired to use, needs to be fixed so that: the
;; project-file-relative column handling does not strip leading "/" and "~/"
;; (which it currently does due to use of `file-relative-name'.)
(defun my-ibuffer-project--root-dir-func--is-in-home-or-not (buf-default-dir)
  "Group buffers as \"projects\" based on if their directory is in a home."
  (let* ((remote (or (file-remote-p buf-default-dir) ""))
         (top (concat remote "/"))
         (home (concat remote "~"))
         (rel-to-home (file-relative-name buf-default-dir home))
         (is-in-home (not (string-prefix-p "../" rel-to-home))))
    (if is-in-home home top)))


(provide 'my-ibuffer-project)
