;; TODO: Maybe pre-compile so these execute faster (right?), in the future when stable.

(require 'ibuffer)
(require 'ibuffer-project)
(require 'rx)
(require 'tramp)
(require 'pcase)


(defcustom my-ibuffer-project-root-regexps '()
  "Regexps whose entire match will be used as a project root directory.

Each is automatically prefixed with
`tramp-initial-file-name-regexp' as optional, so that they also
apply to remote directories."
  :type '(repeat (choice regexp (cons :tag "Rx-form sequence" (const rx) (repeat sexp))))
  :group 'my)

(setq ibuffer-project-root-functions
      `((ibuffer-project-project-root . "")
        ,@(mapcar
           (lambda (r)
             (cons (rx-to-string
                    `(seq string-start
                          (group (? (regex ,tramp-initial-file-name-regexp))
                                 ,(pcase r
                                    (`(rx . ,sexps)  `(seq ,@sexps))
                                    ((pred stringp) `(regex ,r))
                                    (_ (error "Bogus `my-ibuffer-project-root-regexps' element: %s" r))))
                          "/"))
                   ""))
           my-ibuffer-project-root-regexps)
        (identity . "")))


(defun my-ibuffer (&optional clear-cache)
  "Better than relying on a hook added to `ibuffer-hook'.

Causes `ibuffer' to use our filter-groups initially, which avoids
seeing stale state that otherwise would require an extra
`ibuffer-update' call."
  (interactive "P")
  (when clear-cache
    (ibuffer-project-clear-cache))
  (let ((filter-groups (ibuffer-project-generate-filter-groups))
        other-window-p name qualifiers noselect shrink formats)
    (ibuffer
     other-window-p name qualifiers noselect shrink filter-groups formats)))


(defun my-size--summarizer (column-strings)
  (let ((total 0))
    (dolist (string column-strings)
      ;; Because the original string could've been (probably was) aligned with
      ;; space characters, the position of our text property is not fixed, so we
      ;; must search for it.
      (let ((size (seq-some (lambda (interval)
                              (pcase interval
                                (`(,start ,end (my-ibuffer-size ,size))
                                 size)))
                            (object-intervals string))))
        (setq total (+ total size))))
    (file-size-human-readable total)))

(define-ibuffer-column my-size
  (:name "Size"
   :header-mouse-map ibuffer-size-header-map
   :summarizer (lambda (&rest args) (apply #'my-size--summarizer args)))
  (let ((size (buffer-size)))
    (propertize (file-size-human-readable size) 'my-ibuffer-size size)))


(define-ibuffer-column my-process-and-relative-filename
  (:name "Filename/Process"
   :header-mouse-map ibuffer-project-file-relative-header-map
   :summarizer (lambda (&rest args)
                 (apply (get 'ibuffer-make-column-filename-and-process 'ibuffer-column-summarizer)
                        args)))
  (let* ((abbrevname-and-process
          (ibuffer-make-column-filename-and-process buffer mark))
         (filename
          (or (ibuffer-buffer-file-name) ""))
         (process
          (if (and (get-buffer-process buffer)
                   (string-match "^\\((.* .*)\\)" abbrevname-and-process))
              (match-string 1 abbrevname-and-process)))
         (project-root
          (and (length> filename 0) (ibuffer-project-root buffer))))
    (setq filename
          (if-let* (project-root
                    (root-dir (car project-root)))
              (file-relative-name filename root-dir)
            (ibuffer--abbreviate-file-name filename)))
    (if process
        (concat (propertize process 'font-lock-face 'font-lock-comment-face)
                (if (length> filename 0) (format " %s" filename) ""))
      filename)))


(defun my-dir-name-w/o-slash-suffix (dir-name)
  "Don't want to see trailing slashes on group names."
  (if (and (length> (file-local-name dir-name) 1)
           (string-suffix-p "/" dir-name))
      (substring dir-name 0 -1)
    dir-name))

(advice-add 'ibuffer-project-group-name :filter-return
            #'my-dir-name-w/o-slash-suffix)


(defun my--ibuffer-project--filter-group-qualifier-root-dir (filter-group)
  "Extract the project root from an element of a filter-groups alist."
  (pcase filter-group
    (`(,group-label (project-root . ,qualifier) . ,_)
     (pcase qualifier
       (`(,root-dir . ,group-type-title)
        root-dir)))))

(defun my--ibuffer-project--sort-filter-groups-by-local-vs-remote (filter-groups)
  "Further sort the list of groups by whether they are for local or remote."
  (sort filter-groups
        (lambda (a b)
          (let ((root-dir-a (my--ibuffer-project--filter-group-qualifier-root-dir a))
                (root-dir-b (my--ibuffer-project--filter-group-qualifier-root-dir b)))
            (and (not (file-remote-p root-dir-a))
                 (file-remote-p root-dir-b))))))

(advice-add 'ibuffer-project-generate-filter-groups :filter-return
            #'my--ibuffer-project--sort-filter-groups-by-local-vs-remote)


;; Not used currently.  If ever desired to use, needs to be fixed so that: the
;; project-file-relative column handling does not strip leading "/" and "~/"
;; (which it currently does due to use of `file-relative-name'.)
(defun my--ibuffer-project--root-dir-func--is-in-home-or-not (buf-default-dir)
  "Group buffers as \"projects\" based on if their directory is in a home."
  (let* ((remote (or (file-remote-p buf-default-dir) ""))
         (top (concat remote "/"))
         (home (concat remote "~"))
         (rel-to-home (file-relative-name buf-default-dir home))
         (is-in-home (not (string-prefix-p "../" rel-to-home))))
    (if is-in-home home top)))
