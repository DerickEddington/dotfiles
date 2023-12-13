;; -*- lexical-binding: t; -*-

(use-package my-ibuffer :load-path "my/lib"
  :bind ("C-x C-b" . my-ibuffer)
  :autoload
  (my-ibuffer-column--size--body
   my-ibuffer-column--proc-&-rel-fn--body))

(use-package my-ibuffer-project :load-path "my/lib"
  :autoload
  (my-ibuffer-project-root-functions-from-my-regexps
   my-dir-name-w/o-slash-suffix
   my-ibuffer-project--sort-filter-groups-by-local-vs-remote))


(use-package ibuffer-project

  :custom
  (ibuffer-project-root-functions (my-ibuffer-project-root-functions-from-my-regexps))

  :config

  (advice-add 'ibuffer-project-group-name :filter-return
              #'my-dir-name-w/o-slash-suffix)

  (advice-add 'ibuffer-project-generate-filter-groups :filter-return
              #'my-ibuffer-project--sort-filter-groups-by-local-vs-remote))


(use-package ibuffer :ensure nil

  :after ibuffer-project

  :bind (:map ibuffer-mode-map
              ([remap ibuffer-update] . my-ibuffer-update))

  :config

  (define-ibuffer-column my-size
    (:name "Size"
           :header-mouse-map ibuffer-size-header-map
           :summarizer my-ibuffer-column--size--summarizer)
    (my-ibuffer-column--size--body buffer mark))

  (defalias 'my-ibuffer-column--proc-&-rel-fn--summarizer
    (get 'ibuffer-make-column-filename-and-process 'ibuffer-column-summarizer))

  (define-ibuffer-column my-process-and-relative-filename
    (:name "Filename/Process"
           :header-mouse-map ibuffer-project-file-relative-header-map
           :summarizer my-ibuffer-column--proc-&-rel-fn--summarizer)
    (my-ibuffer-column--proc-&-rel-fn--body buffer mark)))
