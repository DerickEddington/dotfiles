;; -*- lexical-binding: t; no-byte-compile: t; -*-

;; Everything in this file is intended only for use by the `my-install-emacs-packages' of
;; `~/.local/share/my/platform/packages.bash'.


(defun my-prepare-needed-for-use-of-use-package ()
  "Define the user options needed to install `use-package' and use it.

Note: For a branch that wants to already have `use-package-always-ensure'
enabled, this function should be deleted and `package-archives' should be
set to whatever the branch needs."

  (require 'package)  ;; Needed to compute our `package-archives' expression.

  (let ((after-save-hook
         ;; Set to nil in case it had included anything (e.g. a function that is not yet
         ;; installed) that would cause an error when the custom-file is saved.
         nil)
        (cus-vars '()))

    ;; Make future evaluation of the uses of `use-package' install the packages.
    (push '(use-package-always-ensure t) cus-vars)

    ;; Need MELPA for the packages my init config uses.
    (unless (assoc "melpa" package-archives)
      (push
       `(package-archives (cons '("melpa" . "https://melpa.org/packages/")
                                ,(let ((saved-value (get 'package-archives 'saved-value)))
                                   (if (and saved-value (car saved-value))
                                       ;; (This value is an expresssion.)
                                       (car saved-value)
                                     ;; Include its default value in our new value.
                                     'package-archives)))
                          ;; Requiring the `package' feature is needed for evaluating our custom
                          ;; expression, but this part of the form can be lost when Customize
                          ;; rewrites the forms in the future, but that's alright because my init
                          ;; file also `require's `package' before evaluating `custom-file'.
                          nil (package))
       cus-vars))

    ;; (We use these functions because they seem to be the simplest and most robust for altering
    ;; the preexisting custom-file (versus the various other APIs to Customize).)
    (apply #'custom-set-variables cus-vars)
    (custom-save-all)))


(defun my-install-use-package ()
  "Install `use-package' if not already."

  ;; Get the indexes for our newly-changed (by the above function) value of `package-archives'.
  (package-refresh-contents)

  ;; (For Emacs 27 (and other versions?) `package-installed-p' returns false for built-in packages
  ;; even though they are in fact installed (at least, that happened to me a few times), but
  ;; because versions less-than 29 don't come with `use-package' built-in, our use of this here is
  ;; alright for 27 (etc?) because this will return false as desired when it's truly not installed
  ;; because it's not built-in.)
  (unless (package-installed-p 'use-package)
    (package-install 'use-package)))


(provide 'my-prepare-to-install-packages)
