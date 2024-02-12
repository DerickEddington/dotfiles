;; -*- lexical-binding: t; -*-

;; How to get started: generate the stage0 sysroot needed by the below:
;; cd rust-lang/rust && ./x check --build-dir /var/tmp/$HOME/rust-lang/build-for-rust-analyzer

(eval-when-compile (require 'use-package))

(use-package my-rust-analyzer :load-path "my/lib"
  :autoload (my--lsp-rust-analyzer--make-init-options--extend))


(defcustom my-rust-lang-proj--is? nil
  "Indicates if a project directory is that of The Rust Project itself.

Intended to be set via a directory-local variable in each
directory that contains The Rust Project source-code."
  :type 'boolean
  :group 'my
  :safe #'booleanp
  :link '(url-link "https://rustc-dev-guide.rust-lang.org/building/suggested.html#configuring-rust-analyzer-for-rustc"))

(defun my-rust-analyzer-settings-for-rust-lang-proj ()
  "Like the official `src/etc/rust_analyzer_settings.json'."
  (let* ((false :json-false) (remove :remove-this-prop)
         (home-dir (getenv "HOME"))
         (build-dir (concat "/var/tmp/" home-dir "/rust-lang/build-for-rust-analyzer/"))
         (x-check-cmd `["python3" "x.py" "check" "--json-output" "--build-dir" ,build-dir])
         (proc-macro-srv (concat build-dir "host/stage0/libexec/rust-analyzer-proc-macro-srv")))

    `(:check (:invocationLocation "root" :invocationStrategy "once"
              :overrideCommand ,x-check-cmd
              :allTargets ,false)  ; Alt: Could remove this line.
      :checkOnSave ,false  ; Alt: Could remove this line.
      :linkedProjects [
        "Cargo.toml"
        "src/tools/x/Cargo.toml"
        "src/bootstrap/Cargo.toml"
        "src/tools/rust-analyzer/Cargo.toml"
        "compiler/rustc_codegen_cranelift/Cargo.toml"
        "compiler/rustc_codegen_gcc/Cargo.toml"
      ]
      :cargo (:buildScripts (:invocationLocation "root" :invocationStrategy "once"
                             :overrideCommand ,x-check-cmd)
              :sysrootSrc "./library"
              :sysrootQueryMetadata t  ; Alt: Could remove this line.
              :sysroot nil  ; Seems to be needed to avoid some issues.
              :extraEnv (:RUSTC_BOOTSTRAP "1"))  ; Allows "stable" rustc to act like "nightly".
      :rustc (:source "./Cargo.toml")
      :rustcSource ,remove  ; Because `lsp-rust.el' adds this but it's deprecated.
      :procMacro (:server ,proc-macro-srv))))

(defcustom my-rust-lang-proj--rust-analyzer-settings (my-rust-analyzer-settings-for-rust-lang-proj)
  "For rust-analyzer to understand a directory containing The Rust Project.

This is merged with what `lsp-rust-analyzer--make-init-options'
returns, when `my-rust-lang-proj--is?' is true."
  :type '(plist)
  :group 'my
  :link '(url-link "https://github.com/rust-lang/rust/blob/master/src/etc/rust_analyzer_settings.json"))

(defun my-is-rust-lang-proj? ()
  ;; TODO?: Could extend to also be true if the current directory structure just looks like that
  ;; of a rust-lang/rust one, to have automatic detection instead of needing to set the dir-local
  ;; var in it.
  my-rust-lang-proj--is?)

(defun my--lsp-rust-analyzer--make-init-options--extend-for-rust-lang-proj (init-options)
  (if (my-is-rust-lang-proj?)
      (my--lsp-rust-analyzer--make-init-options--extend
       init-options
       my-rust-lang-proj--rust-analyzer-settings)
    init-options))
