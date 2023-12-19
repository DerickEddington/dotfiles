;; -*- lexical-binding: t; no-byte-compile: t; -*-  # Prevent possibility of outdated .elc.

(eval-when-compile (require 'pcase))


(defun my-platform-info ()
  (let ((cmd "sh -c '. \"${XDG_DATA_HOME:-${HOME:?}/.local/share}\"/my/sh/helpers.sh \\
                         > /dev/null || exit
                     println \"${MY_CONFIG_HOME:?}\"/
                     println \"${MY_DATA_HOME:?}\"/
                     println \"${MY_STATE_HOME:?}\"/
                     println \"${MY_CACHE_HOME:?}\"/
                     println \"${MY_PLATFORM_ARCH:?}\"
                     println \"${MY_PLATFORM_OS:?}\"
                     println \"${MY_PLATFORM_VARIANT-}\"
                     println \"${MY_PLATFORM_VERSION:?}\"
                     print   \"${MY_PLATFORM_OS_VAR_VER_ARCH:?}\"'")
        exit-code)
    (with-temp-buffer
      (let ((stdout-buf (current-buffer)))
        (with-temp-buffer
          (let ((stderr-buf (current-buffer)))
            (setq exit-code (shell-command cmd stdout-buf stderr-buf))
            (if (= 0 exit-code)
                (let ((out-str (with-current-buffer stdout-buf (buffer-string))))
                  (pcase (split-string out-str "[\n]")
                    (`(,config-home ,data-home ,state-home ,cache-home
                       ,arch ,os ,variant ,version ,os-var-ver-arch)
                     `((config-home . ,config-home) (data-home  . ,data-home)
                       (state-home  . ,state-home)  (cache-home . ,cache-home)
                       (arch . ,arch) (os . ,os) (variant . ,variant) (version . ,version)
                       (os-var-ver-arch . ,os-var-ver-arch)))
                    (_
                     (message "Error: `my-platform-info' malformed command output! Was:\n%s"
                              out-str)
                     nil)))
              (let ((err-str (with-current-buffer stderr-buf (buffer-string))))
                (message "Error: `my-platform-info' failed with code %d! Stderr was:\n%s"
                         exit-code err-str)
                nil))))))))


(provide 'my-platform)
