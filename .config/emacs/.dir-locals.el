;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

;; Loading .eln are actually slower than .elc, and not worth it, for the init, config, and state
;; files in this directory.  Some other particular files, e.g. of ELPA packages or under
;; ./my/lib/, override this so they can have .eln.
((nil . ((no-native-compile . t))))
