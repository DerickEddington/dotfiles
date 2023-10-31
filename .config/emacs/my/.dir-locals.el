;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

;; Loading .elc are actually slower than .el, and not worth it, for the init files in this
;; directory.  Some other particular files, e.g. those under ./lib/, override this so they can
;; have .eln.
((nil . ((no-byte-compile . t))))
