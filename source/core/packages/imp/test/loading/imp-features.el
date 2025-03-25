;; -*- no-byte-compile: t; lexical-binding: t; -*-
;;; emacs/imp/test/loading/imp-features.el


;;------------------------------------------------------------------------------
;; Loading Var/Func
;;------------------------------------------------------------------------------

;; Variable to indicate that this file has been evaluated.
(setq test<imp>:loading:features:loaded t)


;;------------------------------------------------------------------------------
;; Set our feature locations.
;;------------------------------------------------------------------------------

(imp:feature:at :loading
                '((:loading
                    "imp-features.el")
                  ((:loading features)
                   "imp-features.el")
                  ((:loading load)
                  "load.el")
                  ((:loading dont-load)
                   "dont-load.el")))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :loading 'features)
