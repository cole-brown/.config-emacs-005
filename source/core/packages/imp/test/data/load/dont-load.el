;; -*- no-byte-compile: t; lexical-binding: t; -*-
;;; emacs/imp/test/loading/dont-load.el


;; We expect this to never get evaluated.
;; Mirror variable to what 'load.el' has.
(setq test-imp-load--dont-load.el-loaded t)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide test imp-load (imp-file-current))
