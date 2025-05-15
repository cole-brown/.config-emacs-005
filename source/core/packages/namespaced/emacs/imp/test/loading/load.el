;; -*- no-byte-compile: t; lexical-binding: t; -*-
;;; emacs/imp/test/loading/load.el


;;------------------------------------------------------------------------------
;; Loading Var/Func
;;------------------------------------------------------------------------------

;; Mirror variable to what 'dont-load.el' has.
(setq test<imp>:loading:load:loaded t)


(defvar test<imp>:file:loading? nil
  "Gets set via `test<imp>:load'.")


(defun test<imp>:load:set ()
  "Check whether we're loading a file right now.

Sets `test<imp>:file:loading?' with results from `imp:provide:loading?'."
  ;; Use setq so it gets set every time.
  (setq test<imp>:file:loading?
        (imp:provide:loading?)))


(defun test<imp>:load:unset ()
  "Remove `test<imp>:file:loading?' from the symbol table.

That is: delete the variable from existance."
  (makunbound 'test<imp>:file:loading?))
;; Probably exists to start off with:
;;   test<imp>:file:loading?
;;   (test<imp>:load:unset)
;; And now it should not exist:
;;   (should-error test<imp>:file:loading?)


;;------------------------------
;; This exists in `provide.el' instead.
;;------------------------------
;; (defun test<imp>:load:exists? ()
;;   "Returns t if `test<imp>:file:loading?' exists in the symbol table."
;;   (condition-case err
;;       (progn
;;         ;; Just try to access this. If we can't, `condition-case' will catch the `void-variable' signal.
;;         test<imp>:file:loading?
;;         ;; If we could access it, return `t' because it exists.
;;         t)
;;     ;; Couldn't access it, so it doesn't exist, so return nil.
;;     (void-variable nil)))
;; ;; Probably exists to start off with:
;; ;;   test<imp>:file:loading?
;; ;;   (test<imp>:load:exists?)
;; ;;   (test<imp>:load:unset)
;; ;; And now it should not exist:
;; ;;   (test<imp>:load:exists?)


;;------------------------------------------------------------------------------
;; Set on File Exec.
;;------------------------------------------------------------------------------
(test<imp>:load:set)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :loading 'load)
