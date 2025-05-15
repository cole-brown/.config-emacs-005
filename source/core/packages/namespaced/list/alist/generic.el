;;; core/modules/emacs/alist/generic.el --- Better Alist Functions -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-12-15
;; Timestamp:  2023-06-21
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;                                 ──────────
;; ╔════════════════════════════════════════════════════════════════════════╗
;; ║                         Better Alist Functions                         ║
;; ╚════════════════════════════════════════════════════════════════════════╝
;;                                   ──────
;;               At least these all have 'alist' in the name...
;;                                 ──────────

;; Helper functions for alists that follow a convention for naming and stuff.
;; And will assist you in keeping you alist up-to-date, like it's a variable
;; or something.
;;
;; "Generic" in that it can be used to implement e.g. string alists.
;;
;;; Code:


(require 'seq)
(imp:require :alist 'internal)
(imp:require :alist 'type 'types)


;;------------------------------------------------------------------------------
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;                'Generic' Functions - Funcs with TYPE param.
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; Getters
;;------------------------------------------------------------------------------

(defun alist:generic:get/value (key alist &optional type default)
  "Get cdr of KEY's entry in ALIST.

If TYPE is non-nil, get & use the proper equality function for TYPE."
  (int<alist>:type:valid? type key)
  (alist-get key
             alist
             default
             nil
             (int<alist>:type:func/equal? type)))


(defun alist:generic:get/pair (key alist &optional type)
  "Get full assoc/entry of KEY in ALIST.

If TYPE is non-nil, get & use the proper equality function for TYPE."
  (int<alist>:type:valid? type key)
  (assoc key
         alist
         (int<alist>:type:func/equal? type)))


;;------------------------------------------------------------------------------
;; Setters
;;------------------------------------------------------------------------------

(defun int<alist>:generic:update/helper (key value alist &optional type)
  "Set/overwrite an entry in the ALIST. Return the new alist.

If VALUE is nil, it will be set as KEY's value. Use
`alist:generic:delete' if you want to remove it.

If TYPE is non-nil, get & use the proper equality function for TYPE.

Returns either a new alist, which isn't ALIST, or the updated alist which may
or may not be ALIST."
  (int<alist>:type:valid? type key)
  (if (null alist)
      ;; Create a new alist and return it.
      (list (cons key value))

    ;; `setf' creates a new alist sometimes, so buyer beware!
    (setf (alist-get key
                     alist
                     nil
                     nil
                     (int<alist>:type:func/equal? type))
          value)
    alist))
;; (setq test-alist nil)
;; (setq test-alist (int<alist>:generic:update/helper :k :v test-alist))
;; (int<alist>:generic:update/helper :k2 :v2 test-alist)
;; (int<alist>:generic:update/helper :k2 :v2.0 test-alist)
;; test-alist


(defmacro alist:generic:update (key value alist &optional type)
  "Set/overwrite an entry in the ALIST.

SYMBOL/ALIST should be a (quoted) symbol so that this can update it directly.

If TYPE is non-nil, get & use the proper equality function for TYPE.

If VALUE is nil, it will be set as KEY's value. Use
`alist:generic:delete' if you want to remove it.

Returns ALIST."
  ;; Evaluate inputs only once.*
  `(let ((int<alist>:macro:key   ,key)
         (int<alist>:macro:value ,value)
         (int<alist>:macro:alist ,alist)
         (int<alist>:macro:type  ,type))
     (int<alist>:type:valid? int<alist>:macro:type int<alist>:macro:key)
     (cond ((listp int<alist>:macro:alist)
            ;; Overwrite the input symbol value with the updated alist value that `int<alist>:macro:alist' holds.
            (setq ,alist ;; *Have to re-eval ALIST here to actually set it for the caller.
                  (int<alist>:generic:update/helper int<alist>:macro:key
                                                    int<alist>:macro:value
                                                    int<alist>:macro:alist
                                                    int<alist>:macro:type)))

           ((symbolp int<alist>:macro:alist)
            ;; Set our updated alist to the symbol that `int<alist>:macro:alist' holds.
            (set int<alist>:macro:alist
                 (int<alist>:generic:update/helper int<alist>:macro:key
                                                   int<alist>:macro:value
                                                   ;;
                                                   (eval int<alist>:macro:alist))))

           (t
            (int<alist>:error "alist:generic:update"
                              "Unable to update alist with type %S: %S"
                              (type-of int<alist>:macro:alist) int<alist>:macro:alist)))))
;; A global variable:
;;   (setq test-alist nil)
;;   (alist:generic:update :k :v test-alist)
;;   test-alist
;;   (alist:generic:update :k :v test-alist)
;;   (alist:generic:update :k2 :v2 test-alist)
;;   (alist:generic:update :k2 :v2.0 test-alist)
;;   test-alist
;;
;; A scoped variable:
;;   (let (test-alist/let)
;;     (alist:generic:update :k :v test-alist/let)
;;     (alist:generic:update :k2 :v2 test-alist/let)
;;     (alist:generic:update :k2 :v2.0 test-alist/let)
;;     test-alist/let)
;;
;; A +function+ macro call in the macro call:
;;   - Needs `test<alist/alist>:alist:get' and `test<alist/alist>:alist/nil' from 'test/alist.el'.
;; (setq test<alist/alist>:alist/nil nil)
;; test<alist/alist>:alist/nil
;; (alist:generic:update :k :v
;;               (test<alist/alist>:alist:get :global/nil))
;; test<alist/alist>:alist/nil


(defun int<alist>:generic:delete/helper (key alist type)
  "Removes KEY from ALIST.

If TYPE is non-nil, get & use the proper equality function for TYPE.

Returns alist without the key."
  (int<alist>:type:valid? type key)
  ;; If it's null, no need to do anything.
  (unless (null alist)
    (setf (alist-get key
                     alist
                     nil
                     'remove
                     (int<alist>:type:func/equal? type))
          nil))

  ;; Return the alist.
  alist)


(defmacro alist:generic:delete (key alist &optional type)
  "Removes KEY from ALIST.

If TYPE is non-nil, get & use the proper equality function for TYPE.

Returns ALIST."
  ;; Evaluate inputs only once.*
  `(let ((int<alist>:macro:alist ,alist)
         (int<alist>:macro:key   ,key)
         (int<alist>:macro:type  ,type))
     (int<alist>:type:valid? int<alist>:macro:type int<alist>:macro:key)
     (cond ((listp int<alist>:macro:alist)
            (setq ,alist ;; *Have to re-eval ALIST here to actually set it for the caller.
                  (int<alist>:generic:delete/helper int<alist>:macro:key
                                                    int<alist>:macro:alist
                                                    int<alist>:macro:type)))

           ((symbolp int<alist>:macro:alist)
            (set int<alist>:macro:alist
                 (int<alist>:generic:delete/helper int<alist>:macro:key
                                                   (eval int<alist>:macro:alist)
                                                   int<alist>:macro:type)))

           (t
            (int<alist>:error "alist:generic:delete"
                              "Unable to delete key from alist (type-of %S)%s: %S"
                              (type-of int<alist>:macro:alist)
                              (if int<alist>:macro:type
                                  (format " with TYPE %S" int<alist>:macro:type)
                                "")
                              int<alist>:macro:alist)))))
;; (setq test-alist '((:k . :value) (:k2 . :value2) (:jeff . :jeff)))
;; (alist:generic:delete :k test-alist)
;; test-alist
;; (alist:generic:update :k :v test-alist)
;; test-alist
;; (alist:generic:delete :k2 test-alist)
;; test-alist
;; (alist:generic:delete :k test-alist)
;; test-alist
;; (alist:generic:delete :jeff test-alist)
;; test-alist


;;------------------------------------------------------------------------------
;; Alist helpers.
;;------------------------------------------------------------------------------

(defun alist:alist? (item)
  "Returns non-nil if ITEM is an alist.

If ITEM is nil, returns `t', because:
  1. We cannot be sure it's /NOT/ an alist.
  2. `nil' is a valid list, and an empty list is a valid alist."
  ;; We'll consider `nil' a valid alist.
  (cond (nil
         t)

        ;; An alist has to be a list.
        ((not (listp item))
         nil)

        ;; An alist has to have only lists (or cons, which are lists).
        ;; If this is truthy, we'll just return its truthiness.
        ((seq-every-p #'listp item))

        (t
         nil)))


(defun alist:copy/shallow (alist)
  "Returns a shallow copy of ALIST.

Copies the ALIST so that the returned alist does not share structure with
the input. Does not copy the keys/values (not a deep copy)."
  (copy-alist alist))




;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :alist 'generic)
