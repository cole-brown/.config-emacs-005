;;; core/modules/output/nub/alist.el --- Internal Alist Helpers -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-11-30
;; Timestamp:  2023-06-26
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Internal Alist Helpers
;;
;; Helper functions for alists that follow a convention for naming and stuff.
;; And will assist you in keeping you alist up-to-date, like it's a variable
;; or something.
;;
;; Can't use our `elisp/alist' module; `nub' needs to be- oh... this uses
;; `elisp/utils/types'. And it does load after `elisp/alist'... Was I trying to
;; avoid dependencies so `nub' could be a standalone package that is loaded
;; super early (a la `gcmh', `no-littering')? Probably.
;;
;;; Code:

(require 'seq)
(imp:require :nub 'internal)
(imp:require :elisp 'utils 'types)


;;------------------------------------------------------------------------------
;; A-list functions that are sane.
;;------------------------------------------------------------------------------

(defun int<nub>:alist:alist? (item)
  "Return non-nil if ITEM is an alist.

If ITEM is nil, return t, because:
  1. We cannot be sure it's /NOT/ an alist.
  2. nil is a valid list, and an empty list is a valid alist."
  ;; We'll consider `nil' a valid alist.
  (cond (nil
         t)

        ;; An alist cannot be a cons.
        ((elisp:cons? item)
         nil)

        ;; An alist has to be a list.
        ((not (listp item))
         nil)

        ;; An alist has to have only lists (or cons, which are lists).
        ;; If this is truthy, we'll just return its truthiness.
        ((seq-every-p #'listp item))

        ;; I don't know -> not an alist?
        (t
         nil)))


(defun int<nub>:alist:copy/shallow (alist)
  "Return a shallow copy of ALIST.

Copies the ALIST so that the returned alist does not share structure with
the input. Does not copy the keys/values (not a deep copy)."
  (copy-alist alist))


(defun int<nub>:alist:get/value (key alist &optional default)
  "Get cdr of KEY's entry in ALIST.

Return DEFAULT if not found (default: nil)."
  (alist-get key alist default))


(defun int<nub>:alist:get/pair (key alist)
  "Get full assoc/entry of KEY in ALIST."
  (assoc key alist))


(defun int<nub>:alist:update/helper (key value alist)
  "Set/overwrite an entry in the ALIST. Return the new alist.

If VALUE is nil, it will be set as KEY's value. Use
`int<nub>:alist:delete' if you want to remove it.

Returns a new alist, which isn't ALIST."
  ;;---
  ;; Error Checking
  ;;---
  (when (stringp key)
    (int<nub>:error "int<nub>:alist:update"
                    '("String key '%s' won't work... "
                      "Use `int<nub>:alist/string:update' for string keys.")
                    key))

  (if (null alist)
      ;; Create a new alist and return it.
      (list (cons key value))

    ;; `setf' creates a new alist sometimes, so buyer beware!
    (setf (alist-get key alist) value)
    alist))
;; (setq test-alist nil)
;; (setq test-alist (int<nub>:alist:update/helper :k :v test-alist))
;; (int<nub>:alist:update/helper :k2 :v2 test-alist)
;; (int<nub>:alist:update/helper :k2 :v2.0 test-alist)
;; test-alist


(defmacro int<nub>:alist:update (key value alist)
  "Set/overwrite an entry in the ALIST.

SYMBOL/ALIST should be a (quoted) symbol so that this can update it directly.

If VALUE is nil, it will be set as KEY's value. Use
`int<nub>:alist:delete' if you want to remove it.

Returns ALIST."
  `(let ((mmm:alist ,alist))
     (cond
      ((listp mmm:alist)
       (setq ,alist
             (int<nub>:alist:update/helper ,key ,value ,alist)))
      ((symbolp mmm:alist)
       (set mmm:alist
            (int<nub>:alist:update/helper ,key ,value (eval mmm:alist))))

      (t
       (int<nub>:error "int<nub>:alist:update"
                       "Unable to update alist with type %S: %S"
                       (typeof mmm:alist) mmm:alist)))))
;; A global variable:
;;   (setq test-alist nil)
;;   (int<nub>:alist:update :k :v test-alist)
;;   (int<nub>:alist:update :k :v test-alist)
;;   (int<nub>:alist:update :k2 :v2 test-alist)
;;   (int<nub>:alist:update :k2 :v2.0 test-alist)
;;   test-alist
;;
;; A scoped variable:
;;   (let (test-alist/let)
;;     (int<nub>:alist:update :k :v test-alist/let)
;;     (int<nub>:alist:update :k2 :v2 test-alist/let)
;;     (int<nub>:alist:update :k2 :v2.0 test-alist/let)
;;     test-alist/let)
;;
;; A +function+ macro call in the macro call:
;;   - Needs `test<nub/alist>:alist:get' and `test<nub/alist>:alist/nil' from 'test/alist.el'.
;; (setq test<nub/alist>:alist/nil nil)
;; test<nub/alist>:alist/nil
;; (int<nub>:alist:update :k :v
;;                        (test<nub/alist>:alist:get :global/nil))
;; test<nub/alist>:alist/nil


(defun int<nub>:alist:delete/helper (key alist)
  "Remove KEY from ALIST.

Return a copy of ALIST without the KEY."
  ;;---
  ;; Error Checking
  ;;---
  (when (stringp key)
    (int<nub>:error  "int<nub>:alist:delete"
                     '("String key '%s' won't work... "
                       "Use `int<nub>:alist/string:delete' "
                       "for string keys.")
                     key))
  ;; If it's null, no need to do anything.
  (unless (null alist)
    (setf (alist-get key alist nil 'remove) nil))

  ;; Return the alist.
  alist)
;; (setq test-alist nil)
;; (int<nub>:alist:delete/helper :k test-alist)
;; (int<nub>:alist:update :k :v test-alist)
;; test-alist
;; (int<nub>:alist:delete/helper :k2 test-alist)
;; (int<nub>:alist:delete/helper :k test-alist)
;; test-alist


(defmacro int<nub>:alist:delete (key alist)
  "Remove KEY from ALIST.

Return ALIST without the KEY."
  `(let ((mmm:alist ,alist))
     (cond
      ((listp mmm:alist)
       (setq ,alist
             (int<nub>:alist:delete/helper ,key ,alist)))
      ((symbolp mmm:alist)
       (set mmm:alist
            (int<nub>:alist:delete/helper ,key (eval mmm:alist))))

      (t
       (int<nub>:error "int<nub>:alist:delete"
                       "Unable to delete key from alist with type %S: %S"
                       (typeof mmm:alist) mmm:alist)))))
;; (setq test-alist nil)
;; (int<nub>:alist:delete :k test-alist)
;; (int<nub>:alist:update :k :v test-alist)
;; (int<nub>:alist:delete :k2 test-alist)
;; (int<nub>:alist:delete :k test-alist)
;; test-alist


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :nub 'alist)
