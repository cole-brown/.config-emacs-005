;;; namespaced/elisp/types.el --- Functions for Types -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-02-16
;; Timestamp:  2023-06-21
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Functions for Types
;;
;; Functions like:
;;   - cons but not list?
;;     - `consp' is _NOT_ what you want in that case!
;;   - proper list not improper list?
;;   - list, any list, even improper list?
;;
;;; Code:


;; TODO: make this `:type' instead of `:elisp'


;;------------------------------------------------------------------------------
;; Type checking
;;------------------------------------------------------------------------------

(defun elisp:cons/strict? (var)
  "Is VAR a cons, only a cons, and nothing but a cons?

Did you know that a cons is a list?
  (listp '(1 . 2))
    => t

Did you know that a list is a cons?
  (consp '(1 2 3 4))
    => t

Do you want this instead?
  (elisp:cons/strict? '(1 . 2))
    => t
  (elisp:cons/strict? '(1 2 3 4))
    => nil

Originally from:
https://emacs.stackexchange.com/questions/10489/predicate-function-for-dotted-pairs"
  (declare (pure t) (side-effect-free t))
  (and (listp var)
       (cdr var)
       (atom (cdr var))))
;; (elisp:cons? 1)
;; (elisp:cons? '(1 . 2))
;; (elisp:cons? '(1 2))
;; (elisp:cons? '(1))
;; (elisp:cons? '(1 . nil))


(defun elisp:cons/lax? (var)
  "Is VAR a cons? Any cons.

Strict cons? Cons.
  (eq (elisp:cons/strict? '(1 . 2))
      (elisp:cons/lax?    '(1 . 2)))
    => t

List? Also cons.
  (consp '(1 2 3 4))
    => t"
  (declare (pure t) (side-effect-free t))
  (consp var))


(defun elisp:list/strict? (var)
  "Is VAR an actual proper list, strictly speaking?

A proper list is neither circular nor dotted (i.e., its last cdr is nil).
  - `proper-list-p' docstr"
  (declare (pure t) (side-effect-free t))
  (proper-list-p var))


(defun elisp:list/lax? (var)
  "Is VAR anything that Emacs Lisp considers a list?

Includes:
  1. strict/proper lists: '(1 2 3 4)
  2. nil
  3. conses: '(1 . 2)
  4. lists with circular references
  5. lists with a dotted end instead of nil: '(1 2 3 . 4)
  5. etc"
  (declare (pure t) (side-effect-free t))
  (listp var))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide :elisp 'types)
