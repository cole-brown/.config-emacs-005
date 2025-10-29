;;; imp/parser.el --- Plist Parsing -*- lexical-binding: t; -*-
;;
;; Author:     John Wiegley <johnw@newartisans.com>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2025-09-22
;; Timestamp:  2025-10-29
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Flagrantly stolen from `use-package-core.el',
;;  - https://github.com/jwiegley/use-package/blob/cba23de38d0a0c361f591be450d97dfc277469d3/use-package-core.el
;; And lovingly beaten to death.
;;   (apologies to John Wiegley)

;; This file contains the core implementation of the `use-package'
;; macro.
;;
;; See the `use-package' info manual for more information.

;;; Code:

(require 'cl-lib)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Utility functions
;;

;; TODO: Switch to imp--error
(defsubst imp-parser-error (msg)
  "Report MSG as an error, so the user knows it came from this package."
  (error "imp-parser: %s" msg))

(defsubst imp-parser-concat (&rest elems)
  "Delete all empty lists from ELEMS (nil or (list nil)), and append them."
  (apply #'append (delete nil (delete (list nil) elems))))

(defsubst imp-parser-non-nil-symbolp (sym)
  "`nil' doesn't count as a symbol"
  (and sym (symbolp sym)))

(defsubst imp-parser-as-symbol (string-or-symbol)
  "If STRING-OR-SYMBOL is already a symbol, return it.
Otherwise convert it to a symbol and return that."
  (if (symbolp string-or-symbol)
      string-or-symbol
    (intern string-or-symbol)))

(defsubst imp-parser-as-string (string-or-symbol)
  "If STRING-OR-SYMBOL is already a string, return it.
Otherwise convert it to a string and return that."
  (if (stringp string-or-symbol)
      string-or-symbol
    (symbol-name string-or-symbol)))

(defsubst imp-parser-is-pair (x car-pred cdr-pred)
  "Return non-nil if X is a cons satisfying the given predicates.
CAR-PRED and CDR-PRED are applied to X's `car' and `cdr',
respectively."
  (and (consp x)
       (funcall car-pred (car x))
       (funcall cdr-pred (cdr x))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Argument Processing
;;

;; TODO: make 'em all start with `imp-parser-args-'
;; TODO: and try to standardize the naming of the 2 or 3 different kinds of arg processing funcs.

(defun imp-parser-args-only-one (keyword args)
  "Return the first member of ARGS if it has exactly one element.
Else error."
  (cond
   ((and (listp args) (listp (cdr args))
         (= (length args) 1))
    (car args))
   (t
    (imp-parser-error
     (concat (imp-parser-as-string keyword) " wants exactly one argument")))))

(defun imp-parser-only-one (keyword args func)
  "Call FUNC on the first member of ARGS if it has exactly one element.
Else error."
  (cond
   ((and (listp args) (listp (cdr args))
         (= (length args) 1))
    (funcall func keyword (car args)))
   (t
    (imp-parser-error
     (concat (imp-parser-as-string keyword) " wants exactly one argument")))))

(defun imp-parser-as-one (keyword args func &optional allow-empty)
  "Call FUNC on the first element of ARGS if it has one element, or all of ARGS.
If ALLOW-EMPTY is non-nil, it's OK for ARGS to be an empty list."
  (if (if args
          (and (listp args) (listp (cdr args)))
        allow-empty)
      (if (= (length args) 1)
          (funcall func keyword (car args))
        (funcall func keyword args))
    (imp-parser-error
     (concat (imp-parser-as-string keyword) " wants a non-empty list"))))

(defun imp-parser-memoize (func arg)
  "Ensure the macro-expansion of FUNC applied to ARG evaluates ARG
no more than once."
  (let ((loaded (cl-gentemp "imp-parser--loaded"))
        (result (cl-gentemp "imp-parser--result"))
        (next   (cl-gentemp "imp-parser--next")))
    `((defvar ,loaded nil)
      (defvar ,result nil)
      (defvar ,next #'(lambda () (if ,loaded ,result
                                   (setq ,loaded t ,result ,arg))))
      ,@(funcall func `((funcall ,next))))))

(defsubst imp-parser-normalize-value (keyword arg)
  "Normalize the Lisp value given by ARG."
  (cond ((null arg) nil)
        ((eq t arg) t)
        ((imp-parser-non-nil-symbolp arg)
         `(symbol-value ',arg))
        ((functionp arg)
         `(funcall #',arg))
        (t arg)))

(defun imp-parser-normalize-eval (arg)
  "Eval ARG if a form/function. Otherwise return arg."
  (cond ((null arg) nil)
        ((eq t arg) t)
        ((stringp arg) arg)
        ((symbolp arg) arg)
        (t
         (eval arg))))

(defsubst imp-parser-normalize-symbol-or-string (keyword arg)
  "Normalize the Lisp value given by ARG."
  (cond ((null arg) nil)
        ((eq t arg) t)
        ((stringp arg) arg)
        ((functionp arg)
         `(funcall #',arg))
        (t arg)))

(defun imp-parser-normalize-symbols (keyword args &optional recursed)
  "Normalize a list of symbols."
  (cond
   ((imp-parser-non-nil-symbolp args)
    (list args))
   ((and (not recursed) (listp args) (listp (cdr args)))
    (mapcar #'(lambda (x) (car (imp-parser-normalize-symbols keyword x t))) args))
   (t
    (imp-parser-error
     (concat (imp-parser-as-string keyword) " wants a symbol, or list of symbols")))))

(defun imp-parser-normalize-symlist (feature keyword args)
  (imp-parser-as-one keyword args
    #'imp-parser-normalize-symbols))

(defun imp-parser-normalize-flag (feature arg)
  "Converts ARGS to '(t) if there are no args."
  (if (null arg)
      '(t)
    arg))

(defun imp-parser-normalize-only-one-symbol (feature keyword args)
  (imp-parser-only-one keyword args
    #'imp-parser-normalize-symbols))

(defun imp-parser-normalize-only-one-value-or-flag (feature keyword args)
  (if (null args)
      ;; keyword used as a flag (by itself; no value), so normalize to true.
      '(t)
    (imp-parser-only-one keyword args
      #'imp-parser-normalize-value)))

(defun imp-parser-normalize-only-one-symbol-or-string (feature keyword args)
  (imp-parser-only-one keyword
    args
    #'imp-parser-normalize-value))

(defun imp-parser-normalize-only-one-value (feature keyword args)
  (imp-parser-only-one keyword args
    #'imp-parser-normalize-value))

(defun imp-parser-normalize-recursive-symbols (keyword args)
  "Normalize a list of symbols."
  (cond
   ((imp-parser-non-nil-symbolp args)
    args)
   ((and (listp args) (listp (cdr args)))
    (mapcar #'(lambda (x) (imp-parser-normalize-recursive-symbols keyword x))
            args))
   (t
    (imp-parser-error
     (concat (imp-parser-as-string keyword) " wants a symbol, or nested list of symbols")))))

(defun imp-parser-normalize-recursive-symlist (feature keyword args)
  (imp-parser-as-one keyword args
    #'imp-parser-normalize-recursive-symbols))

(defun imp-parser-normalize-predicate (feature keyword args)
  (if (null args)
      t
    (imp-parser-only-one keyword args
      #'imp-parser-normalize-value)))

(defun imp-parser-normalize-form (keyword args)
  "Given a list of forms, return it wrapped in `progn'."
  (unless (listp (car args))
    (imp-parser-error (concat (imp-parser-as-string keyword) " wants a sexp or list of sexps")))
  (mapcar #'(lambda (form)
              (if (and (consp form)
                       (memq (car form)
                             '(imp-parser bind-key bind-key*
                                unbind-key bind-keys bind-keys*)))
                  (macroexpand form)
                form)) args))

(defun imp-parser-normalize-forms (feature keyword args)
  "Given a list of forms, return it wrapped in `progn'."
  (imp-parser-normalize-form keyword args))

;; (defun imp-parser-normalize-pairs
;;     (key-pred val-pred feature keyword arg &optional recursed)
;;   "Normalize a list of pairs.
;; KEY-PRED and VAL-PRED are predicates recognizing valid keys and
;; values, respectively.
;; If RECURSED is non-nil, recurse into sublists."
;;   (cond
;;    ((funcall key-pred arg)
;;     (list (cons arg (imp-parser-as-symbol feature))))
;;    ((imp-parser-is-pair arg key-pred val-pred)
;;     (list arg))
;;    ((and (not recursed) (listp arg) (listp (cdr arg)))
;;     (let (last-item)
;;       (mapcar
;;        #'(lambda (x)
;;            (prog1
;;                (let ((ret (imp-parser-normalize-pairs
;;                            key-pred val-pred feature keyword x t)))
;;                  (if (and (listp ret)
;;                           (not (keywordp last-item)))
;;                      (car ret)
;;                    ret))
;;              (setq last-item x))) arg)))
;;    (t arg)))

;; TODO: use this in path normalizer for the `eval' case?
;; (defun imp-parser-recognize-function (v &optional binding additional-pred)
;;   "A predicate that recognizes functional constructions:
;;   nil
;;   sym
;;   \\='sym
;;   (quote sym)
;;   #\\='sym
;;   (function sym)
;;   (lambda () ...)
;;   \\='(lambda () ...)
;;   (quote (lambda () ...))
;;   #\\='(lambda () ...)
;;   (function (lambda () ...))"
;;   (or (if binding
;;           (symbolp v)
;;         (imp-parser-non-nil-symbolp v))
;;       (and (listp v)
;;            (memq (car v) '(quote function))
;;            (imp-parser-non-nil-symbolp (cadr v)))
;;       (if binding (commandp v) (functionp v))
;;       (and additional-pred
;;            (funcall additional-pred v))))

;; (defun imp-parser-normalize-function (v)
;;   "Reduce functional constructions to one of two normal forms:
;;   sym
;;   #\\='(lambda () ...)"
;;   (cond ((symbolp v) v)
;;         ((and (listp v)
;;               (memq (car v) '(quote function))
;;               (imp-parser-non-nil-symbolp (cadr v)))
;;          (cadr v))
;;         ((and (consp v)
;;               (eq 'lambda (car v)))
;;          v)
;;         ((and (listp v)
;;               (memq (car v) '(quote function))
;;               (eq 'lambda (car (cadr v))))
;;          (cadr v))
;;         (t v)))

;; (defun imp-parser-normalize-commands (args)
;;   "Map over ARGS of the form ((_ . FUNC) ...), normalizing FUNCs."
;;   (mapcar #'(lambda (x)
;;               (if (consp x)
;;                   (cons (car x) (imp-parser-normalize-function (cdr x)))
;;                 x))
;;           args))

(defun imp-parser-handle-state (feature keyword arg rest state)
  "Save KEYWORD & ARG into STATE plist for later use."
  (setq state (imp-parser-plist-maybe-put state keyword arg))
  (imp-parser-process-keywords feature rest state))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide imp parser)
