;;; core/modules/emacs/imp/alist.el --- imp alist helpers -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-05-16
;; Timestamp:  2023-06-22
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; imp alist helpers
;;
;; NOTE: 'imp' only! Everyone else go to: 'core/modules/emacs/alist'
;;
;;; Code:


;;------------------------------------------------------------------------------
;; A random list function.
;;------------------------------------------------------------------------------

(defun int<imp>:list:flatten:recurse (recurse &rest input)
  "Flatten INPUT list to a single list.

If RECURSE is an integer greater than zero, flatten by that many levels.
If RECURSE `:recursive', flatten recursively until flat.
Else, flatten by one level."
  (let ((recurse (if (integerp recurse)
                     ;; We always flatten by one; find out how many extra levels
                     ;; of flattening they want.
                     (1- recurse)
                   recurse)))
    (mapcan (lambda (item)
              "Return item as a list."
              (if (listp item)
                  ;; List & needs flattening; check for special `recurse' cases.
                  (cond ((integerp recurse)
                         ;; Flatten by a certain number of levels?
                         (if (> recurse 0)
                             (apply #'int<imp>:list:flatten:recurse (1- recurse) item)
                           item))
                        ((eq :recursive recurse)
                         ;; Flatten until flat or stack overflow.
                         (apply #'int<imp>:list:flatten:recurse :recursive item))
                        (t
                         item))
                ;; Just an item, turn into a list so it can be
                ;; concatenated by `mapcan'.
                (list item)))
            input)))
;; (int<imp>:list:flatten:recurse 1 '(foo bar (baz) (qux (quux))))
;; (int<imp>:list:flatten:recurse 1 'foo 'bar '(baz) '(qux (quux)))
;; (int<imp>:list:flatten:recurse :recursive '(foo bar (baz) (qux (quux (quuux)))))


(defun int<imp>:list:flatten (&rest input)
  "Flatten INPUT list to a single list.

Flatten by a max of 10 levels."
  ;; &rest wrapped our INPUT in a list, and `int<imp>:list:flatten:recurse' will
  ;; do the same, so +2 to our max of 10.
  (int<imp>:list:flatten:recurse 12 input))
;; (int<imp>:list:flatten '(foo bar (baz) (qux (quux))))
;; (int<imp>:list:flatten '(foo bar (baz) (qux (quux (quuux)))))


;;------------------------------------------------------------------------------
;; A-list Functions
;;------------------------------------------------------------------------------

(defun int<imp>:alist:valid/key (caller key &optional error?)
  "Return non-nil if KEY is valid.

CALLER should be the calling function's name string.

If ERROR? is non-nil, raise an error for invalid keys. Else return nil/non-nil."
  (if (stringp key)
      (if error?
          (int<imp>:error (int<imp>:output:callers "int<imp>:alist:valid/key" caller)
                          "imp alist cannot have a string key! Key: %S"
                          key)
        nil)
    key))
;; (int<imp>:alist:valid/key "test" 'foo t)
;; (int<imp>:alist:valid/key "test" :foo t)
;; (int<imp>:alist:valid/key "test" "foo" t)


(defun int<imp>:alist:get/value (key alist &optional equal-fn)
  "Get value of KEY's entry in ALIST.

EQUAL-FN should be `eq', `eql', `equal', etc - a function that tests equality of
two alist keys."
  (int<imp>:alist:valid/key "int<imp>:alist:get/value" key :error)
  (alist-get key alist nil nil equal-fn))
;; (int<imp>:alist:get/value :foo test-foo #'equal)


(defun int<imp>:alist:get/pair (key alist &optional equal-fn)
  "Get KEY's entire entry (`car' is KEY, `cdr' is value) from ALIST.

EQUAL-FN should be `eq', `eql', `equal', etc - a function that tests equality of
two alist keys."
  (int<imp>:alist:valid/key "int<imp>:alist:get/pair" key :error)
  (assoc key alist equal-fn))


(defun int<imp>:alist:update/helper (key value alist &optional equal-fn)
  "Set/overwrite an entry in the ALIST. Return the new alist.

If VALUE is nil, it will be set as KEY's value. Use
`int<imp>:alist:delete' if you want to remove it.

EQUAL-FN should be `eq', `eql', `equal', etc - a function that tests equality of
two alist keys.

Return an updated alist, which may or may not be ALIST."
  (int<imp>:alist:valid/key "int<imp>:alist:update/helper" key :error)

  (if (null alist)
      ;; Create a new alist and return it.
      (list (cons key value))

    ;; `setf' creates a new alist sometimes, so buyer beware!
    (setf (alist-get key alist nil nil equal-fn) value)
    alist))
;; (setq test-alist nil)
;; (setq test-alist (int<imp>:alist:update/helper :k :v test-alist))
;; (int<imp>:alist:update/helper :k2 :v2 test-alist)
;; (int<imp>:alist:update/helper :k2 :v2.0 test-alist)
;; test-alist


(defmacro int<imp>:alist:update (key value alist &optional equal-fn)
  "Set/overwrite an entry in the ALIST.

If VALUE is nil, it will be set as KEY's value. Use
`int<imp>:alist:delete' if you want to remove it.

EQUAL-FN should be `eq', `eql', `equal', etc - a function that tests equality of
two alist keys.

Return updated ALIST."
  `(let ((macro<imp>:alist ,alist))
     (cond
      ((listp macro<imp>:alist)
       (setq ,alist
             (int<imp>:alist:update/helper ,key ,value ,alist ,equal-fn)))
      ((symbolp macro<imp>:alist)
       (set macro<imp>:alist
            (int<imp>:alist:update/helper ,key ,value (eval macro<imp>:alist) ,equal-fn)))

      (t
       (int<imp>:error "int<imp>:alist:update"
                       "Unable to update alist: not a list or a symbol: %S (type: %S)"
                       macro<imp>:alist
                       (typeof macro<imp>:alist))))))
;; (setq test<imp>:alist nil)
;; (int<imp>:alist:update :k0 :v0 test<imp>:alist)
;; test<imp>:alist


(defun int<imp>:alist:delete/helper (key alist &optional equal-fn)
  "Remove KEY from ALIST.

EQUAL-FN should be `eq', `eql', `equal', etc - a function that tests equality of
two alist keys.

Return updated ALIST sans KEY."
  (int<imp>:alist:valid/key "int<imp>:alist:delete/helper" key :error)

  ;; If it's null, no need to do anything.
  (unless (null alist)
    (setf (alist-get key alist nil 'remove equal-fn) nil))

  ;; Return the alist.
  alist)


(defmacro int<imp>:alist:delete (key alist &optional equal-fn)
  "Remove KEY from ALIST.

EQUAL-FN should be `eq', `eql', `equal', etc - a function that tests equality of
two alist keys.

Returns ALIST."
  `(let ((macro<imp>:alist ,alist))
     (cond ((listp macro<imp>:alist)
            (setq ,alist
                  (int<imp>:alist:delete/helper ,key ,alist ,equal-fn)))
           ((symbolp macro<imp>:alist)
            (set macro<imp>:alist
                 (int<imp>:alist:delete/helper ,key (eval macro<imp>:alist) ,equal-fn)))

           (t
            (int<imp>:error "int<imp>:alist:delete"
                            '("Unable to delete key from alist; "
                              "alist is not a list or a symbol: "
                              "%S (type: %S)")
                            macro<imp>:alist
                            (typeof macro<imp>:alist))))))
;; (setq test-alist nil)
;; (int<imp>:alist:delete :k test-alist)
;; (int<imp>:alist:update :k :v test-alist)
;; (int<imp>:alist:delete :k2 test-alist)
;; (int<imp>:alist:delete :k test-alist)
;; test-alist


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
