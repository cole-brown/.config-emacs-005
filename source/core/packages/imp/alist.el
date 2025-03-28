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

(defun imp--list-flatten-recurse (recurse &rest input)
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
                             (apply #'imp--list-flatten-recurse (1- recurse) item)
                           item))
                        ((eq :recursive recurse)
                         ;; Flatten until flat or stack overflow.
                         (apply #'imp--list-flatten-recurse :recursive item))
                        (t
                         item))
                ;; Just an item, turn into a list so it can be
                ;; concatenated by `mapcan'.
                (list item)))
            input)))
;; (imp--list-flatten-recurse 1 '(foo bar (baz) (qux (quux))))
;; (imp--list-flatten-recurse 1 'foo 'bar '(baz) '(qux (quux)))
;; (imp--list-flatten-recurse :recursive '(foo bar (baz) (qux (quux (quuux)))))


(defun imp--list-flatten (&rest input)
  "Flatten INPUT list to a single list.

Flatten by a max of 10 levels."
  ;; &rest wrapped our INPUT in a list, and `imp--list-flatten-recurse' will
  ;; do the same, so +2 to our max of 10.
  (imp--list-flatten-recurse 12 input))
;; (imp--list-flatten '(foo bar (baz) (qux (quux))))
;; (imp--list-flatten '(foo bar (baz) (qux (quux (quuux)))))


;;------------------------------------------------------------------------------
;; A-list Functions
;;------------------------------------------------------------------------------

(defun imp--alist-valid-key (caller key &optional error?)
  "Return non-nil if KEY is valid.

CALLER should be the calling function's name string.

If ERROR? is non-nil, raise an error for invalid keys. Else return nil/non-nil."
  (if (stringp key)
      (if error?
          (imp--error (imp--output-callers "imp--alist-valid-key" caller)
                          "imp alist cannot have a string key! Key: %S"
                          key)
        nil)
    key))
;; (imp--alist-valid-key "test" 'foo t)
;; (imp--alist-valid-key "test" :foo t)
;; (imp--alist-valid-key "test" "foo" t)


(defun imp--alist-get-value (key alist &optional equal-fn)
  "Get value of KEY's entry in ALIST.

EQUAL-FN should be `eq', `eql', `equal', etc - a function that tests equality of
two alist keys."
  (imp--alist-valid-key "imp--alist-get-value" key :error)
  (alist-get key alist nil nil equal-fn))
;; (imp--alist-get-value :foo test-foo #'equal)


(defun imp--alist-get-pair (key alist &optional equal-fn)
  "Get KEY's entire entry (`car' is KEY, `cdr' is value) from ALIST.

EQUAL-FN should be `eq', `eql', `equal', etc - a function that tests equality of
two alist keys."
  (imp--alist-valid-key "imp--alist-get-pair" key :error)
  (assoc key alist equal-fn))


(defun imp--alist-update-helper (key value alist &optional equal-fn)
  "Set/overwrite an entry in the ALIST. Return the new alist.

If VALUE is nil, it will be set as KEY's value. Use
`imp--alist-delete' if you want to remove it.

EQUAL-FN should be `eq', `eql', `equal', etc - a function that tests equality of
two alist keys.

Return an updated alist, which may or may not be ALIST."
  (imp--alist-valid-key "imp--alist-update-helper" key :error)

  (if (null alist)
      ;; Create a new alist and return it.
      (list (cons key value))

    ;; `setf' creates a new alist sometimes, so buyer beware!
    (setf (alist-get key alist nil nil equal-fn) value)
    alist))
;; (setq test-alist nil)
;; (setq test-alist (imp--alist-update-helper :k :v test-alist))
;; (imp--alist-update-helper :k2 :v2 test-alist)
;; (imp--alist-update-helper :k2 :v2.0 test-alist)
;; test-alist


(defmacro imp--alist-update (key value alist &optional equal-fn)
  "Set/overwrite an entry in the ALIST.

If VALUE is nil, it will be set as KEY's value. Use
`imp--alist-delete' if you want to remove it.

EQUAL-FN should be `eq', `eql', `equal', etc - a function that tests equality of
two alist keys.

Return updated ALIST."
  `(let ((macro<imp>:alist ,alist))
     (cond
      ((listp macro<imp>:alist)
       (setq ,alist
             (imp--alist-update-helper ,key ,value ,alist ,equal-fn)))
      ((symbolp macro<imp>:alist)
       (set macro<imp>:alist
            (imp--alist-update-helper ,key ,value (eval macro<imp>:alist) ,equal-fn)))

      (t
       (imp--error "imp--alist-update"
                       "Unable to update alist: not a list or a symbol: %S (type: %S)"
                       macro<imp>:alist
                       (typeof macro<imp>:alist))))))
;; (setq test<imp>:alist nil)
;; (imp--alist-update :k0 :v0 test<imp>:alist)
;; test<imp>:alist


(defun imp--alist-delete-helper (key alist &optional equal-fn)
  "Remove KEY from ALIST.

EQUAL-FN should be `eq', `eql', `equal', etc - a function that tests equality of
two alist keys.

Return updated ALIST sans KEY."
  (imp--alist-valid-key "imp--alist-delete-helper" key :error)

  ;; If it's null, no need to do anything.
  (unless (null alist)
    (setf (alist-get key alist nil 'remove equal-fn) nil))

  ;; Return the alist.
  alist)


(defmacro imp--alist-delete (key alist &optional equal-fn)
  "Remove KEY from ALIST.

EQUAL-FN should be `eq', `eql', `equal', etc - a function that tests equality of
two alist keys.

Returns ALIST."
  `(let ((macro<imp>:alist ,alist))
     (cond ((listp macro<imp>:alist)
            (setq ,alist
                  (imp--alist-delete-helper ,key ,alist ,equal-fn)))
           ((symbolp macro<imp>:alist)
            (set macro<imp>:alist
                 (imp--alist-delete-helper ,key (eval macro<imp>:alist) ,equal-fn)))

           (t
            (imp--error "imp--alist-delete"
                            '("Unable to delete key from alist; "
                              "alist is not a list or a symbol: "
                              "%S (type: %S)")
                            macro<imp>:alist
                            (typeof macro<imp>:alist))))))
;; (setq test-alist nil)
;; (imp--alist-delete :k test-alist)
;; (imp--alist-update :k :v test-alist)
;; (imp--alist-delete :k2 test-alist)
;; (imp--alist-delete :k test-alist)
;; test-alist


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
