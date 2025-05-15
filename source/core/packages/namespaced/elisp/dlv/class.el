;;; core/modules/emacs/dlv/class.el --- DLV Helpers for DLV "Classes" -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-10-05
;; Timestamp:  2023-06-21
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; DLV Helpers for DLV "Classes"
;;
;; DLVs are categorized into "classes". These are functions for managing those
;; classes.
;;
;;; Code:


(imp:require :nub)
(imp:require :dlv 'debug)

(imp:require :dlv 'path)


;;------------------------------------------------------------------------------
;; Constants
;;------------------------------------------------------------------------------

(defconst int<dlv>:const:class:prefix "dlv:class"
  "Used to create DLV class symbols.

Used in function: `int<dlv>:class:symbol/create'")


(defconst int<dlv>:const:class:separator "://"
  "Used to create DLV class symbols.

Used in functions:
  - `int<dlv>:class:normalize/str'
  - `int<dlv>:class:normalize/all'")


(defconst int<dlv>:const:normalize:separator "/"
  "Used to create DLV class symbols.

Used in function: `int<dlv>:class:normalize/all'")


;;------------------------------------------------------------------------------
;; DLV Class Symbol Creation
;;------------------------------------------------------------------------------

(defun int<dlv>:class:normalize/prefix (str)
  "Remove unwanted prefixes from STR."
  (replace-regexp-in-string
   (rx string-start (one-or-more (any ":" "/" "\\")))
   ""
   str))
;; (int<dlv>:class:normalize/prefix "://hi")
;; (int<dlv>:class:normalize/prefix "greet://hi")


(defun int<dlv>:class:normalize/str (str)
  "Normalize a STR."
  (if (string= str int<dlv>:const:class:separator)
      ;; Allow special strings through even if they aren't "valid".
      str

    ;; Fix any undesired prefixes.
    (int<dlv>:class:normalize/prefix
     ;; NOTE: Should we replace tilde (~)? It's valid in a symbol name,
     ;; e.g.: (setq ~/test 42)
     ;;
     ;; I think leave it alone until it causes it problems, then evaluate
     ;; replacing it.

     ;; backslash -> slash
     (replace-regexp-in-string
      (rx "\\")
      "/"
      ;; Delete any control chars.
      (replace-regexp-in-string
       (rx (one-or-more control))
       ""
       str)))))
;; As-is:
;;   (int<dlv>:class:normalize/str "jeff")
;;   (int<dlv>:class:normalize/str "d:/foo")
;; A backslash:
;;   (int<dlv>:class:normalize/str "d:\\foo")
;; A control character ('\f'):
;;   (int<dlv>:class:normalize/str "d:\foo")


(defun int<dlv>:class:normalize/any (arg)
  "Normalize ARG into a string intended for a DLV class symbol.

Figure out whan to do to convert ARG to a string before
normalization:
  - null     -> \"\" (empty string)
  - string   -> itself
  - symbol   -> symbol's name
  - function -> `format' \"%s\" of function's return value
Otherwise signals an error."
  ;; Convert to a string, then use `int<dlv>:class:normalize/str'.
  (int<dlv>:class:normalize/str
   (cond ((null arg)
          ;; Empty string for nil?
          "")

         ((stringp arg)
          arg)

         ((symbolp arg)
          (symbol-name arg))

         ((functionp arg)
          (format "%s" (funcall arg)))

         ;; Fail - don't want to be crazy and blindly convert something.
         (t
          (error "%s: Can't convert ARG '%S' to string for creating a class symbol"
                 "int<dlv>:class:normalize/any"
                 arg)))))
;; (int<dlv>:class:normalize/any :foo/bar)
;; (int<dlv>:class:normalize/any ":foo/bar")
;; (int<dlv>:class:normalize/any (lambda () :foo/bar))


(defun int<dlv>:class:normalize/all (args)
  "Turn ARGS into a key string.

(int<dlv>:class:normalize/all '(\"a/b\" \"c\"))
  -> a/b/c
(int<dlv>:class:normalize/all '(\"a/b\" c))
  -> a/b/c
(int<dlv>:class:normalize/all '(\"a\" b c))
  -> a/b/c"
  ;; Nuke `int<dlv>:const:normalize:separator' for special args.
  (replace-regexp-in-string
   (rx-to-string (list 'sequence
                       int<dlv>:const:normalize:separator
                       int<dlv>:const:class:separator
                       int<dlv>:const:normalize:separator)
                 :no-group)
   int<dlv>:const:class:separator

   ;; Combine all the normalized args w/ our separator.
   (string-join
    (if (listp args)
        ;; Normalize a list:
        (seq-filter (lambda (x) (not (null x))) ;; Ignore nils from conversion.
                 ;; Convert a list of whatever into a list of strings.
                 (seq-map #'int<dlv>:class:normalize/any args))
      ;; Normalize a thing into a list of string:
      (list (funcall #'int<dlv>:class:normalize/any args)))
    int<dlv>:const:normalize:separator)))
;; (int<dlv>:class:normalize/all "hi")
;; (int<dlv>:class:normalize/all 'hi)
;; (int<dlv>:class:normalize/all '(h i))
;; (int<dlv>:class:normalize/all '(:h :i))
;; (int<dlv>:class:normalize/all (list :hello int<dlv>:const:class:separator :there))


(defun int<dlv>:class:symbol/build (&rest args)
  "Create a DLV 'class' symbol from ARGS.

ARGS can be a path, strings, symbols, functions...
  - Functions should take no args and return a string.
  - Can use `int<dlv>:const:class:separator' to split up e.g. symbol and path."
  ;; NOTE: Use `intern' instead of `make-symbol' so we can assert thing easier
  ;; in ERT unit tests. Fun fact: `make-symbol' keywords are not `eq' to each
  ;; other?! I thought keywords were unique in that they were constant but I
  ;; guess if not in the object array they're not keywords.
  ;;
  ;; For example, these are false:
  ;;   (eq (make-symbol ":foo") (make-symbol ":foo"))
  ;;   (eq (make-symbol ":foo") (intern ":foo"))
  ;; And this is true:
  ;;   (eq (intern ":foo") (intern ":foo"))
  (intern
   (int<dlv>:class:normalize/all args)))
;; (int<dlv>:class:symbol/build 'org-class 'org-journal)
;; (int<dlv>:class:symbol/build 'org-journal)
;; (int<dlv>:class:symbol/build 'org-journal-dir)
;; (int<dlv>:class:symbol/build "D:\\foo\\bar")
;; (int<dlv>:class:symbol/build "~/foo/bar")


(defun int<dlv>:class:symbol/create (dir.normalized)
  "Create a DLV 'class' symbol for the DIR.NORMALIZED path.

DIR.NORMALIZED should be:
  - A path string.
  - A return value from `int<dlv>:dir:normalize'.

Uses `int<dlv>:const:class:separator' to split up const prefix string and path."
  (int<dlv>:class:symbol/build int<dlv>:const:class:prefix
                               int<dlv>:const:class:separator
                               dir.normalized))
;; (int<dlv>:class:symbol/create (int<dlv>:dir:normalize "D:\\foo\\bar"))
;; (int<dlv>:class:symbol/create (int<dlv>:dir:normalize "~/foo/bar"))


(defun int<dlv>:class:get (dir)
  "Get all the paths that need to be set for DIR, along with a DLV class symbol.

For example, if user's home is \"/home/jeff\":
  (int<dlv>:dir:paths \"~/foo/bar\")
    -> (list (cons \"~/foo/bar\"
                   (int<dlv>:class:symbol/create \"~/foo/bar\"))
             (cons \"/home/jeff/foo/bar\"
                   (int<dlv>:class:symbol/create \"/home/jeff/foo/bar\")))"
  (let ((func/name "int<dlv>:class:get")
        (func/tags '(:class)))
    (nub:debug:func/start
        :dlv
        func/name
        func/tags
      (cons 'dir dir))

    (nub:debug:func/return
        :dlv
        func/name
        func/tags

      ;;------------------------------
      ;; Check for errors.
      ;;------------------------------
      (cond ((null dir)
             (error "%s: DIR must not be nil! dir: %S"
                    func/name
                    dir))
            ((not (stringp dir))
             (error "%s: DIR must be a string! dir: %S %S"
                    func/name
                    (type-of dir)
                    dir))

            ;;------------------------------
            ;; Is a string - OK.
            ;;------------------------------
            (t
             ;; `int<dlv>:path:multiplex' will split up DIR path if it's a problematic one.
             ;; We need to make sure to make a class symbol for all paths we get.
             (let* ((dir/multiplex (int<dlv>:path:multiplex dir :dir))
                    (dir/prefixes  (or (car dir/multiplex) '("")))
                    (dir/path      (cdr dir/multiplex))
                    dirs-and-classes)

               ;; Create a class symbol for each dir.
               (dolist (prefix dir/prefixes)
                 (let ((dir/class (concat prefix dir/path)))
                   (push (cons dir/class
                               (int<dlv>:class:symbol/create dir/class))
                         dirs-and-classes)))

               (nreverse dirs-and-classes)))))))
;; (int<dlv>:class:get "d:/some/path/foo/bar/")
;; (int<dlv>:class:get "d:/home/work/foo/bar/")
;; (int<dlv>:class:get "~/foo/bar")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide:with-emacs :dlv 'class)
