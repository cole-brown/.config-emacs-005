;;; core/modules/emacs/alist/type/string.el --- Alist Type: String -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-12-17
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
;;
;; Helper functions for alists that follow a convention for naming and stuff.
;; And will assist you in keeping you alist up-to-date, like it's a variable
;; or something.
;;
;;; Code:


(require 'seq)
(imp:require :alist 'internal)
(imp:require :alist 'generic)


;;------------------------------------------------------------------------------
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;                     Functions for String/`string=' Alists
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;------------------------------------------------------------------------------

(defun alist:string:get/value (key alist &optional default)
  "Get cdr of KEY's entry in ALIST."
  (alist:generic:get/value key alist :type/string default))


(defun alist:string:get/pair (key alist)
  "Get full assoc/entry of KEY in ALIST."
  (alist:generic:get/pair key alist :type/string))


(defmacro alist:string:update (key value alist)
  "Set/overwrite an entry in the ALIST.

SYMBOL/ALIST should be a (quoted) symbol so that this can update it directly.

If VALUE is nil, it will be set as KEY's value. Use
`alist:string:delete' if you want to remove it.

Returns ALIST."
  `(alist:generic:update ,key ,value ,alist :type/string))
;; A global variable:
;;   (setq test-alist nil)
;;   (alist:string:update "k" :v test-alist)
;;   test-alist
;;   (alist:string:update "k" :v test-alist)
;;   test-alist
;;   (alist:string:update "k2" :v2 test-alist)
;;   (alist:string:update "k2" :v2.0 test-alist)
;;   test-alist
;;
;; A scoped variable:
;;   (let (test-alist/let)
;;     (alist:string:update "k" :v test-alist/let)
;;     (alist:string:update "k2" :v2 test-alist/let)
;;     (alist:string:update "k2" :v2.0 test-alist/let)
;;     test-alist/let)


(defmacro alist:string:delete (key alist)
  "Removes KEY from ALIST.

Returns ALIST."
  `(alist:generic:delete ,key ,alist :type/string))
;; (setq test-alist '(("k" . :value) ("k2" . :value2) ("jeff" . :jeff)))
;; (alist:string:delete "k" test-alist)
;; test-alist
;; (alist:string:update "k" :v test-alist)
;; test-alist
;; (alist:string:delete "k2" test-alist)
;; test-alist
;; (alist:string:delete "k" test-alist)
;; test-alist
;; (alist:string:delete "jeff" test-alist)
;; test-alist


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :alist 'type 'string)
