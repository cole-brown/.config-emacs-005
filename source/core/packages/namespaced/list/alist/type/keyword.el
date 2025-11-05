;;; namespaced/list/alist/type/keyword.el --- Alist Type: Keyword -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-12-17
;; Timestamp:  2025-11-04
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
(imp-require 'alist:/internal)
(imp-require 'alist:/generic)


;;------------------------------------------------------------------------------
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;                        Functions for Keyword Alists
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;------------------------------------------------------------------------------

(defun alist:keyword:get/value (key alist &optional default)
  "Get cdr of KEY's entry in ALIST."
  (alist:generic:get/value key alist :type/keyword default))


(defun alist:keyword:get/pair (key alist)
  "Get full assoc/entry of KEY in ALIST."
  (alist:generic:get/pair key alist :type/keyword))


(defmacro alist:keyword:update (key value alist)
  "Set/overwrite an entry in the ALIST.

SYMBOL/ALIST should be a (quoted) symbol so that this can update it directly.

If VALUE is nil, it will be set as KEY's value. Use
`alist:keyword:delete' if you want to remove it.

Returns ALIST."
  `(alist:generic:update ,key ,value ,alist :type/keyword))
;; A global variable:
;;   Error: Invalid key "k".
;;     (setq test-alist nil)
;;     (alist:keyword:update "k" :v test-alist)
;;   Good:
;;     (setq test-alist nil)
;;     (alist:keyword:update :k :v test-alist)
;;     test-alist
;;     (alist:keyword:update :k :v test-alist)
;;     test-alist
;;     (alist:keyword:update :k2 :v2 test-alist)
;;     (alist:keyword:update :k2 :v2.0 test-alist)
;;     test-alist
;;
;; A scoped variable:
;;   (let (test-alist/let)
;;     (alist:keyword:update :k :v test-alist/let)
;;     (alist:keyword:update :k2 :v2 test-alist/let)
;;     (alist:keyword:update :k2 :v2.0 test-alist/let)
;;     test-alist/let)


(defmacro alist:keyword:delete (key alist)
  "Removes KEY from ALIST.

Returns ALIST."
  `(alist:generic:delete ,key ,alist :type/keyword))
;; (setq test-alist '((:k . :value) (:k2 . :value2) (:jeff . :jeff)))
;; (alist:keyword:delete :k test-alist)
;; test-alist
;; (alist:keyword:update :k :v test-alist)
;; test-alist
;; (alist:keyword:delete :k2 test-alist)
;; test-alist
;; (alist:keyword:delete :k test-alist)
;; test-alist
;; (alist:keyword:delete :jeff test-alist)
;; test-alist


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide alist type keyword)
