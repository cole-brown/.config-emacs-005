;;; namespaced/init.el --- LOAD ALL THE NAMESPACES! -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2025-05-15
;; Timestamp:  2025-10-29
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Namespaces!
;; Why?
;; I could never find any of Emacs path/string manipulation funcs.
;; Now I can.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Load files.
;;------------------------------------------------------------------------------

(imp-timing
    :namespaced
    (imp-path-current-file)

  ;; Load all namespaced packages in proper order.
  ;; They use each other.

  ;; TODO: order?
  ;;   - elisp
  ;;   - string
  ;;     - requires: elisp

  ;;------------------------------
  ;; Standalone Libraries
  ;;------------------------------
  ;; TODO: finish refactor
  (imp "./elisp/init") ; standalone

  ;; TODO: finish refactor
  (imp "./buffer/init")   ; standalone... so far
  (imp "./unit/init")     ; standalone... so far
  (imp "./datetime/init") ; standalone... so far

  ;;------------------------------
  ;; Libraries
  ;;------------------------------

  ;; TODO: finish refactor
  (imp "./str/init") ; requires `:elisp'

  ;; TODO: finish refactor
  (imp "./theme/init") ; requires `:elisp', `:str'


  ;; TODO: refactor the rest of 'em.


  ;; TODO: Add something like one of theses to early ones?
  ;;
  ;; ;;-!-!-!-!-!-!-!-!-!-!-!-!-!-!-
  ;; ;; Imports
  ;; ;; -------
  ;; ;; Don't rely on anything more than the core modules... or anything at all?
  ;; ;; This should be low-level stuff for use by other code.
  ;; ;;-!-!-!-!-!-!-!-!-!-!-!-!-!-!-
  ;;
  ;; ;;-!-!-!-!-!-!-!-!-!-!-!-!-!-!-
  ;; ;; `ns' Imports Allowed:
  ;; ;; ----------------
  ;; ;;   - :elisp
  ;; ;;-!-!-!-!-!-!-!-!-!-!-!-!-!-!-


  ;; End load timing.
  )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide namespaced)
