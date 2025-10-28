;;; namespaced/init.el --- LOAD ALL THE NAMESPACES! -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2025-05-15
;; Timestamp:  2025-10-28
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
    "init.el"
    (imp-path-current-dir)

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
  (imp-parser elisp/init    :path pwd) ; standalone

  ;; TODO: finish refactor
  (imp-parser buffer/init   :path pwd) ; standalone... so far

  (imp-parser unit/init     :path pwd) ; standalone... so far

  (imp-parser datetime/init :path pwd) ; standalone... so far

  ;;------------------------------
  ;; Libraries
  ;;------------------------------

  ;; TODO: finish refactor
  (imp-parser str/init :path pwd) ; requires `:elisp'

  ;; TODO: finish refactor
  (imp-parser theme/init :path pwd) ; requires `:elisp', `:str'


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
