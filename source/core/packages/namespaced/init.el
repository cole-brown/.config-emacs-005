;;; namespaced/init.el --- LOAD ALL THE NAMESPACES! -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2025-05-15
;; Timestamp:  2025-11-03
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

  ;;------------------------------
  ;; Standalone Libraries
  ;;------------------------------

  (imp "./elisp/init")    ; standalone            ;; TODO: refactor "elisp/dlv/*"
  (imp "./buffer/init")   ; standalone... so far  ;; TODO: refactor "+blah" ("optional") files
  (imp "./unit/init")     ; standalone... so far
  (imp "./datetime/init") ; standalone... so far

  ;;------------------------------
  ;; Libraries
  ;;------------------------------

  (imp "./str/init")   ; requires `elisp'        ;; TODO: finish refactor
  (imp "./theme/init") ; requires `elisp', `str' ;; TODO: finish refactor


  ;; TODO: refactor the rest of 'em:
  ;; (imp "./color/init")
  ;; (imp "./emacs/init")
  ;; (imp "./input/init")
  ;; (imp "./list/init")
  ;; (imp "./output/init")
  ;; (imp "./path/init")
  ;; (imp "./window/init")


  ;; End load timing.
  )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide namespaced)
