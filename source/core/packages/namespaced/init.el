;;; namespaced/init.el --- LOAD ALL THE NAMESPACES! -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2025-05-15
;; Timestamp:  2025-11-12
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

  ;; Must be standalone:
  (imp "./elisp/init") ; TODO: refactor "elisp/dlv/*"

  ;; Incidentally standalone:
  (imp "./buffer/init")
  (imp "./color/init")
  (imp "./datetime/init")
  (imp "./list/init") ; contains `list', `alist', `plist'
  (imp "./unit/init")
  (imp "./window/init")
  (imp ./package/init)
  (imp ./output/init)

  ;; Require other namespaced libs:
  (imp "./str/init")   ; requires `elisp'
  (imp "./theme/init") ; requires `elisp', `str'
  (imp "./path/init")  ; requires `elisp', `str', `alist'

  (imp ./system/init)  ; requires `path'
  (imp ./emacs-server) ; requires `path'

  ;; TODO: DLV!
  ;; (imp ./dlv/init)
  )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide namespaced)
