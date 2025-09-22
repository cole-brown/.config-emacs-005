;;; namespaced/init.el --- LOAD ALL THE NAMESPACES! -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2025-05-15
;; Timestamp:  2025-09-22
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

  ;; TODO: finish refactor of these:
  (imp-load :feature  :elisp
            :path     "~/ocean/vault/.config/emacs/2025-03-13_sn005/source/core/packages/namespaced/elisp"
            :filename "init.el") ; standalone

  (imp-load :feature  :buffer
            :path     "~/ocean/vault/.config/emacs/2025-03-13_sn005/source/core/packages/namespaced/buffer"
            :filename "init.el") ; standalone... so far

  (imp-load :feature  :str
            :path     "~/ocean/vault/.config/emacs/2025-03-13_sn005/source/core/packages/namespaced/str"
            :filename "init.el") ; requires `:elisp'

  (imp-load :feature  :theme
            :path     "~/ocean/vault/.config/emacs/2025-03-13_sn005/source/core/packages/namespaced/theme"
            :filename "init.el") ; requires `:elisp', `:str'

  (imp-load :feature  :unit
            :path     "~/ocean/vault/.config/emacs/2025-03-13_sn005/source/core/packages/namespaced/unit"
            :filename "init.el") ; standalone... so far

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
(imp-provide :namespaced)
