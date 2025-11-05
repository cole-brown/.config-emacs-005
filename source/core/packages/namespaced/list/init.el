;;; namespaced/list/init.el --- ...what kinda list you want? -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-12-15
;; Timestamp:  2025-11-04
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;; Code:

(imp-path-root-set 'list (imp-path-current-dir))


;;------------------------------------------------------------------------------
;; Load Files
;;------------------------------------------------------------------------------

(imp-timing
    'list
    (imp-path-current-file)

  ;; Plain Old Lists
  ;; TODO: move list funcs to a file here
  ;; (imp ./list)

  ;; Association Lists
  (imp ./alist/init) ; standalone

  ;; TODO: Property Lists
  ;; (imp ./plist/init)

  ;; End load timing.
  )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide list)
