;;; core/modules/elisp/utils/init.el --- The Utilities -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2020-11-16
;; Timestamp:  2025-10-29
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; The Utilities
;;
;; Useful functions that have no home elsewhere.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Set-Up.
;;------------------------------------------------------------------------------


(imp-path-root-set :elisp
                   (imp-path-current-dir))


;;------------------------------------------------------------------------------
;; Load our sub-module thingies.
;;------------------------------------------------------------------------------

(imp-timing
    :elisp
    (imp-path-current-file)

  (imp ./types)
  (imp ./functions)

  ;; TODO: refactor the rest of 'em.
  ;; (imp ./predicates)
  ;; (imp ./test)
  ;; (imp ./units)
  )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide elisp)
