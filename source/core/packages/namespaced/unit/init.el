;;; namespaced/unit/init.el --- Units of Measurements, Conversions, etc -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2020-11-16
;; Timestamp:  2025-11-05
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Kilobytes, hogsheads, nautical miles...
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Set-Up.
;;------------------------------------------------------------------------------


(imp-path-root-set 'unit (imp-path-current-dir))


;;------------------------------------------------------------------------------
;; Load our sub-module thingies.
;;------------------------------------------------------------------------------

(imp-timing
    'unit
    (imp-path-current-file)

  (imp unit:/units))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide unit)
