;;; namespaced/datetime/init.el --- Dates and Times and Datetimes -*- lexical-binding: t; -*-
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
;; Dates and Times and Datetimes
;;
;; Named datetime formatting.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Set-Up.
;;------------------------------------------------------------------------------

(imp-path-root-set 'datetime (imp-path-current-dir))


;;------------------------------------------------------------------------------
;; Load our sub-module thingies.
;;------------------------------------------------------------------------------

(imp-timing
    'datetime
    (imp-path-current-file)

  (imp ./datetime)
  (imp ./format)
  (imp ./timestamp))

;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide datetime)
