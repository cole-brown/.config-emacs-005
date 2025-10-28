;;; namespaced/datetime/init.el --- Dates and Times and Datetimes -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2020-11-16
;; Timestamp:  2025-10-28
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

(imp-path-root-set :datetime
                   (imp-path-current-dir)
                   (imp-file-current))


;;------------------------------------------------------------------------------
;; Load our sub-module thingies.
;;------------------------------------------------------------------------------

(imp-timing
    '(:datetime)
    (imp-file-current)
    (imp-path-current-dir)

  (imp-parser datetime  :path pwd)
  (imp-parser format    :path pwd)
  (imp-parser timestamp :path pwd))

;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide datetime)
