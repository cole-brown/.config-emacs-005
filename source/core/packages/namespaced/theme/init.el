;;; namespaced/theme/init.el --- The Utilities -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2020-11-16
;; Timestamp:  2023-06-21
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


(imp-path-root-set :theme
                   (imp-path-current-dir)
                   (imp-file-current))


;;------------------------------------------------------------------------------
;; Load our sub-module thingies.
;;------------------------------------------------------------------------------

(imp-timing
    :theme
    (imp-file-current)
    (imp-path-current-dir)

  (imp-load :feature  '(:theme face)
            :filename "face"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide :theme)
