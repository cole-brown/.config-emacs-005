;;; core/modules/elisp/utils/init.el --- The Utilities -*- lexical-binding: t; -*-
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


(imp-path-root-set :elisp
                   (imp-path-current-dir)
                   (imp-file-current))


;;------------------------------------------------------------------------------
;; Load our sub-module thingies.
;;------------------------------------------------------------------------------

(imp-timing
    :elisp
    (imp-file-current)
    (imp-path-current-dir)

  (imp-load :feature  '(:elisp types)
            :filename "types")
  (imp-load :feature  '(:elisp utils functions)
            :filename "functions")
  ;; TODO: refactor the rest of 'em.
  ;; (imp-load :feature  '(:elisp utils predicates)
  ;;           :filename "predicates")
  ;; (imp-load :feature  '(:elisp utils test)
  ;;           :filename "test")
  ;; (imp-load :feature  '(:elisp utils units)
  ;;           :filename "units")
  )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide :elisp 'utils)
