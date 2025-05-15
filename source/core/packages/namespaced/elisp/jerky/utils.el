;;; core/modules/elisp/jerky/utils.el --- Small utility functions. -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-06-14
;; Timestamp:  2023-06-21
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Small utility functions.
;;
;;; Code:



;;------------------------------------------------------------------------------
;; Utilities
;;------------------------------------------------------------------------------

(defun int<jerky>:using:dlv? ()
  "Return nil/non-nil for whether or not Jerky has its DLV code enabled."
  ;; Defaults to enabled; can be disabled by `-dlv' imp flag, so invert check
  ;; for enabled.
  (not (imp:flag? :jerky -dlv)))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide:with-emacs :jerky 'utils)
