;;; namespaced/output/init.el --- Messages, stdout, etc -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2025-11-12
;; Timestamp:  2025-11-12
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;; Code:

;;------------------------------------------------------------------------------
;; Setup
;;------------------------------------------------------------------------------

(imp-path-root-set 'output (imp-path-current-dir))


;;------------------------------------------------------------------------------
;; Load files.
;;------------------------------------------------------------------------------

(imp-timing
    'output
    (imp-path-current-file)

  (imp output:/squelch))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide output)
