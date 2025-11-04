;;; namespaced/window/init.el --- Emacs Chrome (Windows, Frames...) -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-07-15
;; Timestamp:  2025-11-03
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Emacs 'chrome' as in:
;;   - Emacs 'windows'
;;   - Emacs 'frames'
;;
;; NOTE: Emacs 'windows' are not:
;;   - Windows the operating system
;;   - The operating system's windows
;;   - Emacs frames, which are basically the operating system's windows
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Set up imp.
;;------------------------------------------------------------------------------

(imp-path-root-set 'window
                   (imp-path-current-dir))


;;------------------------------------------------------------------------------
;; Load files.
;;------------------------------------------------------------------------------

(imp-timing
    'window
    (imp-path-current-dir)

  (imp ./window))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide window)
