;;; core/modules/emacs/chrome/init.el --- Emacs Chrome (Windows, Frames...) -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-07-15
;; Timestamp:  2023-09-26
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

(imp:path:root/set :chrome
                   (imp:path:current:dir)
                   "init.el")


;;------------------------------------------------------------------------------
;; Load files.
;;------------------------------------------------------------------------------

(imp:timing
    '(:chrome)
    (imp:file:current)
    (imp:path:current:dir)

  (imp:load :feature  '(:chrome window)
            :filename "window"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :casement)
