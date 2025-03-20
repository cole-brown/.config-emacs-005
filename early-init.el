;;; settings.el --- Settings for Innit -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2025-03-14
;; Timestamp:  2025-03-14
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Settings that the user needs ASAP (e.g. during "early-init.el" or before user
;; init/config) should go here.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Frame
;;------------------------------------------------------------------------------

;; TODO: Initial size / position should depend on what system we're on...

;;----------------------------
;; Initial Frame Size
;;----------------------------
;; Set `initial-frame-alist' for only the first frame created, or set
;; `default-frame-alist' for _all_ frames created.
;;
;; `fullscreen' options are:
;;   - `fullwidth'  - make the frame as wide as possible, don't touch the vertical
;;   - `fullheight' - make the frame as tall as possible, don't touch the horizontal
;;   - `fullboth'   - set height and width to the size of the screen
;;   - `maximized'  - make it, well, maximized
;; The difference between `fullboth' and `maximized' is that you can resize the
;; former with the mouse, while with the latter you cannot.

;;(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;;------------------------------
;; Frame Resize
;;------------------------------

;; Resize based on pixels, not characters. Allows maximizing to actually
;; maximize.
;;(customize-set-variable frame-resize-pixelwise t)

;; TODO: What is this set to by default?
;; But do not resize windows pixelwise, this can cause crashes in some cases
;; when resizing too many windows at once or rapidly.
;; (innit:customize-set-variable window-resize-pixelwise nil)


;;------------------------------
;; Frame UI Elements
;;------------------------------
;; Disable tool, menu, and scrollbars. Too much screen space and clutter when
;; you should be using the keyboard.

;; Menu Bar (File, Edit... Menus): No.
(when (bound-and-true-p menu-bar-mode)
  (menu-bar-mode -1))

;; Tool Bar (New, Open... Buttons): Go away.
(when (bound-and-true-p tool-bar-mode)
  (tool-bar-mode -1))

;; Scroll Bar: Hm..........
;; Not sure.
;;   - On the one hand, get rid of it to get a bit more screen real estate.
;;   - On the other hand, keep it for showing where in buffer we are and how big
;;     buffer is?
(when (bound-and-true-p scroll-bar-mode)
  (scroll-bar-mode -1))

;; Don't want popups for "help text". Put it in the echo area instead.
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))
