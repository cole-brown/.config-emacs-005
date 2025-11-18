;;; user/config/color.el --- Color, colour. -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2025-11-17
;; Timestamp:  2025-11-17
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Colors, colours, and sundry.
;;
;;; Code:

;;----------------------------------------------------------------------------
;; Chrome?: Colorize Color Codes
;;----------------------------------------------------------------------------

;; https://github.com/DevelopmentCool2449/colorful-mode
;; https://elpa.gnu.org/packages/colorful-mode.html
(use-package colorful-mode
  :demand t
  :diminish

  :custom
  (colorful-use-prefix t)

  ;; TODO: Can it trigger in comments as well?
  ;; #f00
  ;; "#f00"


  :config
  ;; Switch to hooking `colorful-mode' if global is too much.
  (global-colorful-mode +1))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide user (imp-path-relative 'user (imp-path-sans-extension (imp-path-current-file))))
