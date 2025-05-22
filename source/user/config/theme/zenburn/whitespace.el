;;; mantle/theme/zenburn/whitespace.el --- Zenburn Tweaks for Whitespace Mode -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-05-16
;; Timestamp:  2023-12-19
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Zenburn doesn't (didn't? I haven't checked in years...) have any predefined
;; faces for `whitespace-mode', and `whitespace-mode' has some eye-bleedingly
;; visceral colors.
;;
;; Here we tell `whitespace-mode' to chill the fuck down and use these Zenburn
;; faces.
;;
;;; Code:


(imp:require :innit 'theme)


;;------------------------------------------------------------------------------
;; Zenburn Theme Tweaks for Whitespace-Mode
;;------------------------------------------------------------------------------

;; Run this after both `zenburn-theme' and `whitespace' have loaded.
;; Need elisp from both in order to evaluate this.
(imp:eval:after (:and zenburn-theme whitespace)

  ;; Set up keys as variables from :
  ;;   - `zenburn-default-colors-alist'
  ;;   - `zenburn-override-colors-alist'
  (zenburn-with-color-variables

   ;;---------------------------------------------------------------------------
   ;; Configure Org-Mode
   ;;---------------------------------------------------------------------------
   ;; I need to customize some whitespace-mode faces - they're not the greatest (IMO)
   ;; in Zenburn...
   ;;
   ;; ...ok, last I checked, default Zenburn's Whitespace-Mode is downright garish.
   ;; Like, ow, my eyeballs...
   ;;------------------------------

   (innit:theme:face:set 'zenburn
     ;;---
     ;; "Good" Whitespace
     ;;---
     (list 'whitespace-space   :foreground zenburn-bg+2)
     (list 'whitespace-newline :foreground zenburn-bg+2)
     ;; whitespace-hspace - leave as-is?
     ;; whitespace-indentation - leave as-is?

     ;;---
     ;; "Ambivalent" Whitespace
     ;;---
     (list 'whitespace-tab :foreground zenburn-orange)
     ;; whitespace-empty - what is this?
     ;; (list 'whitespace-empty :foreground zenburn-red)

     ;;---
     ;; "Bad" Whitespace
     ;;---
     (list 'whitespace-line
           :foreground zenburn-magenta
           :background zenburn-bg-05) ;; Soften the bg color.
     ;; whitespace-trailing - leave as-is?
     ;; whitespace-big-indent - Don't know if I've ever seen this... leave as-is for now.
     ;; Less eyebleedy.
     (list 'whitespace-space-after-tab
           :foreground zenburn-bg-05
           :background zenburn-magenta)
     ;; Less eyebleedy.
     (list 'whitespace-space-before-tab
           :foreground zenburn-bg-05
           :background zenburn-violet))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'theme 'zenburn 'whitespace)
