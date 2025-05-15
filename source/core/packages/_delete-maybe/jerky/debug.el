;;; core/modules/elisp/jerky/debug.el --- Debugging functionality for jerky. -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2020-07-14
;; Timestamp:  2023-09-27
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Debugging functionality for jerky.
;;
;;; Code:


(imp:require :nub)


;;------------------------------------------------------------------------------
;; Initialization
;;------------------------------------------------------------------------------

(defun int<jerky>:debug:init ()
  "Initialize jerky debugging."
  (nub:vars:init
   :jerky
   ;; default enabled/disabled per output levels
   :alist:enabled? '((:debug . nil))))

;;------------------------------------------------------------------------------
;; Debugging Toggle
;;------------------------------------------------------------------------------

(defun jerky:debug:toggle ()
  "Toggle debugging for jerky."
  (interactive)
  (nub:debug:toggle :jerky))


;;------------------------------------------------------------------------------
;; Debugging Functions
;;------------------------------------------------------------------------------

;; Just use:
;;   - `nub:debug'
;;   - `nub:debug:func/start'
;;   - `nub:debug:func/end'


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :jerky 'debug)
