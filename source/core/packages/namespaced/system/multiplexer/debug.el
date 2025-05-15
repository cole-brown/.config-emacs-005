;;; core/modules/system/multiplexer/debug.el --- Debugging/Error Help -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-03-09
;; Timestamp:  2023-08-16
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Debugging/Error Help
;;
;;; Code:


(imp:require :nub)


;;------------------------------------------------------------------------------
;; Initialization
;;------------------------------------------------------------------------------

(defun int<system/multiplexer>:nub:init ()
  "Initialize nub user & settings for multiplexer."
  (nub:vars:init :system/multiplexer))



;;------------------------------------------------------------------------------
;; Debugging Toggle
;;------------------------------------------------------------------------------

(defun system:multiplexer:debug:toggle ()
  "Toggle debugging for dlv."
  (interactive)
  (nub:debug:toggle :system/multiplexer))


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
(imp:provide :system 'multiplexer 'debug)
