;;; core/modules/system/secret/debug.el --- Debugging -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2023-06-26
;; Timestamp:  2023-06-26
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Debugging
;;
;;; Code:


(imp:require :nub)


;;------------------------------------------------------------------------------
;; Initialization
;;------------------------------------------------------------------------------

(defun int<system/secret>:nub:init ()
  "Initialize nub user & settings for secret."
  ;; Defaults for all the settings.
  (nub:vars:init :system/secret))


;;------------------------------------------------------------------------------
;; Debugging Toggle
;;------------------------------------------------------------------------------

(defun system:secret:debug:toggle ()
  "Toggle debugging for dlv."
  (interactive)
  (nub:debug:toggle :system/secret))


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
(imp:provide :system 'secret 'debug)
