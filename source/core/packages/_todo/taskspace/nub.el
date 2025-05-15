;;; modules/dev-env/taskspace/nub.el --- Debug/Error/Etc Output via `nub' -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-07-06
;; Timestamp:  2023-06-29
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Debug/Error/Etc Output via `nub'
;;
;;; Code:


(imp:require :nub)


;;------------------------------------------------------------------------------
;; Initialization
;;------------------------------------------------------------------------------

(defun int<taskspace>:nub:init ()
  "Initialize nub user & settings for Taskspace."
  ;; Defaults for all the settings.
  (nub:vars:init :taskspace))


;;------------------------------------------------------------------------------
;; Debugging Toggle
;;------------------------------------------------------------------------------

(defun taskspace:debug:toggle ()
  "Toggle debugging for Taskspace."
  (interactive)
  (nub:debug:toggle :taskspace))


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
(imp:provide :taskspace 'nub)
