;;; core/modules/emacs/dlv/debug.el --- Debugging functionality for `dlv'. -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-10-05
;; Timestamp:  2023-08-16
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Debugging functionality for `dlv'.
;;
;;; Code:


(imp:require :nub)


;;------------------------------------------------------------------------------
;; Initialization
;;------------------------------------------------------------------------------

(defun int<dlv>:debug:init ()
  "Initialize dlv debugging."
  ;; Defaults for all the settings except for the levels/enabled setting.
  (nub:vars:init :dlv
                 :alist:enabled? (list (cons :error   t)
                                       (cons :warning t)
                                       (cons :info    (imp:flag? :dlv +debug))
                                       (cons :debug   (imp:flag? :dlv +debug)))))




;;------------------------------------------------------------------------------
;; Debugging Toggle
;;------------------------------------------------------------------------------

(defun dlv:debug:toggle ()
  "Toggle debugging for dlv."
  (interactive)
  (nub:debug:toggle :dlv))


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
(imp:provide :dlv 'debug)
