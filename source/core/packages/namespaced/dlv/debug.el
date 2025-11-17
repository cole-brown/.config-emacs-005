;;; namespaced/dlv/debug.el --- Debugging functionality for `dlv'. -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-10-05
;; Timestamp:  2025-11-13
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

;;------------------------------------------------------------------------------
;; Vars
;;------------------------------------------------------------------------------

(defvar dlv:debugging? nil
  "Set to non-nil to enable debug logs.")


;;------------------------------------------------------------------------------
;; Debugging Commands
;;------------------------------------------------------------------------------

(defun dlv:debug:toggle ()
  "Toggle debugging for dlv."
  (interactive)
  (setq dlv:debugging? (not dlv:debugging?)))


;;------------------------------------------------------------------------------
;; Debugging Functions
;;------------------------------------------------------------------------------

(defun _:dlv:debug (funcname format &rest args)
  "debug messages

FORMAT is one of:
  - a string
  - a list of strings"
  (when dlv:debugging?
    (apply #'message
           (concat "[dlv:DEBUG] %S: "
                   (if (proper-list-p format)
                       (apply #'concat format)
                     format))
           funcname
           args)))
;; (_:dlv:debug 'test '("foo" "bar"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide dlv debug)
