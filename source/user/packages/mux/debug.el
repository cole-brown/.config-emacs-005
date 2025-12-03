;;; mux/debug.el --- Debugging/Error Help -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-03-09
;; Timestamp:  2025-11-19
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


;;------------------------------------------------------------------------------
;; Vars
;;------------------------------------------------------------------------------

(defvar mux-debugging? nil
  "Set to non-nil to enable debug logs.")


;;------------------------------------------------------------------------------
;; Debugging Commands
;;------------------------------------------------------------------------------

(defun mux-debug-toggle ()
  "Toggle debugging for mux."
  (interactive)
  (setq mux-debugging? (not mux-debugging?))
  (message "mux-debugging?: %S" mux-debugging?))


;;------------------------------------------------------------------------------
;; Debugging Functions
;;------------------------------------------------------------------------------

(defun _mux-debug (funcname format &rest args)
  "debug messages

FORMAT is one of:
  - a string
  - a list of strings"
  (declare (indent 1))
  (when mux-debugging?
    (apply #'message
           (concat "[mux:DEBUG] %S: "
                   (if (proper-list-p format)
                       (apply #'concat format)
                     format))
           funcname
           args)))
;; (_mux-debug 'test '("foo" "bar"))


;;------------------------------------------------------------------------------
;; Error Functions
;;------------------------------------------------------------------------------

(defun _mux-error (funcname format &rest args)
  "error messages

FORMAT is one of:
  - a string
  - a list of strings"
  (declare (indent 1))
  (apply #'error
         (concat "[mux:ERROR] %S: "
                 (if (proper-list-p format)
                     (apply #'concat format)
                   format))
         funcname
         args))
;; (_mux-error 'test '("foo" "bar"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide mux debug)
