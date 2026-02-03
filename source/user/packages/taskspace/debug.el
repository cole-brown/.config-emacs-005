;;; taskspace/debug.el --- debugging -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-10-05
;; Timestamp:  2026-02-03
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Debugging functionality for `taskspace'.
;;
;;; Code:

;;------------------------------------------------------------------------------
;; Vars
;;------------------------------------------------------------------------------

(defvar taskspace--debugging? nil
  "Set to non-nil to enable debug logs.")


;;------------------------------------------------------------------------------
;; Debugging Commands
;;------------------------------------------------------------------------------

(defun taskspace--debug-toggle ()
  "Toggle debugging for taskspace."
  (interactive)
  (setq taskspace--debugging? (not taskspace--debugging?)))


;;------------------------------------------------------------------------------
;; Debugging Functions
;;------------------------------------------------------------------------------

(defmacro taskspace--debug (funcname format &rest args)
  "debug messages

FUNCNAME is string or symbol.

FORMAT is one of:
  - a string
  - a list of strings"
  (declare (indent 1))
  `(when taskspace--debugging?
     (let ((format* ,format))
       (message
        (concat "[taskspace-DEBUG] %S: "
                (if (proper-list-p format*)
                    (apply #'concat format*)
                  format*))
        ,funcname
        ,@args))))
;; (taskspace--debug 'test "Hello %s %s" "foo" "bar")
;; (macroexpand-1 '(taskspace--debug 'test "Hello %s %s" '("foo" "bar")))


(defmacro taskspace--debug-args (funcname &rest args)
  "Return an alist of (NAME VALUE) pairs for each symbol in ARGS.
FUNCNAME is currently ignored, but kept for interface compatibility."
  (declare (indent 1))
  `(when taskspace--debugging?
     (message
      (concat "[taskspace-DEBUG] %S:\n"
              "args:\n"
              (mapconcat (lambda (arg)
                           (format "  - %s = %S\n"
                                   (car arg)
                                   (cdr arg)))
                         (list
                          ,@(mapcar (lambda (a)
                                      `(cons ',a ,a))
                                    args))))
      ,funcname)))
;; (let ((foo 1)
;;       (bar 2)
;;       (taskspace--debugging? t))
;;   (taskspace--debug-args 'test foo bar))
;; (let ((foo 1)
;;       (bar 2)
;;       (taskspace--debugging? t))
;; (macroexpand-1 '(taskspace--debug-args 'test foo bar)))


;;------------------------------------------------------------------------------
;; Error Functions
;;------------------------------------------------------------------------------

(defmacro taskspace--error (funcname format &rest args)
  "error messages

FORMAT is one of:
  - a string
  - a list of strings"
  (declare (indent 1))
  ;; eval inputs once
  (let ((funcname* funcname)
        (format* format)
        (args*   args))
    `(apply #'message
            (concat "[taskspace-ERROR] %S: "
                    (if (proper-list-p ,format*)
                        (apply #'concat ,format*)
                      ,format*))
            ,funcname*
            ,@args*)))
;; (taskspace--debug 'test "Hello %s %s" '("foo" "bar"))
;; (macroexpand-1 '(taskspace--debug 'test "Hello %s %s" '("foo" "bar")))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide taskspace:/debug)
