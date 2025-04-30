;;; imp/error.el --- imp error helpers -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2023-06-22
;; Timestamp:  2023-08-18
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;                                 ──────────
;; ╔════════════════════════════════════════════════════════════════════════╗
;; ║                                 Errors                                 ║
;; ╚════════════════════════════════════════════════════════════════════════╝
;;                                   ──────
;;                           404 - Error Not Found
;;                                 ──────────
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Error
;;------------------------------------------------------------------------------

(defun imp--error (caller string &rest args)
  "Output formatted error message to `imp-output-level' `:error' sinks.

CALLER should be one of:
  - a string or symbol representing the calling function's name
  - a list of such
It can be nil, though it is not suggested.

STRING should be a string, which can have formatting info in it (see `format'),
and will be printed as the debug message.

ARGS should be a list of args for formatting the STRING."
  (imp--output :error
               caller
               string
               args))
;; (imp--error "test-func" "True == %s" "False")
;; (let ((imp-error-function nil)) (imp--error "test-func" "True == %s" "False"))
;; (let ((imp-error-function #'message)) (imp--error "test-func" "True == %s" "False"))


(defun imp--error-if (error? caller string &rest args)
  "If ERROR? is non-nil, format STRING & ARGS and raise an error signal.

Uses `:error' level settings in `imp-output-level'.

CALLER should be one of:
  - a string or symbol representing the calling function's name
  - a list of such
It can be nil, though it is not suggested.

STRING should be a string, which can have formatting info in it (see `format'),
and will be printed as the error message.

ARGS should be a list of args for formatting the STRING."
  (when error?
    (imp--error caller
                string
                args)))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
;; Don't provide - entirely internal to imp.
;; (imp-provide :imp 'error)
