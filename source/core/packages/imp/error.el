;;; core/modules/emacs/imp/error.el --- imp error helpers -*- lexical-binding: t; -*-
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
;; ║                            Errors & Output                             ║
;; ╚════════════════════════════════════════════════════════════════════════╝
;;                                   ──────
;;                           404 - Error Not Found
;;                                 ──────────
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Hack?
;;------------------------------------------------------------------------------
;; TODO: Unhack?
;; TODO: Or at least move it into `imp/output-level' by making that able to use
;;       multiple sinks or something of the like.

(defvar imp--error-sink-hack-debug #'message
  "Also send error messages to this function?

Value should be nil or a function with a signature like `message'.

Error messages get truncated and I haven't figured out how not to truncate:
  Lisp error: (error \"[ERROR   ]: imp/require- Failed to find/load requi...\")

This is really annoying when you error out in 'early-init.el' and cannot expand
the messages to find out what happened... So also send to this function, which
does not truncate.")


;;------------------------------------------------------------------------------
;; Output Functions / Variables
;;------------------------------------------------------------------------------

(defcustom imp/output-buffer "ⓘ-imp/output-ⓘ"
  "Name of the output buffer used by `imp--output-insert'.")


(defcustom imp/output-level
  '((:error . (:prefix "[ERROR   ]: "
               :func error))
    (:error:user . (:prefix "[ERROR   ]:USER-ERROR: "
                    :func user-error))
    (:debug . (:prefix "[   debug]: "
               ;; :func message
               :func imp--output-insert))

    ;; Not really a level, but available to debug messages via
    ;; `imp--debug-newline'.
    (:blank . (:prefix ""
               ;; :func message
               :func imp--output-insert)))
  "Output message level (:debug, :error, etc) settings.")


(defun imp--output-level-get (level setting)
  "Get a SETTING for an output LEVEL."
  (plist-get (alist-get level imp/output-level)
             setting))
;; (imp--output-level-get :error :prefix)
;; (imp--output-level-get :debug :func)


(defun imp--output-insert (message &rest args)
  "Output MESSAGE formatted with ARGS to the `imp/output-buffer'."
  (with-current-buffer (get-buffer-create imp/output-buffer)
    ;; We are now in BUFFER, so just insert the formatted string on a new line at the end.
    (goto-char (point-max))
    (insert (apply #'format
                   ;; Add a string format for newline-or-nothing.
                   (concat "%s" message)
                   ;; Prepend a newline, unless this is a new/empty buffer.
                   (if (= (buffer-size) 0)
                       ""
                     "\n")
                   args))))
;; (imp--output-insert "General Kenobi.")
;; (imp--output-insert "Hello there.")


(defun imp--output (level caller string args)
  "Output a message (formatted from STRING & ARGS) from CALLER function.

LEVEL should be one of the alist keys in `imp--output-prefix'.

CALLER should be a string of the calling function's name.
  - It can be nil, though it is /really/ not suggested.

STRING should be:
  - A string (which can have formatting info in it (see `format')).
    Will be printed as the debug message.
  - A list of strings (which can have formatting info in it (see `format')).
    Will be concatenated and printed as the debug message.

ARGS should be a list of args for formatting the STRING, or nil."
  (when-let ((func (imp--output-level-get level :func))
             (prefix (imp--output-level-get level :prefix)))

    ;; TODO-HACK: Format & send error output to *Messages* buffer?
    (when (and (eq level :error)
               (functionp imp--error-sink-hack-debug))
      (apply imp--error-sink-hack-debug
             (concat prefix
                     caller
                     (if caller ": " "")
                     (cond ((stringp string)
                            string)
                           ((null string)
                            nil)
                           ((listp string)
                            (apply #'concat string))))
             args))

    ;; Format & send output to the level's output function.
    (apply func
           (concat prefix
                   caller
                   (if caller ": " "")
                   (cond ((stringp string)
                          string)
                         ((null string)
                          nil)
                         ((listp string)
                          (apply #'concat string))))
           args)))


;;------------------------------------------------------------------------------
;; String Helpers
;;------------------------------------------------------------------------------

(defun imp--output-callers (this &optional callers)
  "Build a caller string from THIS & CALLERS strings.

`imp' users should use `nub:format:callers' instead of this."
  (let ((this (cond ((null this)
                     nil)
                    ((stringp this)
                     this)
                    (t
                     (format "%S" this))))
        (callers (cond ((null callers)
                        nil)
                       ((stringp callers)
                        callers)
                       (t
                        (format "%S" callers)))))
    (if callers
        (concat this " <-via- " callers)
      this)))
;; (imp--output-callers "bob" nil)
;; (imp--output-callers "bob" "alice")
;; (imp--output-callers "C" (nub:format:callers "B" "A"))
;; (imp--output-callers nil nil)


(defun imp--error (caller string &rest args)
  "Create a formatted error message and raise an error signal with it.

Uses `:error' level settings in `imp/output-level'.

STRING should be a string, which can have formatting info in it (see `format'),
and will be printed as the debug message.

ARGS should be a list of args for formatting the STRING."
  (imp--output :error
                   caller
                   string
                   args))
;; (imp--error "test:func" "True == %s" "False")
;; (let ((imp/error-function nil)) (imp--error "test:func" "True == %s" "False"))
;; (let ((imp/error-function #'message)) (imp--error "test:func" "True == %s" "False"))


(defun imp--error-if (error? caller string &rest args)
  "If ERROR? is non-nil, format STRING & ARGS and raise an error signal.

Uses `:error' level settings in `imp/output-level'.

STRING should be a string, which can have formatting info in it (see `format'),
and will be printed as the debug message.

ARGS should be a list of args for formatting the STRING."
  (when error?
    (imp--error caller
                    string
                    args)))


(defun imp--error-user (caller string &rest args)
  "Create a formatted error message and raise an error signal with it.

Uses `:error' level settings in `imp/output-level'.

STRING should be a string, which can have formatting info in it (see `format'),
and will be printed as the debug message.

ARGS should be a list of args for formatting the STRING."
  (imp--output :error:user
                   caller
                   string
                   args))

;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
;; Don't provide - entirely internal to imp.
;; (imp/provide :imp 'error)
