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
;; ║                            Errors & Output                             ║
;; ╚════════════════════════════════════════════════════════════════════════╝
;;                                   ──────
;;                           404 - Error Not Found
;;                                 ──────────
;;
;;; Code:

(require 'seq)

;;------------------------------------------------------------------------------
;; Custom Variables
;;------------------------------------------------------------------------------

(defcustom imp-output-buffer "ⓘ-imp-output-ⓘ"
  "Name of the output buffer used by `imp--output-sink'.")


(defcustom imp-output-level
  '((:error      . (:display "ERROR"
                    :sink (error imp--output-sink)))
    (:error:user . (:display "ERROR:user"
                    :sink (user-error imp--output-sink)))
    (:debug      . (:display "debug"
                    :align right ; default/nil: left
                    ;; :sink message
                    :sink (message imp--output-sink)))

    ;; Not really a level, but available to debug messages via
    ;; `imp--debug-newline'.
    (:blank . (;; :sink message
               :sink imp--output-sink)))
  "Output message level (:debug, :error, etc) settings.")


;;------------------------------------------------------------------------------
;; String Helpers
;;------------------------------------------------------------------------------

(defun imp--output-callers (callers)
  "Format a string from CALLERS.

Return nil if no callers."
  (declare (pure t)
           (side-effect-free t))
  (seq-reduce (lambda (output next)
                "nil-aware concat"
                (if (and output next)
                    (concat next " ⇐ " output)
                  (or output next)))
              (seq-map #'imp--string-or-nil
                       (reverse callers))
              nil))
;; (imp--output-callers '("bob" nil))
;; (imp--output-callers '("bob" "alice"))
;; (imp--output-callers '("bob" alice))
;; (imp--output-callers '(nil nil))
;; (imp--output-callers '(test . (some . (deeper . (callers . nil)))))
;; (imp--output-callers '(fail . (cuz . (not . list))))


;;------------------------------------------------------------------------------
;; Output
;;------------------------------------------------------------------------------

(defun imp--output-level-get (level setting)
  "Get a SETTING for an output LEVEL."
  (plist-get (alist-get level imp-output-level)
             setting))
;; (imp--output-level-get :error :display)
;; (imp--output-level-get :debug :sink)


(defun imp--output-prefix (level)
  ;;  "[ERROR     ]: "
  ;;  "[ERROR:user]: "
  ;;  "[warning   ]: "
  ;;  "[     debug]: "
  (if-let ((display (imp--output-level-get level :display)))
      (format (concat "[%"
                      (when (memq (imp--output-level-get level :align) '(nil left)) "-")
                      (format "%d"
                              (length
                               (plist-get
                                (cdr
                                 (seq-reduce (lambda (a b)
                                               (if (> (length (plist-get (cdr a) :display))
                                                      (length (plist-get (cdr b) :display)))
                                                   a
                                                 b))
                                             imp-output-level
                                             nil))
                                :display)))
                      "s]: ")
              display)
    ""))
;; (imp--output-prefix :error)
;; (imp--output-prefix :debug)
;; (imp--output-prefix :blank)


(defun imp--output-sink (message &rest args)
  "Output MESSAGE formatted with ARGS to the `imp-output-buffer'.

Must have signature like `format' & `message'"
  (with-current-buffer (get-buffer-create imp-output-buffer)
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
;; (imp--output-sink "General Kenobi.")
;; (imp--output-sink "Hello there.")


(defun imp--output (level caller string args)
  "Output a message (formatted from STRING & ARGS) from CALLER function.

Output to LEVEL's `:sinks' functions.

LEVEL should be one of the alist keys in `imp--output-prefix'.

CALLER should be one of:
  - a string or symbol representing the calling function's name
  - a list of such
It can be nil, though it is not suggested.

STRING should be:
  - A string (which can have formatting info in it (see `format')).
    Will be printed as the debug message.
  - A list of strings (which can have formatting info in it (see `format')).
    Will be concatenated.

ARGS should be a list of args for formatting the STRING, or nil."
  (when (imp--output-level-get level :sink)
    (let ((sinks (imp--output-level-get level :sink))
          (prefix (imp--output-prefix level))
          (caller (imp--output-callers caller)))

      (unless (listp sinks)
        (setq lists (list sinks)))

      ;; Send to each sink.
      (dolist (sink sinks result)
        ;; Format & send output to the level's output function.
        (apply sink
               (concat prefix
                       caller
                       (if caller ": " "")
                       (cond ((stringp string)
                              string)
                             ((null string)
                              nil)
                             ((listp string)
                              (apply #'concat string))))
               args)))))
;; (imp--output :error "me" "General Kenobi." nil)
;; (imp--output "Hello there.")


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


(defun imp--error-user (caller string &rest args)
  "Create a formatted error message and raise an error signal with it.

Uses `:error' level settings in `imp-output-level'.

CALLER should be one of:
  - a string or symbol representing the calling function's name
  - a list of such
It can be nil, though it is not suggested.

STRING should be a string, which can have formatting info in it (see `format'),
and will be printed as the error message.

ARGS should be a list of args for formatting the STRING."
  (imp--output :error:user
               caller
               string
               args))

;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
;; Don't provide - entirely internal to imp.
;; (imp-provide :imp 'error)
