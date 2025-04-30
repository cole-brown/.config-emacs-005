;;; imp/output.el --- imp error helpers -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2025-04-16
;; Timestamp:  2025-04-16
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;                                 ──────────
;; ╔════════════════════════════════════════════════════════════════════════╗
;; ║                                 Output                                 ║
;; ╚════════════════════════════════════════════════════════════════════════╝
;;                                   ──────
;;                                 ──────────
;;
;;; Code:

(require 'seq)

;;------------------------------------------------------------------------------
;; String Helpers
;;------------------------------------------------------------------------------

(defun imp--output-callers (callers)
  "Format a string from CALLERS.

Return nil if no callers."
  (declare (pure t)
           (side-effect-free t))
  (if (stringp callers)
      callers
    (seq-reduce (lambda (output next)
                  "nil-aware concat"
                  (if (and output next)
                      (concat next " ⇐ " output)
                    (or output next)))
                (seq-map #'imp--string-or-nil
                         (reverse callers))
                nil)))
;; (imp--output-callers "bob")
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
      (concat "["
              (string-replace
               " "
               "-"
               (format (concat "%"
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
                               "s")
                       display))
              "]: ")
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
      (dolist (sink sinks)
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
;; The End.
;;------------------------------------------------------------------------------
