;;; core/modules/output/nub/output.el --- Output Sinks -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-06-03
;; Timestamp:  2023-08-15
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;                                 ──────────
;; ╔════════════════════════════════════════════════════════════════════════╗
;; ║                           Sinks for Sources                            ║
;; ╚════════════════════════════════════════════════════════════════════════╝
;;                                   ──────
;;                      Do _not_ use as a kitchen sink!
;;                                 ──────────
;;
;;; Code:


(require 'seq)
(imp:require :nub 'internal)
(imp:require :nub 'alist)
(imp:require :nub 'variables)


;;------------------------------------------------------------------------------
;; Output Message Helpers
;;------------------------------------------------------------------------------

(defun int<nub>:output:sinks (user level sinks msg args)
  "Output USER's MSG with ARGS to any/all LEVEL SINKS."
  ;; Inhibit minibuffer message if output sink function is `message'?
  (let ((inhibit-message (int<nub>:var:inhibit-message? user level)))
    ;; List of sinks to output to?
    (if (and sinks
             (listp sinks))
        (dolist (sink sinks)
          (int<nub>:output:sink level sink msg args))

      ;; Just one sink.
      (int<nub>:output:sink level sinks msg args))))


(defun int<nub>:output:sink (level sink msg args)
  "Output MSG with ARGS to one SINK at LEVEL."
  ;; If we should use the default/fallback sink, grab that first.
  (when (or (eq sink int<nub>:var:user:fallback)
            (eq sink t))
    (setq sink (int<nub>:var:sink int<nub>:var:user:fallback level)))

  ;;------------------------------
  ;; Output to sink.
  ;;------------------------------
  ;; Do not output at all.
  (cond ((eq sink nil)
         nil)

        ;; Output to the sink function.
        ((functionp sink)
         (apply sink msg args))

        ;; Anything else is an error.
        (t
         (int<nub>:error "int<nub>:output:sink"
                         '(:line:each
                           "Unknown sink? %S"
                           "Output Message:"
                           "───────────────"
                           "%s")
                         sink
                         (apply #'format msg args)))))


(defun int<nub>:output:message (user level msg args)
  "Output MSG w/ ARGS to USER's LEVEL sink.

Decide how to output LEVEL keyword (`int<nub>:output:enabled?') MSG and
ARGS based on current verbosity for the level."
  (let ((func/name "int<nub>:output:message"))
    (int<nub>:user:exists? func/name user :error)

    (let ((sinks           (int<nub>:var:sink     user level))
          (verbosity:level (int<nub>:var:enabled? user level)))
      ;; Should complain about invalid sink function(s).
      (int<nub>:var:sink:verify func/name
                                sinks
                                :error)

      ;; Output message depending on LEVEL's current verbosity.
      (cond
       ;;------------------------------
       ;; Valid Values
       ;;------------------------------
       ;;---
       ;; Normal on/off
       ;;---
       ((eq verbosity:level t)
        ;; Do the normal/default thing.
        (int<nub>:output:sinks user level sinks msg args))

       ((eq verbosity:level nil)
        ;; Do nothing
        nil)

       ;;---
       ;; Solo Function
       ;;---
       ;; Use the specified function.
       ((functionp verbosity:level)
        ;; Ask user if this level should be enabled (give default for level in case they want that).
        (when (funcall verbosity:level level (int<nub>:var:enabled? int<nub>:var:user:fallback level))
          ;; The user's predicate says the level is enabled; output.
          (int<nub>:output:sinks user level sinks msg args)))

       ;;------------------------------
       ;; Errors/Invalids
       ;;------------------------------
       (t
        ;; Don't know what to do with verbosity's value.
        (int<nub>:error func/name
                        '("Verbosity for user '%S' at level '%S' is '%S', "
                          "and we don't know what to do with that.\n"
                          "Output Message:\n"
                          "───────────────\n"
                          "%s")
                        user
                        level
                        verbosity:level
                        (apply #'format msg args)))))))


(defun int<nub>:output:format (caller user level &rest message-format)
  "Format args into a string for formatting user's message with user's args.

Combines CALLER, LEVEL, and USER's MESSAGE-FORMAT into one string for sending to
the user's output function(s) with MESSAGE-FORMAT's args.

NOTE: Is just for the formatting /message/. Args should be passed to `error',
`warn', etc. Or, best: use `int<nub>:output' for a higher-level API.

Proper use:
  (nub:output :error
              \"example-function\"
              '(\"Imagine this '%s' is a long \"
                \"error string: %S %d\")
              some-string something some-integer)
    -> \"[ERROR] 'examples error prefix here': example-function: <...>\"

Alternative/direct use:
  (error (int<nub>:output:format
          \"example-function\"
          :example-user
          :error
          \"Imagine this '%s' is a long \"
          \"error string: %S %d\")
          some-string something some-integer)
    -> \"[ERROR] 'examples error prefix here': example-function: <...>\""
  (int<nub>:user:exists? "int<nub>:output:format" user :error)

  (let* ((prefix (concat
                  (int<nub>:var:prefix user level)
                  caller
                  ": "))
         (indent (make-string (length prefix) ?\s)))
    ;; Tack the prefix onto the message separately! Need to allow callers full
    ;; control over what gets sent into `int<nub>:format:message` in what
    ;; ordering.
    (concat prefix
            ;; Build message.
            (apply #'int<nub>:format:message
                   indent
                   message-format))))


(defun int<nub>:output (user level/prefix level/sink caller msg args)
  "Output a message for USER at LEVEL.

Format to standard message output for the USER with CALLER info, then output
the message with MSG and ARGS to the correct place according to LEVEL's
current verbosity (e.g. #'error for `:error' verbosity normally).

For valid levels, see `nub:output:levels' keywords.

LEVEL/PREFIX should be a level keyword for determining what prefix string to
use, or the prefix string itself.

LEVEL/SINK should be a level keyword for determining what sink function to use,
or the function itself.

If CALLER is nil, uses relative path from `user-emacs-directory' to
the caller's file.
  Examples:
    - \"init.el\"
    - \"core/modules/output/nub/foo.el\"
    - \"/some/path/outside/user-emacs-directory/file.el\"

Uses MSG string/list-of-strings with `int<nub>:output:format' to create
the message format, then applies that format plus any ARGS to the `error'
signaled.

ARGS should be a list (or nil)."
  ;; Try to be real forgiving about what params are since we might be handling
  ;; an error message... but also try to let them know they did something wrong
  ;; so it can be fixed.

  (let ((func/name "int<nub>:output")
        (caller (int<nub>:caller-or-path user caller)))
    ;;------------------------------
    ;; Validate Inputs
    ;;------------------------------
    (int<nub>:user:exists? func/name user :error)

    (unless (stringp caller)
      (int<nub>:output:message
       user
       :warning
       "%s: Invalid CALLER parameter! Should be a string, got: type: %S, value: %S, stringp?: %S"
       (list func/name
             (type-of caller)
             caller
             (stringp caller))))


    (unless (or (keywordp level/prefix) (stringp level/prefix))
      (int<nub>:output:message
       user
       :warning
       "%s: invalid LEVEL/PREFIX parameter! Should be a keyword or a string, got: type: %S, value: %S, keyword/string?: %S"
       (list func/name
             (type-of level/prefix)
             level/prefix
             (or (keywordp level/prefix) (stringp level/prefix)))))

    (unless (or (keywordp level/sink) (stringp level/sink))
      (int<nub>:output:message
       user
       :warning
       "%s: invalid LEVEL/SINK parameter! Should be a keyword or a string, got: type: %S, value: %S, keyword/string?: %S"
       (list func/name
             (type-of level/sink)
             level/sink
             (or (keywordp level/sink) (stringp level/sink)))))

    (unless (or (stringp msg) (listp msg))
      (int<nub>:output:message
       user
       :warning
       "%s: invalid MSG parameter! Should be a list or a string, got: type: %S, value: %S, string/list?: %S"
       (list func/name
             (type-of msg)
             msg
             (or (stringp msg) (listp msg)))))

    (unless (listp args) ;; nil or a list
      (int<nub>:output:message
       user
       :warning
       "%s: invalid ARGS parameter! Should be a list or nil, got: type: %S, value: %S, string/list?: %S"
       (list func/name
             (type-of args)
             args
             (or (stringp args) (listp args)))))

    ;;------------------------------
    ;; Output a Message
    ;;------------------------------
    (cond
     ;;---
     ;; Valid formatting.
     ;;---
     ((listp msg)
      (int<nub>:output:message user
                               level/sink
                               (apply #'int<nub>:output:format caller user level/prefix msg)
                               args))

     ((stringp msg)
      (int<nub>:output:message user
                               level/sink
                               (int<nub>:output:format caller user level/prefix msg)
                               args))

     ;;---
     ;; Invalid formatting.
     ;;---
     ;; Do your best to get something.
     (t
      (int<nub>:output:message
       user
       :error
       (int<nub>:output:format
        caller
        user
        level/prefix
        (format "%s: Invalid MSG - expected list or string. msg: '%S', args: '%S'"
                func/name msg args))
       nil)

      ;; Don't return `int<nub>:output:message' output.
      ;; Unit tests will disable error signaling sometimes so it's best if this returns nil.
      nil))))


;;------------------------------------------------------------------------------
;; Output API - Errors, Warnings, etc
;;------------------------------------------------------------------------------

(defun nub:output (user level caller msg &rest args)
  "Output a message for USER at LEVEL.

Format to standard message output for the USER with CALLER info, then output
the message with MSG and ARGS to the correct place according to LEVEL's
current verbosity (e.g. #'error for `:error' verbosity normally).

For valid levels, see `nub:output:levels' keywords.

If CALLER is nil, uses relative path from `user-emacs-directory' to
the caller's file.
  Examples:
    - \"init.el\"
    - \"core/modules/output/nub/foo.el\"
    - \"/some/path/outside/user-emacs-directory/file.el\"

Uses MSG string/list-of-strings with `int<nub>:output:format' to create
the message format, then applies that format plus any ARGS to the `error'
signaled."
  (int<nub>:output user
                   level
                   level
                   caller
                   msg
                   args))


;; Shorthand.
(defalias 'nub:out 'nub:output)


(defmacro nub:output:sink (user buffer-name pop-to-buffer?)
  "Create & return a nub sink function.

USER should be the nub user.

BUFFER-NAME should be a string.

POP-TO-BUFFER? should be nil/non-nil. If non-nil, `pop-to-buffer' will be called
after every message output to the sink.

Returns a lambda function."
  (let* ((macro<nub>:user user)
         (macro<nub>:buffer buffer-name)
         (macro<nub>:pop?   pop-to-buffer?))
    `(lambda (msg &rest args)
       ,(format (mapconcat #'identity
                           '("Nub output sink function for user `%s'."
                             ""
                             "MSG and ARGS are passed to `format' for creating the string to be output.")
                           "\n")
                macro<nub>:user)
       (let ((buffer (get-buffer-create ,macro<nub>:buffer)))
         (with-current-buffer buffer
           ;; Go to end of the buffer and add this message to an empty/new line.
           (let ((buffer:end (point-max)))
             (goto-char buffer:end)
             (unless (= (point-min) buffer:end)
               (insert "\n"))
             (insert (apply #'format msg args))))

         ;; Show buffer?
         (when ,macro<nub>:pop?
           ;; TODO: Do this only during start-up?
           ;; Don't want to end up with multiple windows after start up, so be a good
           ;; steward and just use the same window as whatever's already here.
           (let ((display-buffer-alist (list (list ,macro<nub>:buffer
                                                   'display-buffer-same-window))))
             (pop-to-buffer buffer nil :no-record)))))))
;; (funcall (nub:output:sink :test "test-buffer" nil)
;;          "hello %s" "there")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :nub 'output)
