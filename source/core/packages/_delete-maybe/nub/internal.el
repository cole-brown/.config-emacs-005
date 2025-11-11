;;; core/modules/output/nub/internal.el --- Internal Functions -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-12-07
;; Timestamp:  2023-06-26
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;                                 ──────────
;; ╔════════════════════════════════════════════════════════════════════════╗
;; ║               What does the error lib use for erroring?                ║
;; ╚════════════════════════════════════════════════════════════════════════╝
;;                                   ──────
;;            At least it's only turtles a bit of the way down...
;;                                 ──────────
;;
;; Internal Functions
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Formatting
;;------------------------------------------------------------------------------

(defun int<nub>:format:message (indent &rest msg)
  "Format MSG into a message formatting string.

INDENT should be nil or a string. Any newline formatting will prepend INDENT to
the next line.

MSG should be string(s), and maybe some keyword(s):

The acceptable keywords are:
  - `:line:each', `:eachline'
    + Concatenate all strings with newlines separator.
  - `:line:new', `:newline'
    + Replace this keyword with a newline character.

Replacements happen before concatenations."
  (let ((func/name "int<nub>:format:message")
        (keywords/valid/car '(:line:each :eachline
                              :line:new  :newline))
        (keywords/valid/cdr '(:line:new  :newline))
        (line/new (concat "\n" (or indent
                                   "")))
        msg/replaced)

    ;; Null MSG is valid; it formats to an empty string.
    (if (or (null msg)
            (and (null (car msg))
                 (= 1 (length msg))))
        (setq msg nil) ;; nil or '(nil) are both nil as far as we care.

      ;;------------------------------
      ;; Validation.
      ;;------------------------------

      ;; Validate first element of MSG.
      (cond ((keywordp (car msg))
             (unless (memq (car msg) keywords/valid/car)
               (error "%s: MSG starts with invalid keyword `%S'! Valid keywords are: %S"
                      func/name
                      (car msg)
                      keywords/valid/car)))

            ((stringp (car msg)))

            (t
             (error "%s: MSG should start with a string or keyword. Got a `%S': %S"
                    func/name
                    (type-of (car msg))
                    (car msg))))

      ;; Validate rest of MSG.
      (unless (seq-reduce (lambda (reduction element)
                            "Check for valid elements."
                            (and reduction
                                 (or (stringp element)
                                     (and (keywordp element)
                                          (memq element keywords/valid/cdr)))))
                          (cdr msg)
                          t)
        (error "%s: Invalid MSG element; MSG should contain strings or valid keywords `%S'. Got: %S"
               func/name
               keywords/valid/cdr
               (cdr msg)))

      ;;------------------------------
      ;; Replacements/substitutions.
      ;;------------------------------
      (dolist (element msg)
        ;; Replace `:line:new' with its string equivalent.
        (cond ((and (keywordp element)
                    (memq element '(:line:new :newline)))
               (push line/new msg/replaced))

              ;; Keep everything else as-is.
              (t
               (push element msg/replaced))))

      ;; Get `msg/replaced' set up for formatting.
      (setq msg/replaced (nreverse msg/replaced)))

    ;;------------------------------
    ;; Format message parts into final message string.
    ;;------------------------------
    ;;  Null is valid; it formats to an empty string.
    (cond ((null msg/replaced)
           "")

          ;; 1) `:line:each' or `:eachline' first, then strings? Separate lines.
          ((memq (car msg/replaced) '(:line:each :eachline))
           ;; Concat strings w/ newline separator.
           (mapconcat #'identity
                      (cdr msg/replaced)
                      line/new))

          ;; 2) String concat (no separator).
          (t
           ;; Glue all the strings together.
           (apply #'concat msg/replaced)))))
;; (int<nub>:format:message nil nil)
;; (int<nub>:format:message nil "hello there")
;; (int<nub>:format:message nil "hello, " "there")
;; (int<nub>:format:message nil :line:each "Hi." "  -> Line 2")
;; (int<nub>:format:message nil :line:new "All" "Line 2")
;; (int<nub>:format:message nil :line:each "Hi." :line:new "  -> Line 4")
;;
;; (int<nub>:format:message ">>" :line:each "Hi." "  -> Line 2")
;; (int<nub>:format:message ">>" :line:each "Hi." :line:new "  -> Line 4")


;;------------------------------------------------------------------------------
;; Errors
;;------------------------------------------------------------------------------
;; nub can't use `nub:output', unless you're after 'output.el' in the load
;; ordering, like 'debug.el'...
;;
;; So here's an error function for our error library.

(defun int<nub>:error (caller msg &rest args)
  "Format an error message for CALLER and signal an error.

CALLER should be a string.

MSG should be:
  - a string
  - a list of strings to `concat'
  - a cons of: '(:line:each . (string-00 ...))
    + This will concat the list-of-strings with a newline between each string.

ARGS will be passed to `format' with the finalized message string."
  ;; Raise an error after figuring out MSG's formatting.
  (apply #'error (format "%s: %s"
                         caller
                         ;; Formatted message based on what we got passed in.
                         ;; Use `flatten-list' to make MSG a list if it's just a string.
                         ;; It also converts cons to lists, but that's ok here.
                         (apply #'int<nub>:format:message
                                nil ;; No indent.
                                (flatten-list msg)))
         ;; Just pass ARGS directly to error - it will do final format.
         args))
;; (int<nub>:error "test-function-name" "hello there")
;; (int<nub>:error "test-function-name" '("hello, " "there"))
;; (int<nub>:error "test-function-name" '(:line:each . ("Hi." "  -> Line 2")))


;;------------------------------------------------------------------------------
;; Debugging
;;------------------------------------------------------------------------------

(defun int<nub>:debug:init ()
  "Initialize `nub' debugging based on Emacs' variables.

Check these and return t/nil based on their values:
  - Emacs variables:
    - `init-file-debug'
    - `debug-on-error'
  - Environment variables:
    - `DEBUG'

Return a boolean value (t/nil)"
  ;; Set our debug variable (`int<nub>:debug?') and Emacs' variables based on inputs.
  (cond
   ;;---
   ;; Environment Variable: DEBUG
   ;;---
   ((and (getenv-internal "DEBUG")
         (not init-file-debug)
         (not debug-on-error))
    (message (mapconcat #'identity
                        '("int<nub>:debug:init: env var DEBUG:"
                          "  Environment vars:"
                          "    DEBUG:           %S   <--"
                          "  Emacs vars:"
                          "    init-file-debug: %S"
                          "    debug-on-error:  %S"
                          "  <-----result-----: %S")
                        "\n")
             (getenv-internal "DEBUG")
             init-file-debug
             debug-on-error
             (and (getenv-internal "DEBUG")
                  (not init-file-debug)
                  (not debug-on-error)))

    ;; `innit:debug:init' does the cascading currently...
    ;; ;; Also cascade debug setting into Emacs?
    ;; (setq init-file-debug   t
    ;;       debug-on-error    t
    ;;       jka-compr-verbose t)

    ;; Return t or nil.
    t)

   ;;---
   ;; CLI Flag: "--debug-init"
   ;;---
   (init-file-debug
    (message (mapconcat #'identity
                        '("int<nub>:debug:init: Emacs var `init-file-debug':"
                          "  Environment vars:"
                          "    DEBUG:           %S"
                          "  Emacs vars:"
                          "    init-file-debug: %S   <--"
                          "    debug-on-error:  %S"
                          "  <-----result-----: %S")
                        "\n")
             (getenv-internal "DEBUG")
             init-file-debug
             debug-on-error
             init-file-debug)

    ;; `innit:debug:init' does the cascading currently...
    ;; ;; Also cascade into Emacs?
    ;; (setq debug-on-error t
    ;;       jka-compr-verbose t)

    ;; Return t or nil.
    t)

   ;;---
   ;; Interactive Flag?
   ;;---
   ;; How did you get this set and not `init-file-debug'? Debugging some small
   ;; piece of init, maybe?
   (debug-on-error
    (message (mapconcat #'identity
                        '("int<nub>:debug:init: Emacs var `debug-on-error':"
                          "  Environment vars:"
                          "    DEBUG:           %S"
                          "  Emacs vars:"
                          "    init-file-debug: %S"
                          "    debug-on-error:  %S   <--"
                          "  <-----result-----: %S")
                        "\n")
             (getenv-internal "DEBUG")
             init-file-debug
             debug-on-error
             debug-on-error)

    ;; `innit:debug:init' does the cascading currently...
    ;; ;; Also cascade into Emacs?
    ;; (setq jka-compr-verbose t)
    ;; ;; Don't set `init-file-debug'?
    ;; ;; (setq init-file-debug t)

    ;; Return t or nil.
    t)

   ;;---
   ;; _-NOT-_ Debugging
   ;;---
   (t
    ;; Be quite about not debugging?
    ;; (message (mapconcat #'identity
    ;;                     '("int<nub>:debug:init: Not Debugging?"
    ;;                       "  Environment vars:"
    ;;                       "    DEBUG:           %S"
    ;;                       "  Emacs vars:"
    ;;                       "    init-file-debug: %S"
    ;;                       "    debug-on-error:  %S"
    ;;                       "  <-----result-----: %S")
    ;;                     "\n")
    ;;          (getenv-internal "DEBUG")
    ;;          init-file-debug
    ;;          debug-on-error
    ;;          nil)

    ;; `innit:debug:init' does the cascading currently...
    ;; ;; Set everything to "no"?
    ;; (setq init-file-debug   nil
    ;;       debug-on-error    nil
    ;;       jka-compr-verbose nil)

    ;; Return t or nil.
    nil)))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :nub 'internal)
