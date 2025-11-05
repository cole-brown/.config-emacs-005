;;; namespaced/list/alist/internal.el --- Erroring and Private Stuff; Go Away -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-12-07
;; Timestamp:  2025-11-03
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;                                 ──────────
;; ╔════════════════════════════════════════════════════════════════════════╗
;; ║                          Simple-ish Erroring                           ║
;; ╚════════════════════════════════════════════════════════════════════════╝
;;                                 ──────────
;;
;; Bit of Formatting
;;   - `dd` never hurt anyone...
;; Simple-ish Erroring
;;   - I want better errors than "something went wrong"...
;;
;;; Code:



;;------------------------------------------------------------------------------
;; Formatting
;;------------------------------------------------------------------------------

(defun _:alist:string:format (&rest msg)
  "Format MSG into a message formatting string.

MSG should be:
  - string(s)
  - a keyword followed by string(s)

The acceptable keywords are:
  - `:line:each', `:eachline'
    + Concatenate all strings with newlines separator."
  ;;------------------------------
  ;; Formatted message based on what we got passed in.
  ;;------------------------------
  (cond ((null msg)
         "")

        ;; 1) Newline concat? `:line:each' or `:eachline' first, then strings.
        ((and (memq (car msg) '(:line:each :eachline))
              (seq-reduce (lambda (reduction element)
                            "Require all strings."
                            (and reduction
                                 (stringp element)))
                          (cdr msg)
                          t))
         ;; Concat strings w/ newline separator.
         (mapconcat #'identity
                    (cdr msg)
                    "\n"))

        ;; 2) String concat (no separator).
        ((seq-reduce (lambda (reduction element)
                       "Require all strings."
                       (and reduction
                            (stringp element)))
                     msg
                     t)
         ;; Glue all the strings together.
         (apply #'concat msg))

        ;;---
        ;; Error: invalid MSG.
        ;;---
        (t
         (error "_:alist:string:format: Don't know how to format: %S"
                msg))))
;; (_:alist:string:format nil)
;; (_:alist:string:format "hello there")
;; (_:alist:string:format "hello, " "there")
;; (_:alist:string:format :line:each "Hi." "  -> Line 2")


(defun _:alist:callers:format (this callers)
  "Build a 'caller' string.

Builds from THIS (string) and CALLERS (string or nil).

Returns a string."
  ;;------------------------------
  ;; Error Checks:
  ;;------------------------------
  ;; THIS must be a string.
  (cond ((not (stringp this))
         (_:alist:error "_:alist:callers:format"
                         "Invalid THIS param. Expected a string; got: this: %S, callers: %S"
                         this callers))
        ;; CALLERS must be a string if not nil.
        ((and callers
              (not (stringp callers)))
         (_:alist:error "_:alist:callers:format"
                         "Invalid CALLER param. Expected a string; got: callers: %S, this: %S"
                         callers this))

        ;;------------------------------
        ;; Valid: Concat this w/ callers.
        ;;------------------------------
        (t
         (if callers
             (concat this " <-via- " callers)
           this))))
;; (_:alist:callers:format "bob" nil)
;; (_:alist:callers:format "bob" "alice")
;; (_:alist:callers:format "C" (_:alist:callers:format "B" "A"))
;; (_:alist:callers:format nil nil)


;;------------------------------------------------------------------------------
;; Errors
;;------------------------------------------------------------------------------
;; alist can't use `alist:output', unless you're after 'output.el' in the load
;; ordering, like 'debug.el'...
;;
;; So here's an error function for our error library.

(defun _:alist:error (caller msg &rest args)
  "Format an error message for CALLER and signal an error.

CALLER should be a string.

MSG should be:
  - a string
  - a list of strings to `concat'
  - a cons of: '(:line:each . (string-00 ...))
    + This will concat the list-of-strings with newlines.

ARGS will be passed to `format' with the finalized message string."
  ;; Raise an error after figuring out MSG's formatting.
  (apply #'error (format "%s: %s"
                         caller
                         ;; Formatted message based on what we got passed in.
                         ;; Use `flatten-list' to make MSG a list if it's just a string.
                         ;; It also converts cons to lists, but that's ok here.
                         (apply #'_:alist:string:format (flatten-list msg)))
         ;; Just pass ARGS directly to error - it will do final format.
         args))
;; (_:alist:error "test-function-name" "hello there")
;; (_:alist:error "test-function-name" '("hello, " "there"))
;; (_:alist:error "test-function-name" '(:line:each . ("Hi." "  -> Line 2")))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide alist internal)
