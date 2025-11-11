;;; core/modules/output/nub/debug-format.el --- Debug Formatting -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2023-06-26
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
;; ║              Oh, you /don't/ want any bugs. Right; ok...               ║
;; ╚════════════════════════════════════════════════════════════════════════╝
;;                                   ──────
;;                    How to Hide Bugs 101: Look for them.
;;                                 ──────────
;;
;; Debug Formatting
;;
;; Some fancier debugging message helpers.
;;
;;; Code:


(imp:require :nub 'internal)
(imp:require :nub 'alist)
(imp:require :nub 'utils)
(imp:require :nub 'variables)
(imp:require :nub 'output)
(imp:require :nub 'debug)


;;------------------------------------------------------------------------------
;; String Fill Set-Up / Initialization
;;------------------------------------------------------------------------------

(defun nub:debug:string:init (user fill-strings)
  "Set up nub debug FILL-STRINGS for USER's debug messages.

FILL-STRINGS should be nil or a list of strings, like: '(\"  \" \"- \")
They will be used to create a cycling fill string for lists:
    item 0 - - - - - - - - value 0
    next item              value 1
    3  - - - - - - - - - - value 2

If FILL-STRINGS is nil, the user will share the default fill-strings but have
their own fill cycle index.

If a user does not set up their own fills (that is, they don't call this), they
will share the default fill-strings _and_ the cycle index with other users."
  (let ((func/name "nub:debug:string:fill"))
    (int<nub>:user:exists? func/name user :error)

    ;; Verify FILL-STRINGS.
    (when (and fill-strings
               (or (not (listp fill-strings))
                   (not (seq-every-p #'stringp fill-strings))))
      (int<nub>:error func/name
                      "FILL-STRINGS must be nil or a list of strings! Got: %S"
                      fill-strings))

    ;; Set fill-strings if they want non-defaults.
    (if fill-strings
        (int<nub>:var:debug:fills:set user fill-strings))

    ;; And create their own index.
    (int<nub>:var:debug:fills/index:set user 0 :create)))


;;------------------------------------------------------------------------------
;; String Fill Functions
;;---
;; For things like:
;;   item 0 - - - - - - - - value 0
;;   next item              value 1
;;   item three - - - - - - value 2
;;   item 4                 value 3
;;   5  - - - - - - - - - - value 4
;;------------------------------------------------------------------------------

(defun nub:debug:string:fill (user length &optional reversed? restart?)
  "Create a fill string of LENGTH.

Using next fill string as determined by USER, RESTART?, `int<nub>:debug:fills'
and `int<nub>:debug:fills/index'.

Increment indexes and such so that repeatedly calling will cycle the fill
strings properly.

If RESTART? is non-nil, resets to the start of fill string sequence first before
creating the fill string.

If REVERSED? is non-nil, reverses the string returned so that lists like this
will always align the fill strings at the end.
  Not Reversed:
    item 0 - - - - - - - - value 0
    next item              value 1
    3 - - - - - - - - - -  value 2
  Reversed:
    item 0 - - - - - - - - value 0
    next item              value 1
    3  - - - - - - - - - - value 2"
  (int<nub>:user:exists? "nub:debug:string:fill" user :error)

  (when restart?
    (int<nub>:var:debug:fills/index:set user 0))

  (let ((index (int<nub>:var:debug:fills/index user))
        (fills (int<nub>:var:debug:fills       user))
        fill-output)
    ;; Increment index to prep for next time.
    (int<nub>:var:debug:fills/index:set user
                                        (1+ index))

    ;; Create a fill string of the proper length.
    (setq fill-output
          (truncate-string-to-width
           ;; Get this index's fill.
           (let* ((fill-string (nth (% index (length fills)) fills))
                  (fill-length (length fill-string))
                  (times (1+ (/ length fill-length))) ; +1 for int math divide flooring odds.
                  fill-list)
             ;; Create the filling from our "whatever width you want" strings by making a long-enough list of the fills.
             (dotimes (_ times)
               (setq fill-list (cons fill-str fill-list)))

             ;; Have long-enough list - convert to a string.
             (apply #'concat fill-list))
           length))

    ;; Return: Forward or Reversed?
    ;;   - Reversed will aligned at the /end/.
    (if reversed?
        (string-reverse fill-output)
      fill-output)))


(defun nub:debug:string:fill/clear (user)
  "Reset USER's debug fill string cycle."
  (int<nub>:user:exists? "nub:debug:string:fill" user :error)

  (int<nub>:var:debug:fills/index:set user 0))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :nub 'debug 'format)
