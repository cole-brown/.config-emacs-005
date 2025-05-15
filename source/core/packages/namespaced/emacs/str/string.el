;;; core/modules/emacs/str/string.el --- String Functions -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2023-04-17
;; Timestamp:  2023-07-11
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; String Functions that don't go in any of the other string function files.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Utils
;;------------------------------------------------------------------------------

(defun str:contains? (needle haystack)
  "Predicate for for whether HAYSTACK string contains NEEDLE string."
  (not (null (string-match-p (regexp-quote needle) haystack))))


(defun str:trim (str &rest args)
  "Remove whitespace from STR.

ARGS can contain:
  - `:left', 'left'
  - `:right', `right'
  - `:full', `full', `:both', `both'
    - default"
  (declare (pure t) (side-effect-free t))
  (let (trim/left
        trim/right)

    ;; Parse args to figure out which side(s) to trim.
    (dolist (arg args)
      (pcase arg
        ((or :left 'left)
         (setq trim/left t))
        ((or :right 'right)
         (setq trim/right t))
        ((or :full 'full :both 'both)
         (setq trim/right t
               trim/left  t))))

    ;; Trim desired side(s) of STR.
    (cond ((and trim/left
                (not trim/right))
           (string-trim-left str))
          ((and trim/right
                (not trim/left))
           (string-trim-right str))
          (t
           (string-trim str)))))
;; (str:trim " foo ")
;; (str:trim " foo " :left)
;; (str:trim " foo " :left :right)
;; (str:trim " foo " :left :both)


(defun str:empty? (str &optional trim-args)
  "Predicate for if STR is nil or empty.

If TRIM-ARGS is non-nil, trim with `str:trim' before checking for empty string."
  (or (null str)
      (string= ""
               (if trim-args
                   (str:trim str)
                 str))))
;; (str:empty? nil)
;; (str:empty? "")
;; (str:empty? " ")
;; (str:empty? " " :full)


;;------------------------------------------------------------------------------
;; Split / Join
;;------------------------------------------------------------------------------

(defun str:split (regex string)
  "Split STRING based on REGEX separators.

Return a list of strings with nils filtered out.

NOTE: Obeys `case-fold-search'! Set `case-fold-search' appropriately for your
needs, or use `str:rx:with/case.sensitive' macro if you want to have
case sensitivity!"
  (declare (pure t) (side-effect-free t))

  ;; Uses character ?\0 (ASCII 0; null) as a marker in the string while working on it.
  (let ((separator (string ?\0)))
    (split-string (save-match-data
                    (replace-regexp-in-string regex
                                              separator
                                              string))
                  separator :omit-nulls)))
;; (str:split (rx (one-or-more (not alphanumeric))) "hello:there")


(defun str:join (separator &rest strings)
  "Join STRINGS with SEPARATOR between them."
  (declare (pure t) (side-effect-free t))
  (string-join strings separator))


(defun str:join/newline (&rest strings)
  "Join STRINGS with \"\\n\" between them."
  (declare (pure t) (side-effect-free t))
  (string-join strings "\n"))


;;------------------------------------------------------------------------------
;; To String
;;------------------------------------------------------------------------------

(defun int<str>:str:print->str (func &rest args)
  "Call FUNC with ARGS; return `standard-output' as a string."
  (with-output-to-string
    (apply func args)))


(defun int<str>:str:insert->str (func &rest args)
  "Call FUNC with ARGS in a temp buffer; return the temp buffer as a string."
  (with-temp-buffer
    (apply func args)
    (buffer-string)))


;;--------------------------------------------------------------------------------
;; Formatting
;;--------------------------------------------------------------------------------

(defun str:format (format-string &rest objects)
  "Format a string out of a FORMAT-STRING and OBJECTS.

FORMAT-STRING should be a string, with optional format control characters.
OBJECTS can be anything, and are substituted into it to make the result string.

See `format' for details."
  (declare (pure t) (side-effect-free t))
  (apply #'format format-string objects))


(defun str:format/newline (format-string-list &rest objects)
  "Format a string out of a list of formatting strings and OBJECTS.

FORMAT-STRING-LIST should be a list of formatting strings, with optional format
control characters. The list will first be joined together into one string using
newline characters.

OBJECTS can be anything, and are substituted into the formatting string to make
the result string.

See `format' for details."
  (declare (pure t) (side-effect-free t))
  (apply #'str:format
         (apply #'str:join/newline format-string-list)
         objects))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :str 'string)
