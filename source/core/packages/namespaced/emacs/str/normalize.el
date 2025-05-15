;;; core/modules/emacs/str/normalize.el --- String Normalization -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-09-27
;; Timestamp:  2023-07-11
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Normalize to Strings
;; Normalize Strings?
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Convert to a String
;;------------------------------------------------------------------------------

(defun str:normalize:symbol (symbol)
  "Convert a SYMBOL/keyword name to a string.
Remove \":\" from keyword symbols."
  (replace-regexp-in-string ":" ""
                            (symbol-name symbol)))
;; (str:normalize:symbol 'jeff)
;; (str:normalize:symbol :jeff)
;; (str:normalize:symbol 'str:normalize:symbol)
;; (let ((x 'jeff)) (str:normalize:symbol x))
;; (let* ((jeff "geoff") (x 'jeff)) (str:normalize:symbol x))


;;--------------------------------------------------------------------------------
;; Normalize to String(s)
;;--------------------------------------------------------------------------------

(defun str:normalize:any (input)
  "Normalize INPUT to a string.

INPUT should be a string, symbol (or keyword), function, or nil.
  - nil:      nil
  - \"\":     nil
  - list:     nil; use `str:normalize:each' or `str:normalize:join' instead.
  - string:   Use as-is.
  - symbol:   Convert to string with `str:normalize:symbol'.
  - function: Call it with no parameters and use the output string.
    - If output is not a string, return nil.

Return a string or nil."
  (let ((input (elisp:unquote input)))
    ;; nil and empty string -> nil
    (cond ((or (null input)
               (and (stringp input)
                    (string= "" input))
               ;; List? You should use `str:normalize:each' or `str:normalize:join' instead.
               (listp input))
           nil)

          ;; String? Direct to output.
          ((stringp input)
           input)

          ;; Function? Call function, return string or nil.
          ((or (functionp input)
               (fboundp input))
           (let ((value (funcall input)))
             (if (stringp value)
                 value
               ;; Do not allow non-strings to escape a string normalization function.
               nil)))

          ;; Symbol (or keyword)? Use its name.
          ((symbolp input)
           (str:normalize:symbol input)))))
;; (str:normalize:any nil)
;; (str:normalize:any '(quote foo))
;; (str:normalize:any "")
;; (str:normalize:any "jeff")
;; (str:normalize:any (lambda () "geoff"))
;; (str:normalize:any 'jeff)
;; (let ((jeff "jill actually")) (str:normalize:any jeff))
;; (let ((jeff "jeff quoted so still jeff")) (str:normalize:any 'jeff))


(defun str:normalize:each (&rest inputs)
  "Normalize a list of INPUTS to a list of strings.

Normalize each item in INPUTS using `str:normalize:any', which see.

Filter out nils and return the list of strings."
  (seq-remove #'null
              (seq-map #'str:normalize:any (elisp:unquote inputs))))
;; (str:normalize:each "Test/ing" 'test:test :jeff)
;; (let* ((name "jeff") (name-symbol 'name)) (str:normalize:each "Test/ing" 'name 'name-symbol))
;; (let* ((name "jeff")) (str:normalize:each "Test/ing" 'test:test 'name))
;; #'ignore returns nil, filtered out.
;;   (str:normalize:each "Test/ing" 'ignore :jeff)


(defun str:normalize:join (input &optional separator)
  "Normalize a list of INPUTS to strings, and join strings with SEPARATOR.

Normalize INPUTS via `str:normalize:any'.

If SEPARATOR is a string, use it to join strings. Else join with a space."
  (let ((input (elisp:unquote input)))
    (if (listp input)
        (mapconcat #'str:normalize:any
                   input
                   (if (stringp separator) separator " "))
      (str:normalize:any input))))
;; (str:normalize:join '(jeff jeff))
;; (str:normalize:join '(jeff jeff) "/")
;; (str:normalize:join 'jeff)
;; (str:normalize:join '(quote zenburn) "/")


;;------------------------------------------------------------------------------
;; Normalize/convert to... Not a string?!
;;------------------------------------------------------------------------------
;; confused-travolta.jpg

(defun keyword:normalize:any (input)
  "Normalize INPUT to a keyword.

INPUT should be a keyword, symbol, string, function, or nil.
  - nil:      nil
  - \"\":     nil (i.e. empty string is not allowed)
  - `:':      nil (i.e. keyword that is only the colon is not allowed)
  - keyword:  Use as-is.
  - string:   Convert to a keyword.
  - symbol:   Convert to a keyword.
  - function: Call it with no parameters, return keyword or recurse.

Return a keyword or nil."
  ;; Return nil?
  (cond ((or (null input)
             (and (stringp input)
                  (string= "" input))
             (and (keywordp input)
                  (eq : input)))
         nil)

        ;; Keyword? Use as-is.
        ((keywordp input)
         input)

        ;; String? Ensure leading ":" and intern to get a keyword.
        ((stringp input)
         (intern
          ;; INPUT is allowed to optionally have a ":", so ensure a ":" by:
          ;;   1) Removing the leading ":" if it exists.
          ;;   2) Always prefixing a new ":".
          (concat ":"
                  (string-remove-prefix ":" input))))

        ;; Function? Call it for them and ask ourself to figure out what the
        ;; hell happened. Need this separate from the fallback case in case
        ;; the function returns a keyword - don't want `str:normalize:any' to
        ;; nil that out.
        ((or (functionp input)
             (fboundp input))
         (keyword:normalize:any (funcall input)))

        ;; Something else? Normalize to a string and ask ourself to do the thing.
        (t
         ;; `str' already does a lot of work normalizing stuff.
         ;; So use it.
         (keyword:normalize:any (str:normalize:any input)))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :str 'normalize)
