;;; imp/fundamental.el --- string helpers, etc -*- lexical-binding: t; -*-
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
;; ║                              Fundamentals                              ║
;; ╚════════════════════════════════════════════════════════════════════════╝
;;                                   ──────
;;                    Shit too basic to get its own file.
;;                                 ──────────
;;
;; - strings
;; - symbols
;; - firmaments
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Strings
;;------------------------------------------------------------------------------

(defun imp--string-or-nil (whatever)
  "Normalize WHATEVER to string or nil."
  (declare (pure t)
           (side-effect-free t))
  (if (or (stringp whatever)
          (null whatever))
      whatever
    (format "%S" whatever)))


(defun imp--string-empty? (str &optional trim?)
  "Return non-nil if STR is nil or empty.

If TRIM? is non-nil, use `string-trim' before checking if string is empty."
  (or (null str)
      (string= ""
               (if trim?
                   (string-trim str)
                 str))))
;; (imp--str-empty? nil)
;; (imp--str-empty? "")
;; (imp--str-empty? " ")
;; (imp--str-empty? " " :trim)


;;------------------------------------------------------------------------------
;; Symbols
;;------------------------------------------------------------------------------

(defun imp--unquote (arg)
  "Return ARG unquoted.

Removes both `quote' ('foo) and `function' (#'foo) style quoting.

Originaly from `doom-unquote'."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe arg) '(quote function))
    (setq arg (cadr arg)))
  arg)
;; (imp--unquote 'foo)
;; (imp--unquote '''#'foo)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
