;;; imp/string.el --- string helpers -*- lexical-binding: t; -*-
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
;; ║                                 String                                 ║
;; ╚════════════════════════════════════════════════════════════════════════╝
;;                                   ──────
;;                                 ──────────
;;
;; - string
;; - string
;; - also string
;;
;;; Code:


;;------------------------------------------------------------------------------
;; String
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
;; The End.
;;------------------------------------------------------------------------------
