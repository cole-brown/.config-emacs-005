;;; core/modules/emacs/str/regex.el --- Regular Expressions -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-09-21
;; Timestamp:  2023-06-26
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Regular Expressions
;;
;;; Code:


;;------------------------------------------------------------------------------
;; General Constants
;;------------------------------------------------------------------------------

(defcustom str:rx:default:separators.word
  '(any "-" "_" " ")
  "An `rx' list for what is considered 'word-separators'."
  :group 'str:group)


;;------------------------------------------------------------------------------
;; Regexes
;;------------------------------------------------------------------------------

;;------------------------------
;; Case Sensitivitiy Training!
;;------------------------------

;; <rant>
;; JESUS FUCKING WEPT CASE-FOLD-SEARCH
;; `case-fold-search' will fucking wreck your life if you don't realize it's enabled...
;; T_T
;; </rant>

(defmacro str:rx:with/case.sensitive (&rest body)
  "Run BODY forms with `case-fold-search' disabled."
  `(let ((case-fold-search nil))
     ,@body))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide:with-emacs :str 'regex)
