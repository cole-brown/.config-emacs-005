;;; core/modules/emacs/innit/advice.el --- Advice Helper Functions -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-12-01
;; Timestamp:  2023-06-22
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Advice Helper Functions
;;
;;; Code:


(imp:require :nub)
(imp:require :innit 'error)


;;------------------------------------------------------------------------------
;; Helpers for Advice...
;;------------------------------------------------------------------------------

(defun innit:advice:func/name:string (func/target func/advice)
  "Return an advice function name string.

FUNC/TARGET must be a string or symbol.
FUNC/ADVICE must be a string or symbol.

Return a string of the function name created by `define-advice'.
Example:
  ;; This creates a function called `load-file@innit-mute'.
  (define-advice load-file (:override (file) innit:mute)
        (load file nil 'nomessage))

  ;; This returns the string: \"load-file@innit:mute\"
  (innit:advice:func/name:string #'load-file 'innit:mute)"
  (let ((func/name "innit:advice:func/name:string"))
    ;;------------------------------
    ;; Check for Errors.
    ;;------------------------------
    (unless (or (stringp func/target)
                (symbolp func/target))
      (nub:error
       :innit
       func/name
       "FUNC/TARGET must be a string or (quote) symbol. Got: %S"
       func/target))
    (unless (or (stringp func/advice)
                (symbolp func/advice))
      (nub:error
       :innit
       func/name
       "FUNC/ADVICE must be a string or (quote) symbol. Got: %S"
       func/advice))

    ;;------------------------------
    ;; Build Advice Function's Name.
    ;;------------------------------
    (concat (if (stringp func/target)
                func/target
              (symbol-name func/target))
            "@"
            (if (stringp func/advice)
                func/advice
              (symbol-name func/advice)))))
;; (innit:advice:func/name:string 'jeff 'advice)
;; (innit:advice:func/name:string #'jeff #'advice)
;; (innit:advice:func/name:string "jeff" "advice")


(defun innit:advice:func/name:symbol (func/target func/advice)
  "Return an advice function name string.

FUNC/TARGET must be a string or symbol.
FUNC/ADVICE must be a string or symbol.

Return a string of the function name created by `define-advice'.
Example:
  ;; This creates a function called `load-file@innit-mute'.
  (define-advice load-file (:override (file) innit:mute)
        (load file nil 'nomessage))

  ;; This returns the symbol: `load-file@innit:mute'
  (innit:advice:func/name:string #'load-file 'innit:mute)

Advice function name returned will be an interned symbol from
`innit:advice:func/name:string' string return."
  (intern (innit:advice:func/name:string func/target
                                         func/advice)))
;; (innit:advice:func/name:symbol 'jeff 'advice)
;; (innit:advice:func/name:symbol #'jeff #'advice)
;; (innit:advice:func/name:symbol "jeff" "advice")


(defun innit:cmd:advice:func/name (func/target func/advice)
  "Return an advice function name string.

FUNC/TARGET must be a string or symbol.
FUNC/ADVICE must be a string or symbol.

Return a string of the function name created by `define-advice'.
Example:
  ;; This creates a function called `load-file@innit-mute'.
  (define-advice load-file (:override (file) innit:mute)
        (load file nil 'nomessage))

  ;; This returns the symbol: `load-file@innit:mute'
  (innit:advice:func/name:string #'load-file 'innit:mute)

Advice function name returned will be an interned symbol from
`innit:advice:func/name:string' string return."
  (interactive "aTarget Function: \nsAdvice Function: ")
  (insert (innit:advice:func/name:string func/target func/advice)))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :innit 'advice)
