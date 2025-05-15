;;; core/modules/emacs/innit/error.el --- Errors during init -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-04-26
;; Timestamp:  2023-06-22
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Errors during init
;;
;;; Code:


;; TODO:innit: Actually use these errors during start-up.

;;------------------------------------------------------------------------------
;; Custom Error Types
;;------------------------------------------------------------------------------

;; TODO: Trim down to just what I'm using?
(define-error 'innit:error          "Error in Innit Emacs core")
(define-error 'innit:error:hook     "Error in an Innit startup hook"  'innit:error)
(define-error 'innit:error:autoload "Error in Innit's autoloads file" 'innit:error)
(define-error 'innit:error:module   "Error in an Innit module"        'innit:error)
(define-error 'innit:error:private  "Error in private config"         'innit:error)
(define-error 'innit:error:package  "Error with packages"             'innit:error)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide:with-emacs :innit 'error)
