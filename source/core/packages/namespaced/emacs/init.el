;;; modules/tools/init.el --- `:tools' namespace -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-07-11
;; Timestamp:  2023-06-29
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  `:tools' namespace initialization
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Set-Up.
;;------------------------------------------------------------------------------

(imp:path:root/set :tools
                   (imp:path:current:dir))


;;------------------------------------------------------------------------------
;; Loading.
;;------------------------------------------------------------------------------

;; Don't load any of the `:tools' modules - let the user decided what to do.


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :tools)
