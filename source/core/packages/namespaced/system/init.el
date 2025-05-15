;;; core/modules/system/init.el --- System of a Host -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-06-07
;; Timestamp:  2023-06-26
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; System of a Host
;;
;; Emacs uses "system" to refer to the host, so we do too.
;; See `system-name', which is your computer name aka machine name aka hostname.
;;   (system-name)
;;
;; TODO:system: Change to use `host' namespace instead?
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Set-Up.
;;------------------------------------------------------------------------------

(imp:path:root/set :system
                   (imp:path:current:dir))


;;------------------------------------------------------------------------------
;; Loading.
;;------------------------------------------------------------------------------

;; Currently don't load any of the `:system' modules.
;; TODO: Should we change that and load them all in here?


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :system)
