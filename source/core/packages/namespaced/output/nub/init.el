;;; core/modules/output/nub/init.el --- Multi-"user" output message helper. -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2020-10-28
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
;; ║               Nub: /noun/ A small lump or protuberance.                ║
;; ╚════════════════════════════════════════════════════════════════════════╝
;;                                   ──────
;;                       Hey... Naming things is hard.
;;                                 ──────────
;;
;; Output messages with output levels and output sinks settings per user.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Set imp Root
;;------------------------------------------------------------------------------

(imp:path:root/set :nub
                   (imp:path:current:dir)
                   "init.el")


;;------------------------------------------------------------------------------
;; Load Files
;;------------------------------------------------------------------------------

(imp:timing
    :nub
    "init.el"
    (imp:path:current:dir)
  (imp:load :feature  '(:nub internal)
            :filename "internal")
  (imp:load :feature  '(:nub alist)
            :filename "alist")
  (imp:load :feature  '(:nub utils)
            :filename "utils")
  (imp:load :feature  '(:nub variables)
            :filename "variables")
  (imp:load :feature  '(:nub output)
            :filename "output")
  (imp:load :feature  '(:nub debug)
            :filename "debug")
  (imp:load :feature  '(:nub debug format)
            :filename "debug-format")
  (imp:load :feature  '(:nub info)
            :filename "info")
  (imp:load :feature  '(:nub warning)
            :filename "warning")
  (imp:load :feature  '(:nub error)
            :filename "error"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide:with-emacs :nub)
