;;; core/modules/emacs/alist/init.el --- Better Alist Functions -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-12-15
;; Timestamp:  2023-06-21
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;                                 ──────────
;; ╔════════════════════════════════════════════════════════════════════════╗
;; ║                           Association Lists                            ║
;; ╚════════════════════════════════════════════════════════════════════════╝
;;                                 ──────────
;;
;; - Better alist functions.
;; - Helpful alist functions.
;; - Alist functions that are all namespaced so you can find related functions.
;;   - Y'know, like `assoc' and `alist-get' both start with "alist"- oh.
;; - Other useful things probably.
;;   - Oh, like how to make string alists work.
;;     - Didja know that took special effort? I've forgotten that factoid so
;;       many times I made this...
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Set imp Root
;;------------------------------------------------------------------------------

(imp:path:root/set :alist
                   (imp:path:current:dir)
                   "init.el")


;;------------------------------------------------------------------------------
;; Load Files
;;------------------------------------------------------------------------------

(imp:timing
    :alist
    "init.el"
    (imp:path:current:dir)

  (imp:load :feature  '(:alist internal)
            :filename "internal")

  ;;---
  ;; General/Generic Alist Functionality
  ;;---
  (imp:load :feature  '(:alist type types)
            :filename "type/types") ;; 'generic.el' needs these functions/vars.
  (imp:load :feature  '(:alist generic)
            :filename "generic")

  ;;---
  ;; Typed Alists
  ;;---
  (imp:load :feature  '(:alist type default)
            :filename "type/default")
  (imp:load :feature  '(:alist type keyword)
            :filename "type/keyword")
  (imp:load :feature  '(:alist type string)
            :filename "type/string")

  ;; End load timing.
  )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :alist)
