;;; namespaced/list/alist/init.el --- Better Alist Functions -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-12-15
;; Timestamp:  2025-11-04
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

(imp-path-root-set 'alist (imp-path-current-dir))


;;------------------------------------------------------------------------------
;; Load Files
;;------------------------------------------------------------------------------

(imp-timing
    'alist
    (imp-path-current-file)

  (imp alist:/internal)

  ;;---
  ;; General/Generic Alist Functionality
  ;;---
  (imp alist:/type/types)
  (imp alist:/generic) ; requires `alist:/internal' and `alist:/type/types'

  ;;---
  ;; Typed Alists
  ;;---
  (imp alist:/type/default) ; requires `alist:/internal' and `alist:/generic'
  (imp alist:/type/keyword) ; requires `alist:/internal' and `alist:/generic'
  (imp alist:/type/string)  ; requires `alist:/internal' and `alist:/generic'

  ;; End load timing.
  )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide alist)
