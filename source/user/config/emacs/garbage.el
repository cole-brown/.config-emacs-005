;;; user/config/emacs/garbage.el --- Configure garbage collection -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2025-11-17
;; Timestamp:  2026-03-20
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Configure garbage collection for post-init.
;;
;; Use either `gcmh' or just fix the `early-init.el' setting.
;;
;;; Code:

(imp-require unit)

;;------------------------------------------------------------------------------
;; Garbage Collector Magic Hacks (`gcmh')
;;------------------------------------------------------------------------------

;; TODO: core config settings go where?
;; TODO: `defcustom' instead of `defconst'
(defconst /setting/init/garbage/use-gcmh t
  "TODO: docstr")

(cond (/setting/init/garbage/use-gcmh
       ;; https://gitlab.com/koral/gcmh
       ;;   - old: https://github.com/emacsmirror/gcmh
       (use-package gcmh
         ;;------------------------------
         :custom
         ;;------------------------------
         ;; The GC introduces annoying pauses and stuttering into our Emacs experience,
         ;; so we use `gcmh' to stave off the GC while we're using Emacs, and provoke it
         ;; when it's idle. However, if the idle delay is too long, we run the risk of
         ;; runaway memory usage in busy sessions. If it's too low, then we may as well
         ;; not be using `gcmh' at all.

         ;; `gcmh-idle-delay'
         ;;------------------
         ;; Idle time to wait in seconds before triggering GC.
         ;;
         ;; If `auto' this is auto computed based on `gcmh-auto-idle-delay-factor'.
         (gcmh-idle-delay 'auto)  ; default is 15s

         ;; `gcmh-auto-idle-delay-factor'
         ;;------------------------------
         ;; Factor to compute the idle delay when in idle-delay auto mode.
         ;;
         ;; The idle delay will be `gcmh-auto-idle-delay-factor' times the
         ;; time the last non idle garbage collection time.
         ;;
         ;; NOTE [2026-03-20]: Default is higher that my old setting, so try
         ;; leaving it alone.
         ;; (gcmh-auto-idle-delay-factor 10)

         ;; `gcmh-high-cons-threshold'
         ;;---------------------------
         ;; High cons GC threshold.
         ;;
         ;; This should be set to a value that makes GC unlikely but does not
         ;; cause OS paging.
         ;;
         ;; NOTE [2026-03-20]: Default is now 1 GiB, so try leaving it alone.
         ;; (setq gcmh-high-cons-threshold (unit:byte 1 'gib))

         ;;------------------------------
         :config
         ;;------------------------------
         (gcmh-mode 1)))

      ((= gc-cons-threshold most-positive-fixnum)
       ;; WARNING:
       ;;   If you do not want `gcmh', be sure to set this back to something
       ;;   reasonable! It was set to `most-positive-fixnum' during early-init
       ;;   in `early-init.el'.

       ;; Set back to Emacs' default value.
       (setq gc-cons-threshold (get 'gc-cons-threshold 'standard-value))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide user (imp-path-relative 'user (imp-path-sans-extension (imp-path-current-file))))
