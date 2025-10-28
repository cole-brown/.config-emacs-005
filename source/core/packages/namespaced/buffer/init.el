;;; namespaced/buffer/init.el --- Buffer Functions -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2020-11-16
;; Timestamp:  2025-10-28
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Buffer Functions
;;
;; Namespaced for my convenience.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Set up imp.
;;------------------------------------------------------------------------------

(imp-path-root-set :buffer
                   (imp-path-current-dir)
                   "init.el")


;;------------------------------------------------------------------------------
;; Set up custom vars.
;;------------------------------------------------------------------------------

(defgroup buffer nil
  "Group namespace for the `:buffer' defcustoms."
  ;; Not really sure where to stick it..?
  :group 'files)


;;------------------------------------------------------------------------------
;; Load files.
;;------------------------------------------------------------------------------

(imp-timing
    '(:buffer)
    (imp-file-current)
    (imp-path-current-dir)

  ;;------------------------------
  ;; Required
  ;;------------------------------

  (imp-load :feature  '(:buffer delete)
            :filename "delete")
  (imp-load :feature  '(:buffer region)
            :filename "region")

  ;; TODO: the rest

  ;; (imp-load :feature  '(:buffer narrow)
  ;;           :filename "narrow")
  ;; (imp-load :feature  '(:buffer type)
  ;;           :filename "type")
  ;; (imp-load :feature  '(:buffer eval)
  ;;           :filename "eval")
  ;; (imp-load :feature  '(:buffer manage)
  ;;           :filename "manage")
  ;; (imp-load :feature  '(:buffer name)
  ;;           :filename "name")
  ;; (imp-load :feature  '(:buffer point)
  ;;           :filename "point")

  ;; (imp-load :feature  '(:buffer line)
  ;;           :filename "line")
  ;; (imp-load :feature  '(:buffer search)
  ;;           :filename "search")
  ;; (imp-load :feature  '(:buffer yank)
  ;;           :filename "yank")


  ;;------------------------------
  ;; Optional
  ;;------------------------------

  ;; (unless (imp-flag? :buffer '-commands)
  ;;   (imp-load :feature  '(:buffer +commands)
  ;;             :filename "+commands"))

  ;; (unless (imp-flag? :buffer '-hydra)
  ;;   (imp-load :feature  '(:buffer +hydra +line)
  ;;             :filename "+line-hydra"))

  )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide buffer)
