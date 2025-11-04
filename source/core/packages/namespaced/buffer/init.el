;;; namespaced/buffer/init.el --- Buffer Functions -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2020-11-16
;; Timestamp:  2025-11-03
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

(imp-path-root-set 'buffer
                   (imp-path-current-dir))


;;------------------------------------------------------------------------------
;; Set up custom vars.
;;------------------------------------------------------------------------------

(defgroup buffer nil
  "Group namespace for `buffer' defcustoms."
  ;; Not really sure where to stick it..?
  :group 'files)


;;------------------------------------------------------------------------------
;; Load files.
;;------------------------------------------------------------------------------

(imp-timing
    'buffer
    (imp-path-current-file)

  ;;------------------------------
  ;; Required
  ;;------------------------------

  (imp buffer:/delete)
  (imp buffer:/region)

  ;; TODO: the rest

  ;; (imp buffer:/narrow)
  ;; (imp buffer:/type)
  ;; (imp buffer:/eval)
  ;; (imp buffer:/manage)
  ;; (imp buffer:/name)
  ;; (imp buffer:/point)
  ;; (imp buffer:/line)
  ;; (imp buffer:/search)
  ;; (imp buffer:/yank)


  ;;------------------------------
  ;; Optional
  ;;------------------------------

  ;; (imp buffer:/+commands)
  ;; (imp buffer:/+line-hydra)

  )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide buffer)
