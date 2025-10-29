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
                   (imp-path-current-dir))


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
    (imp-path-current-file)

  ;;------------------------------
  ;; Required
  ;;------------------------------

  (imp-parser buffer:/delete)
  (imp-parser buffer:/region)

  ;; TODO: the rest

  ;; (imp-parser buffer:/narrow)
  ;; (imp-parser buffer:/type)
  ;; (imp-parser buffer:/eval)
  ;; (imp-parser buffer:/manage)
  ;; (imp-parser buffer:/name)
  ;; (imp-parser buffer:/point)
  ;; (imp-parser buffer:/line)
  ;; (imp-parser buffer:/search)
  ;; (imp-parser buffer:/yank)


  ;;------------------------------
  ;; Optional
  ;;------------------------------

  ;; (imp-parser buffer:/+commands)
  ;; (imp-parser buffer:/+line-hydra)

  )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide buffer)
