;;; namespaced/emacs/init/init.el --- Emacs init "help", innit? -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-04-13
;; Timestamp:  2025-10-29
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Emacs init "help", innit?
;; Complicating, innit?
;; Adding structure to the uncomplicated, innit?
;;
;;; Code:

;;------------------------------------------------------------------------------
;; Set up imp.
;;------------------------------------------------------------------------------

(imp-path-root-set :init
                   (imp-path-current-dir))


;;------------------------------------------------------------------------------
;; Load files.
;;------------------------------------------------------------------------------

(imp-timing
    :init
    (imp-path-current-file)

  ;; (imp init:/vars)
  ;; (imp init:/error)
  ;; (imp init:/debug)
  ;; (imp init:/nub)
  ;; (imp init:/os)
  ;; (imp init:/time)
  ;; (imp init:/optimize)
  ;; (imp init:/server)
  ;; (imp init:/package)
  ;; (imp init:/package-upgrade-mode)
  ;; (imp init:/package-upgrade-command)
  ;; (imp init:/squelch)
  ;; (imp init:/hook)
  ;; (imp init:/advice)
  ;; (imp init:/theme)

  ;; End load timing.
  )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide init)
