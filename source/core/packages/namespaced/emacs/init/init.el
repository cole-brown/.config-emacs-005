;;; namespaced/emacs/init/init.el --- Emacs init "help", innit? -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-04-13
;; Timestamp:  2025-10-28
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

  ;; (imp-parser init:/vars)
  ;; (imp-parser init:/error)
  ;; (imp-parser init:/debug)
  ;; (imp-parser init:/nub)
  ;; (imp-parser init:/os)
  ;; (imp-parser init:/time)
  ;; (imp-parser init:/optimize)
  ;; (imp-parser init:/server)
  ;; (imp-parser init:/package)
  ;; (imp-parser init:/package-upgrade-mode)
  ;; (imp-parser init:/package-upgrade-command)
  ;; (imp-parser init:/squelch)
  ;; (imp-parser init:/hook)
  ;; (imp-parser init:/advice)
  ;; (imp-parser init:/theme)

  ;; End load timing.
  )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide init)
