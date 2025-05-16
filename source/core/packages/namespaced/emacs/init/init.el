;;; namespaced/emacs/init/init.el --- Emacs init "help", innit? -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-04-13
;; Timestamp:  2023-09-27
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
                   (imp-path-current-dir)
                   (imp-file-current))


;;------------------------------------------------------------------------------
;; Load files.
;;------------------------------------------------------------------------------

(imp-timing
    :init
    (imp-file-current)
    (imp-path-current-dir)

  ;; (imp-load :feature  '(:init vars)
  ;;           :filename "vars")
  ;; (imp-load :feature  '(:init error)
  ;;           :filename "error")
  ;; (imp-load :feature  '(:init debug)
  ;;           :filename "debug")
  ;; (imp-load :feature  '(:init nub)
  ;;           :filename "nub")
  ;; (imp-load :feature  '(:init os)
  ;;           :filename "os")
  ;; (imp-load :feature  '(:init time)
  ;;           :filename "time")
  ;; (imp-load :feature  '(:init optimize)
  ;;           :filename "optimize")
  ;; (imp-load :feature  '(:init server)
  ;;           :filename "server")
  ;; (imp-load :feature  '(:init package)
  ;;           :filename "package")
  ;; (imp-load :feature  '(:init package upgrade mode)
  ;;           :filename "package-upgrade-mode")
  ;; (imp-load :feature  '(:init package upgrade command)
  ;;           :filename "package-upgrade-command")
  ;; (imp-load :feature  '(:init squelch)
  ;;           :filename "squelch")
  ;; (imp-load :feature  '(:init hook)
  ;;           :filename "hook")
  ;; (imp-load :feature  '(:init advice)
  ;;           :filename "advice")
  ;; (imp-load :feature  '(:init theme)
  ;;           :filename "theme")

  ;; End load timing.
  )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide :init)
