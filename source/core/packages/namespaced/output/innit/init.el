;;; core/packages/innit/init.el --- Emacs init "help", innit? -*- lexical-binding: t; -*-
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

(imp-path:root/set :innit
                   (imp-path-current-dir)
                   "init.el")


;;------------------------------------------------------------------------------
;; Load files.
;;------------------------------------------------------------------------------

(imp-timing
    :innit
    "init.el"
    (imp-path-current-dir)

  (imp-load :feature  '(:innit face)
            :filename "face")

  ;; (imp-load :feature  '(:innit vars)
  ;;           :filename "vars")
  ;; (imp-load :feature  '(:innit error)
  ;;           :filename "error")
  ;; (imp-load :feature  '(:innit debug)
  ;;           :filename "debug")
  ;; (imp-load :feature  '(:innit nub)
  ;;           :filename "nub")
  ;; (imp-load :feature  '(:innit os)
  ;;           :filename "os")
  ;; (imp-load :feature  '(:innit time)
  ;;           :filename "time")
  ;; (imp-load :feature  '(:innit optimize)
  ;;           :filename "optimize")
  ;; (imp-load :feature  '(:innit server)
  ;;           :filename "server")
  ;; (imp-load :feature  '(:innit package)
  ;;           :filename "package")
  ;; (imp-load :feature  '(:innit package upgrade mode)
  ;;           :filename "package-upgrade-mode")
  ;; (imp-load :feature  '(:innit package upgrade command)
  ;;           :filename "package-upgrade-command")
  ;; (imp-load :feature  '(:innit squelch)
  ;;           :filename "squelch")
  ;; (imp-load :feature  '(:innit hook)
  ;;           :filename "hook")
  ;; (imp-load :feature  '(:innit advice)
  ;;           :filename "advice")
  ;; (imp-load :feature  '(:innit theme)
  ;;           :filename "theme")

  ;; End load timing.
  )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide :innit)
