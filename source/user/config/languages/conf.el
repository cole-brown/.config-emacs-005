;;; user/config/languages/conf.el --- conf files mode -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-09-27
;; Timestamp:  2025-12-11
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Configure conf mode.
;;   "Mode for Unix and Windows Conf files and Java properties."
;;
;; `conf-mode'
;; `conf-space-mode'
;; `conf-unix-mode'
;; `conf-windows-mode'
;; etc.
;;
;;; Code:

;;------------------------------------------------------------------------------
;; `conf-mode'
;;------------------------------------------------------------------------------

(use-package conf-mode
  :ensure nil ; Emacs builtin

  ;;------------------------------
  :init
  ;;------------------------------

  (defun --/hook/conf/settings ()
    "Settings for conf mode(s)."

    ;; disable line wrap
    (setq truncate-lines t))

  ;;------------------------------
  :hook
  ;;------------------------------
  ((conf-mode-hook           . --/hook/conf/settings)
   (conf-colon-mode-hook     . --/hook/conf/settings)
   (conf-desktop-mode-hook   . --/hook/conf/settings)
   (conf-javaprop-mode-hook  . --/hook/conf/settings)
   (conf-ppd-mode-hook       . --/hook/conf/settings)
   (conf-space-mode-hook     . --/hook/conf/settings)
   (conf-toml-mode-hook      . --/hook/conf/settings)
   (conf-unix-mode-hook      . --/hook/conf/settings)
   (conf-windows-mode-hook   . --/hook/conf/settings)
   (conf-xdefaults-mode-hook . --/hook/conf/settings)))



;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide user (imp-path-relative 'user (imp-path-sans-extension (imp-path-current-file))))
