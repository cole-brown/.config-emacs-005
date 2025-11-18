;;; user/config/languages/hurl.el --- HashiCorp Configuration Language -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2025-11-17
;; Timestamp:  2025-11-17
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Syntax highlighting for `.hurl' REST client scripts.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; `hurl-mode' (CLI REST client/language)
;;------------------------------------------------------------------------------

;; https://hurl.dev/docs/installation.html
;; https://github.com/JasZhe/hurl-mode
(use-package hurl-mode
  :mode "\\.hurl\\'"

  ;;------------------------------
  :init
  ;;------------------------------
  ;; `package-vs-install' "bug":
  ;;   1. It wants to be interactive.
  ;;   2. It doesn't know what to do if the package is already installed.
  ;; Therefore, hide behind installed check:
  (unless (package-installed-p 'hurl-mode)
    ;; Not on (M)ELPA. Tell Emacs where/how to get it.
    (package-vc-install "https://github.com/JasZhe/hurl-mode")))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide user (imp-path-relative 'user (imp-path-sans-extension (imp-path-current-file))))
