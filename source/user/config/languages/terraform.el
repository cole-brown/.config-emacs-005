;;; user/config/languages/terraform.el --- HashiCorp Configuration Language -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2025-11-17
;; Timestamp:  2025-12-10
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Syntax highlighting for `.tf' and `.hcl' files.
;;
;; from 2023-07-23_sn004:/mantle/config/dev-env/languages/terraform.el
;;
;;; Code:


;;------------------------------------------------------------------------------
;; `terraform-mode'
;;------------------------------------------------------------------------------

(use-package terraform-mode
  ;;------------------------------
  :custom
  ;;------------------------------

  ;; `terraform-format-on-save' (default nil)
  (terraform-format-on-save t)

  ;; `terraform fmt` uses 2 spaces per indent level
  (terraform-indent-level 2))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide user (imp-path-relative 'user (imp-path-sans-extension (imp-path-current-file))))
