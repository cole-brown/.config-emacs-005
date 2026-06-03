;;; user/config/languages/treesit.el --- tree-sitter config -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    TODO:(datetime:timestamp:insert :rfc-3339:date)
;; Timestamp:  2026-06-03
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  `treesit-auto'
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Treesit-Auto
;;------------------------------------------------------------------------------

;; https://github.com/renzmann/treesit-auto
(use-package treesit-auto

  ;;------------------------------
  :custom
  ;;------------------------------

  ;; `treesit-auto-install'
  ;;-----------------------
  ;; If non-nil, auto install missing tree-sitter grammars.
  ;;
  ;; This variable enables the automatic clone, compile, and
  ;; installation of tree-sitter grammars whenever visiting a file
  ;; that has a compatible tree-sitter mode.  If set to prompt
  ;; treesit-auto will ask for confirmation before downloading the
  ;; grammar.  Additionally, treesit-auto-install-all will skip the
  ;; yes/no prompt when this variable is t.
  (treesit-auto-install 'prompt) ; default: nil

  ;;------------------------------
  :config
  ;;------------------------------
  (global-treesit-auto-mode))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide user (imp-path-relative 'user (imp-path-sans-extension (imp-path-current-file))))
