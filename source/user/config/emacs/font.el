;;; user/config/emacs/font.el --- fonts & icons -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2025-11-17
;; Timestamp:  2026-06-25
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Nerd Font
;;
;;; Code:

;; ┌──────────────────────────────────═══───────────────────────────────────┐
;; │                               Nerd Icons                               │
;; └──────────────────────────────────═══───────────────────────────────────┘

;; Emacs Package: https://github.com/rainstormstudio/nerd-icons.el
;; Nerd Fonts:    https://www.nerdfonts.com
(use-package nerd-icons

  ;; My Nerd Font: Caskaydia Cove
  ;;-----------------------------
  ;; Downloaded & installed manually:
  ;; https://www.nerdfonts.com/font-downloads

  ;; What Font Am I Even?
  ;;---------------------
  ;; To determine font that Emacs is using in here:
  ;;  `M-x describe-char`
  ;; Something should say the name of the font, like this:
  ;;  > ftcrhb:-SAJA-CaskaydiaCove Nerd Font Mono-regular-normal-normal-*-29-*-*-*-m-0-iso10646-1 (#x38)
  ;; So in this case I have "CaskaydiaCove Nerd Font Mono".

  ;;----------------------------
  :custom
  ;;----------------------------

  ;; `nerd-icons-font-family'
  ;;-------------------------
  ;; The Nerd Font you want to use in GUI.
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want.
  (nerd-icons-font-family "CaskaydiaCove Nerd Font Mono")

  ;;----------------------------
  :config
  ;;----------------------------

  ;; Nerd Icons utils like function `/icon/solo'.
  (imp user:/utilities/icons))


;;------------------------------------------------------------------------------
;; Nerd Icons Addons
;;------------------------------------------------------------------------------

;;----------------------------
;; `nerd-icons-completion'
;;----------------------------
;; https://github.com/rainstormstudio/nerd-icons-completion/

;; TODO: This package ruined the ordering of the completion list for buffers.
(use-package nerd-icons-completion
  :after nerd-icons
  :config

  ;; setup for marginalia
  (eval-after-load 'marginalia
    (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

  (nerd-icons-completion-mode))

;;----------------------------
;; `nerd-icons-corfu'
;;----------------------------
;; https://github.com/LuigiPiucco/nerd-icons-corfu#usage

(use-package nerd-icons-corfu
  :after (nerd-icons corfu)
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))


;;----------------------------
;; `nerd-icons-dired'
;;----------------------------
;; https://github.com/rainstormstudio/nerd-icons-dired

(use-package nerd-icons-dired
  :after (nerd-icons dired)
  :hook
  (dired-mode-hook . nerd-icons-dired-mode))


;;----------------------------
;; `treemacs-nerd-icons'
;;----------------------------
;; https://github.com/rainstormstudio/treemacs-nerd-icons

(use-package treemacs-nerd-icons
  :after (nerd-icons treemacs)
  :config
  (treemacs-nerd-icons-config))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide user (imp-path-relative 'user (imp-path-sans-extension (imp-path-current-file))))
