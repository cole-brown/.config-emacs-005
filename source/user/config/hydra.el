;;; user/config/hydra/init.el --- hydras, pretty and otherwise -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-07-13
;; Timestamp:  2025-11-17
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; `hydra':
;;   https://github.com/abo-abo/hydra
;;
;; `pretty-hydra' & `major-mode-hydra':
;;   https://github.com/jerrypnz/major-mode-hydra.el
;;
;; `hydra-posframe':
;;   https://github.com/Ladicle/hydra-posframe
;;
;;; Code:


;; ┌──────────────────────────────────═══───────────────────────────────────┐ ;;
;; │                                 ═════                                  │ ;;
;; │           Multi-Headed Monster - Major, Pretty, and Original           │ ;;
;; │                                 ═════                                  │ ;;
;; └──────────────────────────────────═══───────────────────────────────────┘ ;;


;; |----------+-----------+-----------------------+-----------------|
;; | Body     | Head      | Executing NON-HEADS   | Executing HEADS |
;; | Color    | Inherited |                       |                 |
;; |          | Color     |                       |                 |
;; |----------+-----------+-----------------------+-----------------|
;; | amaranth | red       | Disallow and Continue | Continue        |
;; | teal     | blue      | Disallow and Continue | Quit            |
;; | pink     | red       | Allow and Continue    | Continue        |
;; | red      | red       | Allow and Quit        | Continue        |
;; | blue     | blue      | Allow and Quit        | Quit            |
;; |----------+-----------+-----------------------+-----------------|


;;------------------------------------------------------------------------------
;; Hydra
;;------------------------------------------------------------------------------

(use-package hydra)


;;------------------------------------------------------------------------------
;; Major-Mode & Pretty Hydra
;;------------------------------------------------------------------------------
;; NOTE: Major-Mode Hydra includes Pretty-Hydra, but you can install
;; Pretty-Hydra on its own if you want...
;;   https://github.com/jerrypnz/major-mode-hydra.el
;;   https://github.com/jerrypnz/major-mode-hydra.el#pretty-hydra

;; NOTE: Slow Rendering
;; If you experience a slow down in performance when rendering multiple icons
;; simultaneously, you can try setting the following (emacs) variable:
;;   (setq inhibit-compacting-font-caches t)

;; NOTE: Troubleshooting
;;   TODO: Some of my Font Awesome icons are wrong, so I might have to do this...
;; https://github.com/domtronn/all-the-icons.el#troubleshooting


;;------------------------------------------------------------------------------
;; Major-Mode Hydra
;;------------------------------------------------------------------------------
;; https://github.com/jerrypnz/major-mode-hydra.el#major-mode-hydra

(use-package major-mode-hydra
  ;; If you want a keybind:
  ;; :bind
  ;; ("M-SPC" . major-mode-hydra)
  ;; NOTE: If desired, maybe do with `:general' keyword instead so it works
  ;; better with `general' & `evil'? Except that might introduce a circular
  ;; dependency if `general' is still wanting `pretty-hydra' for the 'SPC'
  ;; leader key function.
  )


;;------------------------------------------------------------------------------
;; Pretty Hydra
;;------------------------------------------------------------------------------
;; https://github.com/jerrypnz/major-mode-hydra.el#pretty-hydra

;;------------------------------
;; NOTE: `pretty-hydra' & `all-the-icons':
;;---
;; For pretty icons for your pretty hydras, see the `mantle:user:icon/...'
;; functions (e.g. `mantle:user:icon/font-awesome').
;;
;; Or do it manually:
;;   All-the-Icons: Inserting Icons Directly
;;     https://github.com/domtronn/all-the-icons.el#inserting-icons-directly
;;
;;   The icon alists are prefixed with: `all-the-icons-data/'
;;     (pp all-the-icons-data/fa-icon-alist)
;;------------------------------


;;------------------------------
;; NOTE: No package setup, currently. `major-mode-hydra' pulls it in.
;;------------------------------


;;------------------------------------------------------------------------------
;; Hydra Posframe
;;------------------------------------------------------------------------------

;; [2022-07-13] This can cover up what I'm trying to do in the buffer, which is
;; very annoying and not easy to fix on the spot? Skip using it while I think
;; about it.
;; (use-package hydra-posframe
;;   ;;------------------------------
;;   :hook
;;   ;;------------------------------
;;   (after-init . hydra-posframe-enable)
;;
;;   ;;------------------------------
;;   :custom
;;   ;;------------------------------
;;   ;; Defaults to centered, which is a bit far from any feedback printed into the minibuffer...
;;   (hydra-posframe-poshandler 'posframe-poshandler-frame-center)
;;   ;; TODO: If center is no good, try one of these:
;;   ;; (hydra-posframe-poshandler 'posframe-poshandler-frame-bottom-center)
;;   ;; (hydra-posframe-poshandler 'posframe-poshandler-frame-bottom-left-corner)
;;   )


;;------------------------------------------------------------------------------
;; Other Hydra Things
;;------------------------------------------------------------------------------

(imp user:/mode/hydra/init) ; hydra nesting, `hydra:call'


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide user config hydra)
