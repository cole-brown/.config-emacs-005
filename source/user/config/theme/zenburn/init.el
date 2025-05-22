;;; mantle/theme/zenburn/init.el --- Low-Contrast Dark Theme -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-05-12
;; Timestamp:  2023-06-27
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Low-Contrast Dark Theme
;; Best Theme Since Sliced Bread
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Best Theme: Zenburn
;;------------------------------------------------------------------------------
;; Second Best Theme: Also Zenburn
;; Third Best: Tie between Zenburn and a coconut.

(use-package zenburn-theme
  ;; Don't see any reason not to demand the theme.
  :demand t

  ;; ;;----------------------------------------------------------------------------
  ;; :custom
  ;; ;;----------------------------------------------------------------------------
  ;; 
  ;; ;;------------------------------
  ;; ;; Colors
  ;; ;;------------------------------
  ;; ;; NOTE: 'zenburn' uses "-N" for lighter and "+N" for darker in their names
  ;; ;; in the `zenburn-default-colors-alist' variable.
  ;; ;;
  ;; ;; These are some additional colors I'm testing out.
  ;; ;;
  ;; ;; Went to this website and plugged in `zenburn-magenta' and `zenburn-bg'
  ;; ;; with 10 midpoints:
  ;; ;;   https://meyerweb.com/eric/tools/color-blend/#3F3F3F:DC8CC3:10:hex
  ;; ;;
  ;; ;; NOTE: Currently none of these actually override Zenburn's colors;
  ;; ;; they're just additive.
  ;; (zenburn-override-colors-alist '(("zenburn-magenta-bg"   . "#4D464B")
  ;;                                  ("zenburn-magenta-bg-1" . "#5C4D57")
  ;;                                  ("zenburn-magenta-bg-2" . "#6A5463")
  ;;                                  ("zenburn-magenta-bg-3" . "#785B6F")
  ;;                                  ("zenburn-magenta-bg-4" . "#86627B")
  ;;                                  ("zenburn-magenta-bg-5" . "#956987")
  ;;                                  ("zenburn-violet"       . "#a9a1e1")
  ;;                                  ("zenburn-magenta-01"   . "#c67eaf")
  ;;                                  ("zenburn-magenta-03"   . "#9a6288")))


  ;;----------------------------------------------------------------------------
  :config
  ;;----------------------------------------------------------------------------

  ;; ;;------------------------------
  ;; ;; Settings that aren't `defcustom'
  ;; ;;------------------------------
  ;; ;; NOTE: These are all `defvar', so they can't be set in `:custom' section.
  ;;
  ;; (setq zenburn-scale-org-headlines     t ; Scale headings in `org-mode'?
  ;;       zenburn-scale-outline-headlines t ; Scale headings in `outline-mode'?
  ;;       ;; zenburn-use-variable-pitch   t ; Use variable-pitch fonts for some headings and titles
  ;;       )

  ;;------------------------------
  ;; Finally: Load the Theme
  ;;------------------------------
  ;; NOTE: The theme in the `zenburn-theme' package is just called `zenburn'.
  (load-theme 'zenburn t))



;; ;;------------------------------------------------------------------------------
;; ;; Load Other Zenburn Files
;; ;;------------------------------------------------------------------------------
;;
;; (imp:load :feature  '(:mantle theme zenburn org-mode)
;;           :path     (imp:path:current:dir)
;;           :filename "org-mode")
;;
;; (imp:load :feature  '(:mantle theme zenburn whitespace)
;;           :path     (imp:path:current:dir)
;;           :filename "whitespace")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide :user 'config 'theme 'zenburn)
