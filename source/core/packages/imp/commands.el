;;; imp/commands.el --- Command the Imps -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-12-18
;; Timestamp:  2025-10-27
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;                                 ──────────
;; ╔════════════════════════════════════════════════════════════════════════╗
;; ║                            Command the Imps                            ║
;; ╚════════════════════════════════════════════════════════════════════════╝
;;                                   ──────
;;                     Interact with the imps; have fun.
;;                                 ──────────
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Display Features
;;------------------------------------------------------------------------------

(defun imp-cmd-features-print ()
  "Pretty print `imp-features' to a temp buffer."
  (interactive)
  (pp-display-expression imp-features imp-output-features-buffer))
;; (imp-cmd-features-print)


;;------------------------------------------------------------------------------
;; Clear & Purge
;;------------------------------------------------------------------------------

(defun imp-cmd-features-nuke ()
  "The most heavy-handed approach to clearing the clutter away.

Set to nil:
  - `imp-features'"
  (interactive)
  (setq imp-features nil))
;; (imp-cmd-features-nuke)
