;;; core/modules/emacs/imp/commands.el --- Command the Imps -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-12-18
;; Timestamp:  2023-06-22
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
;; Custom Variables
;;------------------------------------------------------------------------------

(defgroup imp:group nil
  "Automatically-ish commit/push git repos for note, docs, etc."
  :prefix "imp:"
  :group 'tools)


(defcustom imp:output:features:buffer
  "*imp:features*"
  "Name of the buffer for `imp:features:print' to output a pretty-printed tree
of the features imp has provided."
  :group 'imp:group)


;;------------------------------------------------------------------------------
;; Display Features
;;------------------------------------------------------------------------------

(defun imp:cmd:features:print ()
  "Pretty print `imp:features' to a temp buffer."
  (interactive)
  (pp-display-expression imp:features imp:output:features:buffer))
;; (imp:cmd:features:print)
