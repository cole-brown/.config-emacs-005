;;; namespaced/buffer/eval.el --- Function for Eval (in) Buffers -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-09-22
;; Timestamp:  2025-11-03
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Functions for evaluating in buffers...
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Eval
;;------------------------------------------------------------------------------

(defun buffer:eval:defun ()
  "Execute the current buffer as Lisp code.

Top-level forms are evaluated with `eval-defun' so that `defvar'
and `defcustom' forms reset to their default values."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (forward-sexp)
      (eval-defun nil))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide buffer eval)
