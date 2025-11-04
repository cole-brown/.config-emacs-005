;;; namespaced/buffer/point.el --- Buffer Point Helpers -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2020-11-16
;; Timestamp:  2025-11-03
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Buffer Point Helpers
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Point Utils - Checking Out Things About Point
;;------------------------------------------------------------------------------

;; http://ergoemacs.org/emacs/elisp_determine_cursor_inside_string_or_comment.html
(defun buffer:point:inside-string? ()
  "Return non-nil if inside string, else nil.

Result depends on syntax table's string quote character."
  (let ((result (nth 3 (syntax-ppss))))
    result))


;; http://ergoemacs.org/emacs/elisp_determine_cursor_inside_string_or_comment.html
(defun buffer:point:inside-comment? ()
  "Return non-nil if inside comment, else nil.

Result depends on syntax table's comment character."
  (let ((result (nth 4 (syntax-ppss))))
    result))


;; https://emacs.stackexchange.com/questions/16792/easiest-way-to-check-if-current-line-is-empty-ignoring-whitespace
(defun buffer:point:current-line-empty? ()
  "Return non-nil if on an empty line."
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))
;; (buffer:point:current-line-empty?)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide buffer point)
