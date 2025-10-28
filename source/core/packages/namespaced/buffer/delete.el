;;; namespaced/buffer/delete.el --- Functions for Deleting (in) Buffers -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-02-24
;; Timestamp:  2025-10-28
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Functions for deleting in buffers...
;; Maybe eventually some functions for deleting buffers...
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Delete Buffer Text
;;------------------------------------------------------------------------------

(defun buffer:delete:word (arg)
  "Delete ARG number of words without putting them in the kill ring.

Kill characters forward until encountering the end of a word."
  (delete-region (point) (progn (forward-word arg) (point))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide buffer delete)
