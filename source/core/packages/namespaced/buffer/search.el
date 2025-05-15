;;; core/modules/emacs/buffer/search.el --- Buffer Searching Functions -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2020-12-04
;; Timestamp:  2023-06-29
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Buffer Searching Functions
;;
;;; Code:


(require 'cl-lib)


(imp:require :buffer 'narrow)


;;------------------------------------------------------------------------------
;; Constants
;;------------------------------------------------------------------------------

(defconst int<buffer>:search:header:max/default 1000
  "Buffer position boundry for `buffer:cmd:search.header'.")


;;------------------------------------------------------------------------------
;; Buffer Searching Functions
;;------------------------------------------------------------------------------

(cl-defun buffer:search:header (string &key max (case 'default))
  "Search for STRING in the first MAX chars of the buffer.

If MAX chars is nil, default to `int<buffer>:search:header:max/default'.

CASE should be:
  - `default'                : Use default/current `case-fold-search'.
  - `ignore' / `insensitive' : Ignore case.
  - `obey'   / `sensitive'   : Obey case."
  (interactive "sSearch for: ")
  (let ((case-fold-search (cond ((eq case 'default)
                                 case-fold-search)
                                ((memq case '(ignore insensitive))
                                 t)
                                ((memq case '(obey sensitive))
                                 nil)
                                (t
                                 case)))
        (max-chars (or max
                       int<buffer>:search:header:max/default))
         found-at-point)

    ;; Like `org-with-wide-buffer' but doesn't depend on `org-mode'.
    (buffer:with-widened
     (goto-char (point-min))
     (setq found-at-point (search-forward
                           ;; search string
                           string
                           ;; search boundry (characters/buffer position)
                           max-chars
                           ;; NOERROR:
                           ;; - nil/default: fail w/ error msg
                           ;; -           t: fail w/ nil return value
                           ;; -       other: fail w/ nil & move point to boundry/end
                           t)))

    ;; Return whatever we found.
    found-at-point))


;;------------------------------
;; Command
;;------------------------------

(defun buffer:cmd:search:header (string)
  "Search for STRING in the first MAX chars of the buffer."
  (interactive "sSearch for: ")
  (let ((found-at-point (buffer:search:header string)))
    ;; Return whatever we found, and if called interactively, also message it.
    (if found-at-point
        (progn
          (goto-char found-at-point)
          (message "Found \"%s\" at buffer position: %d"
                   string found-at-point))
      (message "No \"%s\" in buffer's first %d characters."
               string
               int<buffer>:search:header:max/default))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :buffer 'search)
