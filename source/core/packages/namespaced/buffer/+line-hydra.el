;;; namespaced/buffer/+line-hydra.el --- Join Lines Hydra -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-11-28
;; Timestamp:  2025-11-05
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:


(imp-require elisp:/functions)

;;----------------------------------------------------------------------------
;; Line Joining Hydra, Emacs
;;----------------------------------------------------------------------------

(defun buffer:join-line/no-whitespace (&optional arg)
  "Join this line to previous and delete whitespace at join.

With prefix ARG, join this line to the following instead of previous."
  (interactive "P")
  (join-line arg)
  (delete-horizontal-space))


;; Call `buffer:hydra:join-lines/body' to enter.
(defhydra buffer:hydra:join-lines (:color red  ;; Allow & quit on non-hydra-heads.
                                   :hint none) ;; no hint - just docstr
  "
Join Lines...
_._: ?.?     _>_: ?>?
_e_: ?e?     _E_: ?E?"
  ;;---
  ;; NOTE: Arrow Meanings:
  ;;---
  ;; ↑: Above line.
  ;; ↓: Below line.

  ;; `join-line' does a "smart" trim.
  ("." #'join-line               "↑ `join-line' (\"Smart\" Trim)")
  ("e" (elisp:cmd (join-line 1)) "↓ `join-line' (\"Smart\" Trim)")

  (">" #'buffer:join-line/no-whitespace               "↑ `join-line' (No Whitespace)")
  ("E" (elisp:cmd (buffer:join-line/no-whitespace 1)) "↓ `join-line' (No Whitespace)"))
;; (buffer:hydra:join-lines/body)


;;----------------------------------------------------------------------------
;; Line Joining Hydra, Evil
;;----------------------------------------------------------------------------

;; Evil has some of its own line joining functions, so... it gets its own hydra.
(with-eval-after-load 'evil
  (with-eval-after-load 'evil-collection
    ;; Call `buffer:hydra:join-lines/body' to enter.
    (defhydra buffer:hydra:join-lines (:color red  ;; Allow & quit on non-hydra-heads.
                                       :hint none) ;; no hint - just docstr
      "
Join Lines...
_o_: ?o?     _c_: ?c?
_O_: ?O?     _t_: ?t?"
      ;;---
      ;; NOTE: Arrow Meanings:
      ;;---
      ;; ↑: Above line.
      ;; ↓: Below line.

      ;;------------------------------
      ;; Emacs Functions
      ;;------------------------------
      ("c" #'join-line               "↑ `join-line' (Trim)")
      ("t" (elisp:cmd (join-line 1)) "↓ `join-line' (Trim)")

      ;;------------------------------
      ;; Evil Functions
      ;;------------------------------
      ("o" #'evil-join            (format "%-50s" "↓ `evil-join' (Smart Comments)"))
      ("O" #'evil-join-whitespace (format "%-50s" "↓ `evil-join-whitespace' (As-Is)")))
    ;; (buffer:hydra:join-lines/body)
    ))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide buffer +line-hydra)
