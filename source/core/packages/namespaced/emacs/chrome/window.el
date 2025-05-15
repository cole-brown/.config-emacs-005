;;; core/modules/emacs/chrome/window.el --- Emacs 'Window' Functions -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-07-15
;; Timestamp:  2023-09-26
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Emacs 'window' (as opposed to OS 'window') functions.
;;
;; Manage windows: kill, quit, etc..
;;
;;; Code:


(require 'cl-lib)


;;------------------------------------------------------------------------------
;; Kill Functions
;;------------------------------------------------------------------------------
;; See `:buffer:manage' for similar functions.
;;   git-root://core/modules/emacs/buffer/manage.el

;; Like `kill-buffer-ask' but no confirmation for unmodified buffers.
(defun window:kill-or-quit (&optional quit window)
  "Default to kill, will quit instead on non-nil prefix arg (QUIT).

The inverse of `quit-window' / `window:quit-or-kill' - defaults to kill, will
quit (bury) on prefix arg QUIT.

WINDOW must be a live window and defaults to the current/selected one."
  (interactive "P")
  (quit-restore-window window (if quit 'bury 'kill)))
;; (window:kill-or-quit)
;; (window:kill-or-quit t)


(defun window:quit-or-kill (&optional kill window)
  "Default to quit, will kill instead on non-nil prefix arg (KILL).

The inverse of `window:kill-or-quit' - defaults to quit (bury), will
kill on prefix arg KILL.

WINDOW must be a live window and defaults to the current/selected one."
  (interactive "P")
  (quit-window kill window))
;; (window:quit-or-kill)
;; (window:quit-or-kill t)


;;------------------------------------------------------------------------------
;; Visible?
;;------------------------------------------------------------------------------

(defun window:list/visible (&optional window-list)
  "Return a list of the visible, non-popup (dedicated) windows from WINDOW-LIST.

If WINDOW-LIST is nil, use function `window-list' instead."
  (cl-loop for window in (or window-list (window-list))
           when (or (window-parameter window 'visible)
                    (not (window-dedicated-p window)))
           collect window))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :chrome 'window)
