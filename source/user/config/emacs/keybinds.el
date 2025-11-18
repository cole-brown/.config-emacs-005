;;; user/config/emacs/keybings.el --- keybinds -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2025-11-17
;; Timestamp:  2025-11-17
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Keybinds for Emacs Actual
;;
;;; Code:


;;------------------------------------------------------------------------------
;; The Unbinding of Emacs
;;------------------------------------------------------------------------------

;; `suspend-frame': C-z aka C-x C-z
;;----------------
;; C-z is bound to "BEGON YE FOOLISH EMACS WINDOW!"
;; You know...
;; Ctrl-Z.
;; Undo.
;; Everyone else's "undo" is Emacs' "minimize app".
;; Gets frustrating after the 111th time.
;; So yeah; no; fuck off.
;; C-z is nothing.
(keymap-global-unset "C-z")
;; C-x C-z the same exact thing.
;; Because one default keybind isn't enough.
(keymap-global-unset "C-x C-z")

;; `save-buffers-kill-terminal': C-x C-c
;;-----------------------------
;; Now that I'm back on QWERTY, I've noticed something.
;; The 'x' and 'c' keys are right the fuck next to each other.
;; My fingers are +fat+ big-boned.
;; C-x and C-c are the most and second most chocked full of shit keymaps.
;; This accidentally happens almost as much the fuckin' "Banishment of Emacs"
;; C-z. It accidentally happens so much that I don't care that I don't know
;; how to close `emacsclient' (and leave server alone) right now.
(keymap-global-unset "C-x C-c")

;;------------------------------
;; NOTE: Mice & `kbd` strings:
;;------------------------------
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Mouse-Input.html
;; left   mouse click: `mouse-1'
;; middle mouse click: `mouse-2'
;; right  mouse click: `mouse-3'
;;
;; mouse wheel: `wheel-up', `wheel-down', `wheel-left', `wheel-right'
;;   - those also have `double-*' and `triple-*' varients.
;;   - legacy:  `mouse-4',  `mouse-5',    `mouse-6',    `mouse-7'
;;
;; But you gotta lasso 'em with angles: (key-valid-p "<wheel-up>")
;;
;; Also "C-h f" doesn't understand mouse events so it can't tell me what's what!?
;; This works though:
;;   (describe-key (kbd "<wheel-up>"))
;;   (describe-key (kbd "C-<wheel-up>"))

;; `mouse-wheel-text-scale': C-mouse-wheel-{up,down}
;;----------------------------
;; You ever accidentally used your mousewheel in Emacs?
;;   (Ridiculous.
;;      A mouse? In Emacs‽)
;; It scrolls the buffer around.
;; Neat.
;; Now I need to hit Control-Something;
;; Emacs makes you use the control key sometimes.
;; Oh shit, my mouse wheelin' and controllin' crossed streams.
;; Now the font is 72 points.
;; And it's slightly impossible to figure out where zoom
;; level "DON'T ZOOM MY TEXT; WTF" is anymore.
;; But there's a command for that somewhere!
;; Emacs is famous for its discoverablilty!
;; Quick!
;; "M-x zoom...
;; "M-x text.....
;; "M-x ¯\_(ツ)_/¯"
;; Yeah. No please.
(keymap-global-unset "C-<wheel-up>")
(keymap-global-unset "C-<wheel-down>")
;; BTW: To rezero text scaling:
;;   - C-x C-0
;;     - NOTE: To anyone who can't see this anymore:
;;       "C-x 0" is different. That's `delete-window'.
;;   - `text-scale-adjust' (text-scale-adjust 0)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide user (imp-path-relative 'user (imp-path-sans-extension (imp-path-current-file))))
