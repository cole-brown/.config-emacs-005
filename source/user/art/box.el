;;; user/art/box.el --- Box Drawing -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <code@brown.dev>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-07-11
;; Timestamp:  2025-11-17
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;
;;; Code:


(require 'hydra)
(imp-require 'buffer 'manage)


;;------------------------------------------------------------------------------
;; Manual Unicode Box Drawing Chars That Don't Work Right Now
;;------------------------------------------------------------------------------
;; Don't work with current font(s)....
;;
;;------------------------------
;; Rounded Corners.
;;------------------------------
;; "╭" "arc down and right"
;; "╮" "arc down and left"
;; "╰" "arc up and right"
;; "╯" "arc up and left"
;;
;;------------------------------
;; Heavy Lines
;;------------------------------
;; "━" "heavy horizontal"
;; "┃" "heavy vertical"
;; "┅" "heavy triple dash horizontal"
;; "┇" "heavy triple dash vertical"
;; "┉" "heavy quadruple dash horizontal"
;; "┋" "heavy quadruple dash vertical"
;; "┏" "heavy down and right"
;; "┓" "heavy down and left"
;; "┗" "heavy up and right"
;; "┛" "heavy up and left"
;; "┣" "heavy vertical and right"
;; "┫" "heavy vertical and left"
;; "┳" "heavy down and horizontal"
;; "┻" "heavy up and horizontal"
;; "╋" "heavy vertical and horizontal"
;; "╍" "heavy double dash horizontal"
;; "╏" "heavy double dash vertical"
;; "╸" "heavy left"
;; "╹" "heavy up"
;; "╺" "heavy right"
;; "╻" "heavy down"
;;
;;------------------------------
;; Heavy/Light Transitions
;;------------------------------
;; "┍" "down light and right heavy"
;; "┎" "down heavy and right light"
;; "┑" "down light and left heavy"
;; "┒" "down heavy and left light"
;; "┕" "up light and right heavy"
;; "┖" "up heavy and right light"
;; "┙" "up light and left heavy"
;; "┚" "up heavy and left light"
;; "┝" "vertical light and right heavy"
;; "┞" "up heavy and right down light"
;; "┟" "down heavy and right up light"
;; "┠" "vertical heavy and right light"
;; "┡" "down light and right up heavy"
;; "┢" "up light and right down heavy"
;; "┥" "vertical light and left heavy"
;; "┦" "up heavy and left down light"
;; "┧" "down heavy and left up light"
;; "┨" "vertical heavy and left light"
;; "┩" "down light and left up heavy"
;; "┪" "up light and left down heavy"
;; "┭" "left heavy and right down light"
;; "┮" "right heavy and left down light"
;; "┯" "down light and horizontal heavy"
;; "┰" "down heavy and horizontal light"
;; "┱" "right light and left down heavy"
;; "┲" "left light and right down heavy"
;; "┵" "left heavy and right up light"
;; "┶" "right heavy and left up light"
;; "┷" "up light and horizontal heavy"
;; "┸" "up heavy and horizontal light"
;; "┹" "right light and left up heavy"
;; "┺" "left light and right up heavy"
;; "┽" "left heavy and right vertical light"
;; "┾" "right heavy and left vertical light"
;; "┿" "vertical light and horizontal heavy"
;; "╀" "up heavy and down horizontal light"
;; "╁" "down heavy and up horizontal light"
;; "╂" "vertical heavy and horizontal light"
;; "╃" "left up heavy and right down light"
;; "╄" "right up heavy and left down light"
;; "╅" "left down heavy and right up light"
;; "╆" "right down heavy and left up light"
;; "╇" "down light and up horizontal heavy"
;; "╈" "up light and down horizontal heavy"
;; "╉" "right light and left vertical heavy"
;; "╊" "left light and right vertical heavy"
;; "╼" "light left and heavy right"
;; "╽" "light up and heavy down"
;; "╾" "heavy left and light right"
;; "╿" "heavy up and light down"
;;
;; ;;------------------------------
;; ;; Block Characters
;; ;;------------------------------
;; "█" "full block"
;; ;; left to right "▏▎▍▌▋▊▉█"
;; "▏" "left one eighth block"
;; "▎" "left one quarter block"
;; "▍" "left three eighths block"
;; "▌" "left half block"
;; "▋" "left five eighths block"
;; "▊" "left three quarters block"
;; "▉" "left seven eighths block"
;; ;; bottom to top: "▁▂▃▄▅▆▇█"
;; "▁" "lower one eighth block"
;; "▂" "lower one quarter block"
;; "▃" "lower three eighths block"
;; "▄" "lower half block"
;; "▅" "lower five eighths block"
;; "▆" "lower three quarters block"
;; "▇" "lower seven eighths block"
;; ;; quadrants
;; "▖" "quadrant lower left"
;; "▗" "quadrant lower right"
;; "▘" "quadrant upper left"
;; "▙" "quadrant upper left and lower left and lower right"
;; "▚" "quadrant upper left and lower right"
;; "▛" "quadrant upper left and upper right and lower left"
;; "▜" "quadrant upper left and upper right and lower right"
;; "▝" "quadrant upper right"
;; "▞" "quadrant upper right and lower left"
;; ;; shading
;; "░" "light shade"
;; "▒" "medium shade"
;; "▓" "dark shade"
;; ;; misc.
;; "▐" "right half block"
;; "▔" "upper one eighth block"
;; "▕" "right one eighth block"


;;------------------------------------------------------------------------------
;; Single Lines Hydra
;;------------------------------------------------------------------------------

(defhydra /art/hydra/box/single (:color amaranth ; default to warn if non-hydra key
                                 ;;:color pink   ; defaults to not exit unless explicit
                                 ;;:idle 0.75    ; no help for x seconds
                                 :hint none)     ; no hint - just docstr
  "
Draw box characters.
_q_: ?q?  _w_: ?w?  _e_: ?e?   _r_: ?r?   ^ ^  ^ ^     ^ ^        _i_: up    ^ ^            _/_: undo     _h_: ?h?
_a_: ?a?  _s_: ?s?  _d_: ?d?   _f_: ?f?   ^ ^  ^ ^     _j_: left  _k_: down  _l_: right     ^ ^
_z_: ?z?  _x_: ?x?  _c_: ?c?   ^ ^  ^ ^   ^ ^  ^ ^     ^ ^        ^ ^        ^ ^            ^ ^
^ ^  ^ ^  ^ ^  ^ ^  ^ ^  ^ ^   ^ ^  ^ ^   ^ ^  ^ ^     ^ ^        ^ ^        ^ ^            ^ ^           _<SPC>_: ?<SPC>?
^ ^  ^ ^  ^ ^  ^ ^  ^ ^  ^ ^   ^ ^  ^ ^   ^ ^  ^ ^     ^ ^        _b_: ?b?   ^ ^            ^ ^   ^^^^^^^^_<insert>_: ?<insert>?
^ ^  ^ ^  ^ ^  ^ ^  ^ ^  ^ ^   ^ ^  ^ ^   ^ ^  ^ ^     ^ ^        _B_: ?B?
"
;; Dvorak:  "
;; Draw box characters.
;; _'_: ?'?  _,_: ?,?  _._: ?.?   _p_: ?p?   ^ ^  ^ ^     ^ ^        _c_: up    ^ ^            _-_: undo     _d_: ?d?
;; _a_: ?a?  _o_: ?o?  _e_: ?e?   _u_: ?u?   ^ ^  ^ ^     _h_: left  _t_: down  _n_: right     ^ ^           _g_: ?g?
;; _;_: ?;?  _q_: ?q?  _j_: ?j?   ^ ^  ^ ^   ^ ^  ^ ^     ^ ^        ^ ^        ^ ^            ^ ^           _G_: ?G?
;; ^ ^  ^ ^  ^ ^  ^ ^  ^ ^  ^ ^   ^ ^  ^ ^   ^ ^  ^ ^     ^ ^        ^ ^        ^ ^            ^ ^           _<SPC>_: ?<SPC>?
;; ^ ^  ^ ^  ^ ^  ^ ^  ^ ^  ^ ^   ^ ^  ^ ^   ^ ^  ^ ^     ^ ^        _x_: ?x?   ^ ^            ^ ^   ^^^^^^^^_<insert>_: ?<insert>?
;; ^ ^  ^ ^  ^ ^  ^ ^  ^ ^  ^ ^   ^ ^  ^ ^   ^ ^  ^ ^     ^ ^        _X_: ?X?
;; "

  ;; NOTE: You _MUST_ start off with a newline in docstr, otherwise you will get:
  ;;  > Debugger entered--Lisp error: (error "Not enough arguments for format string")
  ;;  > format("Draw box characters.\n%s: %s  %s: %s  %s: %s   %s: ...")
  ;;  > ...

  ;;------------------------------
  ;; Box Characters
  ;;------------------------------
  ("q" (buffer:insert-or-overwrite "┌") "┌") ;; down and right
  ("w" (buffer:insert-or-overwrite "┬") "┬") ;; down and horizontal
  ("e" (buffer:insert-or-overwrite "┐") "┐") ;; down and left
  ("r" (buffer:insert-or-overwrite "─") "─") ;; horizontal

  ("a" (buffer:insert-or-overwrite "├") "├") ;; vertical and right
  ("s" (buffer:insert-or-overwrite "┼") "┼") ;; vertical and horizontal
  ("d" (buffer:insert-or-overwrite "┤") "┤") ;; vertical and left
  ("f" (buffer:insert-or-overwrite "│") "│") ;; vertical

  ("z" (buffer:insert-or-overwrite "└") "└") ;; up and right
  ("x" (buffer:insert-or-overwrite "┴") "┴") ;; up and horizontal
  ("c" (buffer:insert-or-overwrite "┘") "┘") ;; up and left

  ;;---
  ;; Not Working in Emacs w/ Current Fonts:
  ;;---
  ;; The keymap string if these worked:
  ;; _q_: ?q?  _w_: ?w?  _e_: ?e?   _r_: ?r?   _t_: ?t?     ^ ^        _i_: up    ^ ^            _/_: undo     _d_: ?d?
  ;; _a_: ?a?  _s_: ?s?  _d_: ?d?   _f_: ?f?   _g_: ?g?     _j_: left  _k_: down  _l_: right     ^ ^           _g_: ?g?
  ;; _z_: ?z?  _x_: ?x?  _c_: ?c?   ^ ^  ^ ^   ^ ^  ^ ^     ^ ^        ^ ^        ^ ^            ^ ^           _G_: ?G?
  ;; ^ ^  ^ ^  ^ ^  ^ ^  ^ ^  ^ ^   ^ ^  ^ ^   ^ ^  ^ ^     ^ ^        ^ ^        ^ ^            SPC: ?<SPC>?
  ;; ^ ^  ^ ^  _<_: ?<?  ^ ^  ^ ^   _R_: ?R?   _Y_: ?Y?     ^ ^        ^ ^        ^ ^            _b_: ?b?
  ;; _A_: ?A?  ^ ^  ^ ^  _E_: ?E?   _U_: ?U?   _I_: ?I?     ^ ^        ^ ^        ^ ^            _B_: ?B?
  ;; ^ ^  ^ ^  _Q_: ?Q?  ^ ^  ^ ^   ^ ^  ^ ^   ^ ^  ^ ^     ^ ^        ^ ^        ^ ^            ^ ^           ^ ^  (hi)
  ;;
  ;; ("y" (buffer:insert-or-overwrite "┄") "┄") ;; triple dash horizontal
  ;; ("i" (buffer:insert-or-overwrite "┆") "┆") ;; triple dash vertical
  ;;
  ;; ;; These are named backwards by Unicode - don't blame me.
  ;; ("<" (buffer:insert-or-overwrite "╷") "╷") ;; down
  ;; ("A" (buffer:insert-or-overwrite "╶") "╶") ;; right
  ;; ("Q" (buffer:insert-or-overwrite "╵") "╵") ;; up
  ;; ("E" (buffer:insert-or-overwrite "╴") "╴") ;; left
  ;;
  ;; ("R" (buffer:insert-or-overwrite "╌") "╌") ;; double dash horizontal
  ;; ("U" (buffer:insert-or-overwrite "╎") "╎") ;; double dash vertical
  ;;
  ;; ("Y" (buffer:insert-or-overwrite "┈") "┈") ;; quadruple dash horizontal
  ;; ("I" (buffer:insert-or-overwrite "┊") "┊") ;; quadruple dash vertical


  ;;------------------------------
  ;; Movement Keys
  ;;------------------------------
  ("i"
   (if (bound-and-true-p evil-mode)
       (evil-previous-line)
     (previous-line))
   "up")
  ("j"
   (if (bound-and-true-p evil-mode)
       (evil-backward-char)
     (backward-char))
   "left")
  ("k"
   (if (bound-and-true-p evil-mode)
       (evil-next-line)
     (next-line))
   "down")
  ("l"
   (if (bound-and-true-p evil-mode)
       (evil-forward-char)
     (forward-char))
   "right")

  ;;------------------------------
  ;; Misc.
  ;;------------------------------
  ("<SPC>" (buffer:insert-or-overwrite " ") "insert space")
  ("<insert>"
   #'buffer:overwrite:toggle
   (if (buffer:overwriting?)
       (if (bound-and-true-p evil-mode) "insert state" "insert mode")
     (if (bound-and-true-p evil-mode) "replace state" "overwrite mode")))
  ("b"
   (if (bound-and-true-p evil-mode)
       (evil-delete-char)
     (delete-char 1))
   "delete char")
  ("B"
   (if (bound-and-true-p evil-mode)
       (evil-delete-backward-char)
     (delete-backward-char 1))
   "delete backwards char")
  ("/"
   (if (bound-and-true-p undo-tree)
       (undo-tree-undo)
     (undo))
   "undo")
  ("_"
   (if (bound-and-true-p undo-tree)
       (undo-tree-undo)
     (undo))
   "undo")
  ("C-_"
   (if (bound-and-true-p undo-tree)
       (undo-tree-undo)
     (undo))
   "undo")

  ;;------------------------------
  ;; Get Me Out Of Here!!!
  ;;------------------------------
  ("h"   (hydra:nest '/art/hydra/box/double) "double lines (╬)" :exit t)
  ;; ("G"
  ;;  (cond ((bound-and-true-p evil-mode)
  ;;         ;; TODO-evil: this never did anything special when I was using evil,
  ;;         ;; so I think it works right? Then again, I almost never use(d) it...
  ;;         ;;
  ;;         nil)
  ;;        ((bound-and-true-p meow-global-mode)
  ;;         (unless (meow-insert-mode-p)
  ;;           (meow-insert-mode)))
  ;;        (t
  ;;         nil))
  ;;  (concat "quit"
  ;;          (cond ((bound-and-true-p evil-mode)
  ;;                 " (to insert state)")
  ;;                ((bound-and-true-p meow-global-mode)
  ;;                 " (to insert mode)")
  ;;                (t
  ;;                 nil)))
  ;;  :color blue)
  ;; ("g"
  ;;  (cond ((bound-and-true-p evil-mode)
  ;;         (evil-normal-state))
  ;;        ((bound-and-true-p meow-global-mode)
  ;;         (unless (meow-normal-mode-p)
  ;;           (meow-normal-mode)))
  ;;        (t
  ;;         nil))
  ;;  (concat "quit"
  ;;          (cond ((bound-and-true-p evil-mode)
  ;;                 " (to normal state)")
  ;;                ((bound-and-true-p meow-global-mode)
  ;;                 " (to normal mode)")
  ;;                (t
  ;;                 nil)))
  ;;  :color blue)
  ("C-g"
   (cond ((bound-and-true-p evil-mode)
          (evil-normal-state))
         ((bound-and-true-p meow-global-mode)
          (unless (meow-normal-mode-p)
            (meow-normal-mode)))
         (t
          nil))
   (concat "quit"
           (cond ((bound-and-true-p evil-mode)
                  " (to normal state)")
                 ((bound-and-true-p meow-global-mode)
                  " (to normal mode)")
                 (t
                  nil)))
   :color blue))
;; (/art/hydra/box/single/body)


;;------------------------------------------------------------------------------
;; Double Lines Hydra
;;------------------------------------------------------------------------------

(defhydra /art/hydra/box/double (:color amaranth ;; default to warn if non-hydra key
                                 ;;:color pink   ;; defaults to not exit unless explicit
                                 ;;:idle 0.75    ;; no help for x seconds
                                 :hint none)     ;; no hint - just docstr)
  "
Draw box characters.
_q_: ?q?  _w_: ?w?  _e_: ?e?   _r_: ?r?   ^ ^  ^ ^     ^ ^        _i_: up    ^ ^            _/_: undo     _h_: ?h?
_a_: ?a?  _s_: ?s?  _d_: ?d?   _f_: ?f?   ^ ^  ^ ^     _j_: left  _k_: down  _l_: right     ^ ^
_z_: ?z?  _x_: ?x?  _c_: ?c?   ^ ^  ^ ^   ^ ^  ^ ^     ^ ^        ^ ^        ^ ^            ^ ^
^ ^  ^ ^  ^ ^  ^ ^  ^ ^  ^ ^   ^ ^  ^ ^   ^ ^  ^ ^     ^ ^        ^ ^        ^ ^            ^ ^           _<SPC>_: ?<SPC>?
^ ^  ^ ^  ^ ^  ^ ^  ^ ^  ^ ^   ^ ^  ^ ^   ^ ^  ^ ^     ^ ^        _b_: ?b?   ^ ^            ^ ^   ^^^^^^^^_<insert>_: ?<insert>?
^ ^  ^ ^  ^ ^  ^ ^  ^ ^  ^ ^   ^ ^  ^ ^   ^ ^  ^ ^     ^ ^        _B_: ?B?
"
  ;; If ever the single line hydra gets its dotted lines and/or line-termination chars working, add this to the keymap string:
  ;; ^ ^  ^ ^  ^ ^  ^ ^  ^ ^  ^ ^   ^ ^  ^ ^   ^ ^  ^ ^     ^ ^        ^ ^        ^ ^            ^ ^           ^ ^  (hi)

  ;; NOTE: You _MUST_ start off with a newline in docstr, otherwise you will get:
  ;;  > Debugger entered--Lisp error: (error "Not enough arguments for format string")
  ;;  > format("Draw box characters.\n%s: %s  %s: %s  %s: %s   %s: ...")
  ;;  > ...

  ;;------------------------------
  ;; Box Characters:
  ;; Double Lines! Double Lines!
  ;;------------------------------
  ("q" (buffer:insert-or-overwrite "╔") "╔") ;; double down and right
  ("w" (buffer:insert-or-overwrite "╦") "╦") ;; double down and horizontal
  ("e" (buffer:insert-or-overwrite "╗") "╗") ;; double down and left
  ("r" (buffer:insert-or-overwrite "═") "═") ;; double horizontal

  ("a" (buffer:insert-or-overwrite "╠") "╠") ;; double vertical and righ
  ("s" (buffer:insert-or-overwrite "╬") "╬") ;; double vertical and horizontal
  ("d" (buffer:insert-or-overwrite "╣") "╣") ;; double vertical and left
  ("f" (buffer:insert-or-overwrite "║") "║") ;; double vertical

  ("z" (buffer:insert-or-overwrite "╚") "╚") ;; double up and right
  ("x" (buffer:insert-or-overwrite "╩") "╩") ;; double up and horizontal
  ("c" (buffer:insert-or-overwrite "╝") "╝") ;; double up and left

  ;;------------------------------
  ;; Movement Keys
  ;;------------------------------
  ("i"
   (if (bound-and-true-p evil-mode)
       (evil-previous-line)
     (previous-line))
   "up")
  ("j"
   (if (bound-and-true-p evil-mode)
       (evil-backward-char)
     (backward-char))
   "left")
  ("k"
   (if (bound-and-true-p evil-mode)
       (evil-next-line)
     (next-line))
   "down")
  ("l"
   (if (bound-and-true-p evil-mode)
       (evil-forward-char)
     (forward-char))
   "right")

  ;;------------------------------
  ;; Misc.
  ;;------------------------------
  ("<SPC>" (buffer:insert-or-overwrite " ") "insert space")
  ("<insert>"
   #'buffer:overwrite:toggle
   (if (buffer:overwriting?)
       (if (bound-and-true-p evil-mode) "insert state" "insert mode")
     (if (bound-and-true-p evil-mode) "replace state" "overwrite mode")))
  ("b"
   (if (bound-and-true-p evil-mode)
       (evil-delete-char)
     (delete-char 1))
   "delete char")
  ("B"
   (if (bound-and-true-p evil-mode)
       (evil-delete-backward-char)
     (delete-backward-char 1))
   "delete backwards char")
  ("/"
   (if (bound-and-true-p undo-tree)
       (undo-tree-undo)
     (undo))
   "undo")
  ("_"
   (if (bound-and-true-p undo-tree)
       (undo-tree-undo)
     (undo))
   "undo")
  ("C-_"
   (if (bound-and-true-p undo-tree)
       (undo-tree-undo)
     (undo))
   "undo")

  ;;------------------------------
  ;; Get Me Out Of Here!!!
  ;;------------------------------
  ("h"   (hydra:nest '/art/hydra/box/single) "single lines (┼)" :exit t)
  ;; ("G"
  ;;  (cond ((bound-and-true-p evil-mode)
  ;;         ;; TODO-evil: this never did anything special when I was using evil,
  ;;         ;; so I think it works right? Then again, I almost never use(d) it...
  ;;         ;;
  ;;         nil)
  ;;        ((bound-and-true-p meow-global-mode)
  ;;         (unless (meow-insert-mode-p)
  ;;           (meow-insert-mode)))
  ;;        (t
  ;;         nil))
  ;;  (concat "quit"
  ;;          (cond ((bound-and-true-p evil-mode)
  ;;                 " (to insert state)")
  ;;                ((bound-and-true-p meow-global-mode)
  ;;                 " (to insert mode)")
  ;;                (t
  ;;                 nil)))
  ;;  :color blue)
  ;; ("g"
  ;;  (cond ((bound-and-true-p evil-mode)
  ;;         (evil-normal-state))
  ;;        ((bound-and-true-p meow-global-mode)
  ;;         (unless (meow-normal-mode-p)
  ;;           (meow-normal-mode)))
  ;;        (t
  ;;         nil))
  ;;  (concat "quit"
  ;;          (cond ((bound-and-true-p evil-mode)
  ;;                 " (to normal state)")
  ;;                ((bound-and-true-p meow-global-mode)
  ;;                 " (to normal mode)")
  ;;                (t
  ;;                 nil)))
  ;;  :color blue)
  ("C-g"
   (cond ((bound-and-true-p evil-mode)
          (evil-normal-state))
         ((bound-and-true-p meow-global-mode)
          (unless (meow-normal-mode-p)
            (meow-normal-mode)))
         (t
          nil))
   (concat "quit"
           (cond ((bound-and-true-p evil-mode)
                  " (to normal state)")
                 ((bound-and-true-p meow-global-mode)
                  " (to normal mode)")
                 (t
                  nil)))
   :color blue))
;; (/art/hydra/box/double/body)


;;------------------------------------------------------------------------------
;; Double/Single Line Transitions Hydra
;;------------------------------------------------------------------------------

;; TODO: This hydra.

;; ;;------------------------------
;; ;; Double/Single Transitions.
;; ;;------------------------------
;; "╒" ;; down single and right double
;; "╤" ;; down single and horizontal double
;; "╕" ;; down single and left double

;; "╞" ;; vertical single and right double
;; "╪" ;; vertical single and horizontal double
;; "╡" ;; vertical single and left double

;; "╘" ;; up single and right double
;; "╧" ;; up single and horizontal double
;; "╛" ;; up single and left double

;; "╓" ;; down double and right single
;; "╥" ;; down double and horizontal single
;; "╖" ;; down double and left single

;; "╟" ;; vertical double and right single
;; "╫" ;; vertical double and horizontal single
;; "╢" ;; vertical double and left single

;; "╙" ;; up double and right single
;; "╨" ;; up double and horizontal single
;; "╜" ;; up double and left single


;;------------------------------------------------------------------------------
;; Hydras' Entry Function
;;------------------------------------------------------------------------------

(defun /art/cmd/box/draw ()
  "Get into the box drawing hydra in the proper evil state."
  (interactive)
  (when (bound-and-true-p evil-mode)
    (evil-insert 0))
  (call-interactively #'/art/hydra/box/single/body))
;; ┌────┐
;; ├────┤
;; │ hi │
;; └────┘


;;------------------------------------------------------------------------------
;; The End
;;------------------------------------------------------------------------------
(imp-provide art box)
