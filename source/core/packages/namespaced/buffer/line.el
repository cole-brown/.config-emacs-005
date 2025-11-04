;;; namespaced/buffer/line.el --- Welcome to... (line?) -*- lexical-binding: t; -*-
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
;; Welcome to... (line?)
;; ...Smarter Line Functions dot Com!
;;
;; ------------------------------------------------------------------------------
;; Rebind to Smarter Functions:
;; ------------------------------------------------------------------------------
;;
;; ------------------------------
;; Emacs
;; ------------------------------
;;
;; (use-package emacs
;;
;;   ;;------------------------------
;;   :bind ; emacs
;;   ;;------------------------------
;;
;;   ;; Remap C-a to `buffer:cmd:line/smart:move-beginning/logical'
;;   (([remap move-beginning-of-line] . buffer:cmd:line/smart:move-beginning/logical)
;;
;;    ;; Remap C-a to `buffer:cmd:line/smart:move-beginning/visual' in visual-line-mode-map
;;    :map visual-line-mode-map
;;    ([remap beginning-of-visual-line] . buffer:cmd:line/smart:move-beginning/visual)
;;    ([remap move-beginning-of-line]   . buffer:cmd:line/smart:move-beginning/visual)
;;    ([remap end-of-visual-line]       . buffer:cmd:line/smart:move-end/visual)
;;    ([remap move-end-of-line]         . buffer:cmd:line/smart:move-end/visual)))
;;
;;
;; ------------------------------
;; Meow
;; ------------------------------
;;
;; (use-package package-name
;;   :after meow
;;
;;   ;; TODO-meow: Do I need to make a meow version of the smart line functions?
;;   ;; Meow doesn't have beginning/end of line functions itself, so how do you navigate there in Meow?
;;
;;   ;;------------------------------
;;   :bind ; meow
;;   ;;------------------------------
;;
;;   ;; Remap to smarter BOL function for logical lines.
;;   (([remap move-beginning-of-line] . buffer:cmd:line/smart:move-beginning/logical/select)
;;
;;    ;; Remap to smarter BOL/EOL function for visual lines (in `visual-line-mode-map').
;;    :map visual-line-mode-map
;;    ([remap beginning-of-visual-line] . buffer:cmd:line/smart:move-beginning/visual/select)
;;    ([remap move-beginning-of-line]   . buffer:cmd:line/smart:move-beginning/visual/select)
;;    ([remap end-of-visual-line]       . buffer:cmd:line/smart:move-end/visual/select)
;;    ([remap move-end-of-line]         . buffer:cmd:line/smart:move-end/visual/select)))
;;
;;
;; ------------------------------
;; Evil
;; ------------------------------
;;
;; (use-package package-name
;;   :after (:and evil evil-collection)
;;
;;   ;;------------------------------
;;   :general ; evil
;;   ;;------------------------------
;;
;;   ;;---
;;   ;; Choose either this...
;;   ;;---
;;   ;; ;; Replace `evil-beginning-of-line' and `evil-end-of-line' everywhere?
;;   ;; (:states 'motion
;;   ;;  :keymaps keybind:keymaps:override
;;   ;;
;;   ;;  [remap evil-beginning-of-visual-line] #'buffer:cmd:line/smart:move-beginning/visual
;;   ;;  [remap evil-beginning-of-line]        #'buffer:cmd:line/smart:move-beginning/visual
;;   ;;  [remap evil-end-of-visual-line]       #'buffer:cmd:line/smart:move-end/visual
;;   ;;  [remap evil-end-of-line]              #'buffer:cmd:line/smart:move-end/visual)
;;
;;   ;;---
;;   ;; Or this?..
;;   ;;---
;;   ;; ;; Only use the smarter functions in visual line mode?
;;   ;; (:states 'motion
;;   ;;  :keymaps 'visual-line-mode-map
;;   ;;
;;   ;;  [remap evil-beginning-of-visual-line] #'buffer:cmd:line/smart:move-beginning/visual
;;   ;;  [remap evil-beginning-of-line]        #'buffer:cmd:line/smart:move-beginning/visual
;;   ;;  [remap evil-end-of-visual-line]       #'buffer:cmd:line/smart:move-end/visual
;;   ;;  [remap evil-end-of-line]              #'buffer:cmd:line/smart:move-end/visual)
;;   )
;;
;;; Code:


(imp-require buffer:/region)


;;--------------------------------------------------------------------------------
;; Comments
;;--------------------------------------------------------------------------------

(defun buffer:line/smart:use-comments? (buffer)
  "Should we care about comments in BUFFER for BOL/EOL movement?"
  (with-current-buffer buffer
    ;; Buffer must be a mode we're not ignoring.
    (and (not (memq major-mode '(org-mode)))
         ;; Buffer's mode must have comment chars of some sort registered with Emacs.
         comment-start)))


(defun buffer:line/smart:comment/current/beginning ()
  "Return position of the beginning of the current comment.
Return nil, if not inside a comment.

\"Beginning of current comment\" means just before the comment character(s).

From `mwim': `mwim-current-comment-beginning'
https://github.com/alezost/mwim.el"
  (let ((syn (syntax-ppss)))
    (and (nth 4 syn)
         (nth 8 syn))))


(defun buffer:line/smart:comment:end-of-code ()
  "Go back to before the comment - to the end of the code.

Does nothing if `buffer:line/smart:use-comments?' returns nil for
`current-buffer'.
Does nothing if not inside a comment."
  (when (and (buffer:line/smart:use-comments? (current-buffer))
             (buffer:line/smart:comment/current/beginning))
    (condition-case nil
        (let ((line/start (line-beginning-position 1))
              (line/end   (line-end-position       1)))
          ;; This will error if it can't find a comment on this line.
          (comment-search-forward line/end)
          ;; This won't even get called unless `comment-search-forward' found & went into a comment.
          (goto-char (max (buffer:line/smart:comment/current/beginning)
                          line/start))
          ;; And then go back before whitespace.
          (skip-syntax-backward " " line/start))
      (error nil))))


;;------------------------------------------------------------------------------
;; Lines, Logical
;;------------------------------------------------------------------------------
;; What is "the 'beginning' of the 'line'" anyways?

;; https://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
;; [2019-05-17]: Do beginning of line first, not second.
(defun buffer:cmd:line/smart:move-beginning/logical (arg)
  "Move point to beginning of line, or indentation.

Move point to the beginning of the line. If point is already there, move to the
first non-whitespace character on this line. Effectively toggle between the
beginning of the line and the first non-whitespace character.

If ARG is not nil or 1, move forward ARG - 1 lines first. If point reaches the
beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  ;; Wasn't liking C-a having unexpected effects. Was originally:
  ;;   1) Go to first non-whitespace.
  ;;   2) Else go to beginning of line.
  ;; I've reversed those so one C-a works how my fingers expect but spamming
  ;; is useful to remind me of the new functionality.
  (let ((orig-point (point)))
    (move-beginning-of-line 1)
    ;; If that did nothing, jump to indentation.
    (when (= orig-point (point))
      (back-to-indentation))))


(defun buffer:cmd:line/smart:move-beginning/logical/select (arg)
  "Move point to beginning of line, or indentation.

For use with Meow, which wants to almost always have an active region. Will
start mark at point before moving if a region is not active.

Move point to the beginning of the line. If point is already there, move to the
first non-whitespace character on this line. Effectively toggle between the
beginning of the line and the first non-whitespace character.

If ARG is not nil or 1, move forward ARG - 1 lines first. If point reaches the
beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  (let ((point/before (point))
        point/after)
    ;; Begin a selection region.
    (unless (region-active-p)
      (push-mark-command nil :no-message))

    (buffer:cmd:line/smart:move-beginning/logical arg)
    (setq point/after (point))

    ;; Retcon the selection region if you end up with nothing?
    (when (= (buffer:region:start) (buffer:region:end))
      ;; Select from wherever we started to BOL.
      (goto-char point/before)
      (push-mark-command nil :no-message)
      (goto-char point/after))))


;;------------------------------------------------------------------------------
;; Lines, Visual
;;------------------------------------------------------------------------------

(defun buffer:cmd:line/smart:move-beginning/visual (arg)
  "Move point to beginning of visual line, or actual line, or indentation.

Move point to the beginning of the (visual) line. If point is already there,
move point to the beginning of the (actual/logical) line. If point is already
there, move to the first non-whitespace character on this line. Effectively
toggle between the beginning of the visual line, logical line, and the first
non-whitespace character.

If ARG is not nil or 1, move forward ARG - 1 lines first. If point reaches the
beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  ;; Move in the line now.
  (let ((orig-point (point)))
    (beginning-of-visual-line 1)
    ;; If that did nothing, jump into `buffer:cmd:line/smart:move-beginning/logical'
    ;; for more beginnings.
    (when (= orig-point (point))
      (buffer:cmd:line/smart:move-beginning/logical 1))))


(defun buffer:cmd:line/smart:move-end/visual (arg)
  "Move point to end of visual line, or actual line.

Move point to the end of the (visual) line. If point is already there, move
point to the end of the (actual/logical) line. Effectively toggle between the
end of the visual line and logical line.

If ARG is not nil or 1, move forward ARG - 1 lines first. If point reaches the
beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  ;; Move in the line now.
  (let ((point/orig (point)))
    ;;------------------------------
    ;; Visual Line?
    ;;------------------------------
    (end-of-visual-line 1)

    ;;------------------------------
    ;; Logical Line?
    ;;------------------------------
    ;; If that did (absolutely) nothing, jump to end of actual/logical line.
    (cond ((= point/orig (point))
           (move-end-of-line 1))

          ;; If that did /close to/ nothing, jump to end of actual/logical line?
          ;;---
          ;; NOTE: Used to only check if point didn't change at all, but that only works in
          ;; `visual-line-mode' where the logical lines are wrapped. For non-wrapped
          ;; lines that go past the buffer width, you can get stuck jumping to the new
          ;; visual EOL a few char at a time. This is not "smart". So compare movement
          ;; to make sure it's moved some actually useful amount.
          ((and (not visual-line-mode)
                (< (abs (- point/orig (point)))
                   10))
           (move-end-of-line 1)))))


(defun buffer:cmd:line/smart:move-beginning/visual/select (arg)
  "Move point to beginning of visual line, or actual line, or indentation.

For use with Meow, which wants to almost always have an active region. Will
start mark at point before moving if a region is not active.

Move point to the beginning of the (visual) line. If point is already there,
move point to the beginning of the (actual/logical) line. If point is already
there, move to the first non-whitespace character on this line. Effectively
toggle between the beginning of the visual line, logical line, and the first
non-whitespace character.

If ARG is not nil or 1, move forward ARG - 1 lines first. If point reaches the
beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  (let ((point/before (point))
        point/after)
    ;; Begin a selection region.
    (unless (region-active-p)
      (push-mark-command nil :no-message))

    (buffer:cmd:line/smart:move-beginning/visual arg)
    (setq point/after (point))

    ;; Retcon the selection region if you end up with nothing?
    (when (= (buffer:region:start) (buffer:region:end))
      ;; Select from wherever we started to BOL.
      (goto-char point/before)
      (push-mark-command nil :no-message)
      (goto-char point/after))))


(defun buffer:cmd:line/smart:move-end/visual/select (arg)
  "Move point to end of visual line, or actual line.

Move point to the end of the (visual) line. If point is already there, move
point to the end of the (actual/logical) line. Effectively toggle between the
end of the visual line and logical line.

If ARG is not nil or 1, move forward ARG - 1 lines first. If point reaches the
beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  (let ((point/before (point))
        point/after)
    ;; Begin a selection region?
    (unless (region-active-p)
      (push-mark-command nil :no-message))

    (buffer:cmd:line/smart:move-end/visual arg)
    (setq point/after (point))

    ;; Retcon the selection region if you end up with nothing?
    (when (= (buffer:region:start) (buffer:region:end))
      ;; Select from wherever we started to BOL.
      (goto-char point/before)
      (push-mark-command nil :no-message)
      (goto-char point/after))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide buffer line)
