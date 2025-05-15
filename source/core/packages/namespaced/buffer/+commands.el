;;; core/modules/emacs/buffer/+commands.el --- Interactive Buffer Functions -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-09-22
;; Timestamp:  2023-06-21
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Interactive Buffer Functions
;;
;;; Code:


(imp:require :buffer 'region)


;;------------------------------------------------------------------------------
;; Centering Helpers
;;------------------------------------------------------------------------------

(defun buffer:cmd:center/width (full-width)
  "Center region (or line) on width of FULL-WIDTH.

Set `fill-column' to FULL-WIDTH then invokes `center-region', or
`center-line' if there is no active region."
  (interactive "nColumn Width: ")
  (let ((fill-column full-width))
    (if (buffer:region:active?)
        (center-region (buffer:region:beginning)
                       (buffer:region:end))
      (center-line))))


(defun buffer:cmd:center/to (center-column)
  "Center region (or line) on width of CENTER-COLUMN * 2.

Set `fill-column' to CENTER-COLUMN * 2 then invokes `center-region', or
`center-line' if there is no active region."
  (interactive "nCenter at Column: ")
  (let ((fill-column (* center-column 2)))
    (if (buffer:region:active?)
        (center-region (buffer:region:beginning)
                       (buffer:region:end))
      (center-line))))


;;------------------------------------------------------------------------------
;; Align Regex Helpers
;;------------------------------------------------------------------------------
;; `Align-Regex' helper function idea came from this nice chap:
;; http://pragmaticemacs.com/emacs/aligning-text/

(defun buffer:cmd:align/before (start end text)
  "Align columns by whitespace before TEXT.

E.g. with text \"+=\" and region:
  Jeff.Jet(jeff.it).onClick += OnJeffClick;
  Jeff.Jet(jefferson.it).onClick += OnJeffersonClick;
becomes
  Jeff.Jet(jeff.it).onClick      += OnJeffClick;
  Jeff.Jet(jefferson.it).onClick += OnJeffersonClick;

Align currently selected region (if interactive), or region indicated by START
and END."
  (interactive "r\nsAlign Before: ")

  (let ((regexp (rx-to-string `(sequence
                                ;; target group: whitespace before input text
                                (group (zero-or-more whitespace))
                                ,text))))
    (align-regexp start end
                  regexp
                  ;; target group 1, min spacing 1, no repeat.
                  1 1 nil)))


(defun buffer:cmd:align/after (start end text)
  "Align columns by whitespace after TEXT.

E.g. with text \"+=\" and region:
  Jeff.Jet(jeff.it).onClick += OnJeffClick;
  Jeff.Jet(jefferson.it).onClick += OnJeffersonClick;
becomes
  Jeff.Jet(jeff.it).onClick +=      OnJeffClick;
  Jeff.Jet(jefferson.it).onClick += OnJeffersonClick;

Align currently selected region (if interactive), or region indicated by START
and END."
  (interactive "r\nsAlign After: ")

  (let ((regexp (rx-to-string `(sequence
                                ,text
                                ;; target group: whitespace after input text
                                (group (zero-or-more whitespace))))))
    (align-regexp start end
                  regexp
                  ;; target group 1, min spacing 1, no repeat.
                  1 1 nil)))


;;------------------------------------------------------------------------------
;; Fill/Unfill Commands, Functions, Hydras
;;------------------------------------------------------------------------------

(defun buffer:cmd:fill/paragraph/unfill ()
  "Unfill paragraph.

This is actually the inverse of `fill-paragraph'. Take a multi-line paragraph
and makes it into a single line of text.

from: nhoffman http://nhoffman.github.io/.emacs.d/#org40b27e4
  which is from: http://defindit.com/readme_files/emacs_hints_tricks.html
    which is from: Stefan Monnier <foo at acm.org>
      which is probably from the turtles that go all the way down"
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))


(defun int<buffer>:fill/paragraph/fn-for-mode ()
  "Mode-aware fill-paragraph.

So I only have to bind one thing in the fill hydra. Separated the 'get func' out
here so I can see if in a mode with a special fill for hydra hinting."
  (cond
   ((derived-mode-p 'csharp-mode)
    #'c-fill-paragraph)

   ;; c-mode and all derivatives
   ((and (functionp 'c-buffer-is-cc-mode)
         (c-buffer-is-cc-mode))
    #'c-fill-paragraph)

   ;; elisp, other lispses
   ((or (derived-mode-p 'emacs-lisp-mode)
        (derived-mode-p 'lisp-mode))
    #'lisp-fill-paragraph)
   ;; Might just use `fill-paragraph'?
   ;; Seems to be what "M-q" is using right now?

   ;; python-mode
   ((derived-mode-p 'python-mode) #'python-fill-paragraph)

   ;; org-mode
   ((derived-mode-p 'org-mode) #'org-fill-paragraph)

   ;; default to the usual fill-paragraph
   (t #'fill-paragraph)))


(defun buffer:cmd:fill/paragraph/per-mode (&optional justify?)
  "Mode-aware fill-paragraph.

So I only have to bind one thing in the fill prefix map.

If optional JUSTIFY? is non-nil, justify the text filled (see function
`fill-paragraph')."
  (interactive "P")
  (funcall (int<buffer>:fill/paragraph/fn-for-mode) justify?))


(defun buffer:cmd:fill/region/single-line (&optional justify?)
  "Grab start/end of current line and call `fill-region'.

That is: \"'Fill Region' on just this line, please.\"

If optional JUSTIFY? is non-nil, justify the text filled (see function
`fill-region')."
  (interactive "P")
  (let ((from (save-excursion (beginning-of-line) (point)))
        (to   (save-excursion (end-of-line)       (point))))
    (fill-region from to justify?)))


(defun buffer:cmd:fill/dwim/to-column (fill-to-column &optional justify?)
  "Fill line/region based on FILL-TO-COLUMN.

If a region is selected, fills that region as if the `fill-column' variable was
FILL-TO-COLUMN.

If no region is active, fill current line.

If optional JUSTIFY? is non-nil, justify the text filled (see function
`fill-region')."
  (interactive (list
                (read-number "Fill to Column: " 80)
                current-prefix-arg)) ;; (interactive "P") == `current-prefix-arg'

  (let ((fill-column fill-to-column))
    ;; DWIM: Region? Fill that.
    (if (buffer:region:active?)
        ;; Region selected - fill that.
        (fill-region (region-beginning) (region-end) justify?)

      ;; No region? Fill this line.
      (buffer:cmd:fill/region/single-line justify?))))


;;------------------------------------------------------------------------------
;; Narrow & Widen
;;------------------------------------------------------------------------------

(defun buffer:cmd:narrow/toggle (beg end)
  "Narrow the buffer to BEG END. If narrowed, widen it.

Borrowed from Doom's `doom/toggle-narrow-buffer' in \"core/autoload/ui.el\"."
  (interactive
   ;; TODO-meow: Check for meow too?..
   (list (or (bound-and-true-p evil-visual-beginning) (region-beginning))
         (or (bound-and-true-p evil-visual-end)       (region-end))))
  (if (buffer-narrowed-p)
      (widen)
    (unless (region-active-p)
      (setq beg (line-beginning-position)
            end (line-end-position)))
    (narrow-to-region beg end)))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :buffer '+commands)
