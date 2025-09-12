;;; source/user/config/whitespace.el --- Configure the Unseen -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-08-05
;; Timestamp:  2025-09-10
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Whitespace:
;;    - show it
;;    - keep it clean
;;    - etc
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Fill Column
;;------------------------------------------------------------------------------

(defvar --/fill-column/standard  80  "80")
(defvar --/fill-column/wide     120 "120")

;; Increase default from 70 to 80.
(customize-set-variable 'fill-column --/fill-column/standard)


;;------------------------------------------------------------------------------
;; Tabs
;;------------------------------------------------------------------------------
;; https://www.emacswiki.org/emacs/NoTabs
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Just-Spaces.html
;; https://www.emacswiki.org/emacs/TabsAreEvil
;; https://www.emacswiki.org/emacs/SmartTabs

(defvar --/tab/standard 4 "4")
(defvar --/tab/small    2 "2")
(defvar --/tab/bug/org  8
  "Org barfs up a warning about tab widths that are not 8.

I don't recall this Warning getting plastered all over the place in any of my
previous configs. Maybe it's because I haven't done my org unbinds & rebinds yet?

I'm going to call this a bug. Org probably disagrees. I will counter with...
If you absolutely require tab to be:
  1. exactly 8,
  2. only 8,
  3. and never anything except 8...

Why the _fuck_ do you not just hard-code it to 8 for all of org-mode or lexically
bind it to 8 for the function that requires it or anything proactive at all?

Why the fuck you gotta just dump a warining on top of me, leave my org buffer
indentation broken, and wander off?!

Was this some setting I changed, Org?
Which setting was it, Org?
Why is this error absolutely useless for anything I'm asking here, Org?

  > Warning (org-element): org-element--cache: Org parser error in notes.org::1212. Resetting.
  >  The error was: (error \"Tab width in Org files must be 8, not 4.  Please adjust your ‘tab-width’ settings for Org mode\")
  >  Backtrace:
  >   backtrace-to-string(nil)
  >   org-element-at-point()
  >   #f(compiled-function (&optional indent arg interactive) \"Goto next table row or insert a newline.\\n\\nCalls `org-table-next-row' or `newline', depending on context.\\n\\nWhen optional INDENT argument is non-nil, call\\n`newline-and-indent' with ARG, otherwise call `newline' with ARG\\nand INTERACTIVE.\\n\\nWhen `org-return-follows-link' is non-nil and point is on\\na timestamp, a link or a citation, call `org-open-at-point'.\\nHowever, it will not happen if point is in a table or on a \\\"dead\\\"\\nobject (e.g., within a comment).  In these case, you need to use\\n`org-open-at-point' directly.\" (interactive \"i\\nP\\np\") #<bytecode 0x127fc01a1aa4c85e>)(nil nil 1)
  >   apply(#f(compiled-function (&optional indent arg interactive) \"Goto next table row or insert a newline.\\n\\nCalls `org-table-next-row' or `newline', depending on context.\\n\\nWhen optional INDENT argument is non-nil, call\\n`newline-and-indent' with ARG, otherwise call `newline' with ARG\\nand INTERACTIVE.\\n\\nWhen `org-return-follows-link' is non-nil and point is on\\na timestamp, a link or a citation, call `org-open-at-point'.\\nHowever, it will not happen if point is in a table or on a \\\"dead\\\"\\nobject (e.g., within a comment).  In these case, you need to use\\n`org-open-at-point' directly.\" (interactive \"i\\nP\\np\") #<bytecode 0x127fc01a1aa4c85e>) (nil nil 1))
  >   org-return(nil nil 1)
  >   funcall-interactively(org-return nil nil 1)
  >   call-interactively(org-return nil nil)
  >   command-execute(org-return)
  >
  >  Please report this to Org mode mailing list (M-x org-submit-bug-report).

Why, org, why?
WHY DO YOU ALLOW THIS TO HAPPEN, ORG?!?!?!?
Why are you wasting my and hlissner's time, Org?

https://github.com/doomemacs/doomemacs/commit/43870bf8318f6471c4ce5e14565c9f0a3fb6e368
  > fix(editorconfig): prevent changes to tab-width in org-mode
  > - Add another measure for preventing changes to tab-width in org-mode.
  >   The hook introduced in 2757a97 runs too early and could be overwritten
  >   by editorconfig.
  > - Fix the hook in 2757a97 to run much later, ensuring (as a last resort)
  >   no other packages can overwrite tab-width either.
  >
  > Amend: 2757a97
  > Ref: #7670

You need a fucking setting, Org:
(defcustom org-tab-width-force-to-8-because-org-is-so-complicated-we-cant-support-anything-except-the-OG-tab-width t)

/rant
Sorry.")


;; `indent-tabs-mode'
;;-------------------
;; Always use spaces; never use tabs.
(customize-set-variable 'indent-tabs-mode nil)


;; `tab-width'
;;-------
;; Set default tab width for all buffers.
(customize-set-variable 'tab-width --/tab/standard)


;; `tab-stop-list'
;;----------------
;; Make sure this is at it's default of nil, because:
;;   "A value of nil means a tab stop every `tab-width' columns."
(customize-set-variable 'tab-stop-list nil)

;; NOTE: M-x tabify and M-x untabify exist and work on regions.


;;------------------------------------------------------------------------------
;; Clean Whitespace
;;------------------------------------------------------------------------------

;; `ws-butler'
;;-------------
;; Whitespace Butler: Only removes whitespace from regions you've changed.
;;   https://melpa.org/#/ws-butler
;;
;; NOTE: Use `ws-butler' instead of `whitespace-cleanup' so as to avoid noisy
;; commits. Can still manually call `whitespace-cleanup' on files that bug you
;; too much.
(use-package ws-butler
  :demand t

  ;;------------------------------
  :custom
  ;;------------------------------

  (ws-butler-convert-leading-tabs-or-spaces t)

  ;; NOTE: If modes need to be excluded, customize the variable `ws-butler-global-exempt-modes'.


  ;;------------------------------
  :config
  ;;------------------------------

  ;; TODO [2025-09-10]: Use if bug still exists.
  ;;-----
;;   ;; HACK: Bug fix via advice. See for more info:
;;   ;;   emacs-sn004:/docs/issues/2019-08-26_whitespace-mode-and-move-to-column.org
;;   (define-advice move-to-column (:filter-args (args) ws-butler/whitespace-mode:fix/force-off-by-one)
;;     "Un-lose the one single space that's being lost sometimes.

;; Bug Conditions:
;;   1. `whitespace-mode' is on
;;   2. `move-to-column' is called with 'force' set true (`ws-butler' does this).
;;   3. `ws-butler-keep-whitespace-before-point' is on
;; Number 3 isn't actually necessary but it's the only time I've
;; noticed this bug (aside from contriving it in bug hunts/repros).

;; Repro:
;;   1. Start emacs without user init: `emacs -q`
;;   2. Paste the following code into the scratch buffer.
;;   3. Run `M-x eval-buffer`
;;   4. Run `M-x bug:whitespace:repro`

;; (defun bug:whitespace:move-30-force ()
;;   (interactive)
;;   (move-to-column 30 t))

;; (defun bug:whitespace:repro ()
;;   (interactive)
;;   ;; Set Up
;;   (let ((buffer-name \"bug:whitespace:repro\"))
;;     ;; Get a fresh buffer for each test.
;;     (when-let ((buffer (get-buffer buffer-name)))
;;       (kill-buffer buffer-name))
;;     (pop-to-buffer (get-buffer-create buffer-name)))
;;   ;; Make sure we don't have our bugfix advice running when we're trying to prove the bug.
;;   (require 'nadvice)
;;   (advice-mapc (lambda (advice _props) (advice-remove sym advice)) 'move-to-column)
;;   ;; Don't want tabs when we're trying to count spaces.
;;   (setq indent-tabs-mode nil)
;;   ;; Not needed for bug; for seeing column/line on modeline.
;;   (column-number-mode t)
;;   (line-number-mode t)

;;   ;;------------------------------
;;   ;; Reproduce the bug!
;;   ;;------------------------------
;;   ;; The bug: `whitespace-mode' and `move-to-column' (only when FORCE is non-nil
;;   ;; (and it actually has to force)) interact with each other to produce an
;;   ;; off-by-one error. For this example, we'll do \"Move 1\" without
;;   ;; `whitespace-mode', to see what's expected of `(move-to-column 30 t)'.
;;   ;;
;;   ;; Legend:
;;   ;;   - '█' : our text cursor
;;   ;;   - '-' : space
;;   ;;     - '·' : space

;;   ;;---
;;   ;; [OK] Move 1: Unbugged Move:
;;   ;;---
;;   ;; Move 1: Expect:
;;   ;; move-1:······················█
;;   ;; Move 1: Get:
;;   ;; move-1:······················█
;;   (insert \"move-1:[OK]:\")
;;   (move-to-column 30 t)
;;   (insert (format \"column: %2d\" (current-column)))

;;   ;;---
;;   ;; Bug set up:
;;   ;;---
;;   ;; 1. Whitespace mode must be enabled!
;;   (whitespace-mode 'enable)
;;   ;; 2. We must not be on the final line of the buffer.
;;   (insert \"\n\n\")
;;   (forward-line -1)

;;   ;;---
;;   ;; [ERROR] Move 2: Bugged Move:
;;   ;;---
;;   ;; Move 2: Expect:
;;   ;; move-2:······················█
;;   ;; Move 2: Get:
;;   ;; move-2:·····················█
;;   (insert \"move-2:[ERROR]:\")
;;   (move-to-column 30 t)
;;   (insert (format \"column: %2d\" (current-column)))

;;   ;;---
;;   ;; [OK] Move 3: Final line of buffer is immune to bug:
;;   ;;---
;;   ;; Move 3: Expect:
;;   ;; move-3:······················█
;;   ;; Move 3: Get:
;;   ;; move-3:······················█
;;   (goto-char (point-max))
;;   (insert \"move-3:[OK]:\")
;;   (move-to-column 30 t)
;;   (insert (format \"column: %2d\" (current-column))))"
;;     (let ((column (nth 0 args))
;;           (force (nth 1 args)))
;;       ;; Bug Conditions:
;;       ;;   1. `whitespace-mode' is on
;;       ;;   2. `move-to-column' is called with 'force' set true.
;;       ;;   3. `ws-butler-keep-whitespace-before-point' is on
;;       ;; Number 3 isn't actually necessary but it's the only time I've
;;       ;; noticed this bug (aside from contriving it in bug hunts/repros).
;;       (when (and (or (bound-and-true-p global-whitespace-mode)
;;                      (bound-and-true-p whitespace-mode))
;;                  force
;;                  ;; Not needed but ws-butler is what triggers this all the time
;;                  ;; so I'll contain my brute force fix to only work if ws-butler
;;                  ;; is setup to expect move-to-column to restore point.
;;                  (bound-and-true-p ws-butler-keep-whitespace-before-point))
;;         ;; Possibly a bugged move-to-column... Let's figure out how far we
;;         ;; have to go.
;;         (save-excursion
;;           (let ((at-last-line (> (forward-line 1) 0)))
;;             (unless at-last-line (forward-line -1))
;;             (move-end-of-line nil)
;;             (when (and (> column (current-column))
;;                        (not at-last-line))
;;               ;; We're in bug territory, and we want past current EOL, and this
;;               ;; line has a '\n' in it, so I think we have a bugged
;;               ;; move-to-column case. Up by one to offset for move-to-column's
;;               ;; off-by-one-in-this-instance bug.
;;               (setq column (1+ column))))))
;;       ;; return list of (fixed or ignored) inputs
;;       (list column force)))
;;   ;; (advice-remove 'move-to-column #'move-to-column@ws-butler/whitespace-mode:fix/force-off-by-one)

  ;; Turn on ws-butler globally.
  (ws-butler-global-mode +1))


;;------------------------------------------------------------------------------
;; View Whitespace
;;------------------------------------------------------------------------------

;; `whitespace'
;;-------------
(use-package whitespace
  :ensure nil ; This is an Emacs built-in feature.
  :demand t

  ;;------------------------------
  :init
  ;;------------------------------

  (defun --/whitespace/frame/childless? ()
    "Return non-nil if current buffer should obey `global-whitespace-mode'.

In this case, buffers that have no `parent-frame' should show whitespace.
Otherwise `whitespace-mode' inundates child frames with whitespace markers, so
this will disable it in child frames to fix all that visual noise.

Add to `whitespace-enable-predicate' via `add-function'."
    (null (frame-parameter nil 'parent-frame)))

  (defun --/hook/whitespace/settings/wide ()
    "I like some whitespace-mode stuff in org-mode, but want less than other modes."
    ;; Make a local copy of `whitespace-style' we can modify and...
    (set (make-local-variable 'whitespace-style)
         ;; ...set it as old one with removed 'too-long line' highlighting.
         (remove 'lines-tail whitespace-style)))

  (defun --/hook/whitespace/before-save ()
    "Invoke `whitespace-cleanup' if the `ws-butler' feature is not present."
    (unless (featurep 'ws-butler)
      (whitespace-cleanup)))


  ;;------------------------------
  :hook
  ;;------------------------------
  ((org-mode-hook    . --/hook/whitespace/settings/wide)
   (yaml-mode-hook   . --/hook/whitespace/settings/wide)
   (before-save-hook . --/hook/whitespace/before-save))


  ;;------------------------------
  :custom
  ;;------------------------------

  ;; Set 'lines-tail/'lines in `whitespace-style' to base off of `fill-column' instead of just hard-coded `80'.
  (whitespace-line-column nil)

  ;; Enable specific styles:
  (whitespace-style
   (quote
    ;;---
    ;; visualization via faces (see set-face-attribute below)
    ;;---
    (face

     ;;---
     ;; general/normal whitespace
     ;;---
     tabs spaces newline

     ;;---
     ;; the bad kind
     ;;---
     trailing space-before-tab space-after-tab

     ;; `empty' lines were annoying as emacs or whitespace is bad at cleaning up
     ;; the visualization when the line is no longer matching this whitespace
     ;; warning type.
     ;;empty       ;; ...lines (...at beginning/end of buffer)

     lines-tail  ;; `lines' would be whole line...
     ;; lines-tail is just whatever's past fill-column

     ;;---
     ;; not sure if want or bad or what.
     ;;---
     indentation

     ;;---
     ;; visualize these whitespaces with non-whitespace chars via display-table
     ;;---
     space-mark tab-mark newline-mark)))


  ;;------------------------------
  :config
  ;;------------------------------

  ;; NOTE: See theme's tweaks (e.g. "source/user/config/theme/zenburn/whitespace.el") for
  ;; changes to face attributes.

  ;; `whitespace-mode' inundates child frames with whitespace markers; this
  ;; fixes all that visual noise.
  (add-function :before-while whitespace-enable-predicate #'--/whitespace/frame/childless?)

  ;; Enable whitespace-mode everywhere.
  (global-whitespace-mode +1))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide :user 'config 'whitespace)
