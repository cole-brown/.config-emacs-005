;;; mantle/config/emacs.el --- Configure Emacs -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2023-01-24
;; Timestamp:  2025-09-10
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Settings & Config for Emacs Itself
;;
;;; Code:

;;------------------------------------------------------------------------------
;; Frame
;;------------------------------------------------------------------------------

;;------------------------------
;; Large Monitors / Window Splitting
;;------------------------------

;; For `split-window-sensibly'.
;; To split "vertically" (as in a horizontal split so the new and current are
;; now stacked vertically), must have at least this many lines visible RIGHT
;; NOW (not after split). Or set to nil to disable.
;;
;; Annoying right now since my work monitors aren't big enough to bother getting
;; used to more than 2 side-by-sides.
(customize-set-variable 'split-height-threshold nil)


;; For `split-window-sensibly'.
;; To split "horizontally" (as in a vertical split so the new and current are
;; now stacked side-by-side), must have at least this many columns visible RIGHT
;; NOW (not after split). Or set to nil to disable.
;;
;; Annoying right now since my work monitors aren't big enough to bother getting
;; used to more than 2 side-by-sides. 160 is wide enough that it /should/ work
;; for most situations.
(customize-set-variable 'split-width-threshold 160)

;; ;;------------------------------
;; ;; Files / Buffers
;; ;;------------------------------
;;
;; ;; Don't prompt for confirmation when we create a new file or buffer (assume the
;; ;; user knows what they're doing).
;; ;; Default (`after-completion') is to confirm only after auto-completion.
;; (customize-set-variable 'confirm-nonexistent-file-or-buffer nil)

;;------------------------------
;; Bell
;;------------------------------

;; Visual-Only Bell
(customize-set-variable 'ring-bell-function nil)
(customize-set-variable 'visible-bell       t)

;; ;; Standard Bell that goes "Donk!"
;; (customize-set-variable 'ring-bell-function nil)
;; (customize-set-variable 'visible-bell       nil)

;; ;; Disable Bell
;; (customize-set-variable 'ring-bell-function #'ignore)
;; (customize-set-variable 'visible-bell       nil)

;;------------------------------
;; Frame Title / Icon
;;------------------------------

;; This variable has the same structure as `mode-line-format', except that
;; the %c, %C, and %l constructs are ignored.  It is used only on frames for
;; which no explicit name has been set (see `modify-frame-parameters').
;;
;; Basic Frame Title:
;;   (customize-set-variable 'frame-title-format '("｢%b｣" ; buffer name
;;                                                 " @" ; system/host name
;;                                                 (:eval (or (file-remote-p default-directory 'host) system-name))
;;                                                 " — Emacs " ; And I guess we should say what we are...
;;                                                 emacs-version))
;; Bit Fancier:
;; Can't propertize text in the frame title? :'(
(customize-set-variable 'frame-title-format '(;; Buffer Name
                                              (:eval (or (path:buffer:project nil :pretty)
                                                         "%b"))
                                              ;; Remote Host Name
                                              (:eval (when-let ((host/remote (file-remote-p default-directory 'host)))
                                                       (concat " ⊶ " host/remote)))
                                              ;; Program Name/Version
                                              " — Emacs "
                                              emacs-version))

;; This is for the Emacs icon thingy in the OS app list/taskbar.
;; Just have it be the same as the `frame-title-format'.
(customize-set-variable 'icon-title-format frame-title-format)

;;------------------------------
;; Window Border / Divider
;;------------------------------

;; The native border "consumes" a pixel of the fringe on righter-most splits,
;; `window-divider' does not. Available since Emacs 25.1.
(customize-set-variable 'window-divider-default-places       t)
(customize-set-variable 'window-divider-default-bottom-width 1)
(customize-set-variable 'window-divider-default-right-width  1)

(window-divider-mode +1)

;;------------------------------
;; Dialogs & Tooltips
;;------------------------------

;; Disable system's dialog boxes for mouse commands; have Emacs prompt instead.
;; More consistent interaction paradigm? Forces you back to the keyboard? Etc?
(customize-set-variable 'use-dialog-box nil)

;; NOTE: Doom guarded with equivalent of this, but do we actually need to?:
;;   (when innit:os:linux?
(customize-set-variable 'x-gtk-use-system-tooltips nil)

;;--------------------------------------------------------------------------------
;; Misc Emacs Config
;;--------------------------------------------------------------------------------

;;------------------------------
;; Buffer Settings
;;------------------------------

(customize-set-variable 'global-auto-revert-mode t)

;; TODO: fix ns/buffer lib
;; ;; Make `next-buffer', `other-buffer', etc. ignore unreal buffers.
;; (buffer:next/other:ignore :unreal)

;;------------------------------
;; File Time Formats
;;------------------------------

;; TODO: fix ns/datetime lib
;; ;; Set time format for e.g. dired to a better format:
;; ;;   RFC-3339 (human-readable ISO-8601)
;; ;; Original value:
;; ;;   '(\"%d.%m.%Y %H:%M\" \"%d.%m.%Y %H:%M\"))
;; (customize-set-variable 'ls-lisp-format-time-list (list
;;                            ;; Recent Time Format:
;;                            (datetime:format/get 'rfc-3339 'datetime)
;;                            ;; Not-Recent Time Format:
;;                            (datetime:format/get 'rfc-3339 'datetime)))

;; Force use of `ls-lisp-format-time-list' regardless of locale.
(customize-set-variable 'ls-lisp-use-localized-time-format t)

;;------------------------------
;; Cursor / Point
;;------------------------------

;; TODO: fix ns/unit lib for (unit:second 0.75 'sec)
(customize-set-variable 'blink-cursor-interval 0.75) ; default is 0.5 seconds
(customize-set-variable 'blink-matching-paren  nil)

;; Don't stretch the cursor to fit wide characters; it is disorienting,
;; especially for tabs.
(customize-set-variable 'x-stretch-cursor nil)

;; middle-click paste at point, not at click
(customize-set-variable 'mouse-yank-at-point t)

(blink-cursor-mode +1)

;;------------------------------
;; Lines & Columns
;;------------------------------

;; Show line & column in the modeline.
(customize-set-variable 'line-number-mode t)
(customize-set-variable 'column-number-mode t)
;; TODO: Display fancier? (lll,cc) -> lll cc

;; Enable line/column in modeline.
(line-number-mode +1)

;; Just always show line numbers; why not?
(global-display-line-numbers-mode +1)
;; NOTE: Doom is more judicious; only enabling for certain (widely used) base modes:
;; ;; Enable line numbers in most text-editing modes. We avoid
;; ;; `global-display-line-numbers-mode' because there are many special and
;; ;; temporary modes where we don't need/want them.
;; (add-hook! '(prog-mode-hook text-mode-hook conf-mode-hook)
;;            #'display-line-numbers-mode)

;;This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(customize-set-variable 'display-line-numbers-type t)

;; Explicitly define a width to reduce the cost of on-the-fly computation
(customize-set-variable 'display-line-numbers-width 4)

;; Show absolute line numbers for narrowed regions to make it easier to tell the
;; buffer is narrowed, and where you are, exactly.
(customize-set-variable 'display-line-numbers-widen t)


;;------------------------------
;; Scrolling
;;------------------------------

;; Smooth horizontal scrolling for long lines?
(customize-set-variable 'hscroll-margin 2)
(customize-set-variable 'hscroll-step   1)

;; Emacs spends too much effort recentering the screen if you scroll the
;; cursor more than N lines past window edges (where N is the settings of
;; `scroll-conservatively'). This is especially slow in larger files
;; during large-scale scrolling commands. If kept over 100, the window is
;; never automatically recentered.
(customize-set-variable 'scroll-conservatively 101)
(customize-set-variable 'scroll-margin         0)

;; Keep the point on the screen when scrolling.
(customize-set-variable 'scroll-preserve-screen-position t)

;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
;; for tall lines.
(customize-set-variable 'auto-window-vscroll nil)

;; Mouse Wheel Scroll
(customize-set-variable 'mouse-wheel-scroll-amount '(2                     ; Scroll 2 lines normally.
                             ((shift) . hscroll))) ; Scroll horizontally when shift is held.
(customize-set-variable 'mouse-wheel-scroll-amount-horizontal 2)           ; Scroll 2 columns when hscrolling.

;;------------------------------
;; Fringe
;;------------------------------

;; TODO: Do the buffer boundaries interfere with `git-gutter', `flycheck', etc?
(customize-set-variable 'indicate-buffer-boundaries nil)

;; Just puts a bunch of "~" in the fringe for lines _after_ the EOF.
;; Does nothing for in-the-file empty lines.
(customize-set-variable 'indicate-empty-lines nil)

;; Doesn't exist in terminal Emacs, but some Emacs packages (internal and
;; external) use it anyway, leading to a void-function error, so define a no-op
;; substitute to suppress them.
(unless (fboundp 'define-fringe-bitmap)
  (fset 'define-fringe-bitmap #'ignore))

;;------------------------------
;; Paragraphs
;;------------------------------
;; Flycheck uses this for Emacs Lisp docstring checking, but technically it's
;; a paragraph commands setting. Regardless, "double-space between sentences"
;; is a stupid thing to do/lint/think about.
(customize-set-variable 'sentence-end-double-space nil)


;;------------------------------
;; Regions
;;------------------------------

;; Replace selection with typed, pasted, yanked, etc text.
;; [2023-10-06]: Too annoying, especially with `meow'.
;; (delete-selection-mode +1)


;;------------------------------------------------------------------------------
;; Emacs Profiler
;;------------------------------------------------------------------------------

(use-package profiler
  :ensure nil ; This is an Emacs built-in feature.

  ;;------------------------------
  :config
  ;;------------------------------

  ;; Larger column width for function name in profiler reports
  (setf (caar profiler-report-cpu-line-format)    80
        (caar profiler-report-memory-line-format) 80)


  (defvar --/profiler/enabled? nil
    "Boolean guess for if Emacs profiler is enabled.

Only accurate when using `mantle:cmd:profiler:toggle'.")


  (defun --/cmd/profiler/toggle ()
    "Toggle the Emacs profiler. Display the profiling report when toggling off."
    (interactive)
    (if (not --/profiler/enabled?)
        (profiler-start 'cpu+mem)
      (profiler-report)
      (profiler-stop))
    (setq --/profiler/enabled? (not --/profiler/enabled?))))


;;------------------------------------------------------------------------------
;; Minibuffer Tweaks
;;------------------------------------------------------------------------------

;; Typing yes/no is obnoxious when y/n will do
(advice-add #'yes-or-no-p :override #'y-or-n-p)

;; Try to keep the cursor out of the read-only portions of the minibuffer.
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Allow for minibuffer-ception. Sometimes we need another minibuffer command
;; while we're in the minibuffer.
(customize-set-variable 'enable-recursive-minibuffers t)

;; Show current key-sequence in minibuffer ala 'set showcmd' in vim. Any
;; feedback after typing is better UX than no feedback at all.
(customize-set-variable 'echo-keystrokes 0.02)

;; Expand the minibuffer to fit multi-line text displayed in the echo-area. This
;; doesn't look too great with direnv, however...
(customize-set-variable 'resize-mini-windows 'grow-only)

;; Try to keep the cursor out of the read-only portions of the minibuffer.
(customize-set-variable 'minibuffer-prompt-properties '(read-only         t
                                                        intangible        t
                                                        cursor-intangible t
                                                        face              minibuffer-prompt))

;; TODO: Do I want to try this?
;; Emacs 28: Hide commands in M-x which do not work in the current mode.
;; Vertico commands are hidden in normal buffers.
;; (read-extended-command-predicate #'command-completion-default-include-p)


(define-advice completing-read-multiple (:filter-args (args) --/advice/minibuffer/completing-read-multiple/indicator)
  "Add a prompt indicator to `completing-read-multiple'.

We display a prefix with the `crm-separator' in the prompt.
Assuming:
  1. The separator is a comma.
  2. The prompt is \"prompt❯ \"
Example:
  ├CRM:,┤ prompt❯ _"
  ;; Return list of args.
  (cons
   ;; Add prefix w/ `crm-separator' to PROMPT in ARGS.
   (format "├CRM:%s┤ %s" ; Unicode?
           ;; "[CRM:%s] %s" ; ASCII only
           ;; `crm-separator' is a regex (default: "[ /t]*,[ /t]*")
           ;; Trim that down to just the comma?
           (replace-regexp-in-string (rx (or (group string-start
                                                    "["
                                                    (*? whitespace)
                                                    "]*")
                                             (group "["
                                                    (*? whitespace)
                                                    "]*"
                                                    string-end)))
                                     ""
                                     crm-separator)
           (car args))
   ;; Rest of the args should be unmolested.
   (cdr args)))
;; (completing-read-multiple "prompt> " '("alice" "bob" "eve"))


;;------------------------------------------------------------------------------
;; Command Interpreter (comint)
;;------------------------------------------------------------------------------

;; Doom sets this; not sure exact effect?
(customize-set-variable 'comint-prompt-read-only t)

;; Double the default max buffer size.
(customize-set-variable 'comint-buffer-maximum-size 2048)

(use-package ansi-color
  :ensure nil ; This is an Emacs built-in feature.

  ;;------------------------------
  :custom
  ;;------------------------------

  ;; Translate ANSI SGR control sequences into text properties.
  ;; https://en.wikipedia.org/wiki/ANSI_escape_code#SGR
  (ansi-color-for-comint-mode t))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide :user 'config 'emacs)
