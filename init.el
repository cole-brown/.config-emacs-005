;;; init.el --- Init, innit? -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2025-03-13
;; Timestamp:  2025-03-13
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; In the beginning...
;;   -what?
;; No, early-beginning doesn't count.
;; That just gets ready for the beginning.
;; This is the actual beginning.
;; ...anyways.
;;
;; In the beginning there was
;;
;;; Code:


;; TODO MOVE TO ./scripts
(defvar --/refactor/rx-replacements
  ;; NOTE: Try to keep alphabetically sorted.
  `(

    ("core/modules/emacs/" . "namespaced/")

    ;;---
    ;; imp
    ;;---
    ("imp:file:current" . "imp-file-current")

    (,(rx-to-string "imp:flag?" :no-group) . "imp-flag?")

    ("imp:load" . "imp-load")

    ("imp:path:root/set" . "imp-path-root-set")
    ("imp:path:current:dir" . "imp-path-current-dir")

    ("imp:provide" . "imp-provide")

    ("imp:timing" . "imp-timing")
    )
  "alist of regex to replacement string")

(defun --/refactor/cmd ()
  "replace the old with the new"
  (interactive)
  (save-excursion
    (dolist (pair --/refactor/rx-replacements)
      (goto-char (point-min))

      (while (re-search-forward (car pair) nil t)
        (replace-match (cdr pair))))
    (save-buffer)))


;;------------------------------------------------------------------------------
;; Init Settings
;;------------------------------------------------------------------------------

;; (defvar --/init/debugging nil
;;   "Flag for extra output during init.")

;; (defvar --/init/log/level
;;   '((debug . t)
;;     ...todo
;;     )
;;   "todo")

;; (defun --/init/message/debug (format-string &rest args)
;;   "If `--/init/debugging' is non-nil, output via `message'."
;;   (when --/init/debugging
;;     (message format-string args)))

;; (--/init/message "%s: %b" '--/init/debugging --/init/debugging)


;;------------------------------------------------------------------------------
;; External Requirements
;;------------------------------------------------------------------------------

(require 'cl-macs)

(defun --/exe/require (exe)
  "Assert EXE can be found by Emacs."
  (cl-assert (executable-find exe)
             t
             "Emacs cannot find required exe: `%s'"))


(defun --/exe/optional (exe)
  "Warn if EXE cannot be found by Emacs."
  (unless (executable-find exe)
    (warn "Emacs cannot find optional exe: `%s'" exe )))


(--/exe/require "git")


;;------------------------------------------------------------------------------
;; Packages
;;------------------------------------------------------------------------------

;;------------------------------
;; Emacs `package'
;;------------------------------

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)

(unless package-archive-contents
  (message "[%s] %s"
           "init.el"
           "Update packages list...")
  (package-refresh-contents))


;;------------------------------
;; Emacs `use-package'
;;------------------------------

(require 'use-package)
(require 'use-package-ensure)

;; `use-package-always-ensure'
;;---
;; Automatically install package if not found.
;;   https://github.com/jwiegley/use-package#package-installation
;; NOTE: Does not keep anything up-to-date. For that you would use package
;; `auto-package-update' or something similar.
(customize-set-variable 'use-package-always-ensure t)

;; `use-package-hook-name-suffix'
;;---
;; 'When using :hook omit the "-hook" suffix if you specify the hook
;; explicitly, as this is appended by default.
;;
;; [...]
;;
;; If you do not like this behaviour, set use-package-hook-name-suffix to
;; nil. By default the value of this variable is "-hook".'
;;   - https://github.com/jwiegley/use-package#hooks
;;
;; Need to override this to allow using hooks named something
;; other than `*-hook'.
;; Also to avoid confusion when I look at a broken `use-package' and
;; can't fucking what its hooked up to.
(customize-set-variable 'use-package-hook-name-suffix nil)


;;---
;; Debugging Settings:
;;---

;; TODO: auto on when `--debug-init' -- `innit:debug'?
;; TODO: not on otherwise? IDK. Previous .emacs:
;; (setq use-package-compute-statistics    innit:debug?
;;       use-package-verbose               innit:debug?
;;       use-package-minimum-reported-time (if innit:debug? 0 0.1)
;;       use-package-expand-minimally      innit:interactive?)

;; `use-package-verbose'
;;---
;;   "Whether to report about loading and configuration details.
;; If you customize this, then you should require the `use-package'
;; feature in files that use `use-package', even if these files only
;; contain compiled expansions of the macros.  If you don't do so,
;; then the expanded macros do their job silently."
(customize-set-variable 'use-package-verbose t)


;; `use-package-compute-statistics'
;;---
;;   "If non-nil, compute statistics concerned ‘use-package’ declarations.
;; View the statistical report using ‘use-package-report’.  Note that
;; if this option is enabled, you must require ‘use-package’ in your
;; user init file at loadup time, or you will see errors concerning
;; undefined variables."
;;
;; I love stats.
(customize-set-variable 'use-package-compute-statistics t)


;; `use-package-minimum-reported-time'
;;---
;;   "Minimal load time that will be reported.
;; Note that ‘use-package-verbose’ has to be set to a non-nil value
;; for anything to be reported at all."
;;
;; Default:       0.1 (seconds)
;; Do-Not-Report: nil
;;
;; Hm... I love stats... Try 0?
;; Or set to nil because I'm trying to make `imp' be all that?
(customize-set-variable 'use-package-minimum-reported-time 0)


;; `use-package-expand-minimally'
;;---
;; "If non-nil, make the expanded code as minimal as possible.
;; This disables:
;;   - Printing to the *Messages* buffer of slowly-evaluating forms
;;   - Capturing of load errors (normally redisplayed as warnings)
;;   - Conditional loading of packages (load failures become errors)"
;; blah blah main advantage blah byte-compile blah SPEEEEEEED!
;;
;; NO!
;; Do not want to optimize for SPEEEEEEEED!
;; Want to optimize for less stress when everything is broken.
;; It defaults to off/nil but still...
(customize-set-variable 'use-package-expand-minimally nil)


;;------------------------------------------------------------------------------
;; PRIORITY: `imp'
;;------------------------------------------------------------------------------

;; Let this load error; imps are a fundamental to this init.
(load (expand-file-name "source/core/packages/imp/init.el" user-emacs-directory))

;; Have imp time all `imp-load' and output a pretty buffer of info.
;; This is now default.
;; (customize-set-variable 'imp-timing-enabled? t)

;; TODO: set root? get rid of imp roots feature? IDK.
(imp-path-root-set :emacs.d
                   user-emacs-directory)

(imp-path-root-set :user
                   (imp-path-join user-emacs-directory 'source 'user))

;;------------------------------
;; Time the Rest
;;------------------------------
;; Put the rest of user Emacs init under the `:emacs.d' namespace.
(imp-timing :emacs.d
    (imp-path-current-file)
    (imp-path-current-dir)

  ;;------------------------------------------------------------------------------
  ;; BASICALLY REQUIRED: `namespaced'
  ;;------------------------------------------------------------------------------
  ;; I've been making helpers for a few years because Emacs' naming scheme is...
  ;; um...
  ;; And also matching a function to its purpose is also...
  ;; ...
  ;;
  ;; What does this does? What files can it be used on?
  ;;   - `convert-standard-filename'
  ;;
  ;; Which one of these do you need?
  ;;   - `directory-file-name'
  ;;   - `file-name-directory'
  ;;
  ;; Anyways, the idea with `namespace' is to just do this and browse:
  ;;   C-h f path<TAB>
  ;;   C-h f str<TAB>

  (imp-load :feature :namespaced
            :path    "source/core/packages/namespaced/init.el") ; (imp-path-join :emacs.d 'source 'core 'packages 'namespaced 'init.el)


  ;;------------------------------------------------------------------------------
  ;; PRIORITY: `no-littering'
  ;;------------------------------------------------------------------------------
  ;; Keep the `user-emacs-directory' clean by changing where Emacs & packages
  ;; store their data. Move it from various & sundry places in and under
  ;; `user-emacs-directory' to be in one of two `user-emacs-directory'
  ;; sub-directories:
  ;;   - `no-littering-etc-directory'
  ;;   - `no-littering-var-directory'

  (use-package no-littering
    ;; Make sure this loads ASAP.
    ;; It's dictating where other packages can write their shit to.
    :demand t

    ;;------------------------------
    :config
    ;;------------------------------
    ;; Suggested settings: https://github.com/emacscollective/no-littering#suggested-settings

    ;; backups
    ;;--------
    ;; Backups like auto-saves, `undo-tree' history, etc. should go in the
    ;; `no-littering' directory. They should not be littered in the same dir as
    ;; their real actual file.
    ;; NOTE: "theme" is overloaded here. It has nothing to do with `zenburn' or
    ;; whatever is theming my UI. It is "them[ing] locations where backups of
    ;; various sorts are created".
    (no-littering-theme-backups)

    ;; `recentf'
    ;;----------
    ;; `recentf' should ignore the files in the `no-littering' dirs.
    (imp-eval-after recentf
      (add-to-list 'recentf-exclude no-littering-etc-directory)
      (add-to-list 'recentf-exclude no-littering-var-directory))

    ;; 'custom.el'
    ;;------------
    ;; TODO: send to /dev/null instead. Get code for that from 2023's repo.
    ;; NOTE: Not doing this because I want everything in my init.  No package or
    ;; face will be added manually. I want my .emacs to be reproducable. To get
    ;; a new computer up and running should just be:
    ;;   1) install Emacs
    ;;   2) pull this repo to the correct place
    ;;   3) start Emacs
    ;;---
    ;; ;; Move `custom-set-variables' and `custom-set-faces' out if this file.
    ;; ;; Put in `etc/' instead.
    ;; (setq custom-file (no-littering-expand-etc-file-name "custom.el"))

    ;; Native Compliation (Emacs 29+)
    ;;-------------------------------
    (when (fboundp 'startup-redirect-eln-cache)
      (startup-redirect-eln-cache
       (convert-standard-filename
        (no-littering-expand-var-file-name "eln-cache/")))))


  ;;------------------------------------------------------------------------------
  ;; PRIORITY: Theme
  ;;------------------------------------------------------------------------------
  ;; If this doesn't happen sooner, the Emacs frame will pop, flicker, and
  ;; maybe resize. Set theme ASAP and hope it gets resolved before the OS can
  ;; show Emacs to us.
  ;;   Caveat: Or at least, it acted that way 5 or 10 years ago.
  ;;   I've not done any science on it recently though.

  ;; `emacs-theme-gruvbox'
  ;;----------------------
  ;; https://github.com/greduan/emacs-theme-gruvbox
  ;; Looks good.
  ;; Will try as soon as `zenburn' is proven to cause cancer.

  ;; `hc-zenburn-theme'
  ;;-------------------
  ;; High Contrast Zenburn
  ;; https://github.com/edran/hc-zenburn-emacs
  ;;-----
  ;; [2025-05-16] Oh. This is unmaintained. :( I was liking it.
  ;;   But... last commit was a decade ago.
  ;;   :thinking:
  ;;   Nah. Go back to regular zenburn.
  ;;   hc-zenburn doesn't even have all the named zenburn colors.
  ;;   ;; (use-package hc-zenburn-theme
  ;;   ;;   :demand t
  ;;   ;;   :config (load-theme 'hc-zenburn t))

  ;; `zenburn'
  ;;----------
  ;; https://github.com/bbatsov/zenburn-emacs
  ;;---
  ;; It's just...... good.
  (imp-load :feature '(:user config theme zenburn)
            :path  "config/theme/zenburn/init.el") ; (imp-path-join 'config 'theme 'zenburn 'init.el))


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
  ;; PRIORITY: Everything: Emacs Settings
  ;;------------------------------------------------------------------------------

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

  ;; Set Emacs' standard to our's.
  (customize-set-variable 'standard-indent --/tab/standard)

  ;; Long would be... 8, I think? But that's ridiculous and I don't like it so
  ;; it's not here until it's needed.


  ;;------------------------------
  ;; Tab Settings
  ;;------------------------------
  ;; https://www.emacswiki.org/emacs/NoTabs
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Just-Spaces.html
  ;; https://www.emacswiki.org/emacs/TabsAreEvil
  ;; https://www.emacswiki.org/emacs/SmartTabs

  ;; Always use spaces; never use tabs.
  (customize-set-variable 'indent-tabs-mode nil)

  ;; Set default tab width for all buffers.
  (customize-set-variable 'tab-width --/tab/standard)

  ;; Make sure this is at it's default of nil, because:
  ;;   "A value of nil means a tab stop every `tab-width' columns."
  (customize-set-variable 'tab-stop-list nil)

  ;; NOTE: M-x tabify and M-x untabify exist and work on regions.


  ;;------------------------------------------------------------------------------
  ;; Chrome?: Colorize Color Codes
  ;;------------------------------------------------------------------------------

  ;; https://github.com/DevelopmentCool2449/colorful-mode
  ;; https://elpa.gnu.org/packages/colorful-mode.html
  (use-package colorful-mode
    :demand t
    :diminish

    :custom
    (colorful-use-prefix t)

    ;; TODO: Can it trigger in comments as well?
    ;; #f00
    ;; "#f00"


    :config
    ;; Switch to hooking `colorful-mode' if global is too much.
    (global-colorful-mode +1))


  ;;------------------------------------------------------------------------------
  ;; Secret: Init
  ;;------------------------------------------------------------------------------

  ;; TODO: paths per computer machine host
  ;; use (system-name)?
  ;; system-type and system-name?
  ;; What does the multiplexing do in sn004?
  (message (mapconcat
            #'identity
            '("System:"
              "  - system-type: %s"
              "  - system-name: %s"
              )
            "\n")
           system-type
           system-name)
  ;; TODO: for now, this way.
  (pcase system-type
    ('gnu/linux
     ;; TODO: macro should allow funcs & symbols for values in plist. e.g. call
     ;; to figure out path from system-name/host.
     (imp-load :feature :secret.d
               :path    "~/ocean/vault/.config/secret/emacs/2025-03-13_sn005/init.el"))
    ('windows-nt
     ;; TODO: macro should allow funcs & symbols for values in plist. e.g. call
     ;; to figure out path from system-name/host.
     (imp-load :feature :secret.d
               :path    "~/.secret.d/emacs/2025-03-13_sn005/init.el")))


  ;;--------------------------------------------------------------------------------
  ;; (Automatic) Time Stamps
  ;;--------------------------------------------------------------------------------

  (require 'rx)
  (use-package time-stamp
    :ensure nil ; This is an Emacs built-in feature; don't need to install the package.

    ;;------------------------------
    ;; NOTE: Modes that want timestamps, do something like this:
    ;;------------------------------
    ;; Example: `org-mode':
    ;; ;;------------------------------
    ;; :hook
    ;; ;;------------------------------
    ;; ((org-mode-hook    . --/hook/time-stamp/settings
    ;;  (before-save-hook . --/hook/time-stamp/before-save))


    ;;------------------------------
    ;; WARNING
    ;;------------------------------
    ;; The docstrings for the \\[time-stamp] custom vars says that you
    ;; should not change them globally:
    ;;   > These variables are best changed with file-local variables.
    ;;   > If you were to change `time-stamp-pattern', `time-stamp-line-limit',
    ;;   > `time-stamp-start', or `time-stamp-end' in your init file, you
    ;;   > would be incompatible with other people's files.
    ;;
    ;; The file comments also warn:
    ;;   > ;;; Do not change time-stamp-line-limit, time-stamp-start,
    ;;   > ;;; time-stamp-end, time-stamp-pattern, time-stamp-inserts-lines,
    ;;   > ;;; or time-stamp-count in your .emacs or you will be incompatible
    ;;   > ;;; with other people's files!  If you must change them, do so only
    ;;   > ;;; in the local variables section of the file itself.
    ;;
    ;; See: /usr/share/emacs/28.1/lisp/time-stamp.el.gz
    ;;
    ;; So we'll make hook funcs to set them locally.


    ;;------------------------------
    :init
    ;;------------------------------

    (defun --/hook/time-stamp/settings ()
      "Timestamp settings hook function for major modes.

Set `time-stamp' local vars.

Add to mode's hook variable (usually `MODE-hook', sometimes
something else).
e.g. for `use-package org'
```
  :hook
  ((org-mode-hook    . --/hook/time-stamp/settings
   (before-save-hook . --/hook/time-stamp/before-save))
```

NOTE: This assumes you have set `use-package-hook-name-suffix' to nil:
  (customize-set-variable 'use-package-hook-name-suffix nil)"
      ;; `time-stamp-pattern' (effective) default:
      ;;    "8/Time-stamp:[ \t]+\\\\?[\"<]+%:y-%02m-%02d %02H:%02M:%02S %u\\\\?[\">]"
      ;;  Example:
      ;;    "Time-stamp: <2987-06-05 10:11:12 username>"
      ;;
      ;; ...but `time-stamp-pattern' is a cluttered and confusing combination of
      ;; 3 or more separate variables.  Set the separate variables instead and
      ;; let `time-stamp' figure it out.

      ;;---
      ;; Search Limits
      ;;---
      ;; How far to search a file for the time-stamp?
      ;;   - Positive: from start of file
      ;;   - Negative: from end of file
      ;; default: 8
      (setq-local time-stamp-line-limit 20)

      ;;---
      ;; Search Regexes
      ;;---
      ;; Start!
      ;;   default: "Time-stamp:[ \t]+\\\\?[\"<]+"
      ;;   aka:     (rx "Time-stamp:"
      ;;                (one-or-more (any " " "\t"))
      ;;                (optional "\\")
      ;;                (one-or-more (any ?\" "<")))
      ;; Should we try to be compatible while also letting ourself do timestamps like?:
      ;;   "Timestamp: 2310-03-04" (most files?)
      ;;   "TIMESTAMP: 2310-03-04" (org-mode file property)
      (setq-local time-stamp-start
                  (rx (or "Timestamp" "Time-stamp" "Time-Stamp" "TIMESTAMP")
                      ":"
                      (one-or-more (any " " "\t"))
                      (optional "\\") ; ...wut? Escape char in some file types for "<" maybe?
                      (optional (any ?\" "<"))))

      ;; End!
      ;;   default: "\\\\?[\">]"
      ;;   aka:     (rx (optional "\\") (any ?\" ">"))
      ;;
      ;; Should we try to be compatible while also letting ourself do timestamps like?:
      ;;   "Timestamp: 2310-03-04"
      (setq-local time-stamp-end
                  (rx (or (and (optional "\\")
                               (any ?\" ">"))
                          "\n")))

      ;;---
      ;; Time Format
      ;;---
      ;; default: "%Y-%02m-%02d %02H:%02M:%02S %l"
      (setq-local time-stamp-format "%Y-%02m-%02d"))


    ;; This hook is merely a namespacing so that it's easier for me to remember.
    ;; Could just as easily do (in use-package):
    ;;   :hook
    ;;   ((org-mode-hook . --/hook/time-stamp/settings
    ;;    (before-save-hook . time-stamp))
    ;; But it's easier to grok when both of 'em look like they belong together:
    ;;   :hook
    ;;   ((org-mode-hook    . --/hook/time-stamp/settings
    ;;    (before-save-hook . --/hook/time-stamp/before-save))
    (defun --/hook/time-stamp/before-save ()
      "Auto-timestamp files before save.

Add to hook variable `before-save-hook'.
e.g. for `use-package org'
```
  :hook
  ((org-mode-hook    . --/hook/time-stamp/settings
   (before-save-hook . --/hook/time-stamp/before-save))
```

NOTE: This assumes you have set `use-package-hook-name-suffix' to nil:
  (customize-set-variable 'use-package-hook-name-suffix nil)"

      ;; This hook is merely a namespacing so that it's easier for me to remember.
      ;; Could just as easily do (in use-package):
      ;;   :hook
      ;;   ((org-mode-hook . --/hook/time-stamp/settings
      ;;    (before-save-hook . time-stamp))
      ;; But it's easier to grok when both of 'em look like they belong together:
      ;;   :hook
      ;;   ((org-mode-hook    . --/hook/time-stamp/settings
      ;;    (before-save-hook . --/hook/time-stamp/before-save))
      (time-stamp)))


  ;;------------------------------------------------------------------------------
  ;; Org-Mode
  ;;------------------------------------------------------------------------------

  ;; TODO THIS
  ;; (imp-load :feature '(:user config org)
  ;;           :path  "config/org/init.el"  ;;(imp-path-join 'config 'org 'init.el)
  ;;           )


  ;;------------------------------------------------------------------------------
  ;; Search: Deadgrep (uses Ripgrep)
  ;;------------------------------------------------------------------------------

  ;;------------------------------
  ;; ripgrep
  ;;------------------------------
  ;; https://github.com/BurntSushi/ripgrep
  ;;
  ;; This needs installed on the computer separately from Emacs.
  ;;
  ;;   "ripgrep is a line-oriented search tool that recursively searches your
  ;; current directory for a regex pattern. By default, ripgrep will respect your
  ;; .gitignore and automatically skip hidden files/directories and binary files.
  ;; ripgrep has first class support on Windows, macOS and Linux, with binary
  ;; downloads available for every release. ripgrep is similar to other popular
  ;; search tools like The Silver Searcher, ack and grep."


  ;;------------------------------
  ;; deadgrep
  ;;------------------------------
  ;; https://github.com/Wilfred/deadgrep
  ;; "Deadgrep is the fast, beautiful text search that your Emacs deserves."
  (use-package deadgrep
    :demand t

    ;;------------------------------
    :init
    ;;------------------------------

    (defun //deadgrep/default-directory (search-term)
      "Search for SEARCH-TERM with `deadgrep' at `default-directory'."
      (interactive (list (deadgrep--read-search-term)))
      (call-interactively #'deadgrep
                          search-term
                          default-directory))


    (defun //deadgrep/buffer/kill-all ()
      "Kill all deadgrep buffers."
      (interactive)
      (message "[%s] Kill 'deadgrep' buffers..."
               (datetime:format 'rfc-3339 'datetime))
      (buffer:kill:matching ".*deadgrep.*"
                            :internal
                            :modified
                            :process))

    ;; ;;------------------------------
    ;; :config
    ;; ;;------------------------------
    ;;
    ;; ;;---
    ;; ;; Project Root Overrides
    ;; ;;---
    ;; ;; TODO: Search per-comp configs for `deadgrep-project-root' to find what's set?
    ;; ;;   - `deadgrep-project-root-overrides'
    ;; ;;   - `deadgrep-project-root-function'
    )

  ;;------------------------------------------------------------------------------
  ;; Magit & Friends
  ;;------------------------------------------------------------------------------
  ;;
  ;; The best Git Porcelain:
  ;;   https://magit.vc/
  ;;   https://github.com/magit/magit
  ;;
  ;; Plus some other version control things.
  ;; Whatever.
  ;; The important thing is: Magit.


  ;;------------------------------------------------------------------------------
  ;; Magit: Git Front-End (Porcelain)
  ;;------------------------------------------------------------------------------
  ;; The best Git Porcelain.
  ;;   https://magit.vc/
  ;;   https://github.com/magit/magit

  (use-package magit

    ;;------------------------------
    :init
    ;;------------------------------

    ;; (defun mantle:user:magit:buffer:kill ()
    ;;   "Kill all magit buffers."
    ;;   (interactive)
    ;;   (message "[%s] Kill all 'magit' buffers..."
    ;;            (datetime:format 'rfc-3339 'datetime))
    ;;   (buffer:kill:matching ".*magit.*"
    ;;                         :internal
    ;;                         :modified
    ;;                         :process))
    )

  ;;------------------------------------------------------------------------------
  ;; Git Gutter
  ;;------------------------------------------------------------------------------
  ;; https://github.com/emacsorphanage/git-gutter-fringe

  (use-package git-gutter-fringe
    ;; NOTE: `git-gutter-fringe' does not work in the terminal. Use `git-gutter' if
    ;; needed there.
    :when (display-graphic-p)
    :diminish

    ;;------------------------------
    :config
    ;;------------------------------
    (global-git-gutter-mode +1))

  ;;------------------------------------------------------------------------------
  ;; Git File Modes
  ;;------------------------------------------------------------------------------

  (use-package git-modes
    :defer t

    ;;------------------------------
    :config
    ;;------------------------------

    ;; Docker's ignore file is basically the same format as .gitignore, so use the
    ;; `gitignore-mode' for it.
    (add-to-list 'auto-mode-alist
                 (cons "/.dockerignore\\'" 'gitignore-mode)))




  ;;------------------------------------------------------------------------------
  ;; AI: ChatGPT
  ;;------------------------------------------------------------------------------

  ;; https://github.com/karthink/gptel
  (use-package gptel

    ;;------------------------------
    :init
    ;;------------------------------

    (defvar --/gptel/directive/default
      (string-join
       '("You are a large language model living in Emacs and a helpful assistant."
         "Respond concisely and provide reference links or code examples."
         ""
         "Default to C# for code."
         "There are 2 important .NET versions in use:"
         "1. .NET Framework ≥4.8.1 (latest release of .NET Framework)"
         "2. .NET ≥8 (latest LTS release of .NET (Core))")
       "\n")
      "For adding to alist `gptel-directives' and/or var `gptel--system-message'.")


    ;;------------------------------
    :hook
    ;;------------------------------
    (gptel-mode-hook . visual-line-mode)


    ;;------------------------------
    :custom
    ;;------------------------------

    (gptel-api-key (plist-get --/secret/key/openai :key))

    (gptel-model 'gpt-4o)

    ;; Customize prompt.
    (gptel--system-message --/gptel/directive/default)

    ;; Default: `markdown-mode' if available, else `text-mode'
    ;; ...why would you ever not use org?
    (gptel-default-mode 'org-mode)


    ;;------------------------------
    :config
    ;;------------------------------

    ;; `default' doesn't provide examples as often as I want.
    (add-to-list 'gptel-directives
                 (cons '--/gptel/directive/default --/gptel/directive/default)))
  ;; (pp gptel-directives)


  ;;------------------------------------------------------------------------------
  ;; dev-env: Language: Terraform (HCL)
  ;;------------------------------------------------------------------------------
  ;; 2023-07-23_sn004:/mantle/config/dev-env/languages/terraform.el

  ;; ;;------------------------------------------------------------------------------
  ;; ;; Sanity Check
  ;; ;;------------------------------------------------------------------------------

  ;; Lodge a complaint if 'terraform' isn't installed on the system. But don't
  ;; skip the `terraform-mode' `use-package', since it only needs the exe for the
  ;; compile stuff.
  (--/exe/optional "terraform")

  ;; ;;------------------------------------------------------------------------------
  ;; ;; Syntax Highlighting
  ;; ;;------------------------------------------------------------------------------

  (use-package terraform-mode
    ;;------------------------------
    :custom
    ;;------------------------------

    ;; `terraform fmt` uses 2 spaces per indent level
    (terraform-indent-level 2))


  ;;------------------------------------------------------------------------------
  ;; Fin: End of user Emacs init.
  ;;------------------------------------------------------------------------------
  ;; Clean up, final hellos, load summeries...

  ;; Prep imp for outputting the timing summary.
  ;; Just starts a timer to do something after a second of activity after Emacs
  ;; runs its "init it done(ish), guys" hook.
  (imp-timing-final)

  ;; TODO: saved quote or something.
  (message "hello there")


  ;;------------------------------------------------------------------------------
  ;; The End of Time.
  ;;------------------------------------------------------------------------------
  ) ;; END: imp-timing


;;------------------------------------------------------------------------------
;; The End of File.
;;------------------------------------------------------------------------------
;; (imp-provide :emacs.d 'init)

;;------------------------------------------------------------------------------
;; TODO: Not in here.
;;------------------------------------------------------------------------------

;; ;; Move `custom-set-variables' and `custom-set-faces' out if this file.
;; ;; TODO: send to /dev/null instead? Get code for that from 2023's repo.
;; (setq custom-file (no-littering-expand-etc-file-name "custom.el"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
