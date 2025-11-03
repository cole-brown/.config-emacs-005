;;; init.el --- Init, innit? -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2025-03-13
;; Timestamp:  2025-10-30
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; NOTE: Personal Namespaces for `defun', `defvar', `def.*':
;;---------------------------
;;   Private - Things that will only be used in my init code, probably.
;;     `--/' - general purpose "I made this" prefix
;;   Public:
;;     `/'   - general purpose "I made this" prefix
;;
;; NOTE:
;;------
;;
;;
;; Anyway
;;-------
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





;;------------------------------------------------------------------------------
;; TEMP Scripts for Recovering from Emacs Bankruptcy
;;------------------------------------------------------------------------------
;; TODO MOVE TO ./scripts

(defvar --/refactor/rx-replacements
  ;; NOTE: Try to keep alphabetically sorted.
  `(

    ("core/modules/emacs/" . "namespaced/")

    ;; `imp'
    ;;------
    ("imp:eval:after" . "imp-eval-after")
    ("imp:file:current" . "imp-file-current")
    (,(rx-to-string "imp:flag?" :no-group) . "imp-flag?")
    ("imp:load" . "imp-load")
    ("imp:path:root/set" . "imp-path-root-set")
    ("imp:path:current:dir" . "imp-path-current-dir")
    ("imp:provide" . "imp-provide")
    ("imp:timing" . "imp-timing")
    ("imp:use-package" . "use-package")


    ("mantle/config/" . "source/user/config/")
    (":mantle 'config" . ":user 'config")
    )
  "alist of regex to replacement string")

(defun /cmd/refactor/by-rx ()
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

;; TODO: imp-load user w/ root flag?
;; imp-roots
(imp-path-root-set 'user
                   (imp-path user-emacs-directory 'source 'user))

;;------------------------------
;; Time the Rest
;;------------------------------
;; Put the rest of user Emacs init under the `:emacs.d' namespace.
(imp-timing :emacs.d
    (imp-path-current-file)

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

  (imp namespaced
    :root
    :path (imp-path user-emacs-directory 'source 'core 'packages 'namespaced 'init.el))


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
  (imp user:/config/theme/zenburn/init.el)


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
  (imp secret.d
    :root
    :path (pcase system-type
            ('gnu/linux
             "~/ocean/vault/.config/secret/emacs/2025-03-13_sn005/init.el")
            ('windows-nt
             "~/.secret.d/emacs/2025-03-13_sn005/init.el")))


  ;;--------------------------------------------------------------------------------
  ;; Completion Frameworks
  ;;--------------------------------------------------------------------------------
  ;; "VMOCE Stack": Vertico & Friends
  ;;   - `vertico':    https://github.com/minad/vertico
  ;;   - `marginalia': https://github.com/minad/marginalia
  ;;   - `orderless':  https://github.com/oantolin/orderless
  ;;   - `consult':    https://github.com/minad/consult
  ;;   - `embark':     https://github.com/oantolin/embark
  ;;   - `embark-consult'
  ;;
  ;; Also Other Stuff, Inculding:
  ;;   - `corfu':   https://github.com/minad/corfu
  ;;   - `cape':    https://github.com/minad/cape
  ;;   - `dabbrev': https://www.gnu.org/software/emacs/manual/html_node/emacs/Dynamic-Abbrevs.html

  (imp user:/config/completion)

  ;;------------------------------
  ;; Completion: `yasnippets'
  ;;------------------------------
  (imp user:/config/snippets)


  ;;------------------------------------------------------------------------------
  ;; Emacs
  ;;------------------------------------------------------------------------------

  (imp user:/config/emacs)

  (imp user:/config/help)


  ;;------------------------------------------------------------------------------
  ;; Whitespace
  ;;------------------------------------------------------------------------------

  (imp user:/config/whitespace)


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
  ;; Files
  ;;------------------------------------------------------------------------------

  (imp user:/config/files)


  ;;------------------------------------------------------------------------------
  ;; Org-Mode
  ;;------------------------------------------------------------------------------

  (imp user:/config/org/init)


  ;;------------------------------------------------------------------------------
  ;; Languages
  ;;------------------------------------------------------------------------------
  ;; TODO: make/load "config/languages/init.el", which will load all

  (imp user:/config/languages/common)

  (imp user:/config/languages/elisp)

  (imp user:/config/languages/elisp)


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
  ;; AI: ChatGPT & Friends!
  ;;------------------------------------------------------------------------------

  (imp user:/config/ai)

  ;;------------------------------------------------------------------------------
  ;; dev-env: Language: YAML
  ;;------------------------------------------------------------------------------
  ;; 2023-07-23_sn004:sn004/mantle/config/dev-env/languages/yaml.el

  ;; `yaml-mode'
  ;;------------
  (use-package yaml-mode
    ;;------------------------------
    :init
    ;;------------------------------

    (defun --/hook/yaml/settings ()
      "Settings for YAML mode. Non-LSP stuff."

      ;; `fill-column' is always a buffer-local var (see its help).
      ;; Use `setq-local' so we remember what to use for things that aren't auto-buffer-local?
      (setq-local fill-column --/fill-column/standard)

      ;; NOTE [OLD]: `yaml-mode' does not use `tab-width'. It uses its own var: `yaml-indent-offset'.
      ;; ;; Use smaller indents than is standard for code.
      ;; (setq tab-width yaml-indent-offset)
      )


    ;;------------------------------
    :hook
    ;;------------------------------
    (yaml-mode-hook . --/hook/yaml/settings)


    ;;------------------------------
    :custom
    ;;------------------------------

    ;; Use smaller indents than is standard for code.
    ;; NOTE: `yaml-indent-offset' is 2 by default. Set it explicitly in case I change my mind about tab sizes.
    (yaml-indent-offset --/tab/small))


  ;;------------------------------------------------------------------------------
  ;; dev-env: Language: Terraform (HCL)
  ;;------------------------------------------------------------------------------
  ;; 2023-07-23_sn004:/mantle/config/dev-env/languages/terraform.el

  ;;------------------------------------------------------------------------------
  ;; Sanity Check
  ;;------------------------------------------------------------------------------

  ;; Lodge a complaint if 'terraform' isn't installed on the system. But don't
  ;; skip the `terraform-mode' `use-package', since it only needs the exe for the
  ;; compile stuff.
  (--/exe/optional "terraform")

  ;;------------------------------------------------------------------------------
  ;; Syntax Highlighting
  ;;------------------------------------------------------------------------------

  ;; `terraform-mode'
  ;;-----------------
  (use-package terraform-mode
    ;;------------------------------
    :custom
    ;;------------------------------

    ;; `terraform fmt` uses 2 spaces per indent level
    (terraform-indent-level 2))


  ;;------------------------------------------------------------------------------
  ;; dev-env: Rest Client/Language: Hurl
  ;;------------------------------------------------------------------------------

  ;;`hurl-mode'
  ;;-----------
  ;; https://hurl.dev/docs/installation.html
  ;; https://github.com/JasZhe/hurl-mode
  (use-package hurl-mode
    :mode "\\.hurl\\'"

    ;;------------------------------
    :init
    ;;------------------------------
    ;; `package-vs-install' "bug":
    ;;   1. It wants to be interactive.
    ;;   2. It doesn't know what to do if the package is already installed.
    ;; Therefore, hide behind installed check:
    (unless (package-installed-p 'hurl-mode)
      ;; Not on (M)ELPA. Tell Emacs where/how to get it.
      (package-vc-install "https://github.com/JasZhe/hurl-mode")))


  ;;------------------------------------------------------------------------------
  ;; Fin: End of user Emacs init.
  ;;------------------------------------------------------------------------------
  ;; Clean up, final hellos, load summeries...

  ;; TODO: Can I put this in the `imp' section?
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
;; (imp-provide emacs.d init)

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
 '(org-fold-catch-invisible-edits 'show-and-error nil nil "Customized with use-package org")
 '(package-selected-packages
   '(cape colorful-mode corfu deadgrep elisp-demos embark-consult git-gutter-fringe
          git-modes gptel hc-zenburn-theme helpful highlight-quoted hurl-mode
          macrostep magit marginalia minions no-littering orderless ox-gfm
          rainbow-delimiters terraform-mode vertico ws-butler yaml-mode
          yasnippet zenburn-theme))
 '(package-vc-selected-packages
   '((hurl-mode :vc-backend Git :url "https://github.com/JasZhe/hurl-mode"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
