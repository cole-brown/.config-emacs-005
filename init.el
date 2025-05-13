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
  ;; PRIORITY: `no-littering'
  ;;------------------------------------------------------------------------------
  ;; Keep the `user-emacs-directory' clean by changing where Emacs & packages
  ;; store their data. Move it from various & sundry places in and under
  ;; `user-emacs-directory' to be in one of two `user-emacs-directory'
  ;; sub-directories:
  ;;   - `no-littering-etc-directory'
  ;;   - `no-littering-var-directory'

  (use-package no-littering
    ;; Make sure this loads ASAP. It's used for init/config of other packages.
    :demand t

    ;;------------------------------
    :config
    ;;------------------------------
    ;; Suggested settings: https://github.com/emacscollective/no-littering#suggested-settings

    ;; `recentf' should ignore the files in the `no-littering' dirs.
    (imp-eval-after recentf
      (add-to-list 'recentf-exclude no-littering-etc-directory)
      (add-to-list 'recentf-exclude no-littering-var-directory))

    ;; Auto-saves, `undo-tree' history, etc. should go in the `no-littering' directory.
    ;; They should not be littered in the same dir as their real actual file.
    (no-littering-theme-backups)


    ;; Native Compliation (Emacs 29+):
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

  ;; https://github.com/greduan/emacs-theme-gruvbox
  ;; https://github.com/bbatsov/zenburn-emacs
  ;; https://github.com/edran/hc-zenburn-emacs

  (use-package hc-zenburn-theme
    :demand t
    :config
    (load-theme 'hc-zenburn t))


  ;;------------------------------------------------------------------------------
  ;; PRIORITY: Everything: Emacs Settings
  ;;------------------------------------------------------------------------------

  (defvar --/tab/standard 4 "4")

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

  )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------


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
 '(package-selected-packages
   '(colorful-mode deadgrep git-gutter-fringe git-modes hc-zenburn-theme
                   magit terraform-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
