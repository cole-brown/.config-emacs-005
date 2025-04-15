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
;; `use-package'
;;------------------------------

(require 'use-package)
(require 'use-package-ensure)

;; Automatically install package if not found.
;;   https://github.com/jwiegley/use-package#package-installation
;; NOTE: Does not keep anything up-to-date. For that you would use package
;; `auto-package-update' or something similar.
(customize-set-variable 'use-package-always-ensure t)

;; 'When using :hook omit the "-hook" suffix if you specify the hook
;; explicitly, as this is appended by default.
;;
;; [...]
;;
;; If you do not like this behaviour, set use-package-hook-name-suffix to
;; nil. By default the value of this variable is "-hook".'
;;   - https://github.com/jwiegley/use-package#hooks
;;
;; Need to override this to allow naming hooks something other than
;; `*-hook'.
(customize-set-variable 'use-package-hook-name-suffix nil)

;;---
;; Debugging Settings:
;;---
;; TODO
;; (setq use-package-compute-statistics    innit:debug?
;;       use-package-verbose               innit:debug?
;;       use-package-minimum-reported-time (if innit:debug? 0 0.1)
;;       use-package-expand-minimally      innit:interactive?)

;; TODO
;; TODO
;; TODO no-littering package
;; TODO
;; TODO


;;------------------------------------------------------------------------------
;; PRIORITY: Chrome: Theme
;;------------------------------------------------------------------------------

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
       "1. .NET Framework 4.8.1 (latest release of .NET Framework)"
       "2. .NET 8 (latest LTS release of .NET (Core))")
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
  (add-to-list gptel-directives
	       (cons '--/gptel/directive/default --/gptel/directive/default)))


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



(message "hello there")


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
