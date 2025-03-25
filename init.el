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
;; Init package systems:
;;   - Emacs package stuff (e.g. `package.el').
;;   - `use-package'
;;   - `straight'
;;
;; Oh, and `innit:package:upgrade'.
;; Now we're the Postal Upgrade Service.
;;
;;; Code:


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
(customize-set-variable use-package-always-ensure t)

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
(customize-set-variable use-package-hook-name-suffix nil)

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









(message "hello there")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(colorful-mode deadgrep git-gutter-fringe git-modes hc-zenburn-theme
		   magit)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
