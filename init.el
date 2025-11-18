;;; init.el --- Init, innit? -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2025-03-13
;; Timestamp:  2025-11-17
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
;;; Code:

;;------------------------------------------------------------------------------
;; In the beginning there was `imp'
;;------------------------------------------------------------------------------
;; Let this load error; imps are fundamental to this init.
(load (expand-file-name "source/core/packages/imp/init.el" user-emacs-directory))

;; Have imp time all `imp-load' and output a pretty buffer of info.
;; This is now default.
;; (customize-set-variable 'imp-timing-enabled? t)

;; (pp imp-roots)
(imp-path-root-set 'user
                   (imp-path user-emacs-directory 'source 'user))


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
;;------------------------------
;; Time the Rest
;;------------------------------
;;------------------------------------------------------------------------------
;; Put the rest of user Emacs init under the `:emacs.d' namespace.
(imp-timing 'emacs.d
    (imp-path-current-file)

  ;;----------------------------------------------------------------------------
  ;; Packages: `package' & `use-package'
  ;;----------------------------------------------------------------------------
  (imp user:/config/emacs/package)


  ;;----------------------------------------------------------------------------
  ;; BASICALLY REQUIRED: `namespaced'
  ;;----------------------------------------------------------------------------
  ;; TODO: Move this comment block to namespaced/init.el header
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


  ;;----------------------------------------------------------------------------
  ;; PRIORITY: `no-littering'
  ;;----------------------------------------------------------------------------
  ;; Keep the `user-emacs-directory' clean by changing where Emacs & packages
  ;; store their data. Move it from various & sundry places in and under
  ;; `user-emacs-directory' to be in one of two `user-emacs-directory'
  ;; sub-directories:
  ;;   - `no-littering-etc-directory'
  ;;   - `no-littering-var-directory'

  (imp user:/config/emacs/no-littering)

  ;;----------------------------------------------------------------------------
  ;; PRIORITY: Theme
  ;;----------------------------------------------------------------------------
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


  ;;----------------------------------------------------------------------------
  ;; The Binding of Emacs
  ;;----------------------------------------------------------------------------
  ;; Key (un)binds for Emacs itself.
  ;; Package's keybinds should go in their init file.

  (imp user:/config/emacs/keybinds)


  ;;----------------------------------------------------------------------------
  ;; Secret: Init
  ;;----------------------------------------------------------------------------

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


  ;;----------------------------------------------------------------------------
  ;; Completion Frameworks
  ;;----------------------------------------------------------------------------
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
  (imp user:/config/snippets)


  ;;----------------------------------------------------------------------------
  ;; Emacs
  ;;----------------------------------------------------------------------------
  ;; Config for Emacs Actual or global stuff.

  ;; TODO(init): move all/any of these to new "user/config/emacs/" dir?
  (imp user:/config/color)
  (imp user:/config/emacs)
  (imp user:/config/help)
  (imp user:/config/emacs/timestamp.el)
  (imp user:/config/undo)
  (imp user:/config/whitespace)


  ;;----------------------------------------------------------------------------
  ;; File Stuff. `recentf', `deadgrep', etc
  ;;----------------------------------------------------------------------------
  (imp user:/config/files)


  ;;----------------------------------------------------------------------------
  ;; Modes
  ;;----------------------------------------------------------------------------
  (imp user:/config/org/init)


  ;;----------------------------------------------------------------------------
  ;; dev-env: Taking the "I" out of IDE.
  ;;----------------------------------------------------------------------------

  ;;------------------------------
  ;; dev-env: Languages
  ;;------------------------------

  ;; TODO(imp): make required/optional exe check an imp thing?
  ;;---
  ;; Lodge a complaint if 'terraform' isn't installed on the system. But don't
  ;; skip the `terraform-mode' `use-package', since it only needs the exe for the
  ;; compile stuff.
  (--/exe/optional "hurl")
  (--/exe/optional "terraform")

  (imp user:/config/languages/common)
  (imp user:/config/languages/elisp)
  (imp user:/config/languages/hurl)
  (imp user:/config/languages/json)
  (imp user:/config/languages/terraform)
  (imp user:/config/languages/yaml)


  ;;------------------------------
  ;; dev-env: Version Control
  ;;------------------------------
  (imp user:/config/magit.el)


  ;;------------------------------
  ;; dev-env: AI: ChatGPT & Friends!
  ;;------------------------------
  (imp user:/config/ai)


  ;;----------------------------------------------------------------------------
  ;; Fin: End of user Emacs init.
  ;;----------------------------------------------------------------------------
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
          rainbow-delimiters terraform-mode undo-tree vertico ws-butler
          yaml-mode yasnippet zenburn-theme))
 '(package-vc-selected-packages
   '((hurl-mode :vc-backend Git :url "https://github.com/JasZhe/hurl-mode"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
