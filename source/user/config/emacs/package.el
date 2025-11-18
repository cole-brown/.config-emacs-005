;;; user/config/emacs/package.el --- Configure `package' and `use-package' -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2025-11-17
;; Timestamp:  2025-11-17
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Configure `package' and `use-package'.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Emacs's `package'
;;------------------------------------------------------------------------------

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)

(unless package-archive-contents
  (message "[INFO] %s: %s"
           "init.el"
           "Update packages list...")
  (package-refresh-contents))


;;------------------------------------------------------------------------------
;; Emacs's `use-package'
;;------------------------------------------------------------------------------

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
;; The End.
;;------------------------------------------------------------------------------
(imp-provide user (imp-path-relative 'user (imp-path-sans-extension (imp-path-current-file))))
