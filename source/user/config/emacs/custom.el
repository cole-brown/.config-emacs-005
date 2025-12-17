;;; user/custom.el --- custom.el eats fresh babies -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2025-03-13
;; Timestamp:  2025-12-16
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; ಠ_ಠ
;; Go away, "custom.el".
;;
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Customizations.html
;;
;; We won't support `customize'/"custom.el" and never will. It's a clumsy
;; interface that sets variables at a time where it can be easily and
;; unpredictably overwritten. Always configure things in the Emacs init files.
;;
;;
;;; Code:


;;------------------------------------ಠ_ಠ---------------------------------------
;; Go away, "custom.el"...            ಠ_ಠ
;;------------------------------------ಠ_ಠ---------------------------------------

(let ((path-for-message (path:join user-emacs-directory "source/user/config/*.el")))
  ;;------------------------------
  ;; Disable some 'custom'/'customize' functions.
  ;;------------------------------
  ;; Disabling like this won't affect using them during init, but will show the
  ;; user the message we assign if they call them interactively.
  (dolist (symbol '(customize-option          customize-browse
                    customize-group           customize-face
                    customize-rogue           customize-saved
                    customize-apropos         customize-changed
                    customize-unsaved         customize-variable
                    customize-set-value       customize-customized
                    ;; customize-set-variable ; This guy is cool... Although there's `setopt' now.
                    customize-apropos-faces
                    customize-save-variable   customize-apropos-groups
                    customize-apropos-options customize-changed-options
                    customize-save-customized))
    (put symbol
         'disabled
         (format "`customize' is bad for a reproducable config; configure Emacs from '%s' instead"
                 path-for-message)))

  ;;------------------------------
  ;; Custom File
  ;;------------------------------
  ;; Move all the customizations to null device or a file that is never loaded.
  ;;   https://www.reddit.com/r/emacs/comments/9rrhy8/emacsers_with_beautiful_initel_files_what_about/e8juc8v
  ;;
  ;; Two options from that reddit thread:

  ;;---
  ;; 1. Null device aka '/dev/null':
  ;;---
  (setq custom-file null-device)

  ;; If you get some weird error like this:
  ;;   > custom-initialize-reset: Renaming: Invalid argument, \
  ;;   >   c:/path/to/tmpasnG58, c:/path/to/NUL
  ;; You should try option #2 instead.

  ;;---
  ;; 2. Move all the customizations to a file that is never loaded.
  ;;---
  ;; NOTE: `null-device' didn't work on Windows with Emacs ~26. Haven't tried in
  ;; a few Emacs versions, but if that's still the case, do one of these
  ;; instead:
  ;;
  ;; ;; Using `no-littering':
  ;; (setq custom-file (no-littering-expand-etc-file-name "custom.ignored.el"))
  ;;
  ;; ;; Not using no-littering:
  ;; (setq custom-file (path:join user-emacs-directory "custom.ignored.el"))


  ;;---
  ;; 3. DEBUGGING:
  ;;---
  ;; Set it back to normal if you need to figure out what Emacs is writing to '/dev/null'.
  ;; (setq custom-file (path:join user-emacs-directory "custom.el"))
  ;; ...Just don't leave it like that.
  )


;;------------------------------------ಠ_ಠ---------------------------------------
;; The End!
;;------------------------------------ಠ_ಠ---------------------------------------
