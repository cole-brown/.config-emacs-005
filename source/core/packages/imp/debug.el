;;; core/modules/emacs/imp/debug.el --- Delouse the Imps. -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-03-09
;; Timestamp:  2023-06-22
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;                                 ──────────
;; ╔════════════════════════════════════════════════════════════════════════╗
;; ║              Debugging for Imps & Other Mischievous Sprites            ║
;; ╚════════════════════════════════════════════════════════════════════════╝
;;                                   ──────
;;                        Insert all Heisenbugs here.
;;                                 ──────────
;; Please be sure to:
;;   - Delouse the imps.
;;   - Wash the imps with flea and tick shampoo.
;;   - Do not feed the imps crabs or shrimps.
;;   - Promptly squash all bugs on the imps.
;;
;; Please be sure not to:
;;   - Louse the imps.
;;   - Wash the imps with fleas and ticks.
;;   - Squash the imps.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Debugging Toggle
;;------------------------------------------------------------------------------

(defvar imp--debugging? nil
  "Debug flag boolean.")


(defun imp/debug (enabled?)
  "Set imp debugging flag.

Turns on debugging if ENABLED? is non-nil.
Turns off debugging if ENABLED? is nil."
  (setq imp--debugging? (not (null enabled?))))


(defun imp/debug-toggle ()
  "Toggle debugging for imp."
  (interactive)
  (setq imp--debugging? (not imp--debugging?))
  (imp/debug-status (if imp--debugging?
                        "Enabled debug flag."
                      "Disabled debug flag.")))


(defun imp/debug-status (&optional msg)
  "Print debugging status for imp.

If MSG is non-nil, it is output just before the status but in the same
`message'."
  (interactive)

  (let* ((line "────────────────────────────────")
         (status (list "Debug Feature Flag:    %s"
                   "`imp--debugging?': %s"
                   line
                   " %s")))
    ;;---
    ;; Do we have a message?
    ;;---
    ;; Yes and it's a string - use as-is.
    (cond ((stringp msg)
           (push line status)
           (push msg status))

          ;; It's something else; formatted to a string.
          (msg
           (push line status)
           (push (format "%S" msg) status))

          ;; No msg, do nothing.
          (t
           nil))

    ;;---
    ;; Print status.
    ;;---
    (message (mapconcat #'identity
                        status
                        "\n")
             ;; NOTE: Doom Emacs only, protected by `fboundp' check for the `featurep!' macro.
             ;; "debug.el" is loaded before "flag.el" so can't use `imp/flag?' here.
             (if (and (fboundp 'featurep!)
                      (featurep! +debug))
                 "[FEATURE]"
               "[-------]")
             (if imp--debugging?
                 "[FLAGGED]"
               "[-------]")
             (if (imp--debug-enabled?)
                 "[ENABLED]"
               "[disabled]"))))
;; (imp/debug-status)
;; (imp/debug-status "Toggled a bit via solar radiation.")


;;------------------------------------------------------------------------------
;; Debugging Functions
;;------------------------------------------------------------------------------

(defun imp--debug-init ()
  "Initialize `imp' debugging based on Emacs' variables.

Checks:
  - `init-file-debug'
  - `debug-on-error'

Return non-nil if debugging."
  (let ((func/name "imp--debug-init"))
    ;; Set our debug variable based on inputs.
    (cond
     ;;---
     ;; Environment Variable: DEBUG
     ;;---
     ((and (getenv-internal "DEBUG")
           (not init-file-debug)
           (not debug-on-error))
      (setq imp--debugging? (getenv-internal "DEBUG"))

      (imp--debug
       func/name
       "Enable `imp--debugging?' from environment variable DEBUG: %S"
       imp--debugging?))

     ;;---
     ;; CLI Flag: "--debug-init"
     ;;---
     (init-file-debug
      (setq imp--debugging? init-file-debug)

      (imp--debug
       func/name
       "Enable `imp--debugging?' from '--debug-init' CLI flag: %S"
       imp--debugging?))

     ;;---
     ;; Interactive Flag?
     ;;---
     ;; How did you get this set and not `init-file-debug'? Debugging some small
     ;; piece of init, maybe?
     (debug-on-error
      (setq imp--debugging? debug-on-error)

      (imp--debug
       func/name
       "Enable `imp--debugging?' from `debug-on-error' variable: %S"
       imp--debugging?))

     ;;---
     ;; _-NOT-_ Debugging
     ;;---
     (t
      ;; Do nothing.
      ))

    ;; Return what the `imp--debugging?' setting is now.
    imp--debugging?))


(defun imp--debug-enabled? ()
  "Return non-nil if one or more debug flags are enabled.

Flags:
  - `+debug' feature flag in `doom!' macro in user's \"<doom-dir>/init.el\".
  - `imp--debugging' toggle variable."
  ;; Is a debug flag enabled?
  (cond
   ;; NOTE: Doom Emacs only, protected by `fboundp' check for the `featurep!' macro.
   ;; The `+debug' flag in the `doom!' macro in user's "<doom-dir>/init.el".
   ;; "debug.el" is loaded before "flag.el" so can't use `imp/flag?' here.
   ((and (fboundp 'featurep!)
         (featurep! +debug))
    t)

   ;; `:imp' debugging boolean: Return its value if not nil.
   (imp--debugging?)

   ;; Fallthrough: debugging is not enabled.
   (t
    nil)))


(defun imp--debug (caller string &rest args)
  "Print out a debug message if debugging is enabled.

CALLER should be the calling function's name.

STRING should be a string, which can have formatting info in it (see `format'),
and will be printed as the debug message.

ARGS should be args for formatting the STRING."
  (when (imp--debug-enabled?)
    (imp--output :debug
                     caller
                     string
                     args)))
;; (imp--debug "test_func" "test")


(defun imp--debug-newline ()
  "Prints an empty debug line if debugging."
  (when t ;; (imp--debug-enabled?)
    (imp--output :blank
                     nil
                     " "
                     nil)))
;; (imp--debug-newline)

;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
;; Don't provide. Imp internal only.
