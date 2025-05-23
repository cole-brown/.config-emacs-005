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


(defun imp-debug-toggle ()
  "Toggle debugging for imp."
  (interactive)
  (setq imp--debugging? (not imp--debugging?))
  (imp-debug-status (if imp--debugging?
                        "Enabled debug flag."
                      "Disabled debug flag.")))


(defun imp-debug-status (&optional msg)
  "Print debugging status for imp.

If MSG is non-nil, it is output just before the status but in the same
`message'."
  (interactive)

  (let* ((line "────────────────────────────────")
         (status '("imp--debugging?: [%s]")))

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
             (if imp--debugging?
                 "YES"
               "no"))))
;; (imp-debug-status)
;; (imp-debug-status "Toggled a bit via solar radiation.")


;;------------------------------------------------------------------------------
;; Debugging Functions
;;------------------------------------------------------------------------------

(defun imp--debug (caller string &rest args)
  "Print out a debug message if debugging is enabled.

CALLER should be the calling function's name.

STRING should be a string, which can have formatting info in it (see `format'),
and will be printed as the debug message.

ARGS should be args for formatting the STRING."
  (when imp--debugging?
    (imp--output :debug
                 caller
                 string
                 args)))
;; (imp--debug "test_func" "test")


(defun imp--debug-newline ()
  "Prints an empty debug line if debugging."
  (when imp-debugging?
    (imp--output :blank
                 nil
                 " "
                 nil)))
;; (imp--debug-newline)


;;------------------------------------------------------------------------------
;; Initialization
;;------------------------------------------------------------------------------

(defun imp--debug-init ()
  "Initialize `imp' debugging based on Emacs' variables.

Checks:
  - Env Var: DEBUG
  - CLI param `--debug-on-init' aka variable `init-file-debug'
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


;;------------------------------------------------------------------------------
;; The Init.
;;------------------------------------------------------------------------------

;; Don't reset init flag if file re-evaluated.
(unless (boundp 'imp--debug-init?)
  (defvar imp--debug-init? nil
    "Guard var so debug init only happens once."))
;; (setq imp--debug-init? t)
;; (setq imp--debug-init? nil)

;; Only init the one time.
(when (and (boundp imp--debug-init?)
           (not imp--debug-init?))
  (imp--debug-init)
  (setq imp--debug-init? 'done-at-end-of-debug-el))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
