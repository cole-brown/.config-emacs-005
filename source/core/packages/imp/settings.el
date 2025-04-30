;;; imp/fundamental.el --- string helpers, etc -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2025-04-16
;; Timestamp:  2025-04-16
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;                                 ──────────
;; ╔════════════════════════════════════════════════════════════════════════╗
;; ║                                Settings                                ║
;; ╚════════════════════════════════════════════════════════════════════════╝
;;                                   ──────
;;                                 defcustoms
;;                                 ──────────
;;
;;; Code:


(defgroup imp nil
  "Automatically-ish commit/push git repos for note, docs, etc."
  :prefix "imp-"
  :group 'initialization)


;;------------------------------------------------------------------------------
;; Output (errors, warnings, messages...)
;;------------------------------------------------------------------------------

(defcustom imp-output-buffer "ⓘ-imp-output-ⓘ"
  "Name of the output buffer used by `imp--output-sink'."
  :group 'imp
  :type '(string))


(defcustom imp-output-level
  '((:error      . (:display "ERROR"
                    :sink (error imp--output-sink)))
    ;; (:error:user . (:display "ERROR:user"
    ;;                 :sink (user-error imp--output-sink)))
    (:warning    . (:display "Warning"
                    :sink (warn imp--output-sink)))
    (:info       . (:display "info"
                    :sink (message imp--output-sink)))
    (:debug      . (:display "debug"
                    :align right ; default/nil: left
                    ;; :sink message
                    :sink (message imp--output-sink)))

    ;; No prefix.
    (:blank . (;; :sink message
               :sink imp--output-sink)))
  "Output message level (:debug, :error, etc) settings."
  :group 'imp
  :type '(alist :key-type symbol
                :value-type plist))


(defcustom imp-output-features-buffer
  "*imp-features*"
  "Name of the buffer for `imp-cmd-features-print' to output a pretty-printed tree
of the features imp has provided."
  :group 'imp)


;;------------------------------------------------------------------------------
;; Use-Package Integration
;;------------------------------------------------------------------------------

(defcustom use-package-always-imp t
  "Treat every package as though it had specified using `:imp SEXP'.
See also `use-package-defaults', which uses this value."
  :group 'imp
  :type 'sexp)


;;------------------------------------------------------------------------------
;; Timing
;;------------------------------------------------------------------------------

(defcustom imp-timing-enabled? t
  "Should loading & timing messages be printed?"
  :group 'imp
  :type '(boolean))


(defcustom imp-timing-buffer-tail? t
  "Should the timing buffer be tailed?"
  :group 'imp
  :type '(boolean))


(defcustom imp-timing-buffer-name
  "ⓘ-imp-timing-ⓘ"
  "Buffer name to print to.

If you want it to go to *Messages* with the usual minibuffer interaction, set
to: `:messages'."
  :group 'imp
  :type '(choice (string :tag "Name of Buffer")
                 (const :tag "Use `message' to send to *Messages* buffer with the usual minibuffer interactions."
                        :messages)))


(defcustom imp-timing-buffer-show t
  "If non-nil, show `imp-timing-buffer-name' every time it gets a new message."
  :group 'imp
  :type  '(boolean))


(defcustom imp-timing-format-load "loading %1$S..."
  "Format string for loading a filename.

Args to this format string are:
  1. Feature symbol: :imp/+timing
  2. File name:      +timing.el
  3. File path:      /path/to/imp/+timing.el"
  :group 'imp
  :type '(string)
  :risky t)


(defcustom imp-timing-format-skip "skip: %1$S"
  "Format string for skipping loading of a required file.

Args to this format string are:
  1. Feature symbol: :imp/+timing
  2. File name:      +timing.el
  3. File path:      /path/to/imp/+timing.el"
  :group 'imp
  :type '(string)
  :risky t)


(defcustom imp-timing-reason "reason: "
  "String prefix for _why_ something was or wasn't.

Example: When a file is skipped because it's already provided, the timing buffer
will say (with default settings):
  [...]
  ├─skip: :feature:example
  │ └─reason: already provided
  [...]"
  :group 'imp
  :type '(string)
  :risky t)


(defcustom imp-timing-format-skip-already-provided "feature already provided"
  "Format string for skipping loading of a required file.

Args to this format string are:
  1. Feature symbol: :imp/+timing
  2. File name:      +timing.el
  3. File path:      /path/to/imp/+timing.el"
  :group 'imp
  :type '(string)
  :risky t)


(defcustom imp-timing-format-skip-optional-dne "optional file does not exist: %3$s"
  "Format string for skipping loading of an optional file.

Args to this format string are:
  1. Feature symbol: :imp/+timing
  2. File name:      +timing.el
  3. File path:      /path/to/imp/+timing.el"
  :group 'imp
  :type '(string)
  :risky t)


(defcustom imp-timing-format-time-total
  (concat "\n"
          ;;---
          ;; Open Box.
          ;;---
          "┌───────┬──────────────────┐\n"
          "│ total │ "
          ;;---
          ;; Start of Line
          ;;---

          ;;---
          ;; Elapsed Seconds Format
          ;;---
          ;; Format total elapsed time like: "111.2345", "  1.2345", etc.
          "%"                             ; Fill with spaces.
          (number-to-string
           (+ 3 1 imp--timing-precision)) ; Full seconds precision + "." + fractional seconds precision
          "."
          (number-to-string
           imp--timing-precision)         ; Fractional seconds precision
          "f"                             ; Format as a floating point.

          ;;---
          ;; End of Line
          ;;---
          " seconds │\n"

          ;;---
          ;; Close Box.
          ;;---
          "└───────┴──────────────────┘")
  "String format for total elapsed time according to `imp-timing-sum'."
  :group 'imp
  :type '(string))


(defcustom imp-timing-format-time
  (concat "%0"                            ; Fill with zeros.
          (number-to-string
           (+ 2 1 imp--timing-precision)) ; Full seconds precision + "." + fractional seconds precision
          "."
          (number-to-string
           imp--timing-precision)         ; Fractional seconds precision
          "f"                             ; Format as a floating point.
          " seconds")                     ; And... say what the units are.
  "Format string for number of seconds it took to load a file."
  :group 'imp
  :type '(string)
  :risky t)


(defcustom imp-timing-separator-restart
  (concat "\n\n"
          (make-string 80 ?─ :multibyte))
  "String that can be inserted into the output buffer via `imp--timing-launch'."
  :group 'imp
  :type '(string)
  :risky t)


(defcustom imp-timing-separator-final
  (concat "\n\n"
          (make-string 80 ?═ :multibyte))
  "String that can be inserted into the output buffer via `imp-timing-final'."
  :group 'imp
  :type '(string)
  :risky t)
