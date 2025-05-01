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
