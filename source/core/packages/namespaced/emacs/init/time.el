;;; core/modules/emacs/innit/time.el --- Bout that time, innit? -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-04-26
;; Timestamp:  2023-07-18
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Bout that time, innit?
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Customs, Constants, & Variables
;;------------------------------------------------------------------------------

(defvar innit:time nil
  "The time it took, in seconds, for Emacs & `innit' to finish set-up.")


(defcustom innit:time:benchmark? t
  "Boolean flag for enabling/disabling Emacs start-up benchmark message."
  :group 'innit:group
  :type  '(boolean))


(defcustom innit:time:benchmark:message/clear-delay 10
  "Seconds to delay before clearing message of function `innit:time:benchmark'.

Set to 0/nil to have the message stick around until naturally
cleared/overwritten."
  :group 'innit:group
  :type  '(choice (nil :tag "Never Auto-Clear")
                  (natnum :tag "Seconds")))


(defcustom innit:time:benchmark:message/indent-func
  (lambda (str)
    "Prefix with spaces unless empty string, only whitespace, or not a string."
    (if (or (not (stringp str))
            (string-blank-p str))
        str
      (concat "            " str)))
  "Indent each line of the function `innit:time:benchmark' output with this.

If nil (or e.g. `identity'), do not indent."
  :group 'innit:group
  :type  '(choice (nil :tag "No Indent")
                  (function)))


;;------------------------------------------------------------------------------
;; Functions
;;------------------------------------------------------------------------------

(defun innit:time:benchmark (&optional string?)
  "Display a benchmark including number of packages and modules loaded.

If STRING? is non-nil, return the message as a string instead of displaying it."
  ;; If string is desired instead of message, ignore `innit:time:benchmark?' flag.
  (unless (and (not innit:time:benchmark?)
               (not string?))
    (let ((func/name "innit:time:benchmark")
          (time (or innit:time
                    (setq innit:time
                          (float-time (time-subtract (current-time) before-init-time)))))
          (features/imp (when (functionp #'imp:feature:count)
                          (imp:feature:count)))
          (features/emacs (length features))
          message)

      ;;------------------------------
      ;; What to output?
      ;;------------------------------
      (setq message
            ;;---
            ;; Fancy Indented Box:
            ;;---
            (mapconcat (or innit:time:benchmark:message/indent-func
                           #'identity)
                       (seq-filter #'stringp
                                   (list "" ; Get off of the initial log line; it's something like: "[INFO    ]: innit:time:benchmark: "
                                         "┌──────────┤innit├──────────┐"
                                         ;; No Timestamp:
                                         ;; "├──────────┴─────┴──────────┤"
                                         ;; Yes Timestamp:
                                         "│  ┌───────┴─────┴───────┐  │"
                                         (format "├──┤ %19s ├──┤" (datetime:format 'rfc-3339 'datetime))
                                         "│  └─────────────────────┘  │"
                                         ;; Stats:
                                         (format "│      init time: %8.03fs │" time)
                                         (when features/imp
                                           (format "│   imp features: %4d      │" features/imp))
                                         (format "│ emacs features: %4d      │" features/emacs)
                                         "└───────────────────────────┘"))
                       "\n")
            ;; How's it look? (progn (eval-defun nil) (innit:time:benchmark))

            ;; ;;---
            ;; ;; Or... Plain Ole List:
            ;; ;;---
            ;; (mapconcat #'identity
            ;;            (seq-filter #'stringp
            ;;                        (list "innit:"
            ;;                              (format "       init time: %8.03fs" time)
            ;;                              (when features/imp
            ;;                                (format "    imp features: %4d" features/imp))
            ;;                              (format "  emacs features: %4d" features/emacs)))
            ;;            "\n")
            )

      ;;------------------------------
      ;; Where to output?
      ;;------------------------------
      (if string?
          ;;---
          ;; Simply return as a string.
          ;;---
          message

        ;;---
        ;; Output to minibuffer & '*Messages*' buffer (also return the string).
        ;;---
        (nub:info :innit func/name message)
        ;; A valid clear delay? Use it to eventually clear out the minibuffer.
        (when (and (numberp innit:time:benchmark:message/clear-delay)
                   (> innit:time:benchmark:message/clear-delay 0))
          (run-with-idle-timer innit:time:benchmark:message/clear-delay
                               nil
                               (lambda () (message ""))))))))
;; (innit:time:benchmark)


;;------------------------------------------------------------------------------
;; Set-Up
;;------------------------------------------------------------------------------

(defun innit:time:init ()
  "Set up innit/Emacs start-up benchmark hook."
  ;; Always set up the hook, it will decide when run if it should do anything.
  (add-hook 'window-setup-hook #'innit:time:benchmark))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :innit 'time)
