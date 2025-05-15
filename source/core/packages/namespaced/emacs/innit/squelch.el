;;; core/modules/emacs/innit/squelch.el --- Squelch Output -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-08-01
;; Timestamp:  2023-06-22
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Squelch / Quiet / Silence / Muffle / Whatever your output.
;;
;;; Code:


(require 'cl-macs)

;;------------------------------------------------------------------------------
;; Silent Function Replacements
;;------------------------------------------------------------------------------

;;------------------------------
;; Save Originals
;;------------------------------

(defconst innit:unsquelched:write-region    (symbol-function #'write-region))
(defconst innit:unsquelched:load            (symbol-function #'load))
(defconst innit:unsquelched:message         (symbol-function #'message))
(defconst innit:unsquelched:standard-output standard-output)


;;------------------------------
;; Silent Replacements
;;------------------------------

(defun innit:squelch:write-region (start end filename &optional append visit lockname mustbenew)
  "Silent version of `write-region'.

See `write-region' for details on args: START, END, FILENAME, APPEND, VISIT,
LOCKNAME, and MUSTBENEW.

Will set VISIT to `no-message' before calling `write-region' unless it is
non-nil."
  (funcall innit:unsquelched:write-region
           start
           end
           filename
           append
           (or visit 'no-message)
           lockname
           mustbenew))


(defun innit:squelch:load (file &optional noerror nomessage nosuffix must-suffix)
  "Silent version of `load'.

See `load' for details on args: FILE, NOERROR, NOMESSAGE, NOSUFFIX, MUST-SUFFIX

Ignore NOMESSAGE and call `load' with NOMESSAGE == `:no-message' instead."
  (funcall innit:unsquelched:load
           file
           noerror
           :no-message
           nosuffix
           must-suffix))


(defun innit:squelch:message (&rest _)
  "Like `message', except it will ignore all inputs and do nothing."
  ;; Take message format string and args and I don't care.
  ;; We're being silent.
  nil)


(defun innit:squelch:standard-output (&rest _)
  "A character output stream to nowhere.

`standard-output' is generally t for having `print' output to echo area. Set
`standard-output' to this function to squelch it instead."
  ;; Take output stream character and who cares.
  ;; We're being silent.
  nil)


;;------------------------------------------------------------------------------
;; Squelch API
;;------------------------------------------------------------------------------

(defmacro innit:squelch (&rest forms)
  "Run FORMS without generating any output.

This silences calls to `message', `load', `write-region' and anything that
writes to `standard-output'. In interactive sessions this won't suppress writing
to *Messages*, only inhibit output in the echo area.

Originally from Doom's `quiet!' macro."
  ;; Can use `innit:debug?' optional param `:any' if we want to not squelch for
  ;; any of: `innit:debug?', `debug-on-error', `init-file-debug'
  `(if (innit:debug?)
       ;; Debugging; don't squelch!
       (progn ,@forms)
     ,(if innit:interactive?
          ;; Be less bossy; allow output to *Messages* buffer but not to the
          ;; minibuffer.
          `(let ((inhibit-message t)
                 (save-silently t))
             (prog1
                 ,@forms
               ;; And we prevent output to the minibuffer by... Clearing
               ;; anything out of the minibuffer after it's already been output
               ;; to the minibuffer. Mission Accomplished.
               (message "")))
        ;; Full powered squelching; prevent output by just redefining commonly
        ;; chatty functions.
        `(cl-letf ((standard-output                 #'innit:squelch:standard-output)
                  ((symbol-function #'message)      #'innit:squelch:message)
                  ((symbol-function #'load)         #'innit:squelch:load)
                  ((symbol-function #'write-region) #'innit:squelch:write-region))
           ,@forms))))


(defun innit:advice:squelch (fn &rest args)
  "Generic advice function for silencing noisy functions.

Lexically set some variables to squelch output, then call FN with ARGS,
returning FN's results.

In interactive Emacs, this just inhibits messages from appearing in the
minibuffer. They are still logged to *Messages*.

In tty Emacs, messages are suppressed completely.

Usage:
  (advice-add #'undo-tree-save-history :around #'innit:advice:squelch)

Originally from Doom's `doom-shut-up-a' function."
  (innit:squelch (apply fn args)))


(cl-defmacro innit:squelch/unless (&rest forms
                                   &key  interactive?
                                         innit-debug?
                                         allow-message?
                                   &allow-other-keys)
  "Run FORMS, probably without generating any output.

If INTERACTIVE? is non-nil, squelch unless called by user (as per function
`called-interactively-p').

If INNIT-DEBUG? is non-nil, squelch unless debugging (as per function
`innit:debug?').

If ALLOW-MESSAGE? is non-nil, allow `message' output to '*Messages*' buffer.
Will clear the minibuffer after FORMS are run.
  - Set `inhibit-message' and `save-silently' lexically before running FORMS.

If ALLOW-MESSAGE? is nil or not provided, silence calls to `message', `load',
`write-region' and anything that writes to `standard-output'.

Originally from Doom's `quiet!' macro."
  ;;------------------------------
  ;; Filter keys from FORMS
  ;;------------------------------
  (let (macro:key
        macro:body)
    (dolist (form forms)
      ;; If this is one of our keywords, save that fact for the next `form' in FORMS.
      (cond ((memq form '(:interactive? :innit-debug? :allow-message?))
             (setq macro:key form))

            ;; Last `form' was one of our keywords, so this one is its value.
            ;; Ignore its value; already set in the keyword's variable.
            (macro:key
             (setq macro:key nil)
             nil)

            ;; Just a form; save to the filtered list.
            (t
             (push form macro:body))))

    ;; Get the body forms back into correct order.
    (setq macro:body (nreverse macro:body))

    ;;------------------------------
    ;; To Squelch or not to Squelch?
    ;;------------------------------
    (cond
     ;;------------------------------
     ;; Normal / Unsquelched
     ;;------------------------------
     ;; Squelch unless debugging?
     ((and innit-debug?
           ;; Could use `innit:debug?' optional param `:any' here, if we want to
           ;; not squelch for any of: `innit:debug?', `debug-on-error',
           ;; `init-file-debug'.
           (innit:debug?))
      ;; We are debugging and don't want to squelch!
      `(progn ,@macro:body))

     ;; Squelch unless interactive?
     ((and interactive?
           (called-interactively-p 'any))
      ;; Called by user directly or via keybind/macro; don't squelch!
      `(progn ,@macro:body))

     ;;------------------------------
     ;; Squelched
     ;;------------------------------
     ;; Allow message output?
     ;;   - But only if Emacs able to be interacted with (e.g. not batch mode).
     ((and allow-message?
           innit:interactive?)
      ;; Be less bossy; allow output to *Messages* buffer but not to the
      ;; minibuffer.
      `(let ((inhibit-message t)
             (save-silently t))
         (prog1
             ,@macro:body
           ;; And we "prevent" output to the minibuffer by... Clearing anything
           ;; out of the minibuffer after it's already been output to the
           ;; minibuffer. Mission Accomplished.
           (message ""))))

     ;; Full powered squelching!
     (t
      ;; Prevent output by just redefining commonly chatty functions.
      `(cl-letf ((standard-output                  #'innit:squelch:standard-output)
                 ((symbol-function #'message)      #'innit:squelch:message)
                 ((symbol-function #'load)         #'innit:squelch:load)
                 ((symbol-function #'write-region) #'innit:squelch:write-region))
         ,@macro:body)))))
;; (innit:squelch/unless :interactive? t :allow-message? t (message "hello there"))
;; (innit:squelch/unless :interactive? t (message "hello there"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :innit 'squelch)
