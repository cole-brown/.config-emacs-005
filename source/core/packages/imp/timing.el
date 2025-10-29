;;; imp/timing.el --- Imps with Timecards -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-01-07
;; Timestamp:  2025-10-28
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;                                 ──────────
;; ╔════════════════════════════════════════════════════════════════════════╗
;; ║                            Loading Timing...                           ║
;; ╚════════════════════════════════════════════════════════════════════════╝
;;                                   ──────
;;                  Be done is as little as 98.765 seconds.
;;                                 ──────────
;;
;;; Code:


(require 'rx)
(require 'range)


;;------------------------------
;; TODO:imp: Future Features:
;;------------------------------
;;   - Delayed output mode:
;;     - If enabled, just save info about load ordering to a tree list.
;;       Then once some 'loading done' condition is hit (emacs hook?),
;;       construct the whole loading tree of messages and print.
;;       - Saves some start-up time by not doing (expensive) output.
;;     - If disabled, normal 'print-as-you-go' that slows down start-up some.
;;------------------------------


;;------------------------------------------------------------------------------
;; Timings Toggle
;;------------------------------------------------------------------------------

(defun imp-timing-enabled? ()
  "Return non-nil if timing is enabled.

Specifically, if `imp-timing-enabled?' is non-nil or imp's `+timing'
feature flag is set."
  imp-timing-enabled?)
;; (imp-timing-enabled?)


;;------------------------------------------------------------------------------
;; Constants & Variables
;;------------------------------------------------------------------------------

(defconst imp--timing-format-tree
  '(;; Supplied types:
    :root   ""
    :leaf   "└─"
    ;; Derived types:
    :branch "├─"
    :trunk  "│ ")
  "Strings for creating loading trees like:

loading xxx...
├─loading yyy...
│ └─cc.dd seconds
└─aa.bb seconds")


(defconst imp--timing-precision 4
  "Show this many places after decimal for default `imp--timing-format-time'.")


(defvar imp--timing-indent 0
  "Current tree indent level of loading.

Example:
  loading xxx...     : level 0
  ├─loading yyy...   : level 1
  │ └─cc.dd seconds  : level 1
  └─aa.bb seconds    : level 0")


(defvar imp--timing-feature-current nil
  "Currently timed imp feature.

Used to prevent duplicate start/end timing messages.

List of imp keyword/symbol names.
Example: '(imp example feature)")


(defvar imp-timing-sum 0.0
  "Sum of all timings at indent 0 level.")


;;------------------------------------------------------------------------------
;; Custom Variables
;;------------------------------------------------------------------------------

(defconst imp--timing-format-load "loading %1$S..."
  "Format string for loading a filename.

Args to this format string are:
  1. Feature symbol: :imp/+timing
  2. File name:      +timing.el
  3. File path:      /path/to/imp/+timing.el")


(defconst imp--timing-format-skip "skip: %1$S"
  "Format string for skipping loading of a required file.

Args to this format string are:
  1. Feature symbol: :imp/+timing
  2. File name:      +timing.el
  3. File path:      /path/to/imp/+timing.el")


(defconst imp--timing-reason "reason: "
  "String prefix for _why_ something was or wasn't.

Example: When a file is skipped because it's already provided, the timing buffer
will say (with default settings):
  [...]
  ├─skip: :feature:example
  │ └─reason: already provided
  [...]")


(defconst imp--timing-format-skip-already-provided "feature already provided"
  "Format string for skipping loading of a required file.

Args to this format string are:
  1. Feature symbol: :imp/+timing
  2. File name:      +timing.el
  3. File path:      /path/to/imp/+timing.el")


(defconst imp--timing-format-skip-optional-dne "optional file does not exist: %3$s"
  "Format string for skipping loading of an optional file.

Args to this format string are:
  1. Feature symbol: :imp/+timing
  2. File name:      +timing.el
  3. File path:      /path/to/imp/+timing.el")


(defconst imp--timing-format-time-total
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
  "String format for total elapsed time according to `imp-timing-sum'.")


(defconst imp--timing-format-time
  (concat "%0"                            ; Fill with zeros.
          (number-to-string
           (+ 2 1 imp--timing-precision)) ; Full seconds precision + "." + fractional seconds precision
          "."
          (number-to-string
           imp--timing-precision)         ; Fractional seconds precision
          "f"                             ; Format as a floating point.
          " seconds")                     ; And... say what the units are.
  "Format string for number of seconds it took to load a file.")


(defconst imp--timing-separator-restart
  (concat "\n\n"
          (make-string 80 ?─ :multibyte))
  "String that can be inserted into the output buffer via `imp--timing-launch'.")


(defconst imp--timing-separator-final
  (concat "\n\n"
          (make-string 80 ?═ :multibyte))
  "String that can be inserted into the output buffer via `imp-timing-final'.")


;;------------------------------------------------------------------------------
;; Deduplication
;;------------------------------------------------------------------------------

(defun imp--timing-feature-duplicate? (feature)
  "Is FEATURE the same as currently timed feature?

FEATURE should be a list of keyword/symbol names.

Return non-nil if FEATURE is non-nil and matches feature currently being timed
\(`imp--timing-feature-current')."
  ;; Don't think we want nil to be a match? We /shouldn't/ get a nil anyways...
  (and (not (null feature))
       (equal feature imp--timing-feature-current)))


;;------------------------------------------------------------------------------
;; Indentation
;;------------------------------------------------------------------------------

(defun imp--timing-tree-type (type indent)
  "Get tree type string for current TYPE and INDENT level.

INDENT is an integer where 0 the TYPE's indent and 1+ are its parents
indention levels."
  (plist-get imp--timing-format-tree
             ;;------------------------------
             ;; Indent 0: Return the type's actual format.
             ;;------------------------------
             (cond ((= indent 0)
                    type)
                   ;;------------------------------
                   ;; Indents above 0: Derive a type based on the actual type.
                   ;;------------------------------
                   ((and (= indent 1)
                         (eq type :root))
                    :branch)
                   ((and (= indent 1)
                         (not (eq type :root)))
                    :trunk)
                   (t ;; (> indent 1)
                    :trunk))))
;; (imp--timing-tree-type :root 0)
;; (imp--timing-tree-type :root 1)
;; (imp--timing-tree-type :root 2)
;; (imp--timing-tree-type :leaf 0)
;; (imp--timing-tree-type :leaf 1)
;; (imp--timing-tree-type :leaf 2)


(defun imp--timing-tree-string (type)
  "Get tree type string for current TYPE."
  (mapconcat (lambda (i) (imp--timing-tree-type type i))
             (nreverse (range-uncompress (cons 0 imp--timing-indent)))
             ""))
;; (imp--timing-tree-string :root)
;; (let ((imp--timing-indent 2)) (imp--timing-tree-string :root))


;;------------------------------------------------------------------------------
;; Buffer Functions
;;------------------------------------------------------------------------------

(defun imp--timing-buffer-messages? ()
  "Return t if `imp-timing-buffer' is the \"*Messages*\" buffer.

NOTE: All of these mean 'Emacs messages buffer':
  - `:messages', `:message'
  - `messages', `message'
  - \"*Messages*\", value of `messages-buffer-name'"
  (cond ((and (symbolp imp-timing-buffer)
              (memq imp-timing-buffer
                    '(:message :messages message messages))))
        ((and (stringp imp-timing-buffer)
              (or (string= "*Messages*"
                           imp-timing-buffer)
                  (string= messages-buffer-name
                           imp-timing-buffer))))
        ;; Else it's some other buffer.
        (t
         nil)))


(defun imp-timing-buffer ()
  "Return the string name of `imp-timing-buffer' custom variable.

NOTE: This converts a value `:messages' to \"*Messages*\"."
  (if (imp--timing-buffer-messages?)
      "*Messages*"
    imp-timing-buffer))


(defun imp--timing-buffer-get (&optional buffer-or-name &rest args)
  "Get the imp timing buffer.

BUFFER-OR-NAME should be a string or a buffer object.
If it is nil, return value of function `imp-timing-buffer' will be used.

ARGS can be:
  - `:create', `create' - Create the buffer if it doesn't exist.

NOTE: If created, the buffer will be put into `imp-timing-mode'."
  (let ((buffer-or-name (or buffer-or-name
                            (imp-timing-buffer))))
    ;; If it already exists, we're done; just return it.
    (cond ((get-buffer buffer-or-name))

          ;; Doesn't exist! Create only if requested.
          ((seq-some (lambda (arg) (memq arg '(:create create))) args)
           (with-current-buffer (get-buffer-create buffer-or-name)
             (imp-timing-mode)
             (current-buffer)))

          ;; Fallthrough: Doesn't exist and don't want to create?
          ;; Return nil I guess? That's what `get-buffer' does for buffers that don't exist...
          (t
           nil))))
;; (imp--timing-buffer-get)
;; (imp--timing-buffer-get nil :create)


;; https://stackoverflow.com/a/4685005/425816
(defun imp--timing-buffer-tail ()
  "Tail the timing buffer.

NOTE: Does nothing if `imp-timing-buffer-tail?' is nil, or '*Messages*' buffer
is used for timing.

NOTE: Cannot use `auto-revert-tail-mode' as it only works on file-based buffers.
So, this just throws an error:
  (require 'autorevert)
  (auto-revert-tail-mode +1)"
  (when (and imp-timing-buffer-tail?
             (not (imp--timing-buffer-messages?)))
    (with-current-buffer (imp--timing-buffer-get)
      ;; Set point to max for the buffer in general.
      (goto-char (point-max))

      ;; Set point to max in any windows displaying the buffer. Otherwise those windows just let the
      ;; displayed lines just wander down past the displayed section of the buffer (that is, windows
      ;; don't care about the above point - they have their own).
      (let ((windows (get-buffer-window-list (current-buffer) nil t)))
        (dolist (window windows)
          (set-window-point window (point-max)))))))


(defun imp--timing-buffer-show (force-show?)
  "Show timing buffer or not, depending on settings.

FORCE-SHOW?, if non-nil, will always show the buffer."
  (when (or force-show?
            (imp--timing-buffer-messages?))
    (display-buffer (imp-timing-buffer))))

;; todo move to commands.el?
(defun imp-cmd-timing-buffer-bury (&optional ignore-messages-buffer)
  "Hide and bury the imp timing output buffer.

If IGNORE-MESSAGES-BUFFER is non-nil and the output buffer is \"*Messages*\",
does nothing instead."
  (interactive)
  ;; Ignore entirely if IGNORE-MESSAGES-BUFFER is set and we are using the messages buffer.
  (unless (and ignore-messages-buffer
               (imp--timing-buffer-messages?))
    ;; Bury only when we find the window currently displaying it.
    (when-let* ((name (imp-timing-buffer))
                (window (get-buffer-window name)))
      (with-selected-window window
        (bury-buffer))
      (message "imp-timing- Buried timing buffer: %s"
               name))))
;; (imp-cmd-timing-buffer-bury)


;; todo move to commands.el?
(defun imp-cmd-timing-buffer-kill (&optional ignore-messages-buffer)
  "Kill the imp timing output buffer.

If IGNORE-MESSAGES-BUFFER is non-nil and the output buffer is \"*Messages*\",
does nothing instead."
  (interactive)
  ;; Ignore entirely if IGNORE-MESSAGES-BUFFER is set and we are using the messages buffer.
  (unless (and ignore-messages-buffer
               (imp--timing-buffer-messages?))
    ;; Bury only when we find the window currently displaying it.
    (when-let* ((name (imp-timing-buffer))
                ;; Get the buffer in order to prevent "No buffer named <...>" messages.
                (buffer (imp--timing-buffer-get name)))
      (kill-buffer buffer)
      (message "imp-timing- Killed timing buffer: %s"
               name))))
;; (imp-cmd-timing-buffer-kill)


;;------------------------------------------------------------------------------
;; Output
;;------------------------------------------------------------------------------

(defun imp--timing-buffer-insert (string)
  "Insert finalized message STRING into output buffer."
  ;; Don't do anything unless enabled.
  (when-let ((func-name "imp--timing-buffer-insert")
             (enabled? (imp-timing-enabled?))
             (name (imp-timing-buffer)))
    (cond
     ;;------------------------------
     ;; Buffers
     ;;------------------------------
     ;; *Messages* buffer: Just use the `message' function.
     ((imp--timing-buffer-messages?)
      (message string))

     ;; Some other buffer: Insert at the end.
     ((stringp name)
      ;; Should we force tailing of the buffer or leave that up to user and `auto-revert-tail-mode'?
      ;; I think start off with `auto-revert-tail-mode'.
      (with-current-buffer (imp--timing-buffer-get name :create)
        ;; We are now in BUFFER, so just insert the formatted string on a new line at the end.
        (goto-char (point-max))
        ;; If we're in `imp-timing-mode', we'll probably need to deal with the
        ;; fact that the buffer is read-only...
        (let ((inhibit-read-only t))
          ;; Prepend a newline, unless this is a new/empty buffer.
          (insert (concat (if (= (buffer-size) 0)
                              ""
                            "\n")
                          string)))))

     ;;------------------------------
     ;; Errors
     ;;------------------------------
     (t
      (imp--error func-name
                  "unhandled %s buffer: '%s'"
                  (if (imp--timing-buffer-messages?)
                      "(*Messages*)"
                    "(non-*Messages*)")
                  name)))

    ;; Tail buffer if desired.
    (imp--timing-buffer-tail)

    ;; Show buffer if desired.
    (when imp-timing-buffer-show-auto?
      ;; Don't want to end up with multiple windows after start up, so be a good
      ;; steward and just use the same window as whatever's already here.
      (let ((display-buffer-alist (list (list name
                                              'display-buffer-same-window))))
        (display-buffer name)))))


(defun imp--timing-message (type formatting &rest args)
  "Prepend indentation and print timing message for FORMATTING string and ARGS.

TYPE should be either `:root' or `:leaf'. Uses TYPE to get the indent string."
  (imp--timing-buffer-insert
   (format "%s%s"
           (imp--timing-tree-string type)
           (apply #'format formatting args))))


(defun imp--timing-start (feature path)
  "Print a loading message for FEATURE & PATH.

Message depends on `imp--timing-format-load'."
  (imp--timing-message :root
                       imp--timing-format-load
                       (imp-feature-normalize feature)
                       (imp--path-filename path)
                       path))


(defun imp--timing-end (time-start)
  "Print the time since TIME-START.

Message depends on `imp--timing-format-time'."
  (let ((elapsed (float-time (time-since time-start))))
    (imp--timing-message :leaf
                         imp--timing-format-time
                         elapsed)
    ;; Add `elapsed' to running sum if at base indent level.
    (when (= imp--timing-indent 0)
      (setq imp-timing-sum (+ imp-timing-sum elapsed)))))


(defun imp-timing-skip-already-provided (feature path)
  "Print a message about skipping FEATURE & PATH.

Message depends on `imp--timing-format-skip'."
  (when (imp-timing-enabled?)
    ;; Skip message.
    (imp--timing-message :root
                         imp--timing-format-skip
                         (imp-feature-normalize feature)
                         (imp--path-filename path)
                         path)
    ;; Increase indent level for reason.
    (let ((imp--timing-indent (1+ imp--timing-indent)))
      ;; Skip reason message.
      (imp--timing-message :leaf
                           (concat imp--timing-reason
                                   imp--timing-format-skip-already-provided)
                           (imp-feature-normalize feature)
                           (imp--path-filename path)
                           path))))


(defun imp-timing-skip-optional-dne (feature path)
  "Print a message about optional FEATURE & PATH that doesn't exist.

Message depends on `imp--timing-format-optional'."
  (when (imp-timing-enabled?)
    ;; Skip message.
    (imp--timing-message :root
                         imp--timing-format-skip
                         (imp-feature-normalize feature)
                         (imp--path-filename path)
                         path)
    ;; Increase indent level for reason.
    (let ((imp--timing-indent (1+ imp--timing-indent)))
      ;; Skip reason message.
      (imp--timing-message :leaf
                           (concat imp--timing-reason
                                   imp--timing-format-skip-optional-dne)
                           (imp-feature-normalize feature)
                           (imp--path-filename path)
                           path))))


;; TODO delete; use `imp--timing-core'
(defun imp--timing-macro-helper (feature &rest body)
  "imp timing for use in use-package handler.

TODO better docstr"
  (let ((feature (imp--feature-normalize-chain feature)))
    (if (not (and (imp-timing-enabled?)
                  ;; Don't do (another) timing block/level for a duplicated call.
                  (not (imp--timing-feature-duplicate? feature))))

        ;; Timing disabled: Return body unharmed.
        body

      ;; Timings enabled: Run body in between timing start/end messages.
      `(
        ;; Update current feature being timed.
        (setq imp--timing-feature-current feature)
        ;; Output load message.
        (imp--timing-start feature
                           ,(imp-file-current)
                           ,(imp-path-current-dir))
        (prog1
            ;; Increase indent level for body.
            (let ((imp--timing-indent (1+ imp--timing-indent)))
              ;; Run the body...
              ,@body)

          ;; Clear this feature from current.
          (setq imp--timing-feature-current nil)

          ;; Finish with the timing message.
          (imp--timing-end ,(current-time)))))))


(defmacro imp-timing-core (feature path &rest body)
  "Return forms that measure & print the time it takes to evaluate BODY.

FEATURE should be a list of keyword & symbol names.

PATH should be a file path string.

Output message depends on `imp--timing-format-time'.

Prints timing to the `imp-timing-buffer' buffer.
Return result of evaluating BODY."
  (declare (indent 3))
  (let ((feature-normalized (imp-feature-normalize feature)))
    (if (or (not (imp-timing-enabled?))
            ;; Don't do (another) timing block/level for a duplicated call.
            (imp--timing-feature-duplicate? feature-normalized))
        ;; Timing disabled: Just return BODY.
        `(,@body)
      ;; Timings enabled: Run BODY in between timing start/end messages.
      `((let ((imp--macro-time (current-time)))
          ;; Update current feature being timed.
          (setq imp--timing-feature-current ',feature-normalized)
          ;; Output load message.
          (imp--timing-start ',feature-normalized
                             ,path)
          (prog1
              ;; Increase indent level for body.
              (let ((imp--timing-indent (1+ imp--timing-indent)))
                ;; Run the body...
                ,@body)

            ;; Clear this feature from current.
            (setq imp--timing-feature-current nil)

            ;; Finish with the timing message.
            (imp--timing-end imp--macro-time)))))))



(defmacro imp-timing (feature path &rest body)
  "Measure & print the time it takes to evaluate BODY.

FEATURE should be a list of keyword & symbol names.

PATH should be a file path string.

Output message depends on `imp--timing-format-time'.

Return result of evaluating BODY."
  (declare (indent 3))

  `(let ((imp--macro-feature (imp--feature-normalize-chain ,feature)))
     (if (and (imp-timing-enabled?)
              ;; Don't do (another) timing block/level for a duplicated call.
              (not (imp--timing-feature-duplicate? imp--macro-feature)))

         ;; Timings enabled: Run body in between timing start/end messages.
         (let ((imp--macro-path     ,path)
               (imp--macro-time     (current-time)))
           ;; Update current feature being timed.
           (setq imp--timing-feature-current imp--macro-feature)
           ;; Output load message.
           (imp--timing-start imp--macro-feature imp--macro-path)
           (prog1
               ;; Increase indent level for body.
               (let ((imp--timing-indent (1+ imp--timing-indent)))
                 ;; Run the body...
                 ,@body)

             ;; Clear this feature from current.
             (setq imp--timing-feature-current nil)

             ;; Finish with the timing message.
             (imp--timing-end imp--macro-time)))

       ;; Timing disabled: Just run body.
       ,@body)))


;;------------------------------------------------------------------------------
;; Output: Init / Finalize
;;------------------------------------------------------------------------------

(defun imp-timing-restart ()
  "'Restart' timing.

1) Reset `imp-timing-sum' to zero.
2) Print a starting separator to the timing buffer if needed.
   - If `imp-timing-buffer' doesn't exists or is *Messages*, does nothing."
  ;; Reset timing sum variable.
  (setq imp-timing-sum 0.0)

  ;; Broken up because it's too early for this... :|
  (cond
   ;; Not enabled = no output.
   ((not (imp-timing-enabled?))
    nil)
   ;; *Messages* buffer = no output.
   ((imp--timing-buffer-messages?)
    nil)
   ;; Not *Messages* and exists = output!
   ((imp--timing-buffer-get (imp-timing-buffer))
    ;; Mark where we restarted timing from
    (imp--timing-buffer-insert imp--timing-separator-restart))
   ;; Else, no output.
   (t
    nil)))


(defun imp--timing-final (&optional separator-line?)
  "Actually output the total timing summary.

If SEPARATOR-LINE? is non-nil, print out `imp--timing-separator-final' after
final timing message."
  (when (imp-timing-enabled?)
    (imp--timing-buffer-insert
     (format imp--timing-format-time-total
             imp-timing-sum))
    (when separator-line?
      (imp--timing-buffer-insert imp--timing-separator-final))))
;; (imp--timing-final)
;; (imp--timing-final t)


(defun imp--timing-final-timer (&optional separator-line?)
  "Run timer for final message (total timing summary).

If not ready to output at end of timer, re-run timer.

If SEPARATOR-LINE? is non-nil, print out `imp--timing-separator-final' after
final timing message."
  (run-with-timer 1 ; seconds
                  nil ; do not repeat
                  (lambda (separator?)
                    "Check if ready to output final message, then output or rerun timer."
                    (if (or (> imp--timing-indent 0)
                            imp--timing-feature-current)
                        ;; Still timing something, so wait again.
                        (imp--timing-final-timer separator?)
                      ;; Done I think? Output.
                      (imp--timing-final separator?)))
                  separator-line?))


(defun imp-timing-final (&optional separator-line?)
  "Set up a total timing summary to print out once everything's done loading.

If SEPARATOR-LINE? is non-nil, print out `imp--timing-separator-final' after
final timing message."
  ;; Don't bother trying until start-up is (mostly) over.
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Startup-Summary.html#Startup-Summary
  (add-hook 'emacs-startup-hook
            (lambda ()
              "Print final timing info to imp timing buffer once done timing."
              (imp--timing-final-timer separator-line?))))
;; (imp-timing-final t)
;; (run-hooks 'emacs-startup-hook)


;;--------------------------------------------------------------------------------
;; Major Mode
;;--------------------------------------------------------------------------------

(defun imp--timing-mode-font-lock-keywords-element-create (matcher subexp facename &optional override laxmatch)
  "Create an entry for the `font-lock-keywords' list.

See `font-lock-keywords' for full details.

This creates only the (MATCHER . HIGHLIGHT) form where HIGHLIGHT is
MATCH-HIGHLIGHT.
  - MATCH-HIGHLIGHT is of the form: (SUBEXP FACENAME [OVERRIDE [LAXMATCH]])

MATCHER should be a regex string.

SUBEXP should be a natnum.

FACENAME should be a font face symbol (e.g. from `defface' or
`list-faces-display'.

\(Optional) OVERRIDE should be:
  - nil       : Do not override existing fontifications.
  - t         : Existing fontifications can be overwritten.
  - `keep'    : Only parts not already fontified are highlighted.
  - `prepend' : Merge existing and new fontification; new takes precedence.
  - `append'  : Merge existing and new fontification; existing takes precedence.

\(Optional) LAXMATCH should be nil/non-nil. If LAXMATCH is non-nil, that means
don't signal an error if there is no match for SUBEXP in MATCHER."
  (cons
   ;; MATCHER: Regex to match.
   matcher

   ;; HIGHLIGHT: MATCH-HIGHLIGHT: (SUBEXP FACENAME [OVERRIDE [LAXMATCH]])
   (cons
    ;; SUBEXP: Desired match's group number from MATCHER regex
    subexp

    ;; Build this list so optional OVERRIDE & LAXMATCH don't appear when nil.
    (cons
     ;; FACENAME: Font Face to Use
     facename

     (when (or override laxmatch)
       (cons
        ;; (Optional) OVERRIDE Flag:
        override

        (when laxmatch
          ;; (Optional) LAXMATCH Flag:
          ;; If LAXMATCH is non-nil, that means don't signal an error if there is no match for SUBEXP in MATCHER.
          (list laxmatch))))))))
;; (imp--timing-mode-font-lock-keywords-element-create "foo" 0 'font-lock-comment-face nil t)


(defconst imp--timing-mode-font-lock-keywords
  (let (flk ; The font lock keywords list that will become this const.

        ;;------------------------------
        ;; Timing Tree (Unicode Box Drawing Characters)
        ;;------------------------------
        ;; (tree '(;; Currently only using these to build the timing tree:
        ;;         ;;   "└" "├" "│" "─"
        ;;         ;; But why not add "everything" for completeness:
        ;;         "┌" "┬" "┐" "─" "├" "┼" "┤" "│" "└" "┴" "┘"
        ;;         "╔" "╦" "╗" "═" "╠" "╬" "╣" "║" "╚" "╩" "╝"))
        ;; Or should we use the whole range?
        ;;   - https://www.w3.org/TR/xml-entity-names/025.html
        ;;   - U+02500 - U+025FF
        ;;     - Includes:
        ;;       - Box Drawing
        ;;       - Block Elements
        ;;       - Geometric Shapes
        ;;   - U+02500-U+0257F
        ;;     - Just Box Drawing
        ;;       - Single line, heavy line, double line, dashed, etc. All there.
        (rx/unicode-box (rx-to-string `(one-or-more (any (#x2500 . #x0257F)))))

        ;;------------------------------
        ;; Punctuation and Suchlike
        ;;------------------------------
        ;; (comment '(";")) ; NOTE: Try fontifying this as comments using syntax table first too.
        (throbber '(;; Doesn't actually throb; does indicate something could be in progress.
                    "..."
                    "…"))

        ;;------------------------------
        ;; Words and Things
        ;;------------------------------
        ;; Currently have "loading" and "skip", but use more versions of both in
        ;; case we normalize/change the tenses...
        (rx/status/info    (rx-to-string `(or "load" "loading"  "loaded")  :no-group))
        (rx/status/warning (rx-to-string `(or "skip" "skipping" "skipped") :no-group))
        (rx/reason/warning (rx-to-string `(and ,imp--timing-reason
                                               (one-or-more any)
                                               line-end)
                                         :no-group))

        ;; (amounts '("second" "seconds"
        ;;            "minute" "minutes"
        ;;            "hour"   "hours"))
        (rx/duration (rx-to-string `(and (group
                                          (one-or-more digit)
                                          "."
                                          (one-or-more digit))
                                         " "
                                         (group
                                          (or "second" "seconds"
                                              "minute" "minutes"
                                              "hour"   "hours")))
                                   :no-group))

        ;;------------------------------
        ;; Feature Names
        ;;------------------------------
        (rx/feature (rx-to-string `(and ":"
                                        (one-or-more
                                         ;; Emacs Lisp is super permissive about what a symbol name can be...
                                         ;; How permissive should we be about what an Imp Feature Name should be?
                                         ;; NOTE: Similar but different from `imp--feature-replace-rx'.
                                         ;; Careful how this goes or you lose the question mark...
                                         (any "?" ":" "-" "_" "/" "."
                                              alphanumeric)))
                                  :no-group))

        ;;------------------------------
        ;; Final Status
        ;;------------------------------
        ;; Not currently printed to 'imp-timing' buffer?
        )

    ;;------------------------------
    ;; Timing Tree
    ;;------------------------------
    (push (imp--timing-mode-font-lock-keywords-element-create
           rx/unicode-box ; Match Regex
           0 ; Match's Group Number from Match Regex
           'font-lock-comment-delimiter-face ; Font Face to Use
           ;; TODO: Lighten up on these guys? I shouldn't need them except for other things (e.g.
           ;; `whitespace-mode') want to layer on top of my mode?
           t
           t)
          flk)

    ;;------------------------------
    ;; Punctuation and Suchlike
    ;;------------------------------
    ;; NOTE: `M-x list-faces-display' and go to 'font-lock-' to see all the pre-defined font-lock faces.
    (push (imp--timing-mode-font-lock-keywords-element-create
           (rx-to-string `(or ,@throbber) :no-group) ; Match Regex
           0 ; Match's Group Number from Match Regex
           'font-lock-comment-face ; Font Face to Use
           ;; TODO: Lighten up on these guys? I shouldn't need them except for other things (e.g.
           ;; `whitespace-mode') want to layer on top of my mode?
           t
           t)
          flk)

    ;;------------------------------
    ;; Words and Things
    ;;------------------------------
    (push (imp--timing-mode-font-lock-keywords-element-create
           rx/status/info
           0 ; Match's Group Number from Match Regex
           'font-lock-comment-face ; Font Face to Use
           ;; TODO: Lighten up on these guys? I shouldn't need them except for other things (e.g.
           ;; `whitespace-mode') want to layer on top of my mode?
           t
           t)
          flk)
    (push (imp--timing-mode-font-lock-keywords-element-create
           rx/status/warning
           0 ; Match's Group Number from Match Regex
           'font-lock-warning-face ; Font Face to Use
           ;; TODO: Lighten up on these guys? I shouldn't need them except for other things (e.g.
           ;; `whitespace-mode') want to layer on top of my mode?
           t
           t)
          flk)
    (push (imp--timing-mode-font-lock-keywords-element-create
           rx/reason/warning
           0 ; Match's Group Number from Match Regex
           'font-lock-warning-face ; Font Face to Use
           ;; TODO: Lighten up on these guys? I shouldn't need them except for other things (e.g.
           ;; `whitespace-mode') want to layer on top of my mode?
           t
           t)
          flk)

    ;; NOTE: Try leaving `status' and `amounts' as just... unlocked text? (e.g. white in my theme's case)
    ;; (push (imp--timing-mode-font-lock-keywords-element-create
    ;;        (rx-to-string `(or ,@status) :no-group) ; Match Regex
    ;;        0 ; Match's Group Number from Match Regex
    ;;        'font-lock-comment-face ; Font Face to Use
    ;;        ;; TODO: Lighten up on these guys? I shouldn't need them except for other things (e.g.
    ;;        ;; `whitespace-mode') want to layer on top of my mode?
    ;;        t
    ;;        t)
    ;;       flk)

    ;; Duration: "01.234 seconds"
    ;; Groupings:
    ;;   - #0: "01.234"
    ;;   - #1: "seconds"
    (push (imp--timing-mode-font-lock-keywords-element-create
           rx/duration ; Match Regex
           1 ; Match's Group Number from Match Regex
           'font-lock-function-name-face ; Font Face to Use
           ;; TODO: Lighten up on these guys? I shouldn't need them except for other things (e.g.
           ;; `whitespace-mode') want to layer on top of my mode?
           t
           t)
          flk)

    ;;------------------------------
    ;; Feature Names
    ;;------------------------------
    (push (imp--timing-mode-font-lock-keywords-element-create
           rx/feature ; Match Regex
           0 ; Match's Group Number from Match Regex
           'font-lock-variable-name-face ; Font Face to Use
           ;; 'font-lock-string-face ; Font Face to Use
           ;; TODO: Lighten up on these guys? I shouldn't need them except for other things (e.g.
           ;; `whitespace-mode') want to layer on top of my mode?
           t
           t)
          flk)

    ;;------------------------------
    ;; Final Status
    ;;------------------------------
    ;; Not currently printed to 'imp-timing' buffer?

    ;;------------------------------
    ;; Done; Return Font Lock Keywords
    ;;------------------------------
    flk)
  "Syntax keywords for `imp-timing-mode' buffer.

See `font-lock-keywords' for various formatting options of these entries.
Each element in a user-level keywords list should have one of these forms:
  - MATCHER
  - (MATCHER . SUBEXP)
  - (MATCHER . FACENAME)
  - (MATCHER . HIGHLIGHT)
  - (MATCHER HIGHLIGHT ...)
  - (eval . FORM)")
;; (pp imp--timing-mode-font-lock-keywords)


(defconst imp--timing-mode-font-lock-defaults
  ;; Format for list:
  ;;  (KEYWORDS [KEYWORDS-ONLY [CASE-FOLD [SYNTAX-ALIST ...]]])
  ;; See for more info:
  ;;   - `font-lock-defaults'
  ;;   - http://xahlee.info/emacs/emacs/elisp_font_lock_mode.html
  (list
   ;;------------------------------
   ;; KEYWORDS
   ;;------------------------------
   ;; See `font-lock-keywords' for various formatting options of these entries.
   imp--timing-mode-font-lock-keywords

   ;;------------------------------
   ;; KEYWORDS-ONLY?
   ;;------------------------------
   ;; NOTE: "If KEYWORDS-ONLY is non-nil, syntactic fontification (strings and comments) is not performed."
   nil
   ;; (list
   ;;  ;; See `font-lock-keywords-only' for various formatting options of these entries.
   ;;  )

   ;;------------------------------
   ;; CASE-FOLD?
   ;;------------------------------
   ;; See also `font-lock-keywords-case-fold-search'.
   nil

   ;;------------------------------
   ;; SYNTAX-ALIST
   ;;------------------------------
   ;; See also `font-lock-syntax-table'.
   ;; Why use this when you have `:syntax-table' in `define-derived-mode'?
   nil

   ;;------------------------------
   ;; The Rest: (VARIABLE . VALUE)
   ;;------------------------------
   ;; None.
   )
  "Used by `imp-timing-mode' to set the buffer local `font-lock-defaults'.

See help for `font-lock-defaults' for what all this does/can do.")


;; Just use expected name instead of my... unique naming conventions.
(defvar imp-timing-mode-map
  (let ((map (make-sparse-keymap)))
    ;; This will be set implictly by `define-derived-mode' if not set explictly.
    (set-keymap-parent map special-mode-map)

    ;;------------------------------
    ;; Unbind
    ;;------------------------------
    ;; "g" is `revert-buffer' by default (inherited from `special-mode-map')
    ;; but is often used as "refresh buffer" (e.g. `deadgrep-mode' binds it to `deadgrep-restart').
    ;; `imp-timing-mode' doesn't really have a "refresh", since it's just historical.
    (define-key map (kbd "g") nil) ; unbind `revert-buffer'

    ;;------------------------------
    ;; Bind
    ;;------------------------------
    ;; (define-key map (kbd "n") #'next-line)

    map)
  "Foo map derived from parent explicitly.")


;;;###autoload
(define-derived-mode imp-timing-mode special-mode "imp-timing"
  "Major mode for the `imp-timing-buffer' timing information buffer.

Derive from `special-mode' as a start:
https://www.gnu.org/software/emacs/manual/html_node/elisp/Basic-Major-Modes.html"
  ;;------------------------------
  ;; KEYWORD ARGS:
  ;;------------------------------
  ;; Non-Standard Variable Names?
  ;;---
  ;; If we had a syntax table, and if it's not named `<mode>-syntax-table'
  ;; (`imp-timing-mode-syntax-table'), say so:
  ;; :syntax-table imp--timing-mode-syntax-table
  ;; Don't want an `abbrev-table' as we're a read-only mode so it's a bit
  ;; pointless, but same thing as the `:syntax-table' arg.
  ;; :abbrev-table imp--timing-mode-abbrev-table

  ;;------------------------------
  ;; BODY
  ;;------------------------------

  ;;---
  ;; Fontification / Colorization / Syntax Hilighting
  ;;---
  (setq font-lock-defaults imp--timing-mode-font-lock-defaults)

  ;;---
  ;; Other Things
  ;;---
  ;; Might be nice to tail the buffer?
  ;; Force the buffer to be tailed; users can hook into this mode and disable if desired.
                                        ;(require 'autorevert)
                                        ;(auto-revert-tail-mode +1)
                                        ; Only works for file buffers...
  )


;;------------------------------------------------------------------------------
;; TODO: More stats from the `use-package' raid.
;;------------------------------------------------------------------------------

;; TODO(stats): use any of predecessor's stats?
(defvar imp-parser-statistics (make-hash-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Statistics
;;
;; TODO(stats): use any of predecessor's stats?

;; (defun imp-parser-reset-statistics ()
;;   "Reset statistics for `imp-parser'.
;; See also `imp-parser-statistics'."
;;   (interactive)
;;   (setq imp-parser-statistics (make-hash-table)))

;; (defun imp-parser-statistics-status (package)
;;   "Return loading configuration status of PACKAGE statistics."
;;   (cond ((gethash :config package)      "Configured")
;;         ((gethash :init package)        "Initialized")
;;         ((gethash :preface package)     "Prefaced")
;;         ((gethash :imp-parser package) "Declared")))

;; (defun imp-parser-statistics-last-event (package)
;;   "Return the date when PACKAGE's status last changed.
;; The date is returned as a string."
;;   (or (gethash :config package)
;;       (gethash :init package)
;;       (gethash :preface package)
;;       (gethash :imp-parser package)))

;; (defun imp-parser-statistics-time (package)
;;   "Return the time is took for PACKAGE to load."
;;   (+ (float-time (gethash :config-secs package '(0 0 0 0)))
;;      (float-time (gethash :init-secs package '(0 0 0 0)))
;;      (float-time (gethash :preface-secs package '(0 0 0 0)))
;;      (float-time (gethash :imp-parser-secs package '(0 0 0 0)))))

;; (defun imp-parser-statistics-convert (package)
;;   "Return information about PACKAGE.

;; The information is formatted in a way suitable for
;; `imp-parser-statistics-mode'."
;;   (let ((statistics (gethash package imp-parser-statistics)))
;;     (list
;;      package
;;      (vector
;;       (symbol-name package)
;;       (imp-parser-statistics-status statistics)
;;       (format-time-string
;;        "%H:%M:%S.%6N"
;;        (imp-parser-statistics-last-event statistics))
;;       (format "%.2f" (imp-parser-statistics-time statistics))))))

;; (defun imp-parser-report ()
;;   "Show current statistics gathered about `imp-parser' declarations.
;; In the table that's generated, the status field has the following
;; meaning:
;;   Configured        :config has been processed (the package is loaded!)
;;   Initialized       :init has been processed (load status unknown)
;;   Prefaced          :preface has been processed
;;   Declared          the imp-parser declaration was seen"
;;   (interactive)
;;   (with-current-buffer (get-buffer-create "*imp-parser statistics*")
;;     (setq tabulated-list-entries
;;           (mapcar #'imp-parser-statistics-convert
;;                   (hash-table-keys imp-parser-statistics)))
;;     (imp-parser-statistics-mode)
;;     (tabulated-list-print)
;;     (display-buffer (current-buffer))))

;; (defvar imp-parser-statistics-status-order
;;   '(("Declared"    . 0)
;;     ("Prefaced"    . 1)
;;     ("Initialized" . 2)
;;     ("Configured"  . 3)))

;; (define-derived-mode imp-parser-statistics-mode tabulated-list-mode
;;   "imp-parser statistics"
;;   "Show current statistics gathered about `imp-parser' declarations."
;;   (setq tabulated-list-format
;;         ;; The sum of column width is 80 characters:
;;         [("Package" 25 t)
;;          ("Status" 13
;;           (lambda (a b)
;;             (< (assoc-default
;;                 (imp-parser-statistics-status
;;                  (gethash (car a) imp-parser-statistics))
;;                 imp-parser-statistics-status-order)
;;                (assoc-default
;;                 (imp-parser-statistics-status
;;                  (gethash (car b) imp-parser-statistics))
;;                 imp-parser-statistics-status-order))))
;;          ("Last Event" 23
;;           (lambda (a b)
;;             (< (float-time
;;                 (imp-parser-statistics-last-event
;;                  (gethash (car a) imp-parser-statistics)))
;;                (float-time
;;                 (imp-parser-statistics-last-event
;;                  (gethash (car b) imp-parser-statistics))))))
;;          ("Time" 10
;;           (lambda (a b)
;;             (< (imp-parser-statistics-time
;;                 (gethash (car a) imp-parser-statistics))
;;                (imp-parser-statistics-time
;;                 (gethash (car b) imp-parser-statistics)))))])
;;   (setq tabulated-list-sort-key '("Time" . t))
;;   (tabulated-list-init-header))

;; (defun imp-parser-statistics-gather (keyword feature after)
;;   (let* ((hash (gethash feature imp-parser-statistics
;;                         (make-hash-table)))
;;          (before (and after (gethash keyword hash (current-time)))))
;;     (puthash keyword (current-time) hash)
;;     (when after
;;       (puthash (intern (concat (symbol-name keyword) "-secs"))
;;                (time-subtract (current-time) before) hash))
;;     (puthash feature hash imp-parser-statistics)))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
