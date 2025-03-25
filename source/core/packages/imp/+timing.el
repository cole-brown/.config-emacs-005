;;; core/modules/emacs/imp/+timing.el --- Imps with Timecards -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-01-07
;; Timestamp:  2023-07-19
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;                                 ──────────
;; ╔════════════════════════════════════════════════════════════════════════╗
;; ║                              Load Timing                               ║
;; ╚════════════════════════════════════════════════════════════════════════╝
;;                                   ──────
;;                  Be done is as little as 98.765 seconds.
;;                                 ──────────
;;
;;; Code:


(require 'rx)


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

(defcustom imp:timing:enabled? nil
  "Should loading & timing messages be printed?"
  :group 'imp:group
  :type '(boolean))


(defcustom imp:timing:buffer:tail? t
  "Should the timing buffer be tailed?"
  :group 'imp:group
  :type '(boolean))


;; NOTE: Doom Emacs can add the feature flag `+timing' to "doom/init.el".
(defconst imp:timing:feature? (imp:flag? :imp +timing)
  "Cache of our feature flag.")


(defun imp:timing:enabled? ()
  "Return non-nil if timing is enabled.

Specifically, if `imp:timing:enabled?' is non-nil or imp's `+timing'
feature flag is set."
  (or imp:timing:enabled?
      imp:timing:feature?))
;; (imp:timing:enabled?)


;;------------------------------------------------------------------------------
;; Constants & Variables
;;------------------------------------------------------------------------------

(defconst imp:timing:format:tree
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


(defconst int<imp>:timing:precision:time 4
  "Show this many places after decimal for default `imp:timing:format:time'.")


(defvar int<imp>:timing:indent 0
  "Current tree indent level of loading.

Example:
  loading xxx...     : level 0
  ├─loading yyy...   : level 1
  │ └─cc.dd seconds  : level 1
  └─aa.bb seconds    : level 0")


(defvar int<imp>:timing:feature:current nil
  "Currently timed imp feature.

Used to prevent duplicate start/end timing messages.

List of imp keyword/symbol names.
Example: '(:imp example feature)")


(defvar imp:timing:sum 0.0
  "Sum of all timings at indent 0 level.")


;;------------------------------------------------------------------------------
;; Custom Variables
;;------------------------------------------------------------------------------

(defcustom imp:timing:buffer:name
  "ⓘ-imp:timing-ⓘ"
  "Buffer name to print to.

If you want it to go to *Messages* with the usual minibuffer interaction, set
to: `:messages'."
  :group 'imp:group
  :type '(choice (string :tag "Name of Buffer")
                 (const :tag "Use `message' to send to *Messages* buffer with the usual minibuffer interactions."
                        :messages)))


(defcustom imp:timing:buffer:show t
  "If non-nil, show `imp:timing:buffer:name' every time it gets a new message."
  :group 'imp:group
  :type  '(boolean))


(defcustom imp:timing:format:load "loading %1$S..."
  "Format string for loading a filename.

Args to this format string are:
  1. Feature symbol: :imp/+timing
  2. File name:      +timing.el
  3. File path:      /path/to/imp/+timing.el"
  :group 'imp:group
  :type '(string)
  :risky t)


(defcustom imp:timing:format:skip "skip: %1$S"
  "Format string for skipping loading of a required file.

Args to this format string are:
  1. Feature symbol: :imp/+timing
  2. File name:      +timing.el
  3. File path:      /path/to/imp/+timing.el"
  :group 'imp:group
  :type '(string)
  :risky t)


(defcustom imp:timing:reason "reason: "
  "String prefix for _why_ something was or wasn't.

Example: When a file is skipped because it's already provided, the timing buffer
will say (with default settings):
  [...]
  ├─skip: :feature:example
  │ └─reason: already provided
  [...]"
  :group 'imp:group
  :type '(string)
  :risky t)


(defcustom imp:timing:format:skip/already-provided "feature already provided"
  "Format string for skipping loading of a required file.

Args to this format string are:
  1. Feature symbol: :imp/+timing
  2. File name:      +timing.el
  3. File path:      /path/to/imp/+timing.el"
  :group 'imp:group
  :type '(string)
  :risky t)


(defcustom imp:timing:format:skip/optional-dne "optional file does not exist: %3$s"
  "Format string for skipping loading of an optional file.

Args to this format string are:
  1. Feature symbol: :imp/+timing
  2. File name:      +timing.el
  3. File path:      /path/to/imp/+timing.el"
  :group 'imp:group
  :type '(string)
  :risky t)


(defcustom imp:timing:format:time:total
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
          "%"                                      ;; Fill with spaces.
          (number-to-string
           (+ 3 1 int<imp>:timing:precision:time)) ;; Full seconds precision + "." + fractional seconds precision
          "."
          (number-to-string
           int<imp>:timing:precision:time)         ;; Fractional seconds precision
          "f"                                      ;; Format as a floating point.

          ;;---
          ;; End of Line
          ;;---
          " seconds │\n"

          ;;---
          ;; Close Box.
          ;;---
          "└───────┴──────────────────┘")
  "String format for total elapsed time according to `imp:timing:sum'."
  :group 'imp:group
  :type '(string))


(defcustom imp:timing:format:time
  (concat "%0"                                     ;; Fill with zeros.
          (number-to-string
           (+ 2 1 int<imp>:timing:precision:time)) ;; Full seconds precision + "." + fractional seconds precision
          "."
          (number-to-string
           int<imp>:timing:precision:time)         ;; Fractional seconds precision
          "f"                                      ;; Format as a floating point.
          " seconds")                              ;; And... say what the units are.
  "Format string for number of seconds it took to load a file."
  :group 'imp:group
  :type '(string)
  :risky t)


(defcustom imp:timing:separator:restart
  (concat "\n\n"
          (make-string 80 ?─ :multibyte))
  "String that can be inserted into the output buffer via `int<imp>:timing:launch'."
  :group 'imp:group
  :type '(string)
  :risky t)


(defcustom imp:timing:separator:final
  (concat "\n\n"
          (make-string 80 ?═ :multibyte))
  "String that can be inserted into the output buffer via `imp:timing:final'."
  :group 'imp:group
  :type '(string)
  :risky t)


;;------------------------------------------------------------------------------
;; Deduplication
;;------------------------------------------------------------------------------

(defun int<imp>:timing:feature:duplicate? (feature)
  "Is FEATURE the same as currently timed feature?

FEATURE should be a list of keyword/symbol names.

Return non-nil if FEATURE is non-nil and matches feature currently being timed
\(`int<imp>:timing:feature:current')."
  ;; Don't think we want nil to be a match? We /shouldn't/ get a nil anyways...
  (and (not (null feature))
       (equal feature int<imp>:timing:feature:current)))


;;------------------------------------------------------------------------------
;; Indentation
;;------------------------------------------------------------------------------

(defun int<imp>:timing:tree:type (type indent)
  "Get tree type string for current TYPE and INDENT level.

INDENT is an integer where 0 the TYPE's indent and 1+ are its parents
indention levels."
  (plist-get imp:timing:format:tree
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
;; (int<imp>:timing:tree:type :root 0)
;; (int<imp>:timing:tree:type :root 1)
;; (int<imp>:timing:tree:type :root 2)
;; (int<imp>:timing:tree:type :leaf 0)
;; (int<imp>:timing:tree:type :leaf 1)
;; (int<imp>:timing:tree:type :leaf 2)


(defun int<imp>:timing:tree:string (type)
  "Get tree type string for current TYPE."
  (mapconcat
   #'identity
   (let (prefix)
     ;; For each indent level, build the trunks/branches.
     ;; Need to loop at least once (for indent 0), so 1+ indent level.
     (dotimes (i (1+ int<imp>:timing:indent) prefix)
       (push
        (int<imp>:timing:tree:type type i)
        prefix)))
   ;; Join w/ no padding.
   ""))
;; (int<imp>:timing:tree:string :root)
;; (let ((int<imp>:timing:indent 2)) (int<imp>:timing:tree:string :root))


;;------------------------------------------------------------------------------
;; Buffer Functions
;;------------------------------------------------------------------------------

(defun int<imp>:timing:buffer:messages? ()
  "Return t if `imp:timing:buffer:name' is the \"*Messages*\" buffer.

NOTE: This covers a value `:messages' as well as the string name."
  ;; Check for `:messages' keyword as well as a name match to the buffer.
  (cond ((and (keywordp imp:timing:buffer:name)
              (eq :messages
                  imp:timing:buffer:name)))
        ((and (stringp imp:timing:buffer:name)
              (string= "*Messages*"
                       imp:timing:buffer:name)))
        ;; Else it's some other buffer.
        (t
         nil)))


(defun imp:timing:buffer:name ()
  "Return the string name of `imp:timing:buffer:name' custom variable.

NOTE: This converts a value `:messages' to \"*Messages*\"."
  (if (int<imp>:timing:buffer:messages?)
      "*Messages*"
    imp:timing:buffer:name))


(defun int<imp>:timing:buffer:get (&optional buffer-or-name &rest args)
  "Get the imp timing buffer.

BUFFER-OR-NAME should be a string or a buffer object.
If it is nil, return value of function `imp:timing:buffer:name' will be used.

ARGS can be:
  - `:create', `create' - Create the buffer if it doesn't exist.

NOTE: If created, the buffer will be put into `imp-timing-mode'."
  (let ((buffer-or-name (or buffer-or-name
                            (imp:timing:buffer:name))))
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
;; (int<imp>:timing:buffer:get)
;; (int<imp>:timing:buffer:get nil :create)


;; https://stackoverflow.com/a/4685005/425816
(defun int<imp>:timing:buffer:tail ()
  "Tail the timing buffer.

NOTE: Does nothing if `imp:timing:buffer:tail?' is nil, or '*Messages*' buffer
is used for timing.

NOTE: Cannot use `auto-revert-tail-mode' as it only works on file-based buffers.
So, this just throws an error:
  (require 'autorevert)
  (auto-revert-tail-mode +1)"
  (when (and imp:timing:buffer:tail?
             (not (int<imp>:timing:buffer:messages?)))
    (with-current-buffer (int<imp>:timing:buffer:get)
      ;; Set point to max for the buffer in general.
      (goto-char (point-max))

      ;; Set point to max in any windows displaying the buffer. Otherwise those windows just let the
      ;; displayed lines just wander down past the displayed section of the buffer (that is, windows
      ;; don't care about the above point - they have their own).
      (let ((windows (get-buffer-window-list (current-buffer) nil t)))
        (dolist (window windows)
          (set-window-point window (point-max)))))))


(defun int<imp>:timing:buffer:show (force-show?)
  "Show timing buffer or not, depending on settings.

FORCE-SHOW?, if non-nil, will always show the buffer."
  (when (or force-show?
            (int<imp>:timing:buffer:messages?))
    (display-buffer (imp:timing:buffer:name))))


(defun imp:cmd:timing:buffer:bury (&optional ignore-messages-buffer)
  "Hide and bury the imp timing output buffer.

If IGNORE-MESSAGES-BUFFER is non-nil and the output buffer is \"*Messages*\",
does nothing instead."
  (interactive)
  ;; Ignore entirely if IGNORE-MESSAGES-BUFFER is set and we are using the messages buffer.
  (unless (and ignore-messages-buffer
               (int<imp>:timing:buffer:messages?))
    ;; Bury only when we find the window currently displaying it.
    (when-let* ((name (imp:timing:buffer:name))
                (window (get-buffer-window name)))
      (with-selected-window window
        (bury-buffer))
      (message "imp:timing: Buried timing buffer: %s"
               name))))
;; (imp:cmd:timing:buffer:bury)


(defun imp:cmd:timing:buffer:kill (&optional ignore-messages-buffer)
  "Kill the imp timing output buffer.

If IGNORE-MESSAGES-BUFFER is non-nil and the output buffer is \"*Messages*\",
does nothing instead."
  (interactive)
  ;; Ignore entirely if IGNORE-MESSAGES-BUFFER is set and we are using the messages buffer.
  (unless (and ignore-messages-buffer
               (int<imp>:timing:buffer:messages?))
    ;; Bury only when we find the window currently displaying it.
    (when-let* ((name (imp:timing:buffer:name))
                ;; Get the buffer in order to prevent "No buffer named <...>" messages.
                (buffer (int<imp>:timing:buffer:get name)))
      (kill-buffer buffer)
      (message "imp:timing: Killed timing buffer: %s"
               name))))
;; (imp:cmd:timing:buffer:kill)


;;------------------------------------------------------------------------------
;; Output
;;------------------------------------------------------------------------------

(defun int<imp>:timing:buffer:insert (string)
  "Insert finalized message STRING into output buffer."
  ;; Don't do anything unless enabled.
  (when-let ((enabled? (imp:timing:enabled?))
             (name (imp:timing:buffer:name)))
    (cond
     ;;------------------------------
     ;; Buffers
     ;;------------------------------
     ;; *Messages* buffer: Just use the `message' function.
     ((int<imp>:timing:buffer:messages?)
      (message string))

     ;; Some other buffer: Insert at the end.
     ((stringp name)
      ;; Should we force tailing of the buffer or leave that up to user and `auto-revert-tail-mode'?
      ;; I think start off with `auto-revert-tail-mode'.
      (with-current-buffer (int<imp>:timing:buffer:get name :create)
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
      (error "int<imp>:timing:buffer:insert: unhandled %s buffer: '%s'"
             (if (int<imp>:timing:buffer:messages?)
                 "(*Messages*)"
               "(non-*Messages*)")
             name)))

    ;; Tail buffer if desired.
    (int<imp>:timing:buffer:tail)

    ;; Show buffer if desired.
    (when imp:timing:buffer:show
      ;; Don't want to end up with multiple windows after start up, so be a good
      ;; steward and just use the same window as whatever's already here.
      (let ((display-buffer-alist (list (list name
                                              'display-buffer-same-window))))
        (display-buffer name)))))


(defun int<imp>:timing:message (type formatting &rest args)
  "Prepend indentation and print timing message for FORMATTING string and ARGS.

TYPE should be either `:root' or `:leaf'. Uses TYPE to get the indent string."
  (int<imp>:timing:buffer:insert
   (format "%s%s"
           (int<imp>:timing:tree:string type)
           (apply #'format formatting args))))


(defun int<imp>:timing:start (feature filename path)
  "Print a loading message for this FEATURE, FILENAME, and/or PATH.

Message depends on `imp:timing:format:load'."
  (int<imp>:timing:message :root
                           imp:timing:format:load
                           (int<imp>:feature:normalize:display feature)
                           filename
                           path))


(defun int<imp>:timing:end (time:start)
  "Print the time since TIME:START.

Message depends on `imp:timing:format:time'."
  (let ((elapsed (float-time (time-since time:start))))
    (int<imp>:timing:message :leaf
                             imp:timing:format:time
                             elapsed)
    ;; Add `elapsed' to running sum if at base indent level.
    (when (= int<imp>:timing:indent 0)
      (setq imp:timing:sum (+ imp:timing:sum elapsed)))))


(defun imp:timing:skip/already-provided (feature filename path)
  "Print a message about skipping this FEATURE / FILENAME / PATH.

Message depends on `imp:timing:format:skip'."
  (when (imp:timing:enabled?)
    ;; Skip message.
    (int<imp>:timing:message :root
                             imp:timing:format:skip
                             (int<imp>:feature:normalize:display feature)
                             filename
                             path)
    ;; Increase indent level for reason.
    (let ((int<imp>:timing:indent (1+ int<imp>:timing:indent)))
      ;; Skip reason message.
      (int<imp>:timing:message :leaf
                               (concat imp:timing:reason
                                       imp:timing:format:skip/already-provided)
                               (int<imp>:feature:normalize:display feature)
                               filename
                               path))))


(defun imp:timing:skip/optional-dne (feature filename path)
  "Print a message about optional FEATURE / FILENAME / PATH that doesn't exist.

Message depends on `imp:timing:format:optional'."
  (when (imp:timing:enabled?)
    ;; Skip message.
    (int<imp>:timing:message :root
                             imp:timing:format:skip
                             (int<imp>:feature:normalize:display feature)
                             filename
                             path)
    ;; Increase indent level for reason.
    (let ((int<imp>:timing:indent (1+ int<imp>:timing:indent)))
      ;; Skip reason message.
      (int<imp>:timing:message :leaf
                               (concat imp:timing:reason
                                       imp:timing:format:skip/optional-dne)
                               (int<imp>:feature:normalize:display feature)
                               filename
                               path))))


(defmacro imp:timing (feature filename path &rest body)
  "Measure & print the time it takes to evaluate BODY.

FEATURE should be a list of keyword & symbol names.

FILENAME should be the file's basename.

PATH should be the path to FILENAME's parent directory.

Output message depends on `imp:timing:format:time'.

Return result of evaluating BODY."
  (declare (indent 3))

  `(let ((macro<imp>:feature (int<imp>:feature:normalize ,feature)))
     (if (and (imp:timing:enabled?)
              ;; Don't do (another) timing block/level for a duplicated call.
              (not (int<imp>:timing:feature:duplicate? macro<imp>:feature)))

         ;; Timings enabled: Run body in between timing start/end messages.
         (let ((macro<imp>:filename ,filename)
               (macro<imp>:path     ,path)
               (macro<imp>:time     (current-time)))
           ;; Update current feature being timed.
           (setq int<imp>:timing:feature:current macro<imp>:feature)
           ;; Output load message.
           (int<imp>:timing:start macro<imp>:feature
                                  macro<imp>:filename
                                  macro<imp>:path)
           (prog1
               ;; Increase indent level for body.
               (let ((int<imp>:timing:indent (1+ int<imp>:timing:indent)))
                 ;; Run the body...
                 ,@body)

             ;; Clear this feature from current.
             (setq int<imp>:timing:feature:current nil)

             ;; Finish with the timing message.
             (int<imp>:timing:end macro<imp>:time)))

       ;; Timing disabled: Just run body.
       ,@body)))


;;------------------------------------------------------------------------------
;; Output: Init / Finalize
;;------------------------------------------------------------------------------

(defun imp:timing:restart ()
  "'Restart' timing.

1) Reset `imp:timing:sum' to zero.
2) Print a starting separator to the timing buffer if needed.
   - If `imp:timing:buffer:name' doesn't exists or is *Messages*, does nothing."
  ;; Reset timing sum variable.
  (setq imp:timing:sum 0.0)

  ;; Broken up because it's too early for this... :|
  (cond
   ;; Not enabled = no output.
   ((not (imp:timing:enabled?))
    nil)
   ;; *Messages* buffer = no output.
   ((int<imp>:timing:buffer:messages?)
    nil)
   ;; Not *Messages* and exists = output!
   ((int<imp>:timing:buffer:get (imp:timing:buffer:name))
    ;; Mark where we restarted timing from
    (int<imp>:timing:buffer:insert imp:timing:separator:restart))
   ;; Else, no output.
   (t
    nil)))


(defun int<imp>:timing:final (&optional separator-line?)
  "Actually output the total timing summary.

If SEPARATOR-LINE? is non-nil, print out `imp:timing:separator:final' after
final timing message."
  (when (imp:timing:enabled?)
    (int<imp>:timing:buffer:insert
     (format imp:timing:format:time:total
             imp:timing:sum))
    (when separator-line?
      (int<imp>:timing:buffer:insert imp:timing:separator:final))))
;; (int<imp>:timing:final)
;; (int<imp>:timing:final t)


(defun int<imp>:timing:final/timer (&optional separator-line?)
  "Run timer for final message (total timing summary).

If not ready to output at end of timer, re-run timer.

If SEPARATOR-LINE? is non-nil, print out `imp:timing:separator:final' after
final timing message."
  (run-with-timer 1 ; seconds
                  nil ; do not repeat
                  (lambda (separator?)
                    "Check if ready to output final message, then output or rerun timer."
                    (if (or (> int<imp>:timing:indent 0)
                            int<imp>:timing:feature:current)
                        ;; Still timing something, so wait again.
                        (int<imp>:timing:final/timer separator?)
                      ;; Done I think? Output.
                      (int<imp>:timing:final separator?)))
                  separator-line?))


(defun imp:timing:final (&optional separator-line?)
  "Set up a total timing summary to print out once everything's done loading.

If SEPARATOR-LINE? is non-nil, print out `imp:timing:separator:final' after
final timing message."
  ;; Don't bother trying until start-up is (mostly) over.
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Startup-Summary.html#Startup-Summary
  (add-hook 'emacs-startup-hook
            (lambda ()
              "Print final timing info to imp timing buffer once done timing."
              (int<imp>:timing:final/timer separator-line?))))
;; (imp:timing:final t)
;; (run-hooks 'emacs-startup-hook)


;;--------------------------------------------------------------------------------
;; Major Mode
;;--------------------------------------------------------------------------------

(defun int<imp>:timing/mode:font-lock-keywords:element/create (matcher subexp facename &optional override laxmatch)
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
;; (int<imp>:timing/mode:font-lock-keywords:element/create "foo" 0 'font-lock-comment-face nil t)


(defconst int<imp>:timing/mode:font-lock-keywords
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
        (rx/reason/warning (rx-to-string `(and ,imp:timing:reason
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
                                         ;; NOTE: Similar but different from `int<imp>:feature:replace:rx'.
                                         ;; Careful how this goes or you lose the question mark...
                                         (any "?" ":" "-" "_" "/" "."
                                              alphanumeric)))
                                  :no-group))

        ;;------------------------------
        ;; Final Status
        ;;------------------------------
        ;; Not currently printed to 'imp:timing' buffer?
        )

    ;;------------------------------
    ;; Timing Tree
    ;;------------------------------
    (push (int<imp>:timing/mode:font-lock-keywords:element/create
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
    (push (int<imp>:timing/mode:font-lock-keywords:element/create
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
    (push (int<imp>:timing/mode:font-lock-keywords:element/create
           rx/status/info
           0 ; Match's Group Number from Match Regex
           'font-lock-comment-face ; Font Face to Use
           ;; TODO: Lighten up on these guys? I shouldn't need them except for other things (e.g.
           ;; `whitespace-mode') want to layer on top of my mode?
           t
           t)
          flk)
    (push (int<imp>:timing/mode:font-lock-keywords:element/create
           rx/status/warning
           0 ; Match's Group Number from Match Regex
           'font-lock-warning-face ; Font Face to Use
           ;; TODO: Lighten up on these guys? I shouldn't need them except for other things (e.g.
           ;; `whitespace-mode') want to layer on top of my mode?
           t
           t)
          flk)
    (push (int<imp>:timing/mode:font-lock-keywords:element/create
           rx/reason/warning
           0 ; Match's Group Number from Match Regex
           'font-lock-warning-face ; Font Face to Use
           ;; TODO: Lighten up on these guys? I shouldn't need them except for other things (e.g.
           ;; `whitespace-mode') want to layer on top of my mode?
           t
           t)
          flk)

    ;; NOTE: Try leaving `status' and `amounts' as just... unlocked text? (e.g. white in my theme's case)
    ;; (push (int<imp>:timing/mode:font-lock-keywords:element/create
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
    (push (int<imp>:timing/mode:font-lock-keywords:element/create
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
    (push (int<imp>:timing/mode:font-lock-keywords:element/create
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
    ;; Not currently printed to 'imp:timing' buffer?

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
;; (pp int<imp>:timing/mode:font-lock-keywords)


(defconst int<imp>:timing/mode:font-lock-defaults
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
   int<imp>:timing/mode:font-lock-keywords

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
(define-derived-mode imp-timing-mode special-mode "imp:timing"
  "Major mode for the `imp:timing:buffer:name' timing information buffer.

Derive from `special-mode' as a start:
https://www.gnu.org/software/emacs/manual/html_node/elisp/Basic-Major-Modes.html"
  ;;------------------------------
  ;; KEYWORD ARGS:
  ;;------------------------------
  ;; Non-Standard Variable Names?
  ;;---
  ;; If we had a syntax table, and if it's not named `<mode>-syntax-table'
  ;; (`imp-timing-mode-syntax-table'), say so:
  ;; :syntax-table int<imp>:timing/mode:syntax-table
  ;; Don't want an `abbrev-table' as we're a read-only mode so it's a bit
  ;; pointless, but same thing as the `:syntax-table' arg.
  ;; :abbrev-table int<imp>:timing/mode:abbrev-table

  ;;------------------------------
  ;; BODY
  ;;------------------------------

  ;;---
  ;; Fontification / Colorization / Syntax Hilighting
  ;;---
  (setq font-lock-defaults int<imp>:timing/mode:font-lock-defaults)

  ;;---
  ;; Other Things
  ;;---
  ;; Might be nice to tail the buffer?
  ;; Force the buffer to be tailed; users can hook into this mode and disable if desired.
  ;(require 'autorevert)
  ;(auto-revert-tail-mode +1)
  ; Only works for file buffers...
  )



;; NOTE: We aren't a file-based mode, so we don't register ourselves for anything in `auto-mode-alist'.
;; ;;;###autoload
;; (add-to-list 'auto-mode-alist '("\\.fileextension" . imp-timing-mode))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
