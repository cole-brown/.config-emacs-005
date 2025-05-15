;;; core/modules/emacs/innit/package-upgrade-command.el --- Package Upgrade Command -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2023-07-19
;; Timestamp:  2023-09-27
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Package Upgrade Command & Mode
;;
;;; Code:


(imp:require :innit 'package 'upgrade 'mode)

(require 'rx)


;;--------------------------------------------------------------------------------
;; Settings
;;--------------------------------------------------------------------------------

(defcustom innit:package/upgrade:buffer:tail? t
  "Should the timing buffer be tailed?"
  :group 'innit:group
  :type '(boolean))


(defcustom innit:package/upgrade:buffer:show t
  "If non-nil, show `innit:package/upgrade:buffer:name' every time it gets a new message."
  :group 'innit:group
  :type  '(boolean))


(defcustom innit:package/upgrade:buffer:name
  "ⓘ-innit:package:upgrade-ⓘ"
  "Buffer name to print to.

If you want it to go to *Messages* with the usual minibuffer interaction, set
to: `:messages'."
  :group 'innit:group
  :type '(choice (string :tag "Name of Buffer")
                 (const :tag "Use `message' to send to *Messages* buffer with the usual minibuffer interactions."
                        :messages)))


(defun innit:package/upgrade:buffer:name ()
  "Return the string name of `innit:package/upgrade:buffer:name' custom variable.

NOTE: This converts a value `:messages' to \"*Messages*\"."
  (if (int<innit>:package/upgrade:buffer:messages?)
      "*Messages*"
    innit:package/upgrade:buffer:name))
;; (innit:package/upgrade:buffer:name)


(defcustom innit:package/upgrade:timestamp/format "%Y-%m-%d %H:%M:%S"
  "`format-time-string' datetime formatting string.

Default is RFC-3339 datetime format, which outputs like:
  2001-02-03 14:15:16

ISO-8601 format is: \"%Y-%m-%dT%T%z\", which outputs like:
  2001-02-03T14:15:16-0700"
  :group 'innit:group
  :type 'string)


;;------------------------------------------------------------------------------
;; Package Upgrade Functions: Buffers
;;------------------------------------------------------------------------------

(defun int<innit>:package/upgrade:buffer:messages? ()
  "Is `innit:package/upgrade:buffer:name' equivalent to \"*Messages*\" buffer?

NOTE: This covers our `:messages' value as well as the string name."
  ;; Check for `:messages' keyword as well as a name match to the buffer.
  (cond ((and (keywordp innit:package/upgrade:buffer:name)
              (eq :messages
                  innit:package/upgrade:buffer:name)))
        ((and (stringp innit:package/upgrade:buffer:name)
              (string= "*Messages*"
                       innit:package/upgrade:buffer:name)))
        ;; Else it's some other buffer.
        (t
         nil)))


(defun int<innit>:package/upgrade:buffer:get ()
  "Create buffer and set to `innit-package-upgrade-mode'.

Return created buffer object."
   (let ((buffer-or-name (innit:package/upgrade:buffer:name)))
    ;; If it already exists, we're done; just return it.
    (cond ((get-buffer buffer-or-name))

          ;; Doesn't exist! Create only if requested.
          (t
           (with-current-buffer (get-buffer-create buffer-or-name)
             ;; TODO
             ;; TODO
             ;; TODO
             ;; (innit-package-upgrade-mode)
             (current-buffer))))))
;; (int<innit>:package/upgrade:buffer:get)


;; https://stackoverflow.com/a/4685005/425816
(defun int<innit>:package/upgrade:buffer:tail ()
  "Tail the package upgrade buffer.

NOTE: Does nothing if `innit:package/upgrade:buffer:tail?' is nil, or
the '*Messages*' buffer is used instead of a separate buffer.

NOTE: Cannot use `auto-revert-tail-mode' as it only works on file-based buffers.
So, this just throws an error:
  (require 'autorevert)
  (auto-revert-tail-mode +1)"
  (when (and innit:package/upgrade:buffer:tail?
             (not (int<innit>:package/upgrade:buffer:messages?)))
    (with-current-buffer (int<innit>:package/upgrade:buffer:get)
      ;; Set point to max for the buffer in general.
      (goto-char (point-max))

      ;; Set point to max in any windows displaying the buffer. Otherwise those windows just let the
      ;; displayed lines just wander down past the displayed section of the buffer (that is, windows
      ;; don't care about the above point - they have their own).
      (let ((windows (get-buffer-window-list (current-buffer) nil t)))
        (dolist (window windows)
          (set-window-point window (point-max)))))))


(defun int<innit>:package/upgrade:buffer:show (force-show?)
  "Show package/upgrade buffer or not, depending on settings.

FORCE-SHOW?, if non-nil, will always show the buffer."
  (when (or force-show?
            (int<innit>:package/upgrade:buffer:messages?))
    (display-buffer (innit:package/upgrade:buffer:name))))


(defun innit:cmd:package/upgrade:buffer:bury (&optional ignore-messages-buffer)
  "Hide and bury the innit package/upgrade output buffer.

If IGNORE-MESSAGES-BUFFER is non-nil and the output buffer is \"*Messages*\",
does nothing instead."
  (interactive)
  ;; Ignore entirely if IGNORE-MESSAGES-BUFFER is set and we are using the messages buffer.
  (unless (and ignore-messages-buffer
               (int<innit>:package/upgrade:buffer:messages?))
    ;; Bury only when we find the window currently displaying it.
    (when-let* ((name (innit:package/upgrade:buffer:name))
                (window (get-buffer-window name)))
      (with-selected-window window
        (bury-buffer))
      (message "innit:package/upgrade: Buried package/upgrade buffer: %s"
               name))))
;; (innit:cmd:package/upgrade:buffer:bury)


(defun innit:cmd:package/upgrade:buffer:kill (&optional ignore-messages-buffer)
  "Kill the innit package/upgrade output buffer.

If IGNORE-MESSAGES-BUFFER is non-nil and the output buffer is \"*Messages*\",
does nothing instead."
  (interactive)
  ;; Ignore entirely if IGNORE-MESSAGES-BUFFER is set and we are using the messages buffer.
  (unless (and ignore-messages-buffer
               (int<innit>:package/upgrade:buffer:messages?))
    ;; Bury only when we find the window currently displaying it in order to
    ;; prevent "No buffer named <...>" messages.
    (when-let* ((buffer (int<innit>:package/upgrade:buffer:get)))
      (kill-buffer buffer)
      (message "innit:package/upgrade: Killed package/upgrade buffer: %s"
               name))))
;; (innit:cmd:package/upgrade:buffer:kill)


;;------------------------------------------------------------------------------
;; Package Upgrade Functions: Output
;;------------------------------------------------------------------------------

(defun int<innit>:package/upgrade:buffer:insert (string)
  "Insert finalized message STRING into output buffer."
  ;; *Messages* buffer: Just use the `message' function.
  (if (int<innit>:package/upgrade:buffer:messages?)
      (message string)

    ;; Some other buffer: Insert at the end.
    (with-current-buffer (int<innit>:package/upgrade:buffer:get)
      ;; We are now in BUFFER, so just insert the formatted string on a new line at the end.
      (goto-char (point-max))
      ;; If we're in `innit-package-upgrade-mode', we'll probably need to deal with the
      ;; fact that the buffer is read-only...
      (let ((inhibit-read-only t))
        ;; Prepend a newline, unless this is a new/empty buffer.
        (insert (concat (if (= (buffer-size) 0)
                            ""
                          "\n")
                        string)))))

    ;; Tail buffer if desired.
    (int<innit>:package/upgrade:buffer:tail)

    ;; Show buffer if desired.
    (when innit:package/upgrade:buffer:show
      (let ((name (innit:package/upgrade:buffer:name)))
        ;; Don't want to end up with multiple windows after start up, so be a good
        ;; steward and just use the same window as whatever's already here.
        (let ((display-buffer-alist (list (list name
                                                'display-buffer-same-window))))
          (display-buffer name)))))


(defun int<innit>:package/upgrade:message (message &rest args)
  "Format MESSAGE with ARGS, then insert into output buffer."
  (int<innit>:package/upgrade:buffer:insert
   (apply #'format message args)))


(defun int<innit>:package/upgrade:message/dry (dry-run message &rest args)
  "Output MESSAGE formatted with ARGS, with \"[DRY] \" prefix if DRY-RUN."
  (apply #'int<innit>:package/upgrade:message
         (concat (if dry-run "[DRY] " "")
                 message)
         args))
;; (int<innit>:package/upgrade:message/dry :dry "hello %s" "there")


(defun int<innit>:package/upgrade:message/status (dry-run status message &rest args)
  "Output STATUS with MESSAGE (formatted with ARGS).

If DRY-RUN is non-nil, prefix status with \"DRY:\".
  e.g.:
    [OK]     - DRY-RUN nil
    [DRY:OK] - DRY-RUN non-nil

If MESSAGE is nil, output only the status."
  (apply #'int<innit>:package/upgrade:message
         (concat "  ["
                 (if dry-run "DRY:" "")
                 (pcase status
                   (:ok
                    "OK")
                   (:error
                    "ERROR")
                   (:failure
                    "FAILURE")
                   ((pred stringp)
                    status)
                   (_
                    (format "?%s?" status)))
                 "]"
                 (when message
                     " ")
                 message)
         args))
;; (int<innit>:package/upgrade:message/status :dry :ok "hello")
;; (int<innit>:package/upgrade:message/status :dry :ok nil)


(defun int<innit>:package/upgrade:message/error (dry-run message &rest args)
  "Output ERROR status MESSAGE (formatted with ARGS), then throw to `:error'.

If DRY-RUN is non-nil, prefix status with \"DRY:\".
  e.g.:
    [OK]     - DRY-RUN nil
    [DRY:OK] - DRY-RUN non-nil"
  (int<innit>:package/upgrade:message/newline) ; Always give some whitespace to the error message.
  (apply #'int<innit>:package/upgrade:message/status dry-run :error message args)
  ;; Throw to `:exit' handler with value `:error'.
  (throw :exit :error))


(defun int<innit>:package/upgrade:message/echo (message &rest args)
  "Format MESSAGE with ARGS, insert into output buffer, and echo to minibuffer."
  (let ((output (apply #'format message args)))
    ;; Add to the output buffer.
    (int<innit>:package/upgrade:buffer:insert output)
    ;; Echo to minibuffer without sending to *Messages* buffer.
    (let (message-log-max)
      (message output))))
;; (int<innit>:package/upgrade:message/echo "hello %s" "there")


(defun int<innit>:package/upgrade:message/separator ()
  "Output an ASCII Box Art line separator unless buffer is empty.

https://en.wikipedia.org/wiki/Box-drawing_character

Act like predicate:
Return t if line was acutally inserted, else nil."
  ;; Be in the correct buffer so `window-width' is correct.
  (with-current-buffer (int<innit>:package/upgrade:buffer:get)
    (unless (= (point-min) (point-max)) ; Empty buffer?
      (int<innit>:package/upgrade:message
       "\n\n%s"
       (make-string (- (window-max-chars-per-line) 2) ; One less for `whitespace-mode' EOL char & one more for luck.
                    ?─ ; ASCII Box Art line character
                    :multibyte))
      ;; Return non-nil since we actually did something.
      t)))
;; (int<innit>:package/upgrade:message/separator)


(defun int<innit>:package/upgrade:message/newline (&optional number)
  "Output NUMBER (1 if nil) of empty line(s)."
  ;; If we only want one newline, we need to message with zero newline chars.
  ;; If two, message with 1.
  ;; Etc.
  (let ((num-chars (1- (max 1
                            (if (natnump number)
                                number
                              1)))))
    (int<innit>:package/upgrade:message
     (make-string num-chars ?\n))))
;; (int<innit>:package/upgrade:message/newline)
;; (int<innit>:package/upgrade:message/newline 2)


(defun int<innit>:package/upgrade:message/list (title type items)
  "Format ITEMS into a TYPE list and print each to the output buffer.

TITLE, if non-nil, will be printed just before the list.
TITLE should be:
  - nil
  - string
  - (format-string . args)

TYPE should be:
  - `:numbered', `:number'
  - `:bulleted', `:bullet'

ITEMS should be a list where each item is one of:
  - (format-string . args)
  - string"
  (let* ((count (length items))
         (format/list (concat "  "
                              (pcase type
                                ((or :numbered 'numbered :number 'number)
                                 (concat "%"
                                     (number-to-string (1+ (floor (log count 10))))
                                     "d."))
                                ((or :bulleted 'bulleted :bullet 'bullet)
                                 "•")
                                (_
                                 (error "int<innit>:package/upgrade:message/list: unknown TYPE %S: %S"
                                        (type-of type)
                                        type)))
                              " ")))

    ;;------------------------------
    ;; Print TITLE?
    ;;------------------------------
    (cond ((stringp title)
           (int<innit>:package/upgrade:buffer:insert title))
          ;; Checking for `listp', so need to make sure it's not nil first.
          ((null title)
           ;; No-op.
           nil)
          ((listp title)
           (apply #'int<innit>:package/upgrade:message title))
          (t
           (error "int<innit>:package/upgrade:message/list: unknown TITLE type %S: %S"
                  (type-of title)
                  title)))

    ;;------------------------------
    ;; Print List
    ;;------------------------------
    (dotimes (i count)
      (let ((item (pop items)))
        (int<innit>:package/upgrade:buffer:insert
         (concat
          ;; Bullet or Number
          (format format/list i)
          (cond ((stringp item)
                 item)
                ;; Need to null check before listp.
                ((null item)
                 "")

                ((listp item)
                 (if (listp (cdr item))
                     ;; lists
                     (apply #'format item)
                   ;; conses
                   (format (car item) (cdr item))))
                (t
                 ;; Attempt to translate the unknown to a string.
                 (format "%S" item)))))))))
;; (int<innit>:package/upgrade:message/list "Hello There!"
;;                                          :numbered
;;                                          '(1
;;                                            2
;;                                            ("third %s" . "thing")
;;                                            ("%s %d" foo 33)))
;; (int<innit>:package/upgrade:message/list "Hello There!"
;;                                          :bullet
;;                                          '(1
;;                                            2
;;                                            ("third %s" . "thing")
;;                                            ("%s %d" foo 33)))


;;------------------------------------------------------------------------------
;; Package Upgrade Command
;;------------------------------------------------------------------------------

;;------------------------------
;; NOTE: Update/Upgrade Packages Process
;;------------------------------
;;
;; If you get an error like this on installing a new package:
;;   Error (use-package): Failed to install magit: http://melpa.org/packages/magit-20190113.1949.tar: Not found
;;   Error (use-package): Cannot load magit
;; Or this:
;;   Debugger entered--Lisp error: (file-error "http://melpa.org/packages/markdown-mode-20190305.319.el" "Not found")
;; Just evalulate this again: (package-refresh-contents)

;;------------------------------
;; NOTE: Manual Upgrade Process
;;------------------------------
;;   M-x list-packages
;;     1. 'U' to mark upgrades
;;     2. 'x' to upgrade?
;; After it's all done, maybe run:
;;   M-x package-autoremove
;;     - May need `M-x list-packages' first if custom-file doesn't exist right now.


;;------------------------------
;; Package Upgrade
;;------------------------------

;; TODO [2020-03-11]:
;;   - Instead of a max number, just do X at a time with some
;;     (yasnippet-snippets) on their own, because they can cause too many files
;;     to open and that'll fail the package upgrade.
;;   - [2023-07-19]: Don't have this problem now (because Linux not Windows, or because new(er) Emacs version?)?


;; TODO: Delete? Use for minibuffer only?
(defcustom innit:package:upgrade:pretty-print-packages 10
  "Pretty print packages-to-upgrade list if greater than or equal to this amount.

`innit:cmd:package:upgrade' will pretty print the info buffer, and truncate the
prompt."
  :group 'innit:group
  :type  '(natnum)) ; non-negative integer


(defun int<innit>:package:version (package-name package-description-alist)
  "Get version for PACKAGE-NAME from PACKAGE-DESCRIPTION-ALIST.

Return nil if PACKAGE-NAME is not in PACKAGE-DESCRIPTION-ALIST."
  (let ((package-desc (cadr (assq package-name package-description-alist))))
    (when package-desc
      (package-desc-version package-desc))))


(cl-defun innit:package:upgrade (&key max dry-run)
  "Upgrade all packages automatically without showing the '*Packages*' buffer.

First update package metadata, then upgrade all packages that have newer
versions.

Will only upgrade the first MAX packages if MAX is numberp or can be converted
by `string-to-number', and number to upgrade is greater than MAX.

If DRY-RUN is non-nil, do everything posible except actually upgrading any
packages."
  (let* ((func/name "innit:package:upgrade")
         (length/max (cond ((null max)
                            nil)
                           ((and (stringp max)
                                 (string= "" max))
                            nil)
                           ((stringp max)
                            (string-to-number max))
                           ((numberp max)
                            max)
                           (t
                            (nub:error
                                :innit
                                func/name
                              "Cannot convert MAX to number: %S"
                              max))))
         ;; Sanity:
         ;;   - 0 lurking around as a valid string-to-number for "IDK" is annoying.
         ;;   - Negative or 0 from weird user inputs should be handled...
         ;;   - Also a good spot to convert 'nil' to 'I don't care'.
         (length/max (if (or (null length/max)
                             (< length/max 1))
                         most-positive-fixnum
                       length/max))
         length/all
         length/subset
         upgrades/all
         upgrades/subset
         (length/installed (length package-alist))
         (format/num-packages (format "%%%dd"
                                      (1+ (floor (log length/installed 10)))))
         (time/start (current-time))
         time/end)

    ;;------------------------------
    ;; Catch Early Exits
    ;;------------------------------
    ;; We are a user function.
    ;; We want to be pretty, and useful, and pretty useful.
    ;; So we want to be able to just message the user with the "error" message, mainly.
    ;; And then, importantly, we need to be done.
    ;; Functions `throw' and `catch' let us do that.
    ;; See: https://www.reddit.com/r/emacs/comments/u2u229/dont_explain_show_me_examples_a_tour_of_the/
    (pcase
        (catch :exit
          ;;------------------------------
          ;; Titular Infos
          ;;------------------------------
          ;; Start out with separator, unless we don't need it, in which don't.
          (int<innit>:package/upgrade:message/separator)

          ;; Start Message
          (int<innit>:package/upgrade:message/dry
           dry-run
           "Upgrade Installed Emacs Packages")

          ;; Start Timestamp
          (int<innit>:package/upgrade:message/dry dry-run
                                                  (format-time-string innit:package/upgrade:timestamp/format time/start))

          ;;------------------------------
          ;; Error Checks
          ;;------------------------------
          (when (and (not (null max))
                     (not (natnump length/max)))
            (int<innit>:package/upgrade:message/error
             dry-run
             "Don't know what to do with intended MAX number: %S"
             max))

          ;;------------------------------
          ;; Package Metadata
          ;;------------------------------
          (int<innit>:package/upgrade:message/newline)
          (int<innit>:package/upgrade:message "Refreshing package metadata...")

          ;; Update the package metadata from the package repos.
          (package-refresh-contents)
          (int<innit>:package/upgrade:message/status dry-run :ok nil)

          ;;------------------------------
          ;; Find Packages to Upgrades
          ;;------------------------------
          (int<innit>:package/upgrade:message/newline)
          (int<innit>:package/upgrade:message "Checking for packages to upgrade...")

          (dolist (package (mapcar #'car package-alist))
            (let ((in-archive (int<innit>:package:version package package-archive-contents)))
              (when (and in-archive
                         (version-list-< (int<innit>:package:version package package-alist)
                                         in-archive))
                (push (cadr (assq package package-archive-contents))
                      upgrades/all))))
          (setq length/all (length upgrades/all))

          (int<innit>:package/upgrade:message (concat "    Installed Packages:  "
                                                      format/num-packages)
                                              length/installed)
          (int<innit>:package/upgrade:message (concat "    Upgradable Packages: "
                                                      format/num-packages)
                                              length/all)
          ;; Don't display max when it's "I don't care".
          (when (/= length/max most-positive-fixnum)
            (int<innit>:package/upgrade:message (concat "    Max to Upgrade:      "
                                                        format/num-packages)
                                                length/max))

          (int<innit>:package/upgrade:message/status dry-run :ok nil)

          ;;------------------------------
          ;; Show Packages to Upgrade
          ;;------------------------------
          (int<innit>:package/upgrade:message/newline)

          (when (null upgrades/all)
            ;;---
            ;; Nothing - done!
            ;;---
            (int<innit>:package/upgrade:message/status dry-run
                                                       :ok
                                                       "All packages are up to date.")
            (throw :exit :no-op))

          ;;---
          ;; Show Full List
          ;;---
          (int<innit>:package/upgrade:message/list "Upgradable Packages:"
                                                   :numbered
                                                   (seq-map (lambda (upgrade)
                                                              (concat "`"
                                                                      (package-desc-full-name upgrade)
                                                                      "'"))
                                                            upgrades/all))

          ;;---
          ;; Reduce List?
          ;;---
          (if (and (not (null length/max))
                   (> (length upgrades/all) length/max))
              (progn
                ;; Chop upgrades down to the maximum allowed right now.
                (setq upgrades/subset (seq-take upgrades/all length/max))
                (int<innit>:package/upgrade:message/newline)
                (int<innit>:package/upgrade:message "Too many packages to upgrade!")
                (int<innit>:package/upgrade:message/list "Upgrading Package Subset:"
                                                         :numbered
                                                         (seq-map #'package-desc-full-name
                                                                  upgrades/subset)))
            ;; Just take 'em all
            (setq upgrades/subset upgrades/all))
          (setq length/subset (length upgrades/subset))

          (int<innit>:package/upgrade:message/newline)
          (int<innit>:package/upgrade:message "Upgrade %d Package%s..."
                                              length/subset
                                              (if (= length/subset 1) "" "s"))

          ;;------------------------------
          ;; Upgrade Packages
          ;;------------------------------
          ;; NOTE: If DRY-RUN, do NOT (actually) do anything!
          (let ((packages/prompt (format "%s %s package%s? "
                                         (if dry-run
                                             "[DRY] Dry-run upgrade for"
                                           "Upgrade")
                                         (if (> length/all length/subset)
                                             (format "%d/%d"
                                                     length/subset
                                                     length/all)
                                           (format "%d" length/subset))
                                         (if (= length/subset 1) "" "s"))))
            ;; Push the prompt into the output buffer.
            (int<innit>:package/upgrade:message (concat "  " packages/prompt))
            ;; Actually prompt:
            (if (yes-or-no-p packages/prompt)
                ;;---
                ;; Update
                ;;---
                (progn
                  (int<innit>:package/upgrade:message "    <- Yes")

                  (int<innit>:package/upgrade:message/newline)
                  (int<innit>:package/upgrade:message "%sUpgrading Packages..."
                                                      (if dry-run "[DRY] " ""))
                  (let ((format/list (concat "  %"
                                             (number-to-string (1+ (floor (log length/subset 10))))
                                             "d. "
                                             (if dry-run "[DRY] " "")
                                             ;; "<Actioning> `package-name'..."
                                             "%s `%s'...")))
                    (save-window-excursion
                      (dotimes (i length/subset)
                        (let* ((package-desc/new (pop upgrades/subset))
                               (package-desc/old (cadr (assq (package-desc-name
                                                              package-desc/new)
                                                             package-alist))))
                          ;; Install new version.
                          (int<innit>:package/upgrade:message format/list
                                                              i
                                                              "Upgrading to"
                                                              (package-desc-full-name package-desc/new))
                          (unless dry-run
                            (package-install package-desc/new))

                          ;; Uninstall old version.
                          (int<innit>:package/upgrade:message format/list
                                                              i
                                                              "Deleting old"
                                                              (package-desc-full-name package-desc/old))
                          (unless dry-run
                            (package-delete package-desc/old)))))

                    (int<innit>:package/upgrade:message/status
                     dry-run
                     :ok
                     nil)))

              ;;---
              ;; Cancel
              ;;---
              (int<innit>:package/upgrade:message "    <- No")
              (int<innit>:package/upgrade:message/newline)
              (int<innit>:package/upgrade:message/dry dry-run
                                                      "Package update declined.")
              (throw :exit :cancel))))

      ;;------------------------------
      ;; `catch' Handlers
      ;;------------------------------

      ;;---
      ;; Failure
      ;;---
      (:error
       (int<innit>:package/upgrade:message/newline)
       (int<innit>:package/upgrade:message/dry dry-run
                                               "Done:")
       (int<innit>:package/upgrade:message/status
        dry-run
        :failure
        "Error happened during package upgrade."))

      ;;---
      ;; Success
      ;;---
      ;; Null success case.
      (:no-op
       (int<innit>:package/upgrade:message/newline)
       (int<innit>:package/upgrade:message/dry dry-run
                                               "Done:")
       (int<innit>:package/upgrade:message/status
        dry-run
        :ok
        "No Updates Needed; Nothing To Do"))

      ;; Success through inaction case.
      (:cancel
       (int<innit>:package/upgrade:message/newline)
       (int<innit>:package/upgrade:message/dry dry-run
                                               "Done:")
       (int<innit>:package/upgrade:message/status
        dry-run
        :ok
        "No Updates Applied (0/%d)"
        length/subset))

      ;; Success through action case.
      (_
       (int<innit>:package/upgrade:message/newline)
       (int<innit>:package/upgrade:message/dry dry-run
                                               "Done:")
       (int<innit>:package/upgrade:message/status
        dry-run
        :ok
        "%s Package%s Updated"
        (if (> length/all length/subset)
            (format "%d/%d"
                    length/subset
                    length/all)
          (format "%d" length/subset))
        (if (= length/subset 1) "" "s"))
       (unless dry-run
           (int<innit>:package/upgrade:message/newline)
           (int<innit>:package/upgrade:message "Restart Emacs to start using new packages."))))))
;; (innit:package:upgrade :dry-run t)


;; Less manual. Upgrade all packages without showing *Packages* buffer.
;; https://emacs.stackexchange.com/questions/16398/noninteractively-upgrade-all-packages
(defun innit:cmd:package:upgrade (&optional max)
  "Upgrade all packages automatically without showing the '*Packages*' buffer.

First updates package metadata, then upgrades all packages that have newer
versions.

Will only upgrade the first MAX if MAX is numberp or can be converted by
`string-to-number', and number to upgrade is greater than MAX."
  (interactive "P")

  ;; Interactive code "P" (raw prefix argument) gives us nil or a list with the
  ;; prefix in it. So if we get the list, just use the first element as the MAX.
  (when (listp max)
    (setq max (nth 0 max)))

  (innit:package:upgrade :max max))

;; http://nhoffman.github.io/.emacs.d/#orgf46780c
;; Some useful ELPA variables and functions:
;;   M-x package-list-packages  - open list of packages
;;   package-activated-list     - variable containing list of the names of currently activated packages
;;   package-install            - install a package
;;   package-installed-p        - true if package is installed


(defvar innit:upgrade:hook nil
  "Functions to run after updating packages in `innit:cmd:upgrade'.")


(defun innit:cmd:upgrade (&optional max-packages)
  "Upgrade all the things! Packages, LSP servers...

First update the packages, then run all hooks in `innit:hook:upgrade'

First updates package metadata, then upgrades all packages that have newer
versions.

Will only upgrade the first MAX if MAX is numberp or can be converted by
`string-to-number', and number to upgrade is greater than MAX."
  (interactive "P")

  ;;------------------------------
  ;; 1st: Upgrade packages...
  ;;------------------------------
  (funcall #'innit:cmd:package:upgrade max-packages)

  ;;------------------------------
  ;; 2nd: Upgrade hook...
  ;;------------------------------
  (innit:hook:run 'innit:upgrade:hook))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :innit 'package 'upgrade 'command)
