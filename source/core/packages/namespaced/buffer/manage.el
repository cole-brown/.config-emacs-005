;;; namespaced/buffer/manage.el --- Buffer Management -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-02-24
;; Timestamp:  2025-11-17
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Buffer Management:
;;   - Nicer kill buffer functions
;;   - Insert/overwrite helpers
;;
;;; Code:


(require 'cl-lib)


(imp-require elisp:/functions)


;;------------------------------------------------------------------------------
;; Kill Functions
;;------------------------------------------------------------------------------
;; See `:chrome:window' for similar functions.
;;   git-root://core/modules/emacs/window/manage.el

;; Like `kill-buffer-ask' but no confirmation for unmodified buffers.
(defun buffer:kill:ask (buffer-or-name &optional delete-process)
  "Kill BUFFER-OR-NAME if confirmed.

No confirm given for unmodified buffers; just kill.

If DELETE-PROCESS is non-nil, also delete buffer's processes.

Return `buffer-name' of buffer on kill, nil on no kill."
  (let ((buffer (get-buffer buffer-or-name)))
    ;; so... kill?
    (if (or
         ;; just kill it if not modified
         (not (buffer-modified-p buffer))
         ;; or ask first if modded
         (yes-or-no-p (format "Buffer '%s' HAS BEEN EDITED.  Kill? "
                              (buffer-name buffer))))
        ;; ok - kill
        (prog1
            ;; Return name when killed.
            (buffer-name buffer)
          (when delete-process (buffer:process:delete buffer))
          (kill-buffer buffer))

      ;; Return nil when nothing killed.
      nil)))


(defun buffer:kill:list (&rest buffers)
  "Kill BUFFERS via `buffer:kill:ask'.

BUFFERS will be flattened first, so that callers can provide a list if desired.

Return list of return values from `buffer:kill:ask'.
  - That is, return list of:
    - killed buffer name strings (success)
    - nil (failure)"
  (seq-map #'buffer:kill:ask (elisp:list:flatten buffers)))


(defun buffer:kill:special (arg)
  "Kill special(ly named) buffers and deletes any process they may have running.

If ARG is the keyword `:regex', use `buffer:regex/bookend' to kill
special buffers.

If ARG is a string, the `buffer:regex/bookend' is checked to see if the
stcring/regexp is 'correctly' guarded by them, adding them in if needed. It uses
`buffer:special-name' with nil priority to add the bookends.

If ARG is not a string, assume it's a buffer and try to kill it's process and it
directly."
  (cond ((null arg)
         (user-error "buffer:kill:special: Cannot kill; null arg: %S" arg))

        ((and (keywordp arg)
              (eq arg :regex))
         ;; Use our regex to try to kill them all without confirmation.
           (buffer:kill:matching buffer:regex/bookend nil t t t))

        ((stringp arg)
         ;; We have a string. Make sure it's formatted as "special-buffer",
         ;; and then try to kill any matching without confirmation.
         (let ((arg (if (string-match-p buffer:regex/bookend arg)
                        arg
                      (buffer:special-name arg))))
           (buffer:kill:matching arg nil t t t)))

        (t
         ;; Else we have a buffer, probably? Go for the kill ourselves.
         (buffer:process:delete arg)
         (kill-buffer arg))))
;; (buffer:kill:special :regex)
;; (buffer:kill:special (rx word-boundary (1+ printing) word-boundary))
;; (buffer:kill:special "\\b[[:print:]]+\\b")
;; (buffer:special-name "\\b[[:print:]]+\\b")
;; (buffer:kill:special "§- \\b[[:print:]]+\\b -§")


(defun buffer:process:delete (buffer-or-name)
  "Get buffer, get buffer's process (if any), and delete/kill that process.

BUFFER-OR-NAME must be exact."
  (let ((proc (get-buffer-process (get-buffer buffer-or-name))))
    (if (not (process-live-p proc))
        (message "No live process in '%s'?" buffer-or-name)
      (delete-process proc))))


;;------------------------------------------------------------------------------
;; Better Kill-Matching-Buffer
;;------------------------------------------------------------------------------

;; §-TODO-§ [2020-02-13]: A "bury-matching in all windows" like
;; `buffer:kill:matching'?

(defun buffer:kill:matching (regex &rest keywords)
  "Kill buffers whose name matches the specified REGEX.

KEYWORDS should be nil or keywords. Valid KEYWORDS are:
  - `:internal'
    - Don't ignore buffers whose name starts with a space (internal buffers).
      - Default is to ignore them.
  - `:modified'
    - Kill modified buffers without confirmation.
      - Default is to ask for confirmation for each modified buffer.
  - `:process'
    - Kill buffers with attached/associated processes.
      - Default is to ask for confirmation for each buffer with a process.
  - `:quiet'
    - Do not output the informative messages."
  ;; What KEYWORD flags were set?
  (let ((kill/internal? (memq :internal keywords))
        (kill/modified? (memq :modified keywords))
        (kill/process?  (memq :process  keywords))
        (quiet?         (memq :quiet    keywords))
        ;; List: "buffer name matches regex".
        buffer:matches
        ;; List: "actually killed buffer name".
        buffer:killed
        ;; If not `quiet?', a progress reporter will be created.
        progress-reporter)

    ;;------------------------------
    ;; Regex Match Buffer Names
    ;;------------------------------
    ;; Basically:
    ;;   - Find matching buffers first.
    ;;   - Ask w/ summary if desired.
    ;;   - Kill w/ progress reporter.
    (dolist (buffer (buffer-list))
      (let ((name (buffer-name buffer)))
        (when (and (stringp name)
                   (not (string-equal name ""))
                   ;; Leading space means "internal buffer" - check name obeys `kill/internal?' type restriction.
                   (or kill/internal? (/= (aref name 0) ?\s))
                   ;; ...and matches regex.
                   (string-match regex name))
          (push name buffer:matches))))

    ;;------------------------------
    ;; TODO: Confirmation?
    ;;------------------------------
    ;; TODO: Find all matching buffers and do a "Does this look about right?" prompt?

    (if (not buffer:matches)
        ;;------------------------------
        ;; No matches - inform!
        ;;------------------------------
        (unless quiet?
          (message "Nothing killed; no buffers matching '%s'."
                   regex))

      ;;------------------------------
      ;; Kill!
      ;;------------------------------
      ;; But first, set up progress reporter?
      (unless quiet?
        (setq progress-reporter
              (make-progress-reporter (format "Killing all '%s' buffers..."
                                              regex)
                                      0
                                      (length buffer:matches))))

      ;; Annnd... kill buffers!
      (let ((count 0))
        (dolist (name buffer:matches)
          (setq count (1+ count))
          (if (not kill/modified?)
              ;; Ask if we should kill it if modifed, else just kill it.
              (when-let ((maybe-kill-name (buffer:kill:ask name
                                                               kill/process?)))
                (push maybe-kill-name buffer:killed))

            ;; Just kill it.
            (when kill/process?
              (buffer:process:delete name))
            (kill-buffer name)
            (push name buffer:killed))

          ;; Update progress.
          (unless quiet?
            (progress-reporter-update progress-reporter count))))

      ;;------------------------------
      ;; Final output.
      ;;------------------------------
      (unless quiet?
        (progress-reporter-done progress-reporter)

        ;; And finally, give some goddamn output (looking at you, kill-matching-buffers).
        (let ((num/killed (length buffer:killed)))
          (cond
           ((= 0 num/killed)
            (message "No buffers killed matching '%s'."
                     regex))
           ((= 1 num/killed)
            (message "Killed %s buffer matching '%s': %s"
                     num/killed
                     regex
                     (nth 0 buffer:killed)))
           ;; ;; 10 or more killed: don't list?
           ;; ((< 10 num/killed)
           ;;  (message "Killed %s buffers matching '%s'."
           ;;           num/killed
           ;;           regex))
           (t
            (message "Killed %s buffers matching '%s':\n%s"
                     (length buffer:killed)
                     regex
                     ;; List of buffer names.
                     (mapconcat (lambda (name) (format "  - %s" name))
                                buffer:killed
                                "\n"))))))

      ;; Return number of buffers killed.
      (length buffer:killed))))


(defun buffer:cmd:kill:matching (regex)
  "Kill buffers whose name matches the specified REGEX.

If you want to control what gets killed & how, see `buffer:kill:matching'
for kewords like:
  - `:internal'
    - Don't ignore buffers whose name starts with a space (internal buffers).
      - Default is to ignore them.
  - `:modified'
    - Kill modified buffers without confirmation.
      - Default is to ask for confirmation for each modified buffer.
  - `:process'
    - Kill buffers with attached/associated processes.
      - Default is to ask for confirmation for each buffer with a process.
  - `:quiet'
    - Do not output the informative messages."
  (interactive "sKill buffers matching regex: ")
  (buffer:kill:matching regex))


;;;###autoload
(defun buffer:cmd:kill (buffer &optional dont-save)
  "Kill BUFFER globally.

Ensure all windows previously showing this buffer have switched to a real buffer
or the fallback buffer.

If DONT-SAVE, don't prompt to save modified buffers (discarding their changes).

On loan from Doom's \"core/autoload/buffers.el\"."
  (interactive (list (current-buffer)
                     current-prefix-arg))

  (cl-assert (bufferp buffer) t)

  ;; Clear "buffer is modified" flag if we don't care about saving?
  (when (and (buffer-modified-p buffer) dont-save)
    (with-current-buffer buffer
      (set-buffer-modified-p nil)))

  ;; Will ask if they want to delete a modified buffer.
  (buffer:kill:ask buffer
                   :delete-process))


;;------------------------------------------------------------------------------
;; Overwrite Mode
;;------------------------------------------------------------------------------

(defun buffer:overwriting? ()
  "Is buffer in overwrite mode?

Evil-Aware / Evil-Optional: Is buffer in `evil-replace-state'?
Doesn't need to be Meow-Aware / Meow-Optional."
  ;; Are you evil? Use `evil' functions, evil-doer.
  (if (bound-and-true-p evil-mode)
      (evil-replace-state-p)
    (not (null overwrite-mode))))


(defun buffer:overwrite:toggle ()
  "Toggle overwrite mode.

Evil-Aware / Evil-Optional
Doesn't need to be Meow-Aware / Meow-Optional

Evil: Toggles between 'insert' and 'replace' evil states.
Good: Toggles between 'insert' and 'overwrite'."
  (interactive)
  ;; Are you evil? Use `evil' functions, evil-doer.
  (if (bound-and-true-p evil-mode)
      (if (evil-replace-state-p)
          (evil-append 0)
        (evil-replace-state))

    ;; `meow': Just use standard overwrite functions, cat.
    ;; Vanilla Emacs: Carry on.
    (overwrite-mode)))


(defun buffer:insert-or-overwrite (character)
  "Insert or overwrite CHARACTER into active buffer at point.

Evil-Aware / Evil-Optional
Meow-Aware / Meow-Optional

Need to fix the hydra's deleting before figuring out the integration into
evil's replace state backspace 'undo' functionality."
  ;;------------------------------
  ;; Error Checks
  ;;------------------------------
  (unless (or (characterp character)
              (stringp character))
    (error "%s: CHARACTER must be a character or string. Got %S: %S"
           "buffer:insert-or-overwrite: "
           (type-of character)
           character))

  ;;------------------------------
  ;; Overwriting?
  ;;------------------------------
  ;; First delete a character at point so we end up "replacing" it.

  ;; Evil has its own special overwrite mode called the "replace" state.
  (if (and (bound-and-true-p evil-mode)
           (evil-replace-state-p))
      (progn
      ;; TODO-evil: Will this let evil's backspace/delete 'undo' functionality work?
      ;; (evil-replace-pre-command)

      ;; TODO-evil: Will this work on its own to allow evil's 'undo' functionality?
      (evil-delete-char (point) (1+ (point))))

    ;; Meow just uses vanilla Emacs' `overwrite-mode'.
    (when overwrite-mode
      (delete-char  (point) (1+ (point)))))

  ;;------------------------------
  ;; Now we can insert the new char.
  ;;------------------------------
  (insert character))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide buffer manage)
