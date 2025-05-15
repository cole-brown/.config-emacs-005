;;; core/modules/emacs/innit/optimize.el --- But is it speed? -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-04-27
;; Timestamp:  2023-06-22
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Optimize for... What?
;; Is it speed?
;;
;;; Code:


;; TODO:innit: move out of `innit' and into... idk...
;;   - `dotemacs'?
;;   - innit sub-modules?
;; TODO:innit: Maybe better: TODO:innit: Want all this both optional and obvious in the start-up code, so... how to do that?
;;  - Put it back in the start-up code?


;;------------------------------------------------------------------------------
;; Customize
;;------------------------------------------------------------------------------

;; TODO: defcustom?
;; TODO: move out of innit?
(defvar innit:display:messages? nil
  "Allow output to *Messages* buffer during init?

Default to no messages (nil).")


;; TODO: defcustom?
;; TODO: move out of innit?
(defvar innit:display:load-file nil
  "Allow `load-file' to output its message(s) during init?

Default to no (nil).")


;;------------------------------------------------------------------------------
;; Predicate / Guard
;;------------------------------------------------------------------------------

;; TODO: Does start up go faster when ~innit:optimized?~ is true?
(defun innit:optimize? ()
  "Guard predicate for optimizing interactive start-up.

Return false (i.e. do not optimize) if:
  - Emacs is in:
    - daemon (service) mode
    - noninteractive (batch/script) mode
  - Emacs or `innit' is in:
    - some sort of debug mode"
  (not (or (daemonp)
           noninteractive
           (innit:debug? :any))))


;;------------------------------------------------------------------------------
;; Display
;;------------------------------------------------------------------------------


(defun innit:optimize:display:inhibit ()
  "Speed up start-up a bit by inhibiting displaying things.

Inhibits:
  - Redrawing the display via `inhibit-redisplay'.
  - Writing to the '*Messages*' buffer and the minibuffer via `inhibit-message'.

Revert those changes in a hook on `window-setup-hook'."
  ;; Premature redisplays can substantially affect startup times and produce
  ;; ugly flashes of unstyled Emacs.
  (setq-default inhibit-redisplay t)

  (unless innit:display:messages?
    ;; Writing to the *Messages* buffer & minibuffer slows down startup as well.
    (setq-default inhibit-message t))

  (defun innit:optimize:display:permit ()
    "Re-enable display, messages."
    (setq-default inhibit-redisplay nil
                  inhibit-message nil)
    (redisplay))

  ;; Revert our changes once far enough along in start-up that redisplays and
  ;; messages are necessary.
  (add-hook 'window-setup-hook #'innit:optimize:display:permit))


;;------------------------------------------------------------------------------
;; File Name Handler
;;------------------------------------------------------------------------------

(defun innit:optimize:file-name-handler-alist:inhibit ()
  "Run start-up with empty `file-name-handler-alist' to speed up file loading.

Revert this by merging the original alist with anything added to the empty list
during start-up."
  (let ((file-name-handler-alist:orig file-name-handler-alist))
    ;; `file-name-handler-alist' is consulted on each `require', `load' and
    ;; various path/io functions. You get a minor speed up by unsetting this.
    ;; Some warning, however: this could cause problems on builds of Emacs where
    ;; its site lisp files aren't byte-compiled and we're forced to load the
    ;; *.el.gz files (e.g. on Alpine).
    (setq-default file-name-handler-alist nil)

    ;; ...but restore `file-name-handler-alist' later, because it is needed for
    ;; handling encrypted or compressed files, among other things.
    (defun innit:optimize:file-name-handler-alist:permit ()
      "Merge original `file-name-handler-alist' with the current alist."
      (setq file-name-handler-alist
            ;; Merge instead of overwrite because there may have bene changes to
            ;; `file-name-handler-alist' since startup we want to preserve.
            (delete-dups (append file-name-handler-alist
                                 file-name-handler-alist:orig))))
    (add-hook 'emacs-startup-hook
              #'innit:optimize:file-name-handler-alist:permit
              101)))


;;------------------------------------------------------------------------------
;; Load File Messages
;;------------------------------------------------------------------------------

;; TODO: Put this before the loading of `imp', `innit', etc so those don't get printed?
(defun innit:optimize:load-message:inhibit ()
  "Prevent `load-file' from printing messages during 'early-init.el'.

Revert when `startup--load-user-init-file' is called (i.e. before 'init.el')."
  (unless innit:display:load-file
    ;; Site files tend to use `load-file', which emits "Loading X..." messages in
    ;; the echo area, which in turn triggers a redisplay. Redisplays can have a
    ;; substantial effect on startup times and in this case happens so early that
    ;; Emacs may flash white while starting up.
    (define-advice load-file (:override (file) innit:mute)
      (load file nil 'nomessage))

    ;; Undo our `load-file' advice above, to limit the scope of any edge cases it
    ;; may introduce down the road.
    (define-advice startup--load-user-init-file (:before (&rest _) innit:unmute)
      ;; `M-x innit:cmd:advice:func/name' to create & insert name of func that `define-advice' created above.
      (advice-remove #'load-file #'load-file@innit-mute))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :innit 'optimize)
