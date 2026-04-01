;;; user/config/languages/lsp.el --- Serve Languages over a Protocol. -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-09-27
;; Timestamp:  2026-03-25
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Use Language Servers for auto-complete, docs, errors, warnings, etc.
;;
;; NOTE: This just sets up the LSP stuff itself. Each language needs to have a
;; hook or something for enabling `lsp-mode' in their buffers, and may need to
;; install the actual Language Server (e.g. OmniSharp for C#).
;;
;; https://microsoft.github.io/language-server-protocol/
;;
;; https://melpa.org/#/?q=lsp
;;
;;; Code:

(imp-require 'path)
(imp-require 'unit)


;;------------------------------------------------------------------------------
;; Language Server Protocol
;;------------------------------------------------------------------------------

;; https://github.com/emacs-lsp/lsp-mode
;; https://emacs-lsp.github.io/lsp-mode/page/installation/#use-package
;; https://emacs-lsp.github.io/lsp-mode/page/languages/
(use-package lsp-mode
  :after no-littering

  ;;------------------------------
  :commands
  ;;------------------------------
  (lsp-install-server
   lsp-update-server
   lsp-update-servers
   lsp
   lsp-deferred)

  ;;------------------------------
  :init
  ;;------------------------------

  ;;---
  ;; "Smarter" Shutdown
  ;;---
  (defvar --/lsp/defer-shutdown/sec 3
    "If non-nil, defer shutdown of LSP servers for this many seconds after last
workspace buffer is closed.

This delay prevents premature server shutdown when a user still intends on
working on that project after closing the last buffer, or when programmatically
killing and opening many LSP/eglot-powered buffers.

Borrowed from Doom.")

  (defvar --/lsp/defer-shutdown/timer nil
    "Timer used to defer LSP shutdown for `--/lsp/defer-shutdown/sec' seconds.")


  ;;---
  ;; Optimize Inter-Process Communication?
  ;;---

  (defvar --/lsp/ipc/cache/read-process-output-max nil
    "Alternative value for `read-process-output-max' for LSP use.")

  (defvar --/lsp/ipc/cache/gcmh-high-cons-threshold nil
    "Alternative value for `gcmh-high-cons-threshold' for LSP use.")


  (defvar --/lsp/ipc/optimization-mode/initialize? nil
    "Have the LSP/Emacs IPC optimizations been applied?")


  (define-minor-mode --/lsp/ipc/optimization-mode
    "Deploys _universal_ GC and IPC optimizations for `lsp-mode' and `eglot'.

Borrowed from Doom's `+lsp-optimization-mode' in \"modules/tools/lsp/config.el\"
TODO(lsp): check for changed: https://github.com/doomemacs/doomemacs/blob/master/modules/tools/lsp/config.el."
    :global t
    :init-value nil
    ;; TODO: Why are these all `setq-default'?
    ;;   Not Buffer-Local:
    ;;     - `read-process-output-max'
    ;;     - `gcmh-high-cons-threshold'
    ;;     - `--/lsp/ipc/optimization-mode/initialize?'
    ;;   Buffer-Local:
    ;;     - `--/lsp/ipc/optimization-mode'
    ;;
    ;; Not that it matters, since for non-buffer-local variables `setq-default'
    ;; is just `setq' and just sets the current value, but that's also very
    ;; different from its intended purpose for buffer-local vars, where it
    ;; _doesn't_ set the current value, just the value that _new_ buffers will
    ;; use.
    ;;
    ;; TODO: change to just using `setq'?
    (if (not --/lsp/ipc/optimization-mode)
        (progn
          ;;------------------------------
          ;; Disable: Revert Settings
          ;;------------------------------
          (setq-default read-process-output-max --/lsp/ipc/cache/read-process-output-max)
          (when (boundp 'gcmh-high-cons-threshold)
            (setq-default gcmh-high-cons-threshold --/lsp/ipc/cache/gcmh-high-cons-threshold))
          (setq-default --/lsp/ipc/optimization-mode/initialize? nil))

      ;;------------------------------
      ;; Enable
      ;;------------------------------
      ;; Only apply these settings once!
      (unless --/lsp/ipc/optimization-mode/initialize?
        ;;---
        ;; Save the existing defaults...
        ;;---
        (setq --/lsp/ipc/cache/read-process-output-max read-process-output-max)
        (when (boundp 'gcmh-high-cons-threshold)
          (setq --/lsp/ipc/cache/gcmh-high-cons-threshold gcmh-high-cons-threshold))

        ;;---
        ;; ...and give more wiggle room.
        ;;---
        (setq-default read-process-output-max (max read-process-output-max
                                                   (unit:byte 2 'mb)))
        (when (boundp 'gcmh-high-cons-threshold)
          ;; NOTE: LSP causes a lot of allocations, with or without the native
          ;; JSON library, so we up the GC threshold to stave off GC-induced
          ;; slowdowns/freezes. We use `gcmh' to enforce our GC strategy, so we
          ;; modify its variables rather than `gc-cons-threshold' directly.
          (setq-default gcmh-high-cons-threshold (max (default-value 'gcmh-high-cons-threshold)
                                                      (* 2 --/lsp/ipc/cache/gcmh-high-cons-threshold)))
          (gcmh-set-high-threshold))
        (setq --/lsp/ipc/optimization-mode/initialize? t))))


  ;;---
  ;; Modeline
  ;;---

  (defvar-local lsp-modeline-icon nil
    "LSP enabled/disabled icon for modeline.")


  (defun --/hook/lsp/modeline/update (&optional workspace)
    "Update modeline with LSP state."
    (let ((workspaces (lsp-workspaces)))
      (setq lsp-modeline-icon
            (concat
             " "
             (icon-material (if (or workspace workspaces)
                                "nf-md-rocket_launch"
                              "nf-md-rocket_launch_outline")
                            ""
                            :v-adjust  -0.0575
                            :face      (if (or workspace workspaces)
                                           'success
                                         'warning)
                            :help-echo (if (or workspace workspaces)
                                           "LSP Connected"
                                         "LSP Disconnected"))
             " "))
      (add-to-list 'global-mode-string
                   '(t (:eval lsp-modeline-icon))
                   'append)))
  ;; (--/hook/lsp/modeline/update)


  ;;---
  ;; Headerline
  ;;---

  (defun --/hook/lsp/header/breadcrumb ()
    "Set up `lsp-mode' to show some information in the header line."
    (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
    (lsp-headerline-breadcrumb-mode))


  ;;---
  ;; Hook for Enabling LSP in other `use-package' `:hook' sections.
  ;;---

  (defun --/hook/lsp/enable ()
    "Basic hook to enable `lsp-mode' for a buffer.

NOTE: Use this hook in languages' `use-package' blocks!
Example:
  (use-package csharp-mode
    :hook (csharp-mode-hook . --/hook/lsp/enable))"
    ;; Wait to start language server until a buffer is visible.
    (lsp-deferred)
    ;; Integrate with `which-key' for all major modes active in the buffer
    ;; (e.g. `web-mode' can have a few modes active).
    (lsp-enable-which-key-integration :all))


  ;;------------------------------
  :hook
  ;;------------------------------

  ((lsp-mode-hook . --/lsp/ipc/optimization-mode)
   ;; (lsp-mode-hook . --/hook/lsp/header/breadcrumb)
   ((lsp-before-initialize-hook
     lsp-after-initialize-hook
     lsp-after-uninitialized-functions
     lsp-before-open-hook
     lsp-after-open-hook)
    . --/hook/lsp/modeline/update))


  ;;------------------------------
  :custom
  ;;------------------------------

  ;; `lsp-keep-workspace-alive'
  ;;---------------------------
  ;; If non nil keep workspace alive when the last workspace buffer is closed.
  ;;
  ;; Don't let `lsp-mode' auto-kill LSP server after last workspace buffer is
  ;; killed, because we do it after `--/lsp/defer-shutdown/sec' seconds.
  (lsp-keep-workspace-alive nil)

  ;; `lsp-enable-folding'
  ;;---------------------
  ;; Enable textDocument/onTypeFormatting integration.
  ;;
  ;; Reduce unexpected modifications to code
  (lsp-enable-on-type-formatting nil)

  ;; `lsp-headerline-breadcrumb-enable'
  ;;---------------------
  ;; Whether to enable breadcrumb on headerline.
  ;;
  ;; Make breadcrumbs opt-in; they're redundant with the modeline and imenu
  (lsp-headerline-breadcrumb-enable nil)

  ;;------------------------------
  ;; Semantic Tokens
  ;;------------------------------
  ;; "With semantic token support you get more contextual information via
  ;; different faces."
  ;;   - https://emacs-lsp.github.io/lsp-mode/page/lsp-terraform-ls/#semantic-token-support
  ;; "Make sure to enable these two variables to ensure that you have semantic
  ;; token support for terraform mode."
  ;;  - https://emacs-lsp.github.io/lsp-mode/page/lsp-terraform-ls/#semantic-token-support

  ;; `lsp-semantic-tokens-enable'
  ;;-----------------------------
  ;; Enable/disable support for semantic tokens.
  ;; As defined by the Language Server Protocol 3.16.
  (lsp-semantic-tokens-enable t)

  ;; `lsp-semantic-tokens-honor-refresh-requests'
  ;;---------------------------------------------
  ;; Whether to honor semanticTokens/refresh requests.
  ;; When set to nil, refresh requests will be silently discarded.
  ;; When set to t, semantic tokens will be re-requested for all buffers
  ;; associated with the requesting language server.
  (lsp-semantic-tokens-honor-refresh-requests t)

  ;;------------------------------
  ;; Optimization
  ;;------------------------------
  ;; Disable features that have great potential to be slow.
  ;;
  ;; Change LSP's defaults in order to make its more expensive or imposing
  ;; features opt-in. Some servers implement these poorly and, in most cases,
  ;; it's safer to rely on Emacs' native mechanisms (eldoc vs lsp-ui-doc, open
  ;; in popup vs sideline, etc).

  ;; `lsp-enable-folding'
  ;;---------------------
  ;; Enable/disable code folding support.
  (lsp-enable-folding nil)

  ;; `lsp-enable-text-document-color'
  ;;---------------------
  ;; Enable textDocument/documentColor integration.
  (lsp-enable-text-document-color nil)

  ;;------------------------------
  ;; Keybinds
  ;;------------------------------

  ;; `lsp-keymap-prefix'
  ;;--------------------
  ;; LSP-mode keymap prefix.
  ;;
  ;; TODO(keybinds): Bind the lsp keymap ourselves?
  ;; (lsp-keymap-prefix nil) ; default: "s-l"

  ;; `lsp-enable-links'
  ;;-------------------
  ;; If non-nil, all references to links in a file will be made clickable, if
  ;; supported by the language server.
  ;;
  ;; default keybinds: M-RET & middle mouse click.
  ;; (lsp-enable-links t) ; default: t


  ;;------------------------------
  :config
  ;;------------------------------

  (define-advice lsp-diagnostics-flycheck-enable
      (:around (fn &rest args) --/advice/lsp/respect-user-defined-checker)
    "Ensure user-defined `flycheck-checker' isn't overwritten by `lsp'.

From Doom's `+lsp--respect-user-defined-checkers-a' in \"modules/tools/lsp/lsp.el\"."
    (if flycheck-checker
        (let ((old-checker flycheck-checker))
          (apply fn args)
          (setq-local flycheck-checker old-checker))
      (apply fn args)))


  (define-advice lsp--shutdown-workspace
      (:around (fn &optional restart) --/advice/lsp/defer-shutdown)
    "Defer server shutdown for a few seconds.

This gives the user a chance to open other project files before the server is
auto-killed (which is a potentially expensive process). It also prevents the
server getting expensively restarted when reverting buffers.

From Doom's `+lsp-defer-server-shutdown-a' in \"modules/tools/lsp/lsp.el\"."
    (if (or lsp-keep-workspace-alive
            restart
            (null --/lsp/defer-shutdown/sec)
            (= --/lsp/defer-shutdown/sec 0))
        ;; Do not defer shutdown for some reason, so cancel our optimizations minor mode.
        (prog1 (funcall fn restart)
          (--/lsp/ipc/optimization-mode -1))
      ;;------------------------------
      ;; Defer shutdown!
      ;;------------------------------
      ;; Cancel timer if running; will make another one.
      (when (timerp --/lsp/defer-shutdown/timer)
        (cancel-timer --/lsp/defer-shutdown/timer))
      ;; Make a timer for the actual shutdown.
      (setq --/lsp/defer-shutdown/timer
            (run-at-time
             ;; N seconds from now...
             (if (numberp --/lsp/defer-shutdown/sec)
                 --/lsp/defer-shutdown/sec
               3)
             ;; Never repeat.
             nil
             ;; Shutdown workspace lambda! Also cancel out of optimization mode!
             (lambda (workspace)
               (with-lsp-workspace workspace
                 (unless (lsp--workspace-buffers workspace)
                   (let ((lsp-restart 'ignore))
                     (funcall fn))
                   (--/lsp/ipc/optimization-mode -1))))
             lsp--cur-workspace)))))


  ;; TODO(refactor): U R HERE
;;------------------------------------------------------------------------------
;; `lsp-ui'
;;------------------------------------------------------------------------------

;; repo: https://github.com/emacs-lsp/lsp-ui
;; docs: https://emacs-lsp.github.io/lsp-ui/
(use-package lsp-ui
  :after lsp-mode

  ;; NOTE: `lsp-mode' automatically configures `lsp-ui' unless
  ;; `lsp-auto-configure' is set to nil.

  ;;------------------------------
  :custom
  ;;------------------------------

  ;;------------------------------
  ;; Sideline
  ;;------------------------------
  ;; https://emacs-lsp.github.io/lsp-ui/#lsp-ui-sideline

  ;; `lsp-ui-sideline-show-symbol'
  ;;-----------------------------
  ;; When t, show the symbol name on the right of the information.
  ;; (lsp-ui-sideline-show-hover t) ; default t

  ;; `lsp-ui-sideline-show-hover'
  ;;-----------------------------
  ;; Whether to show hover messages in sideline.
  (lsp-ui-sideline-show-hover t) ; default nil

  ;; `lsp-ui-sideline-show-diagnostics'
  ;;-----------------------------
  ;; Whether to show diagnostics messages in sideline.
  (lsp-ui-sideline-show-diagnostics t) ; default t

  ;; `lsp-ui-sideline-show-code-actions'
  ;;-----------------------------
  ;; Whether to show code actions in sideline.
  (lsp-ui-sideline-show-code-actions t) ; default nil

  ;; `lsp-ui-sideline-update-mode'
  ;;-----------------------------
  ;; Define the mode for updating sideline actions.
  ;;
  ;; When set to line the actions will be updated when user
  ;; changes current line otherwise the actions will be updated
  ;; when user changes current point.
  (lsp-ui-sideline-update-mode 'line) ; default `point'

  ;; `lsp-ui-sideline-delay'
  ;;-----------------------------
  ;; Number of seconds to wait before showing sideline.
  (lsp-ui-sideline-delay (unit:second 0.2 'sec)) ; default 0.2

  ;; `lsp-ui-sideline-diagnostic-max-lines'
  ;;-----------------------------
  ;; Maximum number of lines to show of diagnostics in sideline.
  ;;
  ;; Increase for more verbose messages, decrease if flickering occurs.
  ;; (lsp-ui-sideline-diagnostic-max-lines 1) ; default 1

  ;; `lsp-ui-sideline-ignore-duplicate'
  ;;-----------------------------
  ;; Ignore duplicates when there is a same symbol with the same contents.
  ;; (lsp-ui-sideline-ignore-duplicate t) ; default nil

  ;; `lsp-ui-sideline-actions-icon'
  ;;-----------------------------
  ;; Re-enable icon scaling (it's disabled by default upstream for Emacs
  ;; 26.x compatibility; see emacs-lsp/lsp-ui#573)
  (lsp-ui-sideline-actions-icon lsp-ui-sideline-actions-icon-default)

  ;;------------------------------
  ;; Peek
  ;;------------------------------
  ;; https://emacs-lsp.github.io/lsp-ui/#lsp-ui-peek

  ;; `lsp-ui-peek-enable'
  ;;-----------------------------
  ;; Whether or not to enable ‘lsp-ui-peek’.
  ;; (lsp-ui-peek-enable nil) ; default t

  ;; `lsp-ui-peek-show-directory'
  ;;-----------------------------
  ;; Whether or not to show the directory of files.
  (lsp-ui-peek-show-directory t) ; default t

  ;;------------------------------
  ;; Doc
  ;;------------------------------
  ;; https://emacs-lsp.github.io/lsp-ui/#lsp-ui-doc
  ;; TODO(lsp): make stuff for these
  ;; `lsp-ui-doc-enable' Enable lsp-ui-doc
  ;; `lsp-ui-doc-position' Where to display the doc (top, bottom or at-point)
  ;; `lsp-ui-doc-side' Where to display the doc (left or right)
  ;; `lsp-ui-doc-delay' Number of seconds before showing the doc
  ;; `lsp-ui-doc-show-with-cursor' When non-nil, move the cursor over a symbol to show the doc
  ;; `lsp-ui-doc-show-with-mouse' When non-nil, move the mouse pointer over a symbol to show the doc

  ;; TODO-lsp: Are these from Doom ok tweaks to the default?
  ;; (lsp-ui-doc-max-height            8)
  ;; (lsp-ui-doc-max-width             72)        ; 150 (default) is too wide
  ;; (lsp-ui-doc-delay                 0.75)          ; 0.2 (default) is too naggy
  ;; (lsp-ui-doc-show-with-mouse       nil) ; don't disappear on mouseover
  ;; (lsp-ui-doc-position              'at-point)
  )


;;------------------------------
;; Keybinds
;;------------------------------

;; TODO(keybinds): https://emacs-lsp.github.io/lsp-ui/#lsp-ui-peek
;; (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
;; (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)


;; TODO(keybinds): `lsp-ui' keybinds?
;; (imp:use-package lsp-ui
;;   :when  (imp:flag? :keybinds +meow)
;;   :after (:and lsp meow)
;;
;;   ;;------------------------------
;;   :custom
;;   ;;------------------------------
;;
;;   (map! :map lsp-ui-peek-mode-map
;;         "j"   #'lsp-ui-peek--select-next
;;         "k"   #'lsp-ui-peek--select-prev
;;         "C-k" #'lsp-ui-peek--select-prev-file
;;         "C-j" #'lsp-ui-peek--select-next-file))


;;------------------------------------------------------------------------------
;; `consult-lsp' - LSP integration with `consult'
;;------------------------------------------------------------------------------

;; https://github.com/gagbo/consult-lsp
(use-package consult-lsp
  :defer t
  :after (lsp consult)
  ;;------------------------------
  :bind
  ;;------------------------------
  (:map lsp-mode-map
   ([remap xref-find-apropos] . consult-lsp-symbols)))


;;------------------------------------------------------------------------------
;; `flycheck'
;;------------------------------------------------------------------------------
;; TODO(lsp): add flycheck

;; ;; https://github.com/flycheck/flycheck
;; (use-package flycheck
;;   :commands flycheck-list-errors flycheck-buffer
;;
;;   ;;------------------------------
;;   :custom
;;   ;;------------------------------
;;
;;   ;; Emacs Lisp syntax checker load path(s).
;;   ;; `inherit'       - Use Emacs' current `load-path'
;;   ;; list of strings - Add each dir to 'load-path' before invoking byte compiler.
;;   ;; nil             - Do not explicitly set `load-path'.
;;   (flycheck-emacs-lisp-load-path 'inherit)
;;
;;   ;; Don't recheck on idle as often; wait this many seconds (default: 0.5).
;;   (flycheck-idle-change-delay 1.0)
;;
;;   ;; Should we finish the syntax checking for buffers that get switched in and
;;   ;; out of focus faster than `flycheck-idle-buffer-switch-delay' seconds?
;;   (flycheck-buffer-switch-check-intermediate-buffers t)
;;
;;   ;; Display errors a little quicker (default is 0.9s)
;;   (flycheck-display-errors-delay 0.25)
;;
;;
;;   ;;------------------------------
;;   :config
;;   ;;------------------------------
;;
;;   ;; Rerunning checks on every newline is a mote excessive.
;;   (innit:customize-set-variable flycheck-check-syntax-automatically
;;                                 (delq 'new-line flycheck-check-syntax-automatically))
;;
;;   ;; TODO-lsp: How in the flying fuck to do this in vanilla without all of Doom's popup shit?
;;   ;;   - popup funcs: https://github.com/doomemacs/doomemacs/blob/master/modules/ui/popup/autoload/settings.el
;;   ;;
;;   ;; ;; Don't commandeer input focus if the error message pops up (happens when
;;   ;; ;; tooltips and childframes are disabled).
;;   ;; (set-popup-rules!
;;   ;;   '(("^\\*Flycheck error messages\\*" :select nil)
;;   ;;     ("^\\*Flycheck errors\\*" :size 0.25)))
;;   )
;;
;;
;; ;;------------------------------
;; ;; Keybinds : Meow
;; ;;------------------------------
;;
;; (imp:use-package flycheck
;;   :commands flycheck-list-errors flycheck-buffer
;;
;;   ;;------------------------------
;;   :init ; meow-specific hook
;;   ;;------------------------------
;;
;;   ;; This should be hooked into `meow-normal-mode-hook' or its `evil' equivalent.
;;   (innit:hook:defun
;;       (:name    'lsp/flycheck:syntax/trigger
;;        :docstr  "Trigger a syntax check on switch to normal mode."
;;        :squelch nil)
;;     (when (and flycheck-mode
;;                (not meow-normal-mode))
;;       (ignore-errors (flycheck-buffer))
;;       nil))
;;
;;   ;;------------------------------
;;   :hook
;;   ;;------------------------------
;;   (meow-normal-mode-hook . mantle:hook:lsp/flycheck:syntax/trigger)
;;
;;
;;   ;;------------------------------
;;   :config
;;   ;;------------------------------
;;
;;   ;; TODO: do we want/need to change the `flycheck-error-list-mode-map' keybinds?
;;   ;; Original:
;;   ;;   (defvar flycheck-error-list-mode-map
;;   ;;     (let ((map (make-sparse-keymap)))
;;   ;;       (define-key map (kbd "f") #'flycheck-error-list-set-filter)
;;   ;;       (define-key map (kbd "F") #'flycheck-error-list-reset-filter)
;;   ;;       (define-key map (kbd "n") #'flycheck-error-list-next-error)
;;   ;;       (define-key map (kbd "p") #'flycheck-error-list-previous-error)
;;   ;;       (define-key map (kbd "g") #'flycheck-error-list-check-source)
;;   ;;       (define-key map (kbd "e") #'flycheck-error-list-explain-error)
;;   ;;       (define-key map (kbd "RET") #'flycheck-error-list-goto-error)
;;   ;;       map)
;;   ;;     "The keymap of `flycheck-error-list-mode'.")
;;   ;;
;;   ;; Doom's tweaks for `evil':
;;   ;;   (map! :map flycheck-error-list-mode-map
;;   ;;         :n "C-n"    #'flycheck-error-list-next-error
;;   ;;         :n "C-p"    #'flycheck-error-list-previous-error
;;   ;;         :n "j"      #'flycheck-error-list-next-error
;;   ;;         :n "k"      #'flycheck-error-list-previous-error
;;   ;;         :n "RET"    #'flycheck-error-list-goto-error
;;   ;;         :n [return] #'flycheck-error-list-goto-error))
;;   )
;;
;;
;; ;;------------------------------------------------------------------------------
;; ;; `flycheck-popup-tip'
;; ;;------------------------------------------------------------------------------
;;
;; ;; https://github.com/flycheck/flycheck-popup-tip
;; (imp:use-package flycheck-popup-tip
;;   :after flycheck)
;;
;;
;; ;;------------------------------
;; ;; `meow' Tweaks
;; ;;------------------------------
;;
;; (imp:use-package flycheck-popup-tip
;;   :when  (imp:flag? :keybinds +meow)
;;   :after (:and flycheck meow)
;;
;;   ;;------------------------------
;;   :hook
;;   ;;------------------------------
;;
;;   ;; Don't display popups while in an insert mode.
;;   ;; Popups can affect the cursor's position or cause disruptive input delays.
;;   ;; Or, at least, in evil insert or replace mode they can? So treat the same in meow insert mode.
;;   ((meow-insert-enter-hook . flycheck-popup-tip-delete-popup)
;;    (flycheck-mode-hook . flycheck-popup-tip-mode))
;;
;;
;;   ;;------------------------------
;;   :config
;;   ;;------------------------------
;;
;;   (define-advice flycheck-popup-tip-show-popup (:before-while (fn &rest args) mantle:lsp/flycheck-popup-tip:no-popup-in-modal-insert)
;;     "Don't display popups while in an insert mode.
;;
;; Popups can affect the cursor's position or cause disruptive input delays.
;; Or, at least, in evil insert or replace mode the can?
;; So treat the same in meow insert mode."
;;     (and (bound-and-true-p meow-mode)
;;          (not (meow-insert-mode-p)))))
;;
;;
;; ;;------------------------------------------------------------------------------
;; ;; `flycheck-posframe'
;; ;;------------------------------------------------------------------------------
;;
;; (imp:use-package flycheck-posframe
;;   :hook (flycheck-mode . +syntax-init-popups-h)
;;
;;   ;; ;;------------------------------
;;   ;; :custom
;;   ;; ;;------------------------------
;;   ;;
;;   ;; (flycheck-posframe-info-prefix    "··· ")
;;   ;; (flycheck-posframe-warning-prefix "! ")
;;   ;; (flycheck-posframe-error-prefix   "X ")
;;
;;   ;;------------------------------
;;   :config
;;   ;;------------------------------
;;
;;   ;; Configure to show warnings and errors with nicer faces (derived from
;;   ;; `warning' and `error'). Set prefixes to pretty unicode characters.
;;   (flycheck-posframe-configure-pretty-defaults))
;;
;;
;; ;;------------------------------
;; ;; `meow' Tweaks
;; ;;------------------------------
;;
;; (imp:use-package flycheck-posframe
;;   :after meow
;;   :hook
;;   ;; Don't display popups while in insert or replace mode, as it can affect
;;   ;; the cursor's position or cause disruptive input delays.
;;   (flycheck-posframe-inhibit-functions . meow-insert-mode-p))
;;
;;
;; ;;------------------------------
;; ;; `company' Tweaks
;; ;;------------------------------
;; ;; Don't currently use `company', but just in case?
;; (imp:use-package flycheck-posframe
;;   :after company
;;   :hook
;;   ;; Don't display popups if company is open
;;   (flycheck-posframe-inhibit-functions . company--active-p))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide user (imp-path-relative 'user (imp-path-sans-extension (imp-path-current-file))))
