;;; source/user/config/completion.el --- Ivy, Helm, SMOCE, or... -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-07-20
;; Timestamp:  2025-10-28
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Ivy, Helm, SMOCE, or...
;;    VMOCE?
;;
;;
;; [2022-07-20] "SMOCE" Stack:
;;   There's the standards like `ivy' and `helm', but I want to try newer next gen stuff.
;;   Like this:
;;     https://www.reddit.com/r/emacs/comments/ppg98f/comment/hd3hll1/?utm_source=share&utm_medium=web2x&context=3
;;   From:
;;     https://www.reddit.com/r/emacs/comments/ppg98f/which_completion_framework_do_you_use_and_why/
;;
;;   So... Introducing the "SMOCE Stack":
;;     - https://github.com/radian-software/selectrum#complementary-extensions
;;     - Selectrum:       https://github.com/raxod502/selectrum
;;       - (or Vertico?): https://github.com/minad/vertico
;;     - Marginalia:      https://github.com/minad/marginalia
;;     - Orderless:       https://github.com/oantolin/orderless
;;     - Consult:         https://github.com/minad/consult
;;     - Embark:          https://github.com/oantolin/embark
;;
;;
;; [2023-01-17] Deprecate Selectrum:
;;   Selectrum's maintainer(s) decided Vertico is better (same features, simpler
;;   code); instruct everyone to switch to Vertico.
;;     - https://github.com/radian-software/selectrum#selectrum-is-replaced
;;
;;   So... I guess "SMOCE" is now... "VMOCE"?
;;   "VMOCE Stack" doesn't have quite the ring to it compared to "SMOCE Stack"...
;;
;;   Migration Guide:
;;     https://github.com/minad/vertico/wiki/Migrating-from-Selectrum-to-Vertico
;;
;; [2025-06-02] "VMOCE" Stack:
;;   - Vertico:    https://github.com/minad/vertico
;;   - Marginalia: https://github.com/minad/marginalia
;;   - Orderless:  https://github.com/oantolin/orderless
;;   - Consult:    https://github.com/minad/consult
;;   - Embark:     https://github.com/oantolin/embark
;;
;;; Code:


;;------------------------------------------------------------------------------
;; "VMOCE Stack": Vertico & Friends
;;------------------------------------------------------------------------------
;; There's the standards like `ivy' and `helm', but I want to try newer next gen stuff.
;; Like this:
;;   https://www.reddit.com/r/emacs/comments/ppg98f/comment/hd3hll1/?utm_source=share&utm_medium=web2x&context=3
;; From:
;;   https://www.reddit.com/r/emacs/comments/ppg98f/which_completion_framework_do_you_use_and_why/
;;
;; So... Introducing the "VMOCE Stack"):
;;   - Vertico:    https://github.com/minad/vertico
;;   - Marginalia: https://github.com/minad/marginalia
;;   - Orderless:  https://github.com/oantolin/orderless
;;   - Consult:    https://github.com/minad/consult
;;   - Embark:     https://github.com/oantolin/embark


;;------------------------------------------------------------------------------
;; Vertico: VERTical Interactive COmpletion
;;------------------------------------------------------------------------------
;; https://github.com/minad/vertico

;;------------------------------
;; Vertico
;;------------------------------

;; TODO: Finish README? https://github.com/minad/vertico#completion-styles-and-tab-completion
(use-package vertico
  ;;------------------------------
  :custom
  ;;------------------------------
  ;; https://github.com/minad/vertico/blob/main/vertico.el
  ;;   Search for: (defcustom

  ;; Show more candidates (default 10).
  (vertico-count 20)

  ;; Different scroll margin (default 2).
  ;; (vertico-scroll-margin 0)

  ;; Grow and shrink the Vertico minibuffer.
  ;;   - options: t, nil, `grow-only'
  ;;   - default: `resize-mini-windows' (?!)
  ;; See `resize-mini-windows' for details.
  ;; (vertico-resize t)

  ;; Enable cycling for `vertico-next' and `vertico-previous' (default nil).
  ;; (vertico-cycle t)


  ;;------------------------------
  :config
  ;;------------------------------
  (vertico-mode +1))

;; NOTE: keybinds?
;;   - https://github.com/minad/vertico#key-bindings
;;   - See `C-h v vertico-map' for keybinds, or:
;;     - https://github.com/minad/vertico/blob/main/vertico.el


;;------------------------------
;; Vertico w/ Prescient?
;;------------------------------
;; TODO: Do I want this:
;;   > "In some cases you may want to consider to use Prescient on top of
;;   > Orderless. Prescient can be used to provide frecency-based sorting (a
;;   > combination of frequency and recency) and history persistence by adding
;;   > the following."
;; TODO: If so: https://github.com/minad/vertico/wiki#using-prescientel-filtering-and-sorting


;;------------------------------
;; Save History
;;------------------------------
;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure nil ; This is an Emacs built-in feature.

  ;;------------------------------
  :init
  ;;------------------------------
  (savehist-mode +1))


;;------------------------------------------------------------------------------
;; Marginalia: Notes & Info in the Minibuffer Margin
;;------------------------------------------------------------------------------
;; https://github.com/minad/marginalia

(use-package marginalia
  ;; The :init configuration is always executed (Not lazy!)

  ;;------------------------------
  :custom
  ;;------------------------------
  (marginalia-field-width 310)

  ;;------------------------------
  :init
  ;;------------------------------

  ;;------------------------------
  ;; Field Width Fixes
  ;;------------------------------

 ;;  (define-advice marginalia--affixate (:around (fn &rest args) mantle:advice:fix-max-width)
;;     "Fix Marginalia to allow correct max width of its fields.

;; As of v20230217.2050, Marginalia incorrect assumes that its width is limited by
;; windows. It is not. It is a minibuffer thing. It's limited by the width of the
;; frame.

;; FN should be `marginalia--affixate'. We do not modify its args, so ARGS are just
;; passed along to it as-is."
;;     ;; Use `frame-width' instead of `window-width'?
;;     ;; TODO:marginalia: This is a bug anyways, though, as it gets the max of all
;;     ;; windows(/frames). Really you ought to just get the width of THE GOD DAMN
;;     ;; THING YOU'RE DISPLAYING IN!!!
;;     (cl-letf (((symbol-function 'window-width) (lambda (&optional _window _pixelwise)
;;                                                  "Ignore input params and return frame width in characters."
;;                                                  (frame-width))))
;;       ;; (window-width)
;;       (apply fn args)))

;;   (define-advice marginalia-annotate-buffer (:override (cand &rest args) mantle:advice:bigger-filename)
;;     "Fix Marginalia's buffer annotations to have less wasted empty space.

;; NOTE: This is compounded by the `marginalia--affixate' bug above, so beware of
;; the interplay...

;; As of v20230217.2050, Marginalia wants to truncate the buffer filename to `-0.5'
;; aka 50%, right justified/truncated. And that's 50% _of the remaining half_!!!
;; `marginalia--annotate' automatically divides its max width by 2 so that I assume
;; the main thingy gets at least half of the minibuffer?

;; So it truncates filenames to 25% of the width, which is super useful as that's
;; almost enough to display the filename and one or two parent directories, most of
;; the time... :eyeroll:

;; Be greedier. Lots of shit truncates to 100% aka 1.0."
;;     (when-let (buffer (get-buffer cand))
;;       (marginalia--fields
;;        ((marginalia--buffer-status buffer))
;;        ((marginalia--buffer-file buffer)
;;         :truncate 0.94 :face 'marginalia-file-name))))
;;   ;; (advice-remove 'marginalia-annotate-buffer 'marginalia-annotate-buffer@mantle:advice:bigger-filename)


  ;;------------------------------
  ;; Enable!
  ;;------------------------------

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode +1))

;; ;;------------------------------
;; ;; TODO: Keybinds : Emacs
;; ;;------------------------------
;;
;; (use-package marginalia
;;   ;;------------------------------
;;   :bind ; emacs
;;   ;;------------------------------
;;   ;; TODO: What keybind to give this? What does it do?
;;   (("M-A" . marginalia-cycle)
;;    :map minibuffer-local-map
;;    ("M-A" . marginalia-cycle)))
;;
;; ;; TODO-evil: Evil keybinds for `marginalia-cycle'?
;; ;; TODO-meow: Meow keybinds for `marginalia-cycle'?


;;------------------------------------------------------------------------------
;; Orderless: Orderless, Space-Separated Completion Style
;;------------------------------------------------------------------------------
;; https://github.com/oantolin/orderless

(use-package orderless

  ;;------------------------------
  :custom
  ;;------------------------------
  ;; > The `basic' completion style is specified as fallback in addition to
  ;; > `orderless' in order to ensure that completion commands which rely on
  ;; > dynamic completion tables, e.g., `completion-table-dynamic' or
  ;; > `completion-table-in-turn', work correctly. Furthermore the `basic'
  ;; > completion style needs to be tried /first/ (not as a fallback) for
  ;; > TRAMP hostname completion to work. In order to achieve that, we add an
  ;; > entry for the `file' completion category in the
  ;; > `completion-category-overrides' variable. In addition, the
  ;; > `partial-completion' style allows you to use wildcards for file
  ;; > completion and partial paths, e.g., '/u/s/l' for '/usr/share/local'.
  ;;   - https://github.com/oantolin/orderless?tab=readme-ov-file#overview
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))


;;------------------------------------------------------------------------------
;; Consult: Consulting `completing-read'
;;------------------------------------------------------------------------------
;; https://github.com/minad/consult
;;
;; Available Commands: https://github.com/minad/consult#available-commands

(use-package consult
  ;;------------------------------
  :init
  ;;------------------------------
  ;; Non-lazy things that need to happen?
  ;;   - See: https://github.com/minad/consult#use-package-example
  ;;     - Moved the setting of custom vars to `:custom'.

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)


  ;;------------------------------
  :hook
  ;;------------------------------
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  (completion-list-mode-hook . consult-preview-at-point-mode)


  ;;------------------------------
  :custom
  ;;------------------------------

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (register-preview-delay    0.5)
  (register-preview-function #'consult-register-format)

  ;; Use Consult to select xref locations with preview.
  (xref-show-xrefs-function       #'consult-xref)
  (xref-show-definitions-function #'consult-xref)


  ;;------------------------------
  :config
  ;;------------------------------

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")
  )

;; TODO: Consult packages for integrating with other packages?
;; https://github.com/minad/consult#recommended-packages


;;------------------------------
;; Keybinds : Vanilla Emacs
;;------------------------------

(use-package consult
  ;; TODO: something like this?
  ;; :unless  (or (imp-flag? :keybinds +meow)
  ;;              (imp-flag? :keybinds +evil))

  ;;------------------------------
  :bind ; Emacs
  ;;------------------------------
  ;; TODO: Replace this `:bind' section with a `:general' section.
  ;; Replace bindings. Lazily loaded due by `use-package'.
  (;; C-c bindings (mode-specific-map)
   ("C-c h" . consult-history)
   ("C-c m" . consult-mode-command)
   ("C-c k" . consult-kmacro)
   ;; C-x bindings (ctl-x-map)
   ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
   ("C-x b"   . consult-buffer)                ;; orig. switch-to-buffer
   ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
   ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
   ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
   ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
   ;; Custom M-# bindings for fast register access
   ("M-#"   . consult-register-load)
   ("M-'"   . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
   ("C-M-#" . consult-register)
   ;; Other custom bindings
   ("M-y"      . consult-yank-pop)                ;; orig. yank-pop
   ("<help> a" . consult-apropos)            ;; orig. apropos-command
   ;; M-g bindings (goto-map)
   ("M-g e"   . consult-compile-error)
   ("M-g f"   . consult-flymake)               ;; Alternative: consult-flycheck
   ("M-g g"   . consult-goto-line)             ;; orig. goto-line
   ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
   ("M-g o"   . consult-outline)               ;; Alternative: consult-org-heading
   ("M-g m"   . consult-mark)
   ("M-g k"   . consult-global-mark)
   ("M-g i"   . consult-imenu)
   ("M-g I"   . consult-imenu-multi)
   ;; M-s bindings (search-map)
   ("M-s d" . consult-find)
   ("M-s D" . consult-locate)
   ("M-s g" . consult-grep)
   ("M-s G" . consult-git-grep)
   ("M-s r" . consult-ripgrep)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)
   ("M-s m" . consult-multi-occur)
   ("M-s k" . consult-keep-lines)
   ("M-s u" . consult-focus-lines)
   ;; Isearch integration
   ("M-s e" . consult-isearch-history)
   :map isearch-mode-map
   ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
   ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
   ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
   ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
   ;; Minibuffer history
   :map minibuffer-local-map
   ("M-s" . consult-history)                 ;; orig. next-matching-history-element
   ("M-r" . consult-history))                ;; orig. previous-matching-history-element


  ;;------------------------------
  :config
  ;;------------------------------

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))

  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)

   consult-ripgrep
   consult-git-grep
   consult-grep
   consult-bookmark
   consult-recent-file
   consult-xref
   consult--source-bookmark
   consult--source-recent-file
   consult--source-project-recent-file
   :preview-key "M-.")

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  ;; TODO: EVIL???
  ;; TODO-meow: MEOW???
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)
  )


;; ;;------------------------------
;; ;; Keybinds : meow
;; ;;------------------------------
;;
;; (use-package consult
;;   :when  (imp-flag? :keybinds +meow)
;;   :after meow
;;
;;   ;; TODO-meow: Meow leader keybinds for some of these functions.
;;
;;   ;;------------------------------
;;   :bind ; still do all these vanilla Emacs binds
;;   ;;------------------------------
;;   ;; TODO: Replace this `:bind' section with a `:general' section.
;;   ;; Replace bindings. Lazily loaded due by `use-package'.
;;   (;; C-c bindings (mode-specific-map)
;;    ("C-c h" . consult-history)
;;    ("C-c m" . consult-mode-command)
;;    ("C-c k" . consult-kmacro)
;;    ;; C-x bindings (ctl-x-map)
;;    ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
;;    ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
;;    ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
;;    ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
;;    ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
;;    ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
;;    ;; Custom M-# bindings for fast register access
;;    ("M-#" . consult-register-load)
;;    ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
;;    ("C-M-#" . consult-register)
;;    ;; Other custom bindings
;;    ("M-y" . consult-yank-pop)                ;; orig. yank-pop
;;    ("<help> a" . consult-apropos)            ;; orig. apropos-command
;;    ;; M-g bindings (goto-map)
;;    ("M-g e" . consult-compile-error)
;;    ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
;;    ("M-g g" . consult-goto-line)             ;; orig. goto-line
;;    ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
;;    ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
;;    ("M-g m" . consult-mark)
;;    ("M-g k" . consult-global-mark)
;;    ("M-g i" . consult-imenu)
;;    ("M-g I" . consult-imenu-multi)
;;    ;; M-s bindings (search-map)
;;    ("M-s d" . consult-find)
;;    ("M-s D" . consult-locate)
;;    ("M-s g" . consult-grep)
;;    ("M-s G" . consult-git-grep)
;;    ("M-s r" . consult-ripgrep)
;;    ("M-s l" . consult-line)
;;    ("M-s L" . consult-line-multi)
;;    ("M-s m" . consult-multi-occur)
;;    ("M-s k" . consult-keep-lines)
;;    ("M-s u" . consult-focus-lines)
;;    ;; Isearch integration
;;    ("M-s e" . consult-isearch-history)
;;    :map isearch-mode-map
;;    ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
;;    ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
;;    ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
;;    ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
;;    ;; Minibuffer history
;;    :map minibuffer-local-map
;;    ("M-s" . consult-history)                 ;; orig. next-matching-history-element
;;    ("M-r" . consult-history))                ;; orig. previous-matching-history-element
;;
;;
;;   ;;------------------------------
;;   :config
;;   ;;------------------------------
;;
;;   ;; Optionally configure preview. The default value
;;   ;; is 'any, such that any key triggers the preview.
;;   ;; (setq consult-preview-key 'any)
;;   ;; (setq consult-preview-key "M-.")
;;   ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
;;
;;   ;; For some commands and buffer sources it is useful to configure the
;;   ;; :preview-key on a per-command basis using the `consult-customize' macro.
;;   (consult-customize
;;    consult-theme
;;    :preview-key '(:debounce 0.2 any)
;;
;;    consult-ripgrep
;;    consult-git-grep
;;    consult-grep
;;    consult-bookmark
;;    consult-recent-file
;;    consult-xref
;;    consult--source-bookmark
;;    consult--source-recent-file
;;    consult--source-project-recent-file
;;    :preview-key "M-.")
;;
;;   ;; Optionally make narrowing help available in the minibuffer.
;;   ;; You may want to use `embark-prefix-help-command' or which-key instead.
;;   ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)
;;   )
;;
;;
;; ;;------------------------------
;; ;; Keybinds : Evil
;; ;;------------------------------
;;
;; (use-package consult
;;   :when  (imp-flag? :keybinds +evil)
;;   :after (:and evil evil-collection)
;;
;;   ;;------------------------------
;;   :bind ; still do all these vanilla Emacs binds
;;   ;;------------------------------
;;   ;; Replace bindings. Lazily loaded due by `use-package'.
;;   (;; C-c bindings (mode-specific-map)
;;    ("C-c h" . consult-history)
;;    ("C-c m" . consult-mode-command)
;;    ("C-c k" . consult-kmacro)
;;    ;; C-x bindings (ctl-x-map)
;;    ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
;;    ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
;;    ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
;;    ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
;;    ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
;;    ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
;;    ;; Custom M-# bindings for fast register access
;;    ("M-#" . consult-register-load)
;;    ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
;;    ("C-M-#" . consult-register)
;;    ;; Other custom bindings
;;    ("M-y" . consult-yank-pop)                ;; orig. yank-pop
;;    ("<help> a" . consult-apropos)            ;; orig. apropos-command
;;    ;; M-g bindings (goto-map)
;;    ("M-g e" . consult-compile-error)
;;    ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
;;    ("M-g g" . consult-goto-line)             ;; orig. goto-line
;;    ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
;;    ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
;;    ("M-g m" . consult-mark)
;;    ("M-g k" . consult-global-mark)
;;    ("M-g i" . consult-imenu)
;;    ("M-g I" . consult-imenu-multi)
;;    ;; M-s bindings (search-map)
;;    ("M-s d" . consult-find)
;;    ("M-s D" . consult-locate)
;;    ("M-s g" . consult-grep)
;;    ("M-s G" . consult-git-grep)
;;    ("M-s r" . consult-ripgrep)
;;    ("M-s l" . consult-line)
;;    ("M-s L" . consult-line-multi)
;;    ("M-s m" . consult-multi-occur)
;;    ("M-s k" . consult-keep-lines)
;;    ("M-s u" . consult-focus-lines)
;;    ;; Isearch integration
;;    ("M-s e" . consult-isearch-history)
;;    :map isearch-mode-map
;;    ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
;;    ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
;;    ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
;;    ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
;;    ;; Minibuffer history
;;    :map minibuffer-local-map
;;    ("M-s" . consult-history)                 ;; orig. next-matching-history-element
;;    ("M-r" . consult-history))                ;; orig. previous-matching-history-element
;;
;;
;;   ;;------------------------------
;;   :config
;;   ;;------------------------------
;;
;;   ;; Optionally configure preview. The default value
;;   ;; is 'any, such that any key triggers the preview.
;;   ;; (setq consult-preview-key 'any)
;;   ;; (setq consult-preview-key "M-.")
;;   ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
;;
;;   ;; For some commands and buffer sources it is useful to configure the
;;   ;; :preview-key on a per-command basis using the `consult-customize' macro.
;;   (consult-customize
;;    consult-theme
;;    :preview-key '(:debounce 0.2 any)
;;
;;    consult-ripgrep
;;    consult-git-grep
;;    consult-grep
;;    consult-bookmark
;;    consult-recent-file
;;    consult-xref
;;    consult--source-bookmark
;;    consult--source-recent-file
;;    consult--source-project-recent-file
;;    :preview-key "M-.")
;;
;;   ;; Optionally make narrowing help available in the minibuffer.
;;   ;; You may want to use `embark-prefix-help-command' or which-key instead.
;;   ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)
;;   )


;;------------------------------------------------------------------------------
;; Embark: Emacs Mini-Buffer Action Rooted in Keymaps
;;------------------------------------------------------------------------------
;; https://github.com/oantolin/embark
;;
;; > You can think of `embark-act' as a keyboard-based version of a right-click
;; > contextual menu. The `embark-act' command (which you should bind to a
;; > convenient key), acts as a prefix for a keymap offering you relevant actions
;; > to use on a target determined by the context:
;; >  - In the minibuffer, the target is the current top completion candidate.
;; >  - In the *Completions* buffer the target is the completion at point.
;; >  - In a regular buffer, the target is the region if active, or else the file,
;; >    symbol, URL, s-expression or defun at point.

(use-package embark

  ;;------------------------------
  :init
  ;;------------------------------

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)


  ;;------------------------------
  :config
  ;;------------------------------

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))


;;------------------------------
;; Keybinds : Emacs
;;------------------------------

(use-package embark

  ;; TODO: something like this?
  ;; :unless  (or (imp-flag? :keybinds +meow)
  ;;              (imp-flag? :keybinds +evil))

  ;;------------------------------
  :bind
  ;;------------------------------
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings))) ;; alternative for `describe-bindings'


;; ;;------------------------------
;; ;; Keybinds : Meow
;; ;;------------------------------
;;
;; (use-package embark
;;   :when  (imp-flag? :keybinds +meow)
;;   :after meow
;;
;;   ;;------------------------------
;;   :bind ; meow
;;   ;;------------------------------
;;   ;; TODO-meow: Meow bindings? 'C-.' is `scroll-down-command'...
;;   (("C-." . embark-act)         ;; pick some comfortable binding
;;    ("C-;" . embark-dwim)        ;; good alternative: M-.
;;    ("C-h B" . embark-bindings))) ;; alternative for `describe-bindings'
;;
;;
;; ;;------------------------------
;; ;; Keybinds : Evil
;; ;;------------------------------
;;
;; (use-package embark
;;   :when  (imp-flag? :keybinds +evil)
;;   :after (:and evil evil-collection)
;;
;;   ;;------------------------------
;;   :bind ; evil
;;   ;;------------------------------
;;   ;; TODO-evil: Evil bindings?
;;   (("C-." . embark-act)         ;; pick some comfortable binding
;;    ("C-;" . embark-dwim)        ;; good alternative: M-.
;;    ("C-h B" . embark-bindings))) ;; alternative for `describe-bindings'


;;------------------------------
;; Embark + Consult
;;------------------------------
;; https://github.com/oantolin/embark#quick-start
;; https://github.com/minad/consult#embark-integration

(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer

  ;;------------------------------
  :hook
  ;;------------------------------
  (embark-collect-mode-hook . consult-preview-at-point-mode))


;;------------------------------
;; Embark + Which-Key
;;------------------------------
;; https://github.com/oantolin/embark/wiki/Additional-Configuration#use-which-key-like-a-key-menu-prompt

;; The built-in embark-verbose-indicator displays actions in a buffer
;; along with their keybindings and the first line of their
;; docstrings. Users desiring a more compact display can use which-key
;; instead with the following configuration:
(imp-eval-after (:and embark which-key)

  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))

  (setq embark-indicators
        '(embark-which-key-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))

  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator))


;;------------------------------------------------------------------------------
;; Mini-Frame
;;------------------------------------------------------------------------------
;; https://github.com/muffinmad/emacs-mini-frame
;;
;; TODO: Do I want the minibuffer at the top of the current frame during
;; `read-from-minibuffer'?
;;
;; TODO: In order to find out, try this:
;; (use-package mini-frame
;;   ;; Not all that much config help on the GitHub...
;;   ;; May have to go Googling to see how people use this.
;;
;;   ;;------------------------------
;;   :config
;;   ;;------------------------------
;;   (mini-frame-mode +1))
;;
;; TODO: If I like this, move it elsewhere. It was suggested by a "SMOCE"
;; completion framework package, but it's a minibuffer package, not a completion
;; package.


;;------------------------------------------------------------------------------
;; Corfu: Completion Overlay Region FUnction
;;------------------------------------------------------------------------------

;; https://github.com/minad/corfu
(use-package corfu
  ;; ;;------------------------------
  ;; :custom
  ;; ;;------------------------------
  ;;
  ;; ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; ;; (corfu-auto t)                 ;; Enable auto completion (default nil)
  ;; ;; (corfu-separator ?\s)          ;; Orderless field separator (default: `?\s' aka space)
  ;; ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;;------------------------------
  :init
  ;;------------------------------
  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  (global-corfu-mode +1)

  ;; ;;------------------------------
  ;; :hook
  ;; ;;------------------------------
  ;; ;; Alternative: Enable Corfu only for certain modes.
  ;; ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))
  )


;; ;; Completion settings in Emacs that could be useful for `corfu'.
;; (use-package emacs
;;    :ensure nil ; This is an Emacs built-in feature.

;;   ;;------------------------------
;;   :custom
;;   ;;------------------------------
;;   ;; TAB cycle if there are only few candidates
;;   (completion-cycle-threshold 3)

;;   ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
;;   ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
;;   ;; (read-extended-command-predicate #'command-completion-default-include-p)

;;   ;; Enable indentation+completion using the TAB key?
;;   ;; `completion-at-point' is often bound to M-TAB.
;;   ;;
;;   ;; Options:
;;   ;;   - t          - TAB always just indents the current line.
;;   ;;   - nil        - TAB indents the current line if point is at the left margin or in the line's indentation.
;;   ;;                - Otherwise it inserts a TAB character or spaces, depending.
;;   ;;   - `complete' - TAB first tries to indent the current line, and if the line
;;   ;;                  was already indented, then try to complete the thing at point.
;;   ;;
;;   ;; Also see `tab-first-completion'.
;;   ;;
;;   ;; Some programming language modes have their own variable to control this,
;;   ;; e.g., `c-tab-always-indent', and do not respect this variable.
;;   ;;
;;   ;; NOTE: We'll just bind completion to something other than TAB...
;;   ;; (tab-always-indent 'complete)
;;   )


;;------------------------------------------------------------------------------
;; Cape: Completion At Point Extensions
;;------------------------------------------------------------------------------
;; Used in combination with `corfu' completion UI (or vanilla Emacs completion UI).

;; https://github.com/minad/cape
(use-package cape

  ;;------------------------------
  ;; NOTE: Potential issue with `cape-dabbrev'?
  ;;------------------------------
  ;; "In case you observe a performance issue with autocompletion and
  ;; `cape-dabbrev' it is strongly recommended to disable scanning in other
  ;; buffers. See the user options `cape-dabbrev-min-length' and
  ;; `cape-dabbrev-check-other-buffers'."


  ;;------------------------------
  :init
  ;;------------------------------

  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  ;;   - `cape-dabbrev': Complete word from current buffers (see also `dabbrev-capf' on Emacs 29)
  ;;   - `cape-file'   : Complete file name
  ;;   - `cape-history': Complete from Eshell, Comint or minibuffer history
  ;;   - `cape-keyword': Complete programming language keyword
  ;;   - `cape-symbol' : Complete Elisp symbol
  ;;   - `cape-abbrev' : Complete abbreviation (`add-global-abbrev', `add-mode-abbrev')
  ;;   - `cape-ispell' : Complete word from Ispell dictionary
  ;;   - `cape-dict'   : Complete word from dictionary file
  ;;   - `cape-line'   : Complete entire line from current buffer
  ;;   - `cape-tex'    : Complete unicode char from TeX command, e.g. \hbar.
  ;;   - `cape-sgml'   : Complete unicode char from Sgml entity, e.g., &alpha.
  ;;   - `cape-rfc1345': Complete unicode char using RFC 1345 mnemonics.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;; (add-to-list 'completion-at-point-functions #'cape-history)
  ;; (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;; (add-to-list 'completion-at-point-functions #'cape-tex)
  ;; (add-to-list 'completion-at-point-functions #'cape-sgml)
  ;; (add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;; (add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;; (add-to-list 'completion-at-point-functions #'cape-ispell)
  ;; (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  ;; (add-to-list 'completion-at-point-functions #'cape-line)
  )


;;------------------------------
;; Keybinds : Any/All
;;------------------------------

(use-package cape
  ;;------------------------------
  :bind ; rebinds
  ;;------------------------------

  ;; `cape-dabbrev' is `dabbrev-expand' / `dabbrev-completion' with a UI... Use it instead?
  (([remap dabbrev-expand]     . cape-dabbrev)
   ([remap dabbrev-completion] . cape-dabbrev)))


;; ;;------------------------------
;; ;; Keybinds : meow
;; ;;------------------------------
;;
;; (use-package cape
;;   :when  (imp-flag? :keybinds +meow)
;;   :after meow
;;
;;   ;;------------------------------
;;   :config
;;   ;;------------------------------
;;
;;   ;; Bind dedicated completion commands.
;;   ;; Were all just bound to "C-c p [...]".
;;   ;; https://github.com/minad/cape#configuration
;;   ;; "Alternative prefix keys: C-c p, M-p, M-+, ..."
;;
;;   ;;------------------------------
;;   ;; `General'
;;   ;;------------------------------
;;   (defun mantle:meow/keybind/general:completion ()
;;     "Create the \"Completion...\" keybinds in `general' for `meow'."
;;     (keybind:leader/global:def
;;       :infix (keybind:infix "t" "c")        ; text -> completion
;;       "" '(nil :which-key "Completion...") ; infix title
;;
;;       ;; Emacs Functions:
;;       "p" #'completion-at-point ; capf
;;       "t" #'complete-tag        ; etags
;;
;;       ;; `cape' Functions:
;;       ;;   - `cape-dabbrev': Complete word from current buffers (see also `dabbrev-capf' on Emacs 29)
;;       ;;   - `cape-file'   : Complete file name
;;       ;;   - `cape-history': Complete from Eshell, Comint or minibuffer history
;;       ;;   - `cape-keyword': Complete programming language keyword
;;       ;;   - `cape-symbol' : Complete Elisp symbol
;;       ;;   - `cape-abbrev' : Complete abbreviation (`add-global-abbrev', `add-mode-abbrev')
;;       ;;   - `cape-ispell' : Complete word from Ispell dictionary
;;       ;;   - `cape-dict'   : Complete word from dictionary file
;;       ;;   - `cape-line'   : Complete entire line from current buffer
;;       ;;   - `cape-tex'    : Complete unicode char from TeX command, e.g. \hbar.
;;       ;;   - `cape-sgml'   : Complete unicode char from Sgml entity, e.g., &alpha.
;;       ;;   - `cape-rfc1345': Complete unicode char using RFC 1345 mnemonics.
;;       "d" #'cape-dabbrev ; or dabbrev-completion
;;       "h" #'cape-history
;;       "f" #'cape-file
;;       "k" #'cape-keyword
;;       "s" #'cape-symbol
;;       "a" #'cape-abbrev
;;       "i" #'cape-ispell
;;       "l" #'cape-line
;;       "w" #'cape-dict
;;       ;; Complete unicode from...
;;       ;; ("\\" "cape-tex" cape-tex)     ; tex (e.g. "\hbar")
;;       "&" #'cape-sgml    ; SGML (e.g. "&alpha")
;;       "r" #'cape-rfc1345 ; RFC-1345 (e.g. ...uh... weird? https://www.rfc-editor.org/rfc/rfc1345)
;;       ))
;;
;;
;;   ;;------------------------------
;;   ;; `Transient'
;;   ;;------------------------------
;;
;;   (defun mantle:meow/keybind/transient:completion ()
;;     "Create the \"Completion...\" keybinds in `transient' for `meow'."
;;
;;     (transient-define-prefix mantle:meow/transient:completion:at-point ()
;;       "Buffer commands that should be available globally."
;;       ["Completion..."
;;        ["At Point"
;;         ;; Emacs Functions:
;;         ("p" "completion-at-point" completion-at-point) ;; capf
;;         ("t" "complete-tag" complete-tag)        ;; etags
;;         ;; `cape' Functions:
;;         ;;   - `cape-dabbrev': Complete word from current buffers (see also `dabbrev-capf' on Emacs 29)
;;         ;;   - `cape-file'   : Complete file name
;;         ;;   - `cape-history': Complete from Eshell, Comint or minibuffer history
;;         ;;   - `cape-keyword': Complete programming language keyword
;;         ;;   - `cape-symbol' : Complete Elisp symbol
;;         ;;   - `cape-abbrev' : Complete abbreviation (`add-global-abbrev', `add-mode-abbrev')
;;         ;;   - `cape-ispell' : Complete word from Ispell dictionary
;;         ;;   - `cape-dict'   : Complete word from dictionary file
;;         ;;   - `cape-line'   : Complete entire line from current buffer
;;         ;;   - `cape-tex'    : Complete unicode char from TeX command, e.g. \hbar.
;;         ;;   - `cape-sgml'   : Complete unicode char from Sgml entity, e.g., &alpha.
;;         ;;   - `cape-rfc1345': Complete unicode char using RFC 1345 mnemonics.
;;         ("d" "cape-dabbrev" cape-dabbrev)        ;; or dabbrev-completion
;;         ("h" "cape-history" cape-history)
;;         ("f" "cape-file" cape-file)
;;         ("k" "cape-keyword" cape-keyword)
;;         ("s" "cape-symbol" cape-symbol)
;;         ("a" "cape-abbrev" cape-abbrev)
;;         ("i" "cape-ispell" cape-ispell)
;;         ("l" "cape-line" cape-line)
;;         ("w" "cape-dict" cape-dict)
;;         ;; Complete unicode from...
;;         ;; ("\\" "cape-tex" cape-tex)     ; tex (e.g. "\hbar")
;;         ("&" "cape-sgml" cape-sgml)       ; SGML (e.g. "&alpha")
;;         ("r" "cape-rfc1345" cape-rfc1345)]]) ; RFC-1345 (e.g. ...uh... weird? https://www.rfc-editor.org/rfc/rfc1345)
;;     ;; (mantle:meow/transient:completion:at-point)
;;
;;     (meow-leader-define-key
;;      '("p" . mantle:meow/transient:completion:at-point)))
;;
;;
;;   ;;------------------------------
;;   ;; Actually Create Keybinds:
;;   ;;------------------------------
;;
;;   (if (imp-provided? :keybinds 'general 'meow)
;;       (mantle:meow/keybind/general:completion)
;;     (mantle:meow/keybind/transient:completion)))


;;------------------------------------------------------------------------------
;; Dabbrev: Dynamic Abbrev Expansion: Dynamic Abbreviation Expansion
;;------------------------------------------------------------------------------

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Dynamic-Abbrevs.html
(use-package dabbrev
  :ensure nil ; This is an Emacs built-in feature.

  ;;------------------------------
  :custom
  ;;------------------------------
  ;; Other useful Dabbrev configurations.
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))


;; ;;------------------------------
;; ;; Keybinds : Emacs
;; ;;------------------------------
;;
;; ;; (use-package dabbrev
;; ;;   :ensure nil ; This is an Emacs built-in feature.
;; ;;   ;; Always do this keybind swap.
;; ;;
;; ;;   ;;------------------------------
;; ;;   :bind ; emacs
;; ;;   ;;------------------------------
;; ;;   ;; Swap 'M-/' and 'C-M-/'
;; ;;   ;; TODO: Yes or no on this? Doom doesn't swap 'em.
;; ;;   (("M-/" . dabbrev-completion)
;; ;;    ("C-M-/" . dabbrev-expand)))



;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide user config completion)
