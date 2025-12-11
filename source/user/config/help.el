;;; user/config/help.el --- Which Key & other Helpful things -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-07-15
;; Timestamp:  2025-12-10
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Which Key & other Helpful things
;;
;;; Code:

(imp-require unit)

;;------------------------------------------------------------------------------
;; `which-key'
;;------------------------------------------------------------------------------

;; Which Key:
;;   https://github.com/justbur/emacs-which-key
;; Which-Key vs Guide-Key:
;;   https://github.com/justbur/emacs-which-key/issues/29
;;
;; Super useful. Shows what all can be done from the buttons you just pressed.
;; E.g. "C-h" and pausing for a sec will bring up which-key with all the
;; commands in the help-command map.
(use-package which-key
  ;; Don't want to wait 5 seconds for `which-key' to load interactively.
  :defer 30 ; Wait til emacs has been idle for 30 seconds, then load this.

  ;;------------------------------
  :custom
  ;;------------------------------

  ;; `which-key-sort-*'
  ;;-----------------
  ;; Sort lower and uppercase keybinds together instead of all upper then all lower.
  ;; And lowercase goes first.
  (which-key-sort-order           'which-key-key-order-alpha)
  (which-key-sort-uppercase-first nil)

  ;; `which-key-idle-delay'
  ;;-----------------------
  (which-key-idle-delay (unit:second 0.5 'sec)) ; Default: 1.0 seconds

  ;; 'window' size
  ;;------------
  ;; (which-key-side-window-max-width 0.333)    ; Default: 0.333
  ;; which-key-side-window-max-height

  ;; Which-Key & `evil':
  ;;--------------------
  ;;   https://github.com/justbur/emacs-which-key#evil-operators
  ;;   This is done in `:config' so we can check if we have `evil' enabled.

  ;;------------------------------
  :config
  ;;------------------------------

  ;; 'window' location
  ;;------------
  ;; https://github.com/justbur/emacs-which-key#special-features-and-configuration-options
  ;; Can change where the which-key windows shows up with one of these:
  ;;   (which-key-setup-side-window-bottom) ;; This is the default.
  ;;   (which-key-setup-side-window-right)
  ;;   (which-key-setup-side-window-right-bottom)
  ;; [2022-07-15] Try out right for a while... It's nice that it's one column of
  ;; keys -> binds instead of 1-5 or whatever for the bottom.
  ;; (which-key-setup-side-window-right)
  ;; [2022-07-22] Getting this error with `which-key-setup-side-window-right' if Emacs is small:
  ;;   > which-key can’t show keys: There is not enough space based on your
  ;;   > settings and frame size.
  ;; Try `which-key-setup-side-window-right-bottom' and see if that makes it happier?
  ;; (which-key-setup-side-window-right-bottom)
  ;; If I don't like that, try tweaking these variables:
  ;;   `which-key-side-window-max-height'
  ;;   `which-key-side-window-max-width'
  ;; [2023-04-18] Don't like it...
  ;;   1. Just too far over to look most of the time.
  ;;   2. Doesn't jive with everyone else defaulting to popping up on the bottom.
  ;;   3. Changing buffer widths all the time makes word-wrapped buffers jitter, sometimes.

  ;; TODO: change to eval after
  ;; (when (imp-feature? 'evil)
  ;;   ;; https://github.com/justbur/emacs-which-key#evil-operators
  ;;   ;; Evil motions and text objects following an operator like `d' are not all looked up
  ;;   ;; in a standard way. Support is controlled through `which-key-allow-evil-operators'
  ;;   ;; which should be non-nil if 'evil' is loaded before 'which-key' and through
  ;;   ;; `which-key-show-operator-state-maps' which needs to be enabled explicitly because
  ;;   ;; it is more of a hack. The former allows for the inner and outer text object maps
  ;;   ;; to show, while the latter shows motions as well.
  ;;   (setq which-key-allow-evil-operators t
  ;;         ;; NOTE: The docstring says this is experimental and Doom doesn't set it.
  ;;         ;; So remove if weird?
  ;;         which-key-show-operator-state-maps t))

  ;; Enable Global `which-key-mode'
  ;;-------------------------------
  (which-key-mode +1))


;;------------------------------
;; NOTE: Which-Key: Additional Commands
;;---
;; Commands:
;;   https://github.com/justbur/emacs-which-key#additional-commands
;;
;; `which-key-show-top-level'
;;   - Will show most key bindings without a prefix. It is most and not all,
;;     because many are probably not interesting to most users.
;; `which-key-show-major-mode'
;;   - Will show the currently active major-mode bindings. It’s similar to C-h m
;;     but in a which-key format. It is also aware of evil commands defined
;;     using evil-define-key.
;; `which-key-show-next-page-cycle' / `which-key-show-previous-page-cycle'
;;   - Will flip pages in a circle.
;; `which-key-show-next-page-no-cycle' / `which-key-show-previous-page-no-cycle'
;;   - Will flip pages and stop at first/last page.
;; `which-key-undo'
;;   - Can be used to undo the last keypress when in the middle of a key sequence.
;;------------------------------


;;------------------------------------------------------------------------------
;; `helpful' - "A better Emacs *help* buffer"
;;------------------------------------------------------------------------------
;; Replace Emacs' default help bindings/buffer with Helpful's much more helpful
;; help buffer.

;; https://github.com/Wilfred/helpful
(use-package helpful
  ;; Don't want to wait 5 seconds for `helpful' to load interactively.
  :defer 30 ; Wait til emacs has been idle for 30 seconds, then load this.

  ;; ;;------------------------------
  ;; :custom
  ;; ;;------------------------------

  ;; `helpful-max-buffers'
  ;;----------------------
  ;; "If this many helpful buffers or more exist, kill oldest when opening another."
  ;; Default is 5. Can change if desired, but I tried 2 and went back to 5.
  ;; ;; (helpful-max-buffers 5)


  ;;------------------------------
  :bind ; emacs keybinds
  ;;------------------------------
  ;; Replace Emacs's help keybindings with helpful's.

  ;;---
  ;; global map
  ;;---
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-h o" . helpful-symbol)
   ;; replaces `finder-by-keyword' but I've never used that...
   ("C-h p" . helpful-at-point)
   ;; replaces `describe-coding-system' but I've never used that...
   ("C-h C" . helpful-command)

   ;; TODO: needs namespaced:window lib
   ;; ;;---
   ;; ;; helpful-mode-map
   ;; ;;---
   ;; :map helpful-mode-map
   ;; ;; kill-this-buffer instead of quit (bury)
   ;; ;; TRIAL: [2019-10-28]
   ;; ;; kill-or-quit instead of original quit-or-kill?
   ;; ;;  - "quit" as in bury...
   ;; ("q" . window:kill-or-quit)
   )


  ;; ;;------------------------------
  ;; :config
  ;; ;;------------------------------

  ;; TODO: change to eval-after:
  ;; ;; Set up Ivy to use Helpful:
  ;; (when (imp-feature? 'ivy)
  ;;   (setq counsel-describe-function-function #'helpful-callable
  ;;         counsel-describe-variable-function #'helpful-variable))

  ;; TODO: change to eval-after:
  ;; ;; Set up Helm to use Helpful:
  ;; (when (imp-feature? 'helm)
  ;;   (setq helm-describe-function-function 'helpful-callable
  ;;         helm-describe-variable-function 'helpful-variable))

  )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide user config help)
