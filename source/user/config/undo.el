;;; user/config/undo-tree.el --- undo-tree configuration -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-07-13
;; Timestamp:  2025-11-17
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Configure Undo and its Tree.
;;
;; NOTE: Prereq for `evil'!
;;       ...if you're using both this and that, obviously...
;;
;;; Code:


(imp-require output:/squelch)
(imp-require unit)
(imp-require window)


;;------------------------------------------------------------------------------
;; Undo-Tree
;;------------------------------------------------------------------------------

(use-package undo-tree
  :demand t ;; Always load.

  ;;------------------------------
  :custom
  ;;------------------------------

  (undo-tree-visualizer-diff t)
  (undo-tree-auto-save-history t)
  ;; [2023-09-14] I hardly ever want undo-in-region... why not try having it turned off?
  (undo-tree-enable-undo-in-region nil)

  ;; Increase undo limits to avoid emacs prematurely truncating the undo
  ;; history and corrupting the tree. Think big... `undo-tree' history trees
  ;; consume exponentially more space than linear undo histories, and then
  ;; some when `undo-tree-enable-undo-in-region' is involved.
  ;; See: syl20bnr/spacemacs#12110
  (undo-limit        (min undo-limit        (unit:byte  16 'mb))) ; (default is 160kb)
  (undo-strong-limit (min undo-strong-limit (unit:byte  64 'mb))) ; (default is 240kb)
  (undo-outer-limit  (min undo-outer-limit  (unit:byte 256 'mb))) ; (default is 24mb)


  ;;------------------------------
  :bind
  ;;------------------------------

  ;;---
  ;; BUG [2023-09-15]: "Error reading undo-tree history [...]"
  ;;---
  ;; `undo-tree-visualizer-mode' has a bug where if you leave
  ;; it by just killing the buffer (e.g. `kill-buffer'), the `undo-tree' history
  ;; file becomes corrupted with a dead marker or dead overlay, and then you get
  ;; this complaint when opening the file next time:
  ;;   > Error reading undo-tree history from "/home/user/.emacs/var/undo-tree-hist/.!home!user!file.ext.~undo-tree~.zst"
  ;;
  ;; See: https://www.reddit.com/r/emacs/comments/cn5rdy/comment/ew8e2db/?context=3
  ;; Note: That post & reply is from around 2019, so I doubt `undo-tree' will
  ;; fix it any time soon?
  ;;
  ;; Solution?: Just try to not be able to kill/quit that buffer except by it's
  ;; own kill/quit-and-cleanup functions?
  ;;   - `undo-tree-visualizer-quit'  - quit
  ;;   - `undo-tree-visualizer-abort' - quit & discard changes made while in visualizer
  (:map undo-tree-visualizer-mode-map
   ([remap kill-buffer]         . #'undo-tree-visualizer-quit)
   ([remap window:kill-or-quit] . #'undo-tree-visualizer-quit)
   ([remap window:quit-or-kill] . #'undo-tree-visualizer-quit))


  ;;------------------------------
  :config
  ;;------------------------------

  ;; Prefer `zstd' for compressing history files.
  (cond ((executable-find "zstd")
         (message "[INFO] `undo-tree': Using compression `zstd' for undo history files.")
         (define-advice undo-tree-make-history-save-file-name (:filter-return (file) user/compress/zstd)
           (concat file ".zst")))
        ((executable-find "gzip")
         (message "[INFO] `undo-tree': `zstd' not found; using compression `gzip' for undo history files.")
         (define-advice undo-tree-make-history-save-file-name (:filter-return (file) user/compress/gzip)
           (concat file ".gz")))
        ;; Fallback: Just don't compress. :o
        (t
         (warn
          (concat
           "`undo-tree': No compression found for undo history files; install "
           "`zstd' (preferred) or `gzip' to enable compression of `undo-tree' "
           "history files."))
         nil))

  ;; Strip text properties from undo-tree data to stave off bloat. File size
  ;; isn't the concern here; undo cache files bloat easily, which can cause
  ;; freezing, crashes, GC-induced stuttering or delays when opening files.
  (define-advice undo-list-transfer-to-tree (:before nil user/strip-text-props)
    (dolist (item buffer-undo-list)
      (and (consp item)
           (stringp (car item))
           (setcar item (substring-no-properties (car item))))))

  ;; Undo-tree is too chatty about saving its history files. This doesn't
  ;; totally suppress it logging to *Messages*, it only stops it from appearing
  ;; in the echo-area.
  (define-advice undo-tree-save-history (:around (fn &rest args) user/squelch)
    "Undo-tree is too chatty about saving its history files; prevent it from using minibuffer."
    (output:squelch/unless :interactive? t
                           :allow-messages? t
                           (apply fn args)))

  (global-undo-tree-mode +1))


;;------------------------------------------------------------------------------
;; Keybinds
;;------------------------------------------------------------------------------

;;------------------------------
;; Evil
;;------------------------------

;; (use-package undo-tree
;;   :demand t
;;   :when  (imp:flag? :keybinds +evil)
;;   :after (:and evil evil-collection)

;;   ;;------------------------------
;;   :general ; evil
;;   ;;------------------------------
;;   ;; No prefix. Used very frequently.
;;   (:states  'normal
;;    :keymaps 'override
;;    "h" (list #'undo-tree-undo :which-key "undo")
;;    "n" (list #'undo-tree-redo :which-key "redo")
;;    "t" (list #'undo-tree-visualize :which-key "visualize")
;;    "c" (list #'undo-tree-switch-branch :which-key "switch undo branch")

;;    ;; TODO: Do I want to change/add keybinds for these modes?
;;    ;; `undo-tree-visualizer-selection-mode-map'
;;    ;; `undo-tree-visualizer-mode-map'
;;    ))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide user config undo-tree)
