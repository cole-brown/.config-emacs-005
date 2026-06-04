;;; user/config/languages/rust.el --- rust lang config -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2023-01-03
;; Timestamp:  2026-06-03
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  This file is rusting.
;;
;;; Code:

(require 'treesit)


;;------------------------------------------------------------------------------
;; Hooks for `rust-ts-mode' and `rust-mode'
;;------------------------------------------------------------------------------

(defun --/hook/rust/settings ()
  "Set up buffer local vars."
  (setq tab-width   --/tab/small)
  (setq fill-column --/fill-column/wide))

(defun --/hook/lsp/rust/enable ()
  ;; Flymake is making up errors in rust. Spamming E0432 & E0433 all over.
  ;; Use rust-analyzer diagnostics via lsp-mode; disable rust-ts-mode's
  ;; standalone Flymake backend, which doesn't understand Cargo deps.
  (remove-hook 'flymake-diagnostic-functions #'rust-ts-flymake t)

  (--/hook/lsp/enable))


;;------------------------------------------------------------------------------
;; `rust-ts-mode'
;;------------------------------------------------------------------------------
;; https://github.com/rust-lang/rust-mode?tab=readme-ov-file#tree-sitter

(use-package rust-ts-mode
  :ensure nil ; Emacs builtin mode
  :after treesit
  :mode "\\.rs\\'"

  ;;------------------------------
  :hook
  ;;------------------------------
  ((rust-ts-mode-hook . --/hook/rust/settings)
   (rust-ts-mode-hook . --/hook/lsp/rust/enable)))


;;------------------------------------------------------------------------------
;; Fallback: `rust-mode'
;;------------------------------------------------------------------------------
;; https://github.com/rust-lang/rust-mode?tab=readme-ov-file#tree-sitter
;; `rust-ts-mode' will fallback to `rust-mode' via `treesit-auto' (./treesit.el).

(use-package rust-mode
  :ensure nil ; Emacs builtin mode

  ;;------------------------------
  :hook
  ;;------------------------------
  ((rust-mode-hook . --/hook/rust/settings)
   (rust-mode-hook . --/hook/lsp/rust/enable)))


;;------------------------------------------------------------------------------
;; Rust + LSP
;;------------------------------------------------------------------------------

;; Sanity check for the Rust LSP `rust-analyzer':
;;   > error: Unknown binary 'rust-analyzer.exe' in official toolchain 'stable-x86_64-pc-windows-msvc'.
;; Install `rust-analyzer':
;;
;; Check asynchronously during init.
;; TODO: Put this on a timer that runs after emacs init is finished?
(let ((path-here-rel (path:join "user"
                                (imp-path-relative 'user
                                                   (imp-path-current-file)))))
  (make-process
   :name "rust-analyzer-check"
   :buffer "*rust-analyzer-check*"
   :command '("rust-analyzer" "-h")
   :stderr (generate-new-buffer " *rust-analyzer-check-stderr*")
   :sentinel
   (lambda (proc _event)
     (when (eq (process-status proc) 'exit)
       (let* ((code (process-exit-status proc))
              (stderr-buf (process-get proc 'stderr-buf))
              (stderr (when (buffer-live-p stderr-buf)
                        (with-current-buffer stderr-buf (buffer-string)))))
         (cond
          ((= code 0)) ;; Good!
          ((and stderr
                (string-match
                 "error: Unknown binary 'rust-analyzer"
                 stderr))
           (warn "%s: Install Rust LSP: `%s`"
                 path-here-rel
                 "rustup component add rust-analyzer"))
          (t
           (warn "%s: rust-analyzer check failed: %s"
                 path-here-rel
                 stderr)))
         (when (buffer-live-p stderr-buf)
           (kill-buffer stderr-buf)))))
   :file-handler t))

;; Actually set up LSP for Rust.
(with-eval-after-load 'lsp-rust
  ;; Set Rust LSP to `rust-analyzer'.
  (setq lsp-rust-server 'rust-analyzer)
  (setq lsp-disabled-clients '(rls))
  ;; Use Rust Clippy instead of Rust Check.
  (setq lsp-rust-analyzer-cargo-watch-command "clippy") ; or "check"
  ;; (setq lsp-rust-analyzer-cargo-load-out-dirs-from-check t)
  ;; (setq lsp-rust-analyzer-proc-macro-enable t)

  (--/exe/require "rust-analyzer"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide user config languages rust)
