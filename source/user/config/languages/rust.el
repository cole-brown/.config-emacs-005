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

;; TODO: Made redundant by `treesit-auto', I think?
;; ;;------------------------------------------------------------------------------ ;;
;; ;; Rust Treesitter Grammer                                                      ;;
;; ;;------------------------------------------------------------------------------ ;;
;;                                                                                 ;;
;; (add-to-list 'treesit-language-source-alist                                     ;;
;;              ;; (LANG . (URL REVISION SOURCE-DIR CC C++))                       ;;
;;              `(rust                                                             ;;
;;                "https://github.com/tree-sitter/tree-sitter-rust"                ;;
;;                ,--/lib/version/treesit))                                        ;;
;;                                                                                 ;;
;; (unless (treesit-language-available-p 'rust)                                    ;;
;;   (treesit-install-language-grammar 'rust))                                      ;;


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

(with-eval-after-load 'lsp-rust
  (setq lsp-rust-server 'rust-analyzer)
  (setq lsp-disabled-clients '(rls))
  (setq lsp-rust-analyzer-cargo-watch-command "clippy") ; or "check"
  ;; (setq lsp-rust-analyzer-cargo-load-out-dirs-from-check t)
  ;; (setq lsp-rust-analyzer-proc-macro-enable t)

  (--/exe/require "rust-analyzer"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide user config languages rust)
