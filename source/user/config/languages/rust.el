;;; user/config/languages/rust.el --- rust lang config -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2023-01-03
;; Timestamp:  2026-04-01
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

;; TODO: move this block to "./common.el" or "./treesit.el"
;;------------------------------------------------------------------------------
;; Tree-Sitter Version
;;------------------------------------------------------------------------------
;; Emacs uses the OS's version of tree-sitter.
;; To find out what that version is so that you can set it in `treesit-language-source-alist'
;; run one or both of these:
;;   > dpkg-query --show --showformat='${Version}\n' libtree-sitter0
;;   0.20.3-1
;;
;;   > ldconfig -p | grep tree
;;   libtree-sitter.so.0 (libc6,x86-64) => /lib/x86_64-linux-gnu/libtree-sitter.so.0
;;   libostree-1.so.1 (libc6,x86-64) => /lib/x86_64-linux-gnu/libostree-1.so.1
;;   libostree-1.so (libc6,x86-64) => /lib/x86_64-linux-gnu/libostree-1.so
;;   > dpkg -l | grep libtree-sitter
;;   ii  libtree-sitter0:amd64                   0.20.3-1    [...]
;;
;; In this case, I want rust's grammer for tree-sitter v0.20.3.

(setq --/lib/version/treesit "v0.20.3")


;; TODO: Do this for other treesit modes as well.
;; TODO: maybe make it a func/macro?
;;------------------------------------------------------------------------------
;; Rust Treesitter Grammer
;;------------------------------------------------------------------------------

(add-to-list 'treesit-language-source-alist
             ;; (LANG . (URL REVISION SOURCE-DIR CC C++))
             `(rust
               "https://github.com/tree-sitter/tree-sitter-rust"
               ,--/lib/version/treesit))

(unless (treesit-language-available-p 'rust)
  (treesit-install-language-grammar 'rust))


;;------------------------------------------------------------------------------
;; `rust-ts-mode'
;;------------------------------------------------------------------------------
;; https://github.com/rust-lang/rust-mode?tab=readme-ov-file#tree-sitter

(use-package rust-ts-mode
  :ensure nil ; Emacs builtin mode
  :after treesit
  :mode "\\.rs\\'"

  ;;------------------------------
  :init
  ;;------------------------------

  (defun --/hook/rust/settings ()
    "Set up buffer local vars."
    (setq tab-width   --/tab/small)
    (setq fill-column --/fill-column/wide))

  (defun --/hook/lsp/rust/enable ()
    ;; TODO(lsp): put in their own use-package? or like:
    ;;    (with-eval-after-load 'lsp-rust
    ;;      (setq ...))
    (setq lsp-rust-server 'rust-analyzer)
    (setq lsp-disabled-clients '(rls))
    (setq lsp-rust-analyzer-cargo-watch-command "clippy") ; or "check"
    ;; (setq lsp-rust-analyzer-cargo-load-out-dirs-from-check t)
    ;; (setq lsp-rust-analyzer-proc-macro-enable t)

    ;; Flymake is making up errors in rust. Spamming E0432 & E0433 all over.
    ;; Use rust-analyzer diagnostics via lsp-mode; disable rust-ts-mode's
    ;; standalone Flymake backend, which doesn't understand Cargo deps.
    (remove-hook 'flymake-diagnostic-functions #'rust-ts-flymake t)

    (--/hook/lsp/enable))

  (setq rust-mode-treesitter-derive t)

  ;;------------------------------
  :hook
  ;;------------------------------
  ((rust-ts-mode-hook . --/hook/rust/settings)
   (rust-ts-mode-hook . --/hook/lsp/rust/enable)))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide user config languages rust)
