;;; mantle/config/dev-env/snippets.el --- Snippets, Templates, and $1 -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-08-05
;; Timestamp:  2024-05-31
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Snippets, Templates, and $1
;;
;;; Code:

(use-package yasnippet
  :demand t

  ;;------------------------------
  :custom
  ;;------------------------------

  ;; `yas-triggers-in-field'
  ;;---
  ;; Allow snippetception: snippets inside of snippets.
  (yas-triggers-in-field t)

  ;; ;; `yas-indent-line'
  ;; ;;---
  ;; ;; `fixed': Indent the snippet to the current column;
  ;; ;; `auto': Indent each line of the snippet with indent-according-to-mode
  ;; ;; [2023-11-14]: Does `fixed' make my snippets better or worse overall? It's better in `org-mode'.
  ;; ;; (yas-indent-line 'fixed)
  ;; ;; [2024-05-31]: Is `fixed' better in `org-mode'? Really? Cuz I'm still having trouble with `org-mode'...
  ;; ;; [2024-05-31]: Try `nothing'??
  ;; (yas-indent-line 'nothing)

  ;; ;; `yas-also*-indent-*'
  ;; ;;---
  ;; ;; No-op if `yas-indent-line' != `auto'.
  ;; ;; TODO: Do I want either of these in prog modes? Don't /think/ I want in `org-mode';
  ;; ;; see `src' indent issues.
  ;; (yas-also-auto-indent-first-line nil)
  ;; (yas-also-indent-empty-lines nil)

  ;; `yas-key-syntaxes'
  ;;---
  ;; This modifies how yas looks for matching keys to expand into templates.
  ;;   - https://emacs.stackexchange.com/a/35603
  ;;   - Also check out its documentation at: =C-h v yas-key-syntaxes=
  ;; So if I have keys that don't feel like they're getting triggered right
  ;; from wherever my cursor is when I try, we can adjust this.
  ;; Don't see a need right now, though.
  ;; (yas-key-syntaxes '("w_" "w_." "^ "))

  ;;------------------------------
  :config
  ;;------------------------------

  ;;---
  ;; Warnings
  ;;---
  ;; TODO: this? Need new imp's 'after'.
  ;; ;; Suppress a warning about backquote behavior:
  ;; ;;   > Warning (yasnippet): ‘/sec//: section <width>: prog-mode comment header section w/ settable width’ modified buffer in a backquote expression.
  ;; ;;   > To hide this warning, add (yasnippet backquote-change) to ‘warning-suppress-types’.
  ;; (imp:eval:after warnings
  ;;   (add-to-list 'warning-suppress-types '(yasnippet backquote-change)))

  ;;---
  ;; Snippet Paths
  ;;---
  ;; Want my snippets at the front of the list (ahead of yasnippet-snippets', if installed)
  ;; E.g.: mine, yasnippet's snippets dir, yasnippet-snippets' dir.
  ;;---TODO---
  ;; TODO: When path' lib is ready, guard like so:
  ;; (when (path:exists? innit:path:snippet :dir)
  ;;  (add-to-list 'yas-snippet-dirs ...))
  (add-to-list 'yas-snippet-dirs (expand-file-name "snippets" user-emacs-directory))

  ;;---
  ;; Enable everywhere!
  ;;---
  (yas-global-mode +1))

;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide :user 'config 'snippets)
