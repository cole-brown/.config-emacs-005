;;; user/config/languages/markdown.el --- Mark up with markdown! -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2026-06-02
;; Timestamp:  2026-06-03
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Markdown Mode
;;  Markdown Previews
;;
;;; Code:


;;------------------------------------------------------------------------------
;; `markdown-mode'
;;------------------------------------------------------------------------------

;; https://github.com/jrblevin/markdown-mode
;; https://jblevins.org/projects/markdown-mode/
(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)

  ;; ;;------------------------------ ;;
  ;; :custom                         ;;
  ;; ;;------------------------------ ;;
  ;;
  ;; `markdown-command'
  ;;-------------------
  ;; For markdown live previews.
  ;; (markdown-command "pandoc") ; default: "pandoc"
  ;; pandoc: https://pandoc.org
  ;;   - Debian/Ubuntu: `apt get pandoc`
  ;;   - Windows: `winget install --source winget --exact --id JohnMacFarlane.Pandoc`
  ;;     - https://pandoc.org/installing.html#windows

  ;;------------------------------
  :bind
  ;;------------------------------
  (:map markdown-mode-map
   ("C-c C-e" . markdown-do)))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide user (imp-path-relative 'user (imp-path-sans-extension (imp-path-current-file))))
