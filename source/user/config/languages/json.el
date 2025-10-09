;;; mantle/config/dev-env/json.el --- JSON... JSON everywhere. -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2023-01-03
;; Timestamp:  2025-10-08
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  JSON... JSON everywhere.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; JSON Mode
;;------------------------------------------------------------------------------

(use-package json-ts-mode
  :ensure nil ; Emacs builtin mod
  :mode "\\.js\\(?:on\\|[hl]int\\(?:rc\\)?\\)\\'"

  ;;------------------------------
  :init
  ;;------------------------------

    (defun --/hook/json/settings ()
      "Set up buffer local vars."
      (setq tab-width   --/tab/small)
      (setq fill-column --/fill-column/wide))

  ;;------------------------------
  :hook
  ;;------------------------------
  (json-ts-mode-hook . --/hook/json/settings))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide :user 'config 'languages 'json)
