;;; user/config/languages/yaml.el --- Yet Another Markup Language -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2025-11-17
;; Timestamp:  2025-11-17
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; from 2023-07-23_sn004:sn004/mantle/config/dev-env/languages/yaml.el
;;
;;; Code:

;;------------------------------------------------------------------------------
;; `yaml-mode'
;;------------------------------------------------------------------------------

(use-package yaml-mode
  ;;------------------------------
  :init
  ;;------------------------------

  (defun --/hook/yaml/settings ()
    "Settings for YAML mode. Non-LSP stuff."

    ;; `fill-column' is always a buffer-local var (see its help).
    ;; Use `setq-local' so we remember what to use for things that aren't auto-buffer-local?
    (setq-local fill-column --/fill-column/standard)

    ;; NOTE [OLD]: `yaml-mode' does not use `tab-width'. It uses its own var: `yaml-indent-offset'.
    ;; ;; Use smaller indents than is standard for code.
    ;; (setq tab-width yaml-indent-offset)
    )


  ;;------------------------------
  :hook
  ;;------------------------------
  (yaml-mode-hook . --/hook/yaml/settings)


  ;;------------------------------
  :custom
  ;;------------------------------

  ;; Use smaller indents than is standard for code.
  ;; NOTE: `yaml-indent-offset' is 2 by default. Set it explicitly in case I change my mind about tab sizes.
  (yaml-indent-offset --/tab/small))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide user (imp-path-relative 'user (imp-path-sans-extension (imp-path-current-file))))
