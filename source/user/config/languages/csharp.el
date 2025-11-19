;;; user/config/languages/csharp.el --- C# -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-09-27
;; Timestamp:  2025-11-18
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; C# aka cs aka csharp aka c-sharp... whatever.
;; Just don't call it C♯.
;;
;;; Code:

;;------------------------------------------------------------------------------
;; `csharp-mode': Now in Vanilla (Emacs) Flavor!
;;------------------------------------------------------------------------------

;; TODO(csharp): switch to `csharp-ts-mode'.
(use-package csharp-mode
  :ensure nil ; Emacs builtin since Emacs ~29

  ;;------------------------------
  :init
  ;;------------------------------

  ;; TODO: Do I want this from Doom? Are strings still breaking `csharp-mode'?
  ;;   (defadvice! +csharp-disable-clear-string-fences-a (fn &rest args)
  ;;     "This turns off `c-clear-string-fences' for `csharp-mode'. When
  ;; on for `csharp-mode' font lock breaks after an interpolated string
  ;; or terminating simple string."
  ;;     :around #'csharp-disable-clear-string-fences
  ;;     (unless (eq major-mode 'csharp-mode)
  ;;       (apply fn args)))

  (defun --/hook/csharp/settings ()
    "Settings for C# mode. Non-LSP stuff.

Note that `csharp-mode' is not derived from `cc-mode' (though it does
require/use it), so do All The Things!"

     ;; Use BSD in C, C++. Used to use it in C#.
     ;; But now C# shares a style with Java, so see if that's better.
     ;; Or go back to BSD if I hate it.
     ;; (c-set-style "bsd") ; See c-style-alist for supported types
     (c-set-style "csharp") ; See c-style-alist for supported types

     (setq c-basic-offset --/tab/standard)
     (c-set-offset 'innamespace 0) ; Don't indent namespace - waste of indent level
     ;; Set by "csharp" style.
     ;; (c-set-offset 'case-label '+) ; indent case labels by c-indent-level, too

     ;; 'wide' is a decent default, probably?
     (setq fill-column --/fill-column/wide)

     ;; TODO(csharp): is this true?
     ;; electric-indent-mode is true and might take care of this?
     ;; (local-set-key [return] 'newline-and-indent)

     ;; TODO(csharp) [2019-10-11]: I probably want most or all of these, or some
     ;;   competing package/feature?
     ;; (paredit-mode 1)
     ;; (my/disable-paredit-spaces-before-paren)
     ;; (company-mode 1)
     ;; (flycheck-mode 1)

     ;; TODO(csharp) [2019-10-11]: check out these ideas from:
     ;;   https://github.com/dholm/dotemacs/blob/master/.emacs.d/lisp/modes/c-c%2B%2B.el
     ;; TODO(csharp) [2019-10-11]: enable/disable in c-common-hook as well.

     ;; Separate camel-case into separate words?
     ;; (subword-mode t)
     )


  ;;------------------------------
  :hook
  ;;------------------------------
  ((csharp-mode-hook . --/hook/csharp/settings)
   (csharp-mode-hook . rainbow-delimiters-mode)))


;; TODO(lsp): uncomment:
;; ;;------------------------------
;; ;; C# LSP
;; ;;------------------------------
;;
;; (use-package csharp-mode
;;   :when (and (imp:flag? :dev-env +lsp)
;;              (imp:flag? :lsp +csharp))
;;
;;   ;; NOTE: `M-x lsp-install-server' has 2 options for C# LSPs:
;;   ;;   1) 'omnisharp-roslyn' : https://emacs-lsp.github.io/lsp-mode/page/lsp-csharp-omnisharp/
;;   ;;   2) 'csharp-ls'        : https://emacs-lsp.github.io/lsp-mode/page/lsp-csharp-ls/
;;   ;;
;;   ;; Not sure which is best, but I've had issues with 'omnisharp-roslyn' in the
;;   ;; past so... try the other one this time?
;;   ;;
;;   ;; Also 'omnisharp-roslyn' says it's built with Mono on Linux, whereas
;;   ;; 'csharp-ls' is installed with `dotnet' CLI.
;;
;;   ;;------------------------------
;;   :hook
;;   ;;------------------------------
;;   (csharp-mode-hook . mantle:hook:lsp:enable))


;;------------------------------------------------------------------------------
;; `sharper' - .NET CLI Transient Menu
;;------------------------------------------------------------------------------
;; https://github.com/sebasmonia/sharper
(use-package sharper
  :bind
  ("C-c n" . sharper-main-transient))


;;------------------------------------------------------------------------------
;; `csproj-mode' for `.csproj' files
;;------------------------------------------------------------------------------
;; https://github.com/omajid/csproj-mode
(use-package csproj-mode
  ;; Nothing to do... super simple mode.
  )


;;------------------------------------------------------------------------------
;; `sln-mode' for `.sln' files
;;------------------------------------------------------------------------------
;; https://github.com/sensorflo/sln-mode
(use-package sln-mode
  ;; ;; Not on a package repository so get it from GitHub:
  ;; :straight (:type git
  ;;            :host github
  ;;            :repo "sensorflo/sln-mode")

  ;; This is all the set-up that Doom does:
  :mode "\\.sln\\'"

  ;;------------------------------
  :init
  ;;------------------------------
  ;; `package-vs-install' "bug":
  ;;   1. It wants to be interactive.
  ;;   2. It doesn't know what to do if the package is already installed.
  ;; Therefore, hide behind installed check:
  (unless (package-installed-p 'sln-mode)
    ;; Not on (M)ELPA. Tell Emacs where/how to get it.
    (package-vc-install "https://github.com/sensorflo/sln-mode")))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide user (imp-path-relative 'user (imp-path-sans-extension (imp-path-current-file))))
