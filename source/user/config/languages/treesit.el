;;; user/config/languages/treesit.el --- tree-sitter config -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    TODO:(datetime:timestamp:insert :rfc-3339:date)
;; Timestamp:  2026-06-16
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  `treesit-auto'
;;
;;; Code:

;;------------------------------------------------------------------------------
;; Treesit-Auto
;;------------------------------------------------------------------------------
;; Automatically install and use tree-sitter major modes in Emacs 29+. If the
;; tree-sitter version can’t be used, fall back to the original major mode.

;; https://github.com/renzmann/treesit-auto
(use-package treesit-auto

  ;;------------------------------
  :custom
  ;;------------------------------

  ;; `treesit-auto-install'
  ;;-----------------------
  ;; If non-nil, auto install missing tree-sitter grammars.
  ;;
  ;; This variable enables the automatic clone, compile, and
  ;; installation of tree-sitter grammars whenever visiting a file
  ;; that has a compatible tree-sitter mode.  If set to prompt
  ;; treesit-auto will ask for confirmation before downloading the
  ;; grammar.  Additionally, treesit-auto-install-all will skip the
  ;; yes/no prompt when this variable is t.
  (treesit-auto-install 'prompt) ; default: nil

  ;;------------------------------
  :config
  ;;------------------------------

  ;; WINDOWS INSTALL DEPENDENCIES
  ;; ----------------------------
  ;; 1. Download and install "MSYS2".
  ;; 2. Open "MSYS2 MinGW UCRT64 x64".
  ;; 3. `pacman -Syu mingw-w64-ucrt-x86_64-toolchain git make`
  (when (eq system-type 'windows-nt)
    (let ((path-here-rel (path:join "user"
                                    (imp-path-relative 'user
                                                       (imp-path-current-file)))))

      (unless (file-directory-p "C:/msys64/ucrt64/bin")
        (error "%s: MSYS2 must be installed. https://www.msys2.org/"
               path-here-rel))

      ;; Dir exists; add to path.
      (setenv "PATH" (concat "C:\\msys64\\ucrt64\\bin;" (getenv "PATH")))
      (add-to-list 'exec-path "C:/msys64/ucrt64/bin")

      (cond ((file-exists-p "C:/msys64/ucrt64/bin/cc.exe")) ;; `cc` exists; leave env var "CC" alone.

            ((file-exists-p "C:/msys64/ucrt64/bin/gcc.exe")
             (setenv "CC" "gcc"))

            (t
             (error "%s: No compiler exists. In '%s', run `%s`"
                    path-here-rel
                    "MSYS MinGW UCRT x64"
                    "pacman -Syu mingw-w64-ucrt-x86_64-toolchain git make")))))

  ;; treesit lib version vs language grammer version
  ;;------------------------------------------------
  ;; For:
  ;;   > Warning (treesit): The installed language grammar for rust cannot be located or has problems (version-mismatch): 15
  ;;   > Warning (treesit): Cannot activate tree-sitter, because language grammar for rust is unavailable (version-mismatch): 15
  ;;------------------------------------------------
  ;; (treesit-library-abi-version)
  ;; Emacs 30.2: treesit ABI v14

  (setq --/treesit-auto/recipe/c
        (make-treesit-auto-recipe
         :lang 'c
         :ts-mode 'c-ts-mode
         :remap 'c-mode
         :requires 'cpp
         :url "https://github.com/tree-sitter/tree-sitter-c"
         :abi14-revision "v0.21.2"
         :ext "\\.c\\'"))
  (add-to-list 'treesit-auto-recipe-list --/treesit-auto/recipe/c)

  (setq --/treesit-auto/recipe/csharp
        (make-treesit-auto-recipe
         :lang 'c-sharp
         :ts-mode 'csharp-ts-mode
         :remap 'csharp-mode
         :url "https://github.com/tree-sitter/tree-sitter-c-sharp"
         :abi14-revision "v0.21.2"
         :ext "\\.cs\\'"))
  (add-to-list 'treesit-auto-recipe-list --/treesit-auto/recipe/csharp)

  (setq --/treesit-auto/recipe/html
        (make-treesit-auto-recipe
         :lang 'html
         :ts-mode 'html-ts-mode
         :remap '(mhtml-mode sgml-mode)
         :url "https://github.com/tree-sitter/tree-sitter-html"
         :abi14-revision "v0.21.2"
         :ext "\\.html\\'"))
  (add-to-list 'treesit-auto-recipe-list --/treesit-auto/recipe/html)

  (setq --/treesit-auto/recipe/js
        (make-treesit-auto-recipe
         :lang 'javascript
         :ts-mode 'js-ts-mode
         :remap '(js-mode javascript-mode js2-mode)
         :url "https://github.com/tree-sitter/tree-sitter-javascript"
         :abi14-revision "v0.21.2"
         :ext "\\.js\\'"))
  (add-to-list 'treesit-auto-recipe-list --/treesit-auto/recipe/js)

  (setq --/treesit-auto/recipe/json
        (make-treesit-auto-recipe
         :lang 'json
         :ts-mode 'json-ts-mode
         :remap 'js-json-mode
         :url "https://github.com/tree-sitter/tree-sitter-json"
         :abi14-revision "v0.21.2"
         :ext "\\.json\\'"))
  (add-to-list 'treesit-auto-recipe-list --/treesit-auto/recipe/json)

  (setq --/treesit-auto/recipe/rust
        (make-treesit-auto-recipe
         :lang 'rust
         :url "https://github.com/tree-sitter/tree-sitter-rust"
         :abi14-revision "v0.21.2"
         :ext "\\.rs\\'"))
  (add-to-list 'treesit-auto-recipe-list --/treesit-auto/recipe/rust)

  ;; Enable globally!
  (global-treesit-auto-mode))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide user (imp-path-relative 'user (imp-path-sans-extension (imp-path-current-file))))
