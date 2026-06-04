;;; user/config/languages/treesit.el --- tree-sitter config -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    TODO:(datetime:timestamp:insert :rfc-3339:date)
;; Timestamp:  2026-06-03
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

;; TODO: Don't need this anymore due to `treesit-auto' pkg?
;; TODO: Update lib to newer version?
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

;; (defvar --/lib/version/treesit "v0.20.3"       ;;
;;   "Installed version of treesit in Ubuntu 24.") ;;


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

  (setq --/treesit-auto/recipe/rust
        (make-treesit-auto-recipe
         :lang 'rust
         ;; :ts-mode 'rust-ts-mode                    ;;
         ;; :remap '(js2-mode js-mode javascript-mode) ;;
         :url "https://github.com/tree-sitter/tree-sitter-rust"
         :abi14-revision "v0.21.2"))

  (add-to-list 'treesit-auto-recipe-list --/treesit-auto/recipe/rust)

  ;; (add-to-list 'treesit-language-source-alist
  ;;              ;; (LANG . (URL REVISION SOURCE-DIR CC C++))
  ;;              `(rust
  ;;                "https://github.com/tree-sitter/tree-sitter-rust"
  ;;                ,--/lib/version/treesit))

  (global-treesit-auto-mode))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide user (imp-path-relative 'user (imp-path-sans-extension (imp-path-current-file))))
