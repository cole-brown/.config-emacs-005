;;; mantle/config/dev-env/languages/common.el --- Configure All Languages -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-08-05
;; Timestamp:  2025-10-28
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Configure for All Languages.
;; Common stuff.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Comments
;;------------------------------------------------------------------------------

;;------------------------------
;; Block Commenting Style
;;------------------------------

;; TODO: just set it to aligned in those lang's `:custom' section.
(defun --/hook/comments/block-align ()
  "Set `comment-style' to `aligned'.

Good for start/end line comment languages like:
  - C:    /*    */
  - HTML: <!--  -->
  - etc

`aligned' is like (default) `indent' but also aligns end-of-line comment chars
if the language has them."
  (setq 'comment-style 'aligned))


;; TODO: Move to "languages/html.el"
;; `html-mode-hook' (HTML mode) or `mhtml-mode-hook' (HTML+ mode (derived from HTML mode))?
(add-hook 'html-mode-hook '--/hook/comments/block-align)


;;------------------------------------------------------------------------------
;; Metasyntatic Variables
;;------------------------------------------------------------------------------

(defvar --/naming/variables/metasyntatic
  '((:metasyntactic
     (foo (bar (baz (qux (quux (quuux (quuuux (quuuuux))))))
               (thud (grunt))
               (bletch)
               (fum)
               (bongo)
               (zot)))
     (bazola (ztesch))
     (fred (jim (sheila (barney))))
     (corge (grault (flarp)))
     (zxc (spqr (wombat)))
     (shme)
     (spam (eggs))
     (snork)
     (blarg (wibble))
     (toto (titi (tata (tutu))))
     (pippo (pluto (paperino)))
     (aap (noot (mies)))
     (oogle (foogle (boogle (zork (gork (bork)))))))

    (:pinky (narf (zort (poit (egad (troz (fiddely-posh))))))))
  "Alist of meaningless/placeholder variable name progressions.

See: http://www.catb.org/jargon/html/M/metasyntactic-variable.html")


;; (defun /cmd/naming/metasyntatic (&optional type)
;;   "Insert a metasyntatic variable at point.
;;
;; If TYPE is a keyword, use that list in variable
;; `--/naming/variables/metasyntatic'."
;;   (interactive)
;;   (let ((tree-of-meta (alist-get (or nil :metasyntactic) --/naming/variables/metasyntatic)))
;;     ;; TODO: Would be nice to progress in the metavars usage but idk how to, easily.
;;     ;; So just... chose a random?
;;     ;; TODO: finish this?
;;     ))


;;------------------------------------------------------------------------------
;; Documentation: Function Signatures
;;------------------------------------------------------------------------------

(use-package which-func
  :ensure  nil ; This is an Emacs built-in feature.

  ;;------------------------------
  :config
  ;;------------------------------

  ;; Enable `which-function-mode', which will enable the function info for
  ;; whatever modes it supports.
  (which-function-mode +1)

  ;; Display which-function info in a header line instead of in the mode line.
  (setq-default header-line-format
                '((which-func-mode ("" which-func-format " "))))

  (setq mode-line-misc-info
        ;; Remove the current function name from the mode line, because it's
        ;; mostly invisible here anyway.
        (assq-delete-all 'which-func-mode mode-line-misc-info)))


;; ;;------------------------------------------------------------------------------
;; ;; Which Language?
;; ;;------------------------------------------------------------------------------
;; Don't technically need this; it's just used by `format-all' and looks useful.
;; https://github.com/lassik/emacs-language-id
;; (use-package language-id
;;   :autoload language-id-buffer)


;;------------------------------------------------------------------------------
;; Documentation
;;------------------------------------------------------------------------------

;; Turn on the online documentation mode for all programming modes (not all of
;; them support it).
(use-package eldoc
  :ensure nil ; This is an Emacs built-in feature.
  :delight    ; Don't put in the modes in the modeline.
  :hook
  ((prog-mode-hook       . turn-on-eldoc-mode)
   (emacs-lisp-mode-hook . turn-on-eldoc-mode)))


;;------------------------------------------------------------------------------
;; Formatting
;;------------------------------------------------------------------------------

;; TODO: `format-all'
;; ;; https://github.com/lassik/emacs-format-all-the-code
;; ;; NOTE: This relies on external programs for the actual formatting (e.g. LSPs,
;; ;; `rustfmt`, `terraform fmt`...)
;; (imp:use-package format-all
;;   :delight ; Don't put in the modes in the modeline.

;;   :autoload format-all--language-id-buffer

;;   ;;------------------------------
;;   :init
;;   ;;------------------------------

;;   (defcustom mantle:user:format-all:on-save/disabled
;;     '("Emacs Lisp"     ; elisp's mechanisms are good enough... but not 100% so auto-format would fuck up shit.

;;       ;; python-mode   ; Trial [2022-12-22]: Test out the python formatting.    ; Don't even want to know what this would do if it has one.
;;       ;; csharp-mode   ; TODO: Should I disable this one (again)?
;;       ;; sql-mode      ; Doom says: "sqlformat is currently broken"
;;       ;; tex-mode      ; Doom says: "latexindent is broken"
;;       ;; latex-mode    ; Doom says: "" (probably just ditto to `tex-mode')

;;       fundamental-mode) ; Doom hard-coded this one.
;;     "Modes/languages for which we do not enable `format-all' on save.

;; Values in this list should be one of:
;;   - major-mode symbols
;;   - language strings from `language-id' package
;;   - language strings from `format-all' package

;; For language strings, see:
;;   - `format-all-default-formatters' or `format-all--language-id-buffer'
;;   - `language-id' package
;;     - `language-id--definitions'
;;     - https://raw.githubusercontent.com/github/linguist/master/lib/linguist/languages.yml

;; Originally from Doom's `+format-on-save-enabled-modes' in \"modules/editor/format/config.el\"."
;;     :group 'innit:group
;;     :type  '(set (string :tag "`language-id' string")
;;                  (symbol :tag "`major-mode' symbol")))

;;   (innit:hook:defun
;;       (:name   'format-all:enable?
;;        :docstr "Decide whether or not to enable auto-formatting on save.")

;;     (let ((language (format-all--language-id-buffer))) ; string, likely from `language-id-buffer'.
;;       ;;------------------------------
;;       ;; Don't Enable If...
;;       ;;------------------------------
;;       ;; No known language for buffer.
;;       (cond ((null language))

;;             ;; Explicitly disabled.
;;             ((or (seq-contains-p mantle:user:format-all:on-save/disabled language)
;;                  (seq-contains-p mantle:user:format-all:on-save/disabled major-mode)))

;;             ;; Hidden buffers probably don't need auto-formatting?
;;             ((string-prefix-p " " (buffer-name)))

;;             ;; Can we actually load `format-all'?
;;             ((not (require 'format-all nil :no-error)))

;;             ;; NOTE: Formatting with the LSP vs the `format-all' exe is decided
;;             ;; in the `format-all--probe' advice in the `:config' section.

;;             ;;------------------------------
;;             ;; Ok; Enable.
;;             ;;------------------------------
;;             (t
;;              (format-all-mode +1)))))

;;   ;;------------------------------
;;   :hook
;;   ;;------------------------------
;;   (after-change-major-mode-hook . mantle:hook:format-all:enable?)


;;   ;;------------------------------
;;   :config
;;   ;;------------------------------

;;   (define-advice format-all--probe (:around (fn &rest args) mantle:user:format-all:use-lsp?)
;;     "Get the formatter for the current buffer.

;; Answer questions like:
;;   - Should buffer even be formatted (e.g. read-only buffers)?
;;   - Should formatting be done with LSP formatter instead?"
;;     ;;------------------------------
;;     ;; Never Format
;;     ;;------------------------------
;;     (cond ((or buffer-read-only (eq +format-with :none))
;;            (list nil nil))

;;           ;;------------------------------
;;           ;; Use the LSP?
;;           ;;------------------------------
;;           ;; Always use the LSP if it has the capability to format?
;;           ;; If that's too optimistic, could make another filter var like
;;           ;; `mantle:user:format-all:on-save/disabled'.
;;           ((and (bound-and-true-p lsp-managed-mode)
;;                 (lsp-feature? "textDocument/formatting"))
;;            (list 'lsp nil))

;;           ((and (bound-and-true-p eglot--managed-mode)
;;                 (eglot--server-capable :documentFormattingProvider))

;;            (list 'eglot nil))

;;           ;;------------------------------
;;           ;; Actually let `format-all' decide:
;;           ;;------------------------------
;;           ((funcall fn))))

;;   ;; NOTE: Do not enable `format-all-mode' globally; it's optionally enabled in its hook.
;;   )


;;------------------------------------------------------------------------------
;; <{[( Parentheseses )]}>
;;------------------------------------------------------------------------------

;; https://github.com/Fanael/rainbow-delimiters
(use-package rainbow-delimiters

  ;;------------------------------
  ;; NOTE: Usage
  ;;------------------------------
  ;; Enable on a per-major-mode basis in a hook.
  ;; `use-package' Example:
  ;;   ;;------------------------------
  ;;   :hook
  ;;   ;;------------------------------
  ;;   (js-mode-hook . rainbow-delimiters-mode)

  ;;------------------------------
  :hook
  ;;------------------------------
  ((emacs-lisp-mode-hook . rainbow-delimiters-mode))


  ;;------------------------------
  :custom
  ;;------------------------------

  ;; `rainbow-delimiters-max-face-count'
  ;;------------------------------------
  ;; Helps us distinguish stacked delimiter pairs, especially in parentheses-drunk
  ;; languages like Lisp (default: 9).
  (rainbow-delimiters-max-face-count 4))


(use-package paren
  :ensure nil ; This is an Emacs built-in feature.


  ;;------------------------------
  :custom
  ;;------------------------------

  ;; Seconds to delay before showing the matching parenthesis.
  ;; (show-paren-delay 0.1)

  (show-paren-highlight-openparen     t)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide user config languages common)
