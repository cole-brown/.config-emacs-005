;;; core/modules/emacs/innit/package-upgrade-mode.el --- Package Upgrade Mode -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2023-07-19
;; Timestamp:  2023-09-27
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Package Upgrade Mode
;;
;;; Code:


(require 'rx)


;;--------------------------------------------------------------------------------
;; Settings
;;--------------------------------------------------------------------------------

(defcustom innit:package/upgrade:timestamp/rx
  (rx-to-string
   ;; yyyy-mm-dd
   `(sequence
     (= 4 digit)
     "-"
     (= 2 digit)
     "-"
     (= 2 digit)
     ;; date/time separator
     " "
     ;; HH:MM:SS
     (= 2 digit)
     ":"
     (= 2 digit)
     ":"
     (= 2 digit))
   :no-group)
  "Regex to match timestamp strings.

Must match strings formatted using `format-time-string' and
`innit:package/upgrade:timestamp/format'."
  :group 'innit:group
  :type 'string)


;;--------------------------------------------------------------------------------
;; Major Mode
;;--------------------------------------------------------------------------------

(defun int<innit>:package/mode:font-lock-keywords:highlight/create (matcher subexp facename &optional override laxmatch)
  "Create a 'MATCH-HIGHLIGHT' entry for the `font-lock-keywords' list.

See `font-lock-keywords' for full details.

This creates only the (MATCHER . HIGHLIGHT) form where HIGHLIGHT is
MATCH-HIGHLIGHT.
  - MATCH-HIGHLIGHT is of the form: (SUBEXP FACENAME [OVERRIDE [LAXMATCH]])

MATCHER should be a regex string.

SUBEXP should be a natnum.

FACENAME should be a font face symbol (e.g. from `defface' or
`list-faces-display'.

\(Optional) OVERRIDE should be:
  - nil       : Do not override existing fontifications.
  - t         : Existing fontifications can be overwritten.
  - `keep'    : Only parts not already fontified are highlighted.
  - `prepend' : Merge existing and new fontification; new takes precedence.
  - `append'  : Merge existing and new fontification; existing takes precedence.

\(Optional) LAXMATCH should be nil/non-nil. If LAXMATCH is non-nil, that means
don't signal an error if there is no match for SUBEXP in MATCHER."
  (cons
   ;; MATCHER: Regex to match.
   matcher

   ;; HIGHLIGHT: MATCH-HIGHLIGHT: (SUBEXP FACENAME [OVERRIDE [LAXMATCH]])
   (cons
    ;; SUBEXP: Desired match's group number from MATCHER regex
    subexp

    ;; Build this list so optional OVERRIDE & LAXMATCH don't appear when nil.
    (cons
     ;; FACENAME: Font Face to Use
     facename

     (when (or override laxmatch)
       (cons
        ;; (Optional) OVERRIDE Flag:
        override

        (when laxmatch
          ;; (Optional) LAXMATCH Flag:
          ;; If LAXMATCH is non-nil, that means don't signal an error if there is no match for SUBEXP in MATCHER.
          (list laxmatch))))))))
;; (int<innit>:package/mode:font-lock-keywords:highlight/create "foo" 0 'font-lock-comment-face nil t)

;; TODO: Make this function or delete
;; (defun int<innit>:package/mode:font-lock-keywords:anchored/create (matcher subexp facename &optional override laxmatch)
;;   "Create a 'MATCH-' entry for the `font-lock-keywords' list.

;; See `font-lock-keywords' for full details.

;; This creates only the (MATCHER . HIGHLIGHT) form where HIGHLIGHT is
;; MATCH-HIGHLIGHT.
;;   - MATCH-HIGHLIGHT is of the form: (SUBEXP FACENAME [OVERRIDE [LAXMATCH]])

;; MATCHER should be a regex string.

;; SUBEXP should be a natnum.

;; FACENAME should be a font face symbol (e.g. from `defface' or
;; `list-faces-display'.

;; \(Optional) OVERRIDE should be:
;;   - nil       : Do not override existing fontifications.
;;   - t         : Existing fontifications can be overwritten.
;;   - `keep'    : Only parts not already fontified are highlighted.
;;   - `prepend' : Merge existing and new fontification; new takes precedence.
;;   - `append'  : Merge existing and new fontification; existing takes precedence.

;; \(Optional) LAXMATCH should be nil/non-nil. If LAXMATCH is non-nil, that means
;; don't signal an error if there is no match for SUBEXP in MATCHER.

;; TODO NEW DOCSTR FROM:
;; MATCH-ANCHORED should be of the form:

;;  (MATCHER PRE-MATCH-FORM POST-MATCH-FORM MATCH-HIGHLIGHT ...)

;; where MATCHER is a regexp to search for or the function name to
;; call to make the search, as for MATCH-HIGHLIGHT above, but with
;; one exception; see below.  PRE-MATCH-FORM and POST-MATCH-FORM are
;; evaluated before the first, and after the last, instance
;; MATCH-ANCHORED's MATCHER is used.  Therefore they can be used to
;; initialize before, and cleanup after, MATCHER is used.
;; Typically, PRE-MATCH-FORM is used to move to some position
;; relative to the original MATCHER, before starting with
;; MATCH-ANCHORED's MATCHER.  POST-MATCH-FORM might be used to move
;; back, before resuming with MATCH-ANCHORED's parent's MATCHER.

;; For example, an element of the form highlights (if not already
;; highlighted):

;;  (\"\\\\\\=<anchor\\\\\\=>\" (0 anchor-face)
;;   (\"\\\\\\=<item\\\\\\=>\" nil nil (0 item-face)))

;;   Discrete occurrences of \"anchor\" in the value of
;;   anchor-face, and subsequent discrete occurrences of
;;   \"item\" (on the same line) in the value of item-face.
;;   (Here PRE-MATCH-FORM and POST-MATCH-FORM are nil.  Therefore
;;   \"item\" is initially searched for starting from the end of the
;;   match of \"anchor\", and searching for subsequent instances of
;;   \"anchor\" resumes from where searching for \"item\" concluded.)

;; The above-mentioned exception is as follows.  The limit of the
;; MATCHER search defaults to the end of the line after
;; PRE-MATCH-FORM is evaluated.  However, if PRE-MATCH-FORM returns
;; a position greater than the position after PRE-MATCH-FORM is
;; evaluated, that position is used as the limit of the search.  It
;; is generally a bad idea to return a position greater than the end
;; of the line, i.e., cause the MATCHER search to span lines."
;;   '(;; matcher
;;     "\\\\\\=<anchor\\\\\\=>"
;;     ;; pre-match-form
;;     (0 anchor-face) ;; (subexp facename)? == match-highlight?
;;     ;; post-match-form
;;     (\"\\\\\\=<item\\\\\\=>\" ;; matcher
;;      nil ;; pre-match-form?
;;      nil ;; post-match-form?
;;      (0 item-face) ;; match-highlight
;;      )
;;     )
;;   (cons
;;    ;; MATCHER: Regex to match.
;;    matcher

;;    ;; HIGHLIGHT: MATCH-HIGHLIGHT: (SUBEXP FACENAME [OVERRIDE [LAXMATCH]])
;;    (cons
;;     ;; SUBEXP: Desired match's group number from MATCHER regex
;;     subexp

;;     ;; Build this list so optional OVERRIDE & LAXMATCH don't appear when nil.
;;     (cons
;;      ;; FACENAME: Font Face to Use
;;      facename

;;      (when (or override laxmatch)
;;        (cons
;;         ;; (Optional) OVERRIDE Flag:
;;         override

;;         (when laxmatch
;;           ;; (Optional) LAXMATCH Flag:
;;           ;; If LAXMATCH is non-nil, that means don't signal an error if there is no match for SUBEXP in MATCHER.
;;           (list laxmatch))))))))
;; ;; (int<innit>:package/mode:font-lock-keywords:anchored/create "foo" 0 'font-lock-comment-face nil t)



(defconst int<innit>:package/mode:font-lock-keywords
  (let (flk ; The font lock keywords list that will become this const.

        ;;------------------------------
        ;; Unicode Box Drawing Characters
        ;;------------------------------
        ;; Use the whole range?
        ;;   - https://www.w3.org/TR/xml-entity-names/025.html
        ;;   - U+02500 - U+025FF
        ;;     - Includes:
        ;;       - Box Drawing
        ;;       - Block Elements
        ;;       - Geometric Shapes
        ;;   - U+02500-U+0257F
        ;;     - Just Box Drawing
        ;;       - Single line, heavy line, double line, dashed, etc. All there.
        (rx/unicode-box (rx-to-string `(one-or-more (any (#x2500 . #x0257F)))
                                      :no-group))

        ;; (let ((str      "───"))
        ;;   (string-match (rx-to-string `(one-or-more (any (#x2500 . #x0257F)))
        ;;                               :no-group)
        ;;                 str)
        ;;   (match-string 0 str))

        ;;------------------------------
        ;; Statuses
        ;;------------------------------
        (rx/status (rx-to-string `(group
                                   "["
                                   (one-or-more
                                    (group (one-or-more alphabetic))
                                    (optional ":"))
                                   "]")
                                 :no-group))
        (rx/status/delimiters (rx-to-string `(and
                                              "["
                                              (group
                                               (one-or-more
                                                (one-or-more alphabetic)
                                                (optional ":")))
                                              "]")
                                            :no-group))
        (rx/status/words (rx-to-string `(and
                                         "["
                                         (group
                                          (one-or-more
                                           (one-or-more alphabetic)
                                           (optional ":")))
                                         "]")
                                       :no-group))

        ;; ;;------------------------------
        ;; ;; Words and Things
        ;; ;;------------------------------
        ;; ;; Currently have "loading" and "skip", but use more versions of both in
        ;; ;; case we normalize/change the tenses...
        ;; (rx/status/info    (rx-to-string `(or "load" "loading"  "loaded")  :no-group))
        ;; (rx/status/warning (rx-to-string `(or "skip" "skipping" "skipped") :no-group))
        ;; (rx/reason/warning (rx-to-string `(and ,innit:package:reason
        ;;                                        (one-or-more any)
        ;;                                        line-end)
        ;;                                  :no-group))

        ;; ;; (amounts '("second" "seconds"
        ;; ;;            "minute" "minutes"
        ;; ;;            "hour"   "hours"))
        ;; (rx/duration (rx-to-string `(and (group
        ;;                                   (one-or-more digit)
        ;;                                   "."
        ;;                                   (one-or-more digit))
        ;;                                  " "
        ;;                                  (group
        ;;                                   (or "second" "seconds"
        ;;                                       "minute" "minutes"
        ;;                                       "hour"   "hours")))
        ;;                            :no-group))

        ;; ;;------------------------------
        ;; ;; Feature Names
        ;; ;;------------------------------
        ;; (rx/feature (rx-to-string `(and ":"
        ;;                                 (one-or-more
        ;;                                  ;; Emacs Lisp is super permissive about what a symbol name can be...
        ;;                                  ;; How permissive should we be about what an Innit Feature Name should be?
        ;;                                  ;; NOTE: Similar but different from `int<innit>:feature:replace:rx'.
        ;;                                  ;; Careful how this goes or you lose the question mark...
        ;;                                  (any "?" ":" "-" "_" "/" "."
        ;;                                       alphanumeric)))
        ;;                           :no-group))

        ;; ;;------------------------------
        ;; ;; Final Status
        ;; ;;------------------------------
        ;; ;; Not currently printed to 'innit:package' buffer?
        )

    ;; NOTE: `M-x list-faces-display' and go to 'font-lock-' to see all the
    ;; pre-defined font-lock faces.

    ;; NOTE: low -> high priority
    ;; `flk' is ordered high-priority first, so when building it here,
    ;; lower priority are pushed first.

    ;;------------------------------
    ;; Statuses
    ;;------------------------------
    (push (int<innit>:package/mode:font-lock-keywords:highlight/create
           (rx-to-string `(or "[" ":" "]")
                         :no-group)
           0
           'font-lock-comment-delimiter-face
           nil
           nil)
          flk)

    (push (int<innit>:package/mode:font-lock-keywords:highlight/create
           "OK"
           0
           ;; 'success ;; `success' face doesn't work? Just use a font-lock face?
           'font-lock-comment-face
           t
           t)
          flk)
    (push (int<innit>:package/mode:font-lock-keywords:highlight/create
           (rx-to-string `(or "ERROR" "FAILURE")
                         :no-group)
           0
           'font-lock-string-face ;; In theme `zenburn', this is dark red.
           ;; 'font-lock-warning-face ;; In theme `zenburn', this is dark goldish.
           t)
          flk)

    ;; HIGHEST-PRIORITY STATUS!
    (push (int<innit>:package/mode:font-lock-keywords:highlight/create
           "DRY"
           0
           'font-lock-warning-face
           t)
          flk)

    ;;------------------------------
    ;; Unicode Box Drawing Characters
    ;;------------------------------
    (push (int<innit>:package/mode:font-lock-keywords:highlight/create
           rx/unicode-box ; Match Regex
           0 ; Match's Group Number from Match Regex
           'font-lock-comment-delimiter-face
           t
           t)
          flk)

    ;;------------------------------
    ;; Lists
    ;;------------------------------
    (push (int<innit>:package/mode:font-lock-keywords:highlight/create
           (rx-to-string `(and
                           line-start
                           (one-or-more " ")
                           (group
                            (one-or-more digit)
                            ".")
                           " ")
                         :no-group)
           1
           'font-lock-comment-delimiter-face
           nil
           nil)
          flk)

    ;;------------------------------
    ;; Package Names
    ;;------------------------------
    (push (int<innit>:package/mode:font-lock-keywords:highlight/create
           (rx-to-string `(and
                           "`"
                           (group (one-or-more
                                   (one-or-more alphanumeric)
                                   (optional "-")))
                           "-"
                           (group (one-or-more
                                   (one-or-more digit)
                                   (optional ".")))
                           )
                         :no-group)
           1
           'font-lock-string-face
           nil
           nil)
          flk)
    (push (int<innit>:package/mode:font-lock-keywords:highlight/create
           (rx-to-string `(and
                           "`"
                           (group (one-or-more
                                   (one-or-more alphanumeric)
                                   (optional "-")))
                           "-"
                           (group (one-or-more
                                   (one-or-more digit)
                                   (optional ".")))
                           )
                         :no-group)
           2
           'font-lock-type-face
           nil
           nil)
          flk)

    ;;------------------------------
    ;; Intro Lines
    ;;------------------------------
    (push (int<innit>:package/mode:font-lock-keywords:highlight/create
           "Upgrade Installed Emacs Packages"
           0
           'font-lock-preprocessor-face
           nil
           nil)
          flk)
    (push (int<innit>:package/mode:font-lock-keywords:highlight/create
           innit:package/upgrade:timestamp/rx
           0
           'font-lock-preprocessor-face
           nil
           nil)
          flk)

    ;;------------------------------
    ;; Sections
    ;;------------------------------
    ;; Getting a bit too colored?
    ;; Also this didn't cover "[DRY] Upgrading Packages..."
    ;; (push (int<innit>:package/mode:font-lock-keywords:highlight/create
    ;;        (rx-to-string `(and
    ;;                        line-start
    ;;                        (or "Refreshing package metadata..."
    ;;                           "Checking for packages to upgrade..."
    ;;                           (and "Upgradable Package"
    ;;                                (optional "s")
    ;;                                ":")
    ;;                           (and "Upgrade "
    ;;                                (one-or-more digit)
    ;;                                " Package"
    ;;                                (optional "s")
    ;;                                "...")
    ;;                           (and "Upgrading Package"
    ;;                                (optional "s")
    ;;                                "...")
    ;;                           )
    ;;                        line-end)
    ;;                      :no-group)
    ;;        0
    ;;        'font-lock-function-name-face
    ;;        nil
    ;;        nil)
    ;;       flk)

    ;;------------------------------
    ;; Done; Return Font Lock Keywords
    ;;------------------------------
    flk)
  "Syntax keywords for `innit-package-upgrade-mode' buffer.

See `font-lock-keywords' for various formatting options of these entries.
Each element in a user-level keywords list should have one of these forms:
  - MATCHER
  - (MATCHER . SUBEXP)
  - (MATCHER . FACENAME)
  - (MATCHER . HIGHLIGHT)
  - (MATCHER HIGHLIGHT ...)
  - (eval . FORM)")
;; (pp int<innit>:package/mode:font-lock-keywords)


(defconst int<innit>:package/mode:font-lock-defaults
  ;; Format for list:
  ;;  (KEYWORDS [KEYWORDS-ONLY [CASE-FOLD [SYNTAX-ALIST ...]]])
  ;; See for more info:
  ;;   - `font-lock-defaults'
  ;;   - http://xahlee.info/emacs/emacs/elisp_font_lock_mode.html
  (list
   ;;------------------------------
   ;; KEYWORDS
   ;;------------------------------
   ;; See `font-lock-keywords' for various formatting options of these entries.
   int<innit>:package/mode:font-lock-keywords

   ;;------------------------------
   ;; KEYWORDS-ONLY?
   ;;------------------------------
   ;; NOTE: "If KEYWORDS-ONLY is non-nil, syntactic fontification (strings and comments) is not performed."
   nil
   ;; (list
   ;;  ;; See `font-lock-keywords-only' for various formatting options of these entries.
   ;;  )

   ;;------------------------------
   ;; CASE-FOLD?
   ;;------------------------------
   ;; See also `font-lock-keywords-case-fold-search'.
   nil

   ;;------------------------------
   ;; SYNTAX-ALIST
   ;;------------------------------
   ;; See also `font-lock-syntax-table'.
   ;; Why use this when you have `:syntax-table' in `define-derived-mode'?
   nil

   ;;------------------------------
   ;; The Rest: (VARIABLE . VALUE)
   ;;------------------------------
   ;; None.
   )
  "Used by the mode to set the buffer local `font-lock-defaults'.

See help for `font-lock-defaults' for what all this does/can do.")


;; Just use expected name instead of my... unique naming conventions.
(defvar innit-package-upgrade-mode-map
  (let ((map (make-sparse-keymap)))
    ;; This will be set innitlictly by `define-derived-mode' if not set explictly.
    (set-keymap-parent map special-mode-map)

    ;;------------------------------
    ;; Unbind
    ;;------------------------------
    ;; "g" is `revert-buffer' by default (inherited from `special-mode-map')
    ;; but is often used as "refresh buffer" (e.g. `deadgrep-mode' binds it to `deadgrep-restart').
    ;; `innit-package-upgrade-mode' doesn't really have a "refresh", since it's just historical.
    (define-key map (kbd "g") nil) ; unbind `revert-buffer'

    ;;------------------------------
    ;; Bind
    ;;------------------------------
    ;; (define-key map (kbd "n") #'next-line)

    map)
  "Mode map derived from parent explicitly.")


;;;###autoload
(define-derived-mode innit-package-upgrade-mode special-mode "innit:package:upgrade"
  "Major mode for output of the `innit:cmd:package:upgrade' command.

Derive from `special-mode' as a start:
https://www.gnu.org/software/emacs/manual/html_node/elisp/Basic-Major-Modes.html"
  ;;------------------------------
  ;; KEYWORD ARGS:
  ;;------------------------------
  ;; Non-Standard Variable Names?
  ;;---
  ;; If we had a syntax table, and if it's not named `<mode>-syntax-table'
  ;; (`innit-package-upgrade-mode-syntax-table'), say so:
  ;; :syntax-table int<innit>:package/mode:syntax-table
  ;; Don't want an `abbrev-table' as we're a read-only mode so it's a bit
  ;; pointless, but same thing as the `:syntax-table' arg.
  ;; :abbrev-table int<innit>:package/mode:abbrev-table

  ;;------------------------------
  ;; BODY
  ;;------------------------------

  ;;---
  ;; Fontification / Colorization / Syntax Highlighting
  ;;---
  (setq font-lock-defaults int<innit>:package/mode:font-lock-defaults)

  ;;---
  ;; Other Things
  ;;---
  ;; Might be nice to tail the buffer?
  ;; Force the buffer to be tailed; users can hook into this mode and disable if desired.
  ;(require 'autorevert)
  ;(auto-revert-tail-mode +1)
  ; Only works for file buffers...
  )


;; NOTE: We aren't a file-based mode, so we don't register ourselves for anything in `auto-mode-alist'.
;; ;;;###autoload
;; (add-to-list 'auto-mode-alist '("\\.fileextension" . innit-package-upgrade-mode))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :innit 'package 'upgrade 'mode)
