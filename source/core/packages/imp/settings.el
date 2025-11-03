;;; imp/fundamental.el --- string helpers, etc -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2025-04-16
;; Timestamp:  2025-11-03
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;                                 ──────────
;; ╔════════════════════════════════════════════════════════════════════════╗
;; ║                                Settings                                ║
;; ╚════════════════════════════════════════════════════════════════════════╝
;;                                   ──────
;;                                 defcustoms
;;                                 ──────────
;;
;;; Code:


;;------------------------------------------------------------------------------
;; THE GROUP!
;;------------------------------------------------------------------------------

(defgroup imp nil
  "A `imp' declaration for simplifying your `.emacs'."
  :group 'initialization
  ;; :link '(custom-manual "(imp) Top")
  :version "30.2")

;; Below here: the groupies.


;;------------------------------------------------------------------------------
;; `imp' Macro Settings
;;------------------------------------------------------------------------------

(defcustom imp-parser-keywords
  '(:disabled
    ;; TODO(stats): uncomment
    ;; :stats :statistics
    :path
    :root
    :error
    :optional
    :if :when :unless
    :after
    :requires
    ;; NOTE: file load is currently parsed/handled after all keywords.
    )
  "The set of valid keywords, in the order they are processed in.
The order of this list is *very important*, so it is only
advisable to insert new keywords, never to delete or reorder
them.  Further, attention should be paid to the NEWS.md if the
default order ever changes, as they may have subtle effects on
the semantics of `imp' declarations and may necessitate
changing where you had inserted a new keyword earlier.

NOTE: `:disabled' is special in this list, as it causes
nothing at all to happen, even if the rest of the `imp'
declaration is incorrect."
  :type '(repeat symbol)
  :group 'imp)

(defcustom imp-parser-ignore-unknown-keywords nil
  "If non-nil, warn instead of signaling error for unknown keywords.
The unknown keyword and its associated arguments will be ignored
in the `imp' expansion."
  :type 'boolean
  :group 'imp)

;; TODO: use imp's debugging flag? merge with it?
(defcustom imp-parser-verbose nil
  "Whether to report about loading and configuration details.
If you customize this, then you should require the `imp'
feature in files that use `imp', even if these files only
contain compiled expansions of the macros.  If you don't do so,
then the expanded macros do their job silently."
  :type '(choice (const :tag "Quiet, without catching errors" errors)
                 (const :tag "Quiet" nil)
                 (const :tag "Verbose" t)
                 (const :tag "Debug" debug))
  :group 'imp)

(defcustom imp-parser-defaults
  '(;; (KEYWORD DEFAULT-VALUE USAGE-PREDICATE)
    ;; TODO(stats): uncomment
    ;; (:stats t t)
    (:path
     (lambda (feature args) (imp-parser-normalize/:path feature :path nil))
     (lambda (feature args) (not (plist-member args :path))))
    (:error    t   t)
    (:optional nil t))
  "Default values for specified `imp' keywords.
Each entry in the alist is a list of three elements:
The first element is the `imp' keyword.

The second is a form that can be evaluated to get the default
value.  It can also be a function that will receive the FEATURE from
the `imp' declaration and the keyword plist given to
`imp', in normalized form.  The value it returns should
also be in normalized form (which is sometimes *not* what one
would normally write in a `imp' declaration, so use
caution).

The third element is a form that can be evaluated to determine
whether or not to assign a default value; if it evaluates to nil,
then the default value is not assigned even if the keyword is not
present in the `imp' form.  This third element may also be
a function, in which case it receives the FEATURE from `imp' (as
a symbol) and a list of keywords (in normalized form).  It should
return nil or non-nil depending on whether defaulting should be
attempted."
  :type `(repeat
          (list (choice :tag "Keyword"
                        ,@(mapcar #'(lambda (k) (list 'const k))
                                  imp-parser-keywords))
                (choice :tag "Default value" sexp function)
                (choice :tag "Enable if non-nil" sexp function)))
  :group 'imp)

(defcustom imp-parser-merge-key-alist
  '((:if    . (lambda (new old) `(and ,new ,old)))
    (:after . (lambda (new old) `(:all ,new ,old))))
  "Alist of keys and the functions used to merge multiple values.
For example, if the following form is provided:

  (imp foo :if pred1 :if pred2)

Then based on the above defaults, the merged result will be:

  (imp foo :if (and pred1 pred2))

This is done so that, at the stage of invoking handlers, each
handler is called only once."
  :type `(repeat
          (cons (choice :tag "Keyword"
                        ,@(mapcar #'(lambda (k) (list 'const k))
                                  imp-parser-keywords)
                        (const :tag "Any" t))
                function))
  :group 'imp)

;; TODO(stats): use any of predecessor's stats?
;; (defcustom imp-parser-minimum-reported-time 0.1
;;   "Minimal load time that will be reported.
;; Note that `imp-parser-verbose' has to be set to a non-nil value
;; for anything to be reported at all."
;;   :type 'number
;;   :group 'imp)

;; TODO(stats): delete if we don't end up using this.
(defcustom imp-parser-expand-minimally nil
  "If non-nil, make the expanded code as minimal as possible.
This disables:

  - Printing to the *Messages* buffer of slowly-evaluating forms
  - Capturing of load errors (normally redisplayed as warnings)
  - Conditional loading of packages (load failures become errors)

The main advantage to this variable is that, if you know your
configuration works, it will make the byte-compiled file as
minimal as possible.  It can also help with reading macro-expanded
definitions, to understand the main intent of what's happening."
  :type 'boolean
  :group 'imp)

;; TODO: figure out wtf this regex soup is doing & adapt to imp?
;; (defconst imp-parser-font-lock-keywords
;;   '(("(\\(imp\\)\\_>[ \t']*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
;;      (1 font-lock-keyword-face)
;;      (2 font-lock-constant-face nil t))))
;;
;; (font-lock-add-keywords 'emacs-lisp-mode imp-parser-font-lock-keywords)


;;------------------------------------------------------------------------------
;; Output (errors, warnings, messages...)
;;------------------------------------------------------------------------------

(defcustom imp-output-buffer "*imp-log*"
  "Name of the output buffer used by `imp--output-sink'."
  :group 'imp
  :type '(string))


(defcustom imp-output-level
  '((:error      . (:display "ERROR"
                    :sink (error imp--output-sink)))
    ;; (:error:user . (:display "ERROR:user"
    ;;                 :sink (user-error imp--output-sink)))
    (:warning    . (:display "Warning"
                    :sink (warn imp--output-sink)))
    (:info       . (:display "info"
                    :sink (message imp--output-sink)))
    (:debug      . (:display "debug"
                    :align right ; default/nil: left
                    ;; :sink message
                    :sink (message imp--output-sink)))

    ;; No prefix.
    (:blank . (;; :sink message
               :sink imp--output-sink)))
  "Output message level (:debug, :error, etc) settings."
  :group 'imp
  :type '(alist :key-type symbol
                :value-type plist))


(defcustom imp-output-features-buffer
  "*imp-features*"
  "Name of the buffer for `imp-cmd-features-print' to output a pretty-printed tree
of the features imp has provided."
  :group 'imp)


;;------------------------------------------------------------------------------
;; Use-Package Integration
;;------------------------------------------------------------------------------

(defcustom use-package-always-imp t
  "Treat every package as though it had specified using `:imp SEXP'.
See also `use-package-defaults', which uses this value."
  :group 'imp
  :type 'sexp)


;;------------------------------------------------------------------------------
;; Timing
;;------------------------------------------------------------------------------

;; TODO(stats): use any of predecessor's stats?
;; (defcustom imp-parser-compute-statistics nil
;;   "If non-nil, compute statistics concerned `imp-parser' declarations.
;; View the statistical report using `imp-parser-report'.  Note that
;; if this option is enabled, you must require `imp' in your
;; user init file at loadup time, or you will see errors concerning
;; undefined variables."
;;   :type 'boolean
;;   :group 'imp)


(defcustom imp-timing-enabled? t
  "Should loading & timing messages be printed?"
  :group 'imp
  :type '(boolean))


(defcustom imp-timing-buffer
  ;; "ⓘ-imp-timing-ⓘ"
  "*imp-statistics*"
  "Buffer name to print to.

If you want it to go to *Messages* with the usual minibuffer interaction, set
to: `:messages'."
  :group 'imp
  :type '(choice (string :tag "Name of Buffer")
                 (const :tag "Use `:messages' to send to *Messages* buffer with the usual minibuffer interactions."
                        :messages)))


(defcustom imp-timing-buffer-tail? t
  "Should the timing buffer be tailed?"
  :group 'imp
  :type '(boolean))


(defcustom imp-timing-buffer-show-auto? t
  "If non-nil, show `imp-timing-buffer' every time it gets a new message."
  :group 'imp
  :type  '(boolean))
