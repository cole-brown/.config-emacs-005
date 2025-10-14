;;; imp/parser.el --- Plist Parsing -*- lexical-binding: t; -*-
;;
;; Author:     John Wiegley <johnw@newartisans.com>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2025-09-22
;; Timestamp:  2025-10-14
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Flagrantly stolen from `use-package-core.el',
;;  - https://github.com/jwiegley/use-package/blob/cba23de38d0a0c361f591be450d97dfc277469d3/use-package-core.el
;; And lovingly beaten to death.
;;   (apologies to John Wiegley)

;; This file contains the core implementation of the `use-package'
;; macro.
;;
;; See the `use-package' info manual for more information.

;;; Code:

;;(require 'bytecomp)
(require 'cl-lib)
;;(require 'tabulated-list)

;; (eval-and-compile
;;   ;; Declare a synthetic theme for :custom variables.
;;   ;; Necessary in order to avoid having those variables saved by custom.el.
;;   (deftheme imp-parser))

;; (enable-theme 'imp-parser)
;; ;; Remove the synthetic imp-parser theme from the enabled themes, so
;; ;; iterating over them to "disable all themes" won't disable it.
;; (setq custom-enabled-themes (remq 'imp-parser custom-enabled-themes))

;; (eval-when-compile
;;   (if (and (eq emacs-major-version 24) (eq emacs-minor-version 3))
;;       (progn
;;         (defsubst hash-table-keys (hash-table)
;;           "Return a list of keys in HASH-TABLE."
;;           (cl-loop for k being the hash-keys of hash-table collect k))
;;         (defsubst string-suffix-p (suffix string  &optional ignore-case)
;;           (let ((start-pos (- (length string) (length suffix))))
;;             (and (>= start-pos 0)
;;                  (eq t (compare-strings suffix nil nil
;;                                         string start-pos nil ignore-case))))))
;;     (require 'subr-x)))

;; (eval-when-compile
;;   (require 'regexp-opt))

;; (defgroup imp-parser nil
;;   "A `imp-parser' declaration for simplifying your `.emacs'."
;;   :group 'initialization
;;   :link '(custom-manual "(imp-parser) Top")
;;   :version "29.1")

;; (defconst imp-parser-version "2.4.4"
;;   "This version of `imp-parser'.")

(defcustom imp-parser-keywords
  '(:disabled
    :error
    :optional
    :requires
    :if :when :unless
    :path
    :after)
  "The set of valid keywords, in the order they are processed in.
The order of this list is *very important*, so it is only
advisable to insert new keywords, never to delete or reorder
them.  Further, attention should be paid to the NEWS.md if the
default order ever changes, as they may have subtle effects on
the semantics of `imp-parser' declarations and may necessitate
changing where you had inserted a new keyword earlier.

Note that `:disabled' is special in this list, as it causes
nothing at all to happen, even if the rest of the `imp-parser'
declaration is incorrect."
  :type '(repeat symbol)
  :group 'imp-parser)

;; (defcustom imp-parser-deferring-keywords
;;   '(:bind-keymap
;;     :bind-keymap*
;;     :commands
;;     :autoload)
;;   "Unless `:demand' is used, keywords in this list imply deferred loading.
;; The reason keywords like `:hook' are not in this list is that
;; they only imply deferred loading if they reference actual
;; function symbols that can be autoloaded from the module; whereas
;; the default keywords provided here always defer loading unless
;; otherwise requested."
;;   :type '(repeat symbol)
;;   :group 'imp-parser)

(defcustom imp-parser-ignore-unknown-keywords nil
  "If non-nil, warn instead of signaling error for unknown keywords.
The unknown keyword and its associated arguments will be ignored
in the `imp-parser' expansion."
  :type 'boolean
  :group 'imp-parser)

;; (defcustom imp-parser-use-theme t
;;   "If non-nil, use a custom theme to avoid saving :custom
;; variables twice (once in the Custom file, once in the imp-parser
;; call)."
;;   :type 'boolean
;;   :group 'imp-parser)

(defcustom imp-parser-verbose nil
  "Whether to report about loading and configuration details.
If you customize this, then you should require the `imp-parser'
feature in files that use `imp-parser', even if these files only
contain compiled expansions of the macros.  If you don't do so,
then the expanded macros do their job silently."
  :type '(choice (const :tag "Quiet, without catching errors" errors)
                 (const :tag "Quiet" nil)
                 (const :tag "Verbose" t)
                 (const :tag "Debug" debug))
  :group 'imp-parser)

;; (defcustom imp-parser-check-before-init nil
;;   "If non-nil, check that package exists before executing its `:init' block.
;; This check is performed by calling `locate-library'."
;;   :type 'boolean
;;   :group 'imp-parser)

;; (defcustom imp-parser-always-defer nil
;;   "If non-nil, assume `:defer t' unless `:demand' is used.
;; See also `imp-parser-defaults', which uses this value."
;;   :type 'boolean
;;   :group 'imp-parser)

;; (defcustom imp-parser-always-demand nil
;;   "If non-nil, assume `:demand t' unless `:defer' is used.
;; See also `imp-parser-defaults', which uses this value."
;;   :type 'boolean
;;   :group 'imp-parser)

(defcustom imp-parser-defaults
  '(;; (KEYWORD DEFAULT-VALUE USAGE-PREDICATE)
    ;;(:error t t)
    ;;(:optional nil t)
    ;;(:path 'guess (lambda (name args) (not (plist-member args :path))))
    )
  "Default values for specified `imp-parser' keywords.
Each entry in the alist is a list of three elements:
The first element is the `imp-parser' keyword.

The second is a form that can be evaluated to get the default
value.  It can also be a function that will receive the name of
the `imp-parser' declaration and the keyword plist given to
`imp-parser', in normalized form.  The value it returns should
also be in normalized form (which is sometimes *not* what one
would normally write in a `imp-parser' declaration, so use
caution).

The third element is a form that can be evaluated to determine
whether or not to assign a default value; if it evaluates to nil,
then the default value is not assigned even if the keyword is not
present in the `imp-parser' form.  This third element may also be
a function, in which case it receives the name of the package (as
a symbol) and a list of keywords (in normalized form).  It should
return nil or non-nil depending on whether defaulting should be
attempted."
  :type `(repeat
          (list (choice :tag "Keyword"
                        ,@(mapcar #'(lambda (k) (list 'const k))
                                  imp-parser-keywords))
                (choice :tag "Default value" sexp function)
                (choice :tag "Enable if non-nil" sexp function)))
  :group 'imp-parser)

;; (defcustom imp-parser-merge-key-alist
;;   '((:if    . (lambda (new old) `(and ,new ,old)))
;;     (:after . (lambda (new old) `(:all ,new ,old)))
;;     (:defer . (lambda (new old) old))
;;     (:bind  . (lambda (new old) (append new (list :break) old))))
;;   "Alist of keys and the functions used to merge multiple values.
;; For example, if the following form is provided:

;;   (imp-parser foo :if pred1 :if pred2)

;; Then based on the above defaults, the merged result will be:

;;   (imp-parser foo :if (and pred1 pred2))

;; This is done so that, at the stage of invoking handlers, each
;; handler is called only once."
;;   :type `(repeat
;;           (cons (choice :tag "Keyword"
;;                         ,@(mapcar #'(lambda (k) (list 'const k))
;;                                   imp-parser-keywords)
;;                         (const :tag "Any" t))
;;                 function))
;;   :group 'imp-parser)

;; (defcustom imp-parser-hook-name-suffix "-hook"
;;   "Text append to the name of hooks mentioned by :hook.
;; Set to nil if you don't want this to happen; it's only a
;; convenience."
;;   :type '(choice string (const :tag "No suffix" nil))
;;   :group 'imp-parser)

;; (defcustom imp-parser-minimum-reported-time 0.1
;;   "Minimal load time that will be reported.
;; Note that `imp-parser-verbose' has to be set to a non-nil value
;; for anything to be reported at all."
;;   :type 'number
;;   :group 'imp-parser)

;; (defcustom imp-parser-inject-hooks nil
;;   "If non-nil, add hooks to the `:init' and `:config' sections.
;; In particular, for a given package `foo', the following hooks
;; become available:

;;   `imp-parser--foo--pre-init-hook'
;;   `imp-parser--foo--post-init-hook'
;;   `imp-parser--foo--pre-config-hook'
;;   `imp-parser--foo--post-config-hook'

;; This way, you can add to these hooks before evaluation of a
;; `imp-parser` declaration, and exercise some control over what
;; happens.

;; NOTE: These hooks are run even if the user does not specify an
;; `:init' or `:config' block, and they will happen at the regular
;; time when initialization and configuration would have been
;; performed.

;; NOTE: If the `pre-init' hook return a nil value, that block's
;; user-supplied configuration is not evaluated, so be certain to
;; return t if you only wish to add behavior to what the user had
;; specified."
;;   :type 'boolean
;;   :group 'imp-parser)

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
  :group 'imp-parser)

;; (defcustom imp-parser-form-regexp-eval
;;   `(concat ,(eval-when-compile
;;               (concat "^\\s-*("
;;                       (regexp-opt '("imp-parser" "require") t)
;;                       "\\s-+\\("))
;;            (or (bound-and-true-p lisp-mode-symbol-regexp)
;;                "\\(?:\\sw\\|\\s_\\|\\\\.\\)+") "\\)")
;;   "Sexp providing regexp for finding `imp-parser' forms in user files.
;; This is used by `imp-parser-jump-to-package-form' and
;; `imp-parser-enable-imenu-support'."
;;   :type 'sexp
;;   :group 'imp-parser)

;; (defcustom imp-parser-enable-imenu-support nil
;;   "If non-nil, cause imenu to see `imp-parser' declarations.
;; This is done by adjusting `lisp-imenu-generic-expression' to
;; include support for finding `imp-parser' and `require' forms.

;; Must be set before loading `imp-parser'."
;;   :type 'boolean
;;   :set
;;   #'(lambda (sym value)
;;       (eval-after-load 'lisp-mode
;;         (if value
;;             `(add-to-list 'lisp-imenu-generic-expression
;;                           (list "Packages" ,imp-parser-form-regexp-eval 2))
;;           `(setq lisp-imenu-generic-expression
;;                  (remove (list "Packages" ,imp-parser-form-regexp-eval 2)
;;                          lisp-imenu-generic-expression))))
;;       (set-default sym value))
;;   :group 'imp-parser)

;; (defconst imp-parser-font-lock-keywords
;;   '(("(\\(imp-parser\\)\\_>[ \t']*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
;;      (1 font-lock-keyword-face)
;;      (2 font-lock-constant-face nil t))))

;; (font-lock-add-keywords 'emacs-lisp-mode imp-parser-font-lock-keywords)

(defcustom imp-parser-compute-statistics nil
  "If non-nil, compute statistics concerned `imp-parser' declarations.
View the statistical report using `imp-parser-report'.  Note that
if this option is enabled, you must require `imp-parser' in your
user init file at loadup time, or you will see errors concerning
undefined variables."
  :type 'boolean
  :group 'imp-parser)

(defvar imp-parser-statistics (make-hash-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Utility functions
;;

(defsubst imp-parser-error (msg)
  "Report MSG as an error, so the user knows it came from this package."
  (error "imp-parser: %s" msg))

(defsubst imp-parser-concat (&rest elems)
  "Delete all empty lists from ELEMS (nil or (list nil)), and append them."
  (apply #'append (delete nil (delete (list nil) elems))))

(defsubst imp-parser-non-nil-symbolp (sym)
  "`nil' doesn't count as a symbol"
  (and sym (symbolp sym)))

(defsubst imp-parser-as-symbol (string-or-symbol)
  "If STRING-OR-SYMBOL is already a symbol, return it.
Otherwise convert it to a symbol and return that."
  (if (symbolp string-or-symbol) string-or-symbol
    (intern string-or-symbol)))

(defsubst imp-parser-as-string (string-or-symbol)
  "If STRING-OR-SYMBOL is already a string, return it.
Otherwise convert it to a string and return that."
  (if (stringp string-or-symbol) string-or-symbol
    (symbol-name string-or-symbol)))

;; (defsubst imp-parser-regex-p (re)
;;   "Return t if RE is some regexp-like thing."
;;   (or (and (listp re) (eq (car re) 'rx))
;;       (stringp re)))

;; (defun imp-parser-normalize-regex (re)
;;   "Given some regexp-like thing in RE, resolve to a regular expression."
;;   (cond
;;    ((and (listp re) (eq (car re) 'rx)) (eval re))
;;    ((stringp re) re)
;;    (t (error "Not recognized as regular expression: %s" re))))

;; (defsubst imp-parser-is-pair (x car-pred cdr-pred)
;;   "Return non-nil if X is a cons satisfying the given predicates.
;; CAR-PRED and CDR-PRED are applied to X's `car' and `cdr',
;; respectively."
;;   (and (consp x)
;;        (funcall car-pred (car x))
;;        (funcall cdr-pred (cdr x))))

;; (defun imp-parser-as-mode (string-or-symbol)
;;   "If STRING-OR-SYMBOL ends in `-mode' (or its name does), return
;; it as a symbol.  Otherwise, return it as a symbol with `-mode'
;; appended."
;;   (let ((string (imp-parser-as-string string-or-symbol)))
;;     (intern (if (string-match "-mode\\'" string)
;;                 string
;;               (concat string "-mode")))))

;; (defsubst imp-parser-load-name (name &optional noerror)
;;   "Return a form which will load or require NAME.
;; It does the right thing no matter if NAME is a string or symbol.
;; Argument NOERROR means to indicate load failures as a warning."
;;   (if (stringp name)
;;       `(load ,name ,noerror)
;;     `(require ',name nil ,noerror)))

;; (defun imp-parser-hook-injector (name-string keyword body)
;;   "Wrap pre/post hook injections around the given BODY for KEYWORD.
;; The BODY is a list of forms, so `((foo))' if only `foo' is being called."
;;   (if (not imp-parser-inject-hooks)
;;       body
;;     (let ((keyword-name (substring (format "%s" keyword) 1)))
;;       `((when (run-hook-with-args-until-failure
;;                ',(intern (concat "imp-parser--" name-string
;;                                  "--pre-" keyword-name "-hook")))
;;           ,@body
;;           (run-hooks
;;            ',(intern (concat "imp-parser--" name-string
;;                              "--post-" keyword-name "-hook"))))))))

;; (defun imp-parser-with-elapsed-timer (text body)
;;   "BODY is a list of forms, so `((foo))' if only `foo' is being called."
;;   (declare (indent 1))
;;   (if imp-parser-expand-minimally
;;       body
;;     (let ((nowvar (make-symbol "now")))
;;       (if (bound-and-true-p imp-parser-verbose)
;;           `((let ((,nowvar (current-time)))
;;               (message "%s..." ,text)
;;               (prog1
;;                   ,(macroexp-progn body)
;;                 (let ((elapsed
;;                        (float-time (time-subtract (current-time) ,nowvar))))
;;                   (if (> elapsed ,imp-parser-minimum-reported-time)
;;                       (message "%s...done (%.3fs)" ,text elapsed)
;;                     (message "%s...done" ,text))))))
;;         body))))

;; (put 'imp-parser-with-elapsed-timer 'lisp-indent-function 1)

;; (defun imp-parser-require (name &optional no-require body)
;;   (if imp-parser-expand-minimally
;;       (imp-parser-concat
;;        (unless no-require
;;          (list (imp-parser-load-name name)))
;;        body)
;;     (if no-require
;;         body
;;       (imp-parser-with-elapsed-timer
;;           (format "Loading package %s" name)
;;         `((if (not ,(imp-parser-load-name name t))
;;               (display-warning 'imp-parser
;;                                (format "Cannot load %s" ',name)
;;                                :error)
;;             ,@body))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;;; Property lists
;; ;;

;; (defun imp-parser-plist-delete (plist property)
;;   "Delete PROPERTY from PLIST.
;; This is in contrast to merely setting it to 0."
;;   (let (p)
;;     (while plist
;;       (if (not (eq property (car plist)))
;;           (setq p (plist-put p (car plist) (nth 1 plist))))
;;       (setq plist (cddr plist)))
;;     p))

(defun imp-parser-plist-delete-first (plist property)
  "Delete PROPERTY from PLIST.
This is in contrast to merely setting it to 0."
  (let (p)
    (while plist
      (if (eq property (car plist))
          (setq p (nconc p (cddr plist))
                plist nil)
        (setq p (nconc p (list (car plist) (cadr plist)))
              plist (cddr plist))))
    p))

(defsubst imp-parser-plist-maybe-put (plist property value)
  "Add a VALUE for PROPERTY to PLIST, if it does not already exist."
  (if (plist-member plist property)
      plist
    (plist-put plist property value)))

;; (defsubst imp-parser-plist-cons (plist property value)
;;   "Cons VALUE onto the head of the list at PROPERTY in PLIST."
;;   (plist-put plist property (cons value (plist-get plist property))))

;; (defsubst imp-parser-plist-append (plist property value)
;;   "Append VALUE onto the front of the list at PROPERTY in PLIST."
;;   (plist-put plist property (append value (plist-get plist property))))

(defun imp-parser-split-list (pred xs)
  (let ((ys (list nil))
        (zs (list nil))
        flip)
    (cl-dolist (x xs)
      (if flip
          (nconc zs (list x))
        (if (funcall pred x)
            (progn
              (setq flip t)
              (nconc zs (list x)))
          (nconc ys (list x)))))
    (cons (cdr ys) (cdr zs))))

;; (defun imp-parser-split-list-at-keys (key lst)
;;   (and lst
;;        (let ((xs (imp-parser-split-list (apply-partially #'eq key) lst)))
;;          (cons (car xs) (imp-parser-split-list-at-keys key (cddr xs))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Keywords
;;

(defun imp-parser-keyword-index (keyword)
  (cl-loop named outer
           with index = 0
           for k in imp-parser-keywords do
           (if (eq k keyword)
               (cl-return-from outer index))
           (cl-incf index)))

(defun imp-parser-normalize-plist (name input &optional plist merge-function)
  "Given a pseudo-plist, normalize it to a regular plist.
The normalized key/value pairs from input are added to PLIST,
extending any keys already present."
  (if (null input)
      plist
    (let* ((keyword (car input))
           (xs (imp-parser-split-list #'keywordp (cdr input)))
           (args (car xs))
           (tail (cdr xs))
           (normalizer
            (intern-soft (concat "imp-parser-normalize/"
                                 (symbol-name keyword))))
           (arg (and (functionp normalizer)
                     (funcall normalizer name keyword args)))
           (error-string (format "Unrecognized keyword: %s" keyword)))
      (if (memq keyword imp-parser-keywords)
          (progn
            (setq plist (imp-parser-normalize-plist
                         name tail plist merge-function))
            (plist-put plist keyword
                       (if (plist-member plist keyword)
                           (funcall merge-function keyword arg
                                    (plist-get plist keyword))
                         arg)))
        (if imp-parser-ignore-unknown-keywords
            (progn
              (display-warning 'imp-parser error-string)
              (imp-parser-normalize-plist
               name tail plist merge-function))
          (imp-parser-error error-string))))))

(defun imp-parser-unalias-keywords (_name args)
  "Convert `:when' and `:unless' in ARGS to `:if'."
  (setq args (cl-nsubstitute :if :when args))
  (let (temp)
    (while (setq temp (plist-get args :unless))
      (setq args (imp-parser-plist-delete-first args :unless)
            args (append args `(:if (not ,temp))))))
  args)

(defun imp-parser-merge-keys (key new old)
  (let ((merger (assq key imp-parser-merge-key-alist)))
    (if merger
        (funcall (cdr merger) new old)
      (append new old))))

(defun imp-parser-sort-keywords (plist)
  (let (plist-grouped)
    (while plist
      (push (cons (car plist) (cadr plist))
            plist-grouped)
      (setq plist (cddr plist)))
    (let (result)
      (cl-dolist
          (x
           (nreverse
            (sort plist-grouped
                  #'(lambda (l r) (< (imp-parser-keyword-index (car l))
                                (imp-parser-keyword-index (car r)))))))
        (setq result (cons (car x) (cons (cdr x) result))))
      result)))

(defun imp-parser-normalize-keywords (name args)
  (let* ((name-symbol (if (stringp name) (intern name) name))
         (name-string (symbol-name name-symbol)))

    ;;------------------------------
    ;; Input Prep & Validation
    ;;------------------------------

    ;; The function `elisp--local-variables' inserts this unbound variable into
    ;; macro forms to determine the locally bound variables for
    ;; `elisp-completion-at-point'. It ends up throwing a lot of errors since it
    ;; can occupy the position of a keyword (or look like a second argument to a
    ;; keyword that takes one). Deleting it when it's at the top level should be
    ;; harmless since there should be no locally bound variables to discover
    ;; here anyway.
    (setq args (delq 'elisp--witness--lisp args))

    ;; Reduce the set of keywords down to its most fundamental expression.
    (setq args (imp-parser-unalias-keywords name-symbol args))

    ;;------------------------------
    ;; Per-Keyword Normalization
    ;;------------------------------

    ;; Normalize keyword values, coalescing multiple occurrences.
    (setq args (imp-parser-normalize-plist name-symbol args nil
                                           #'imp-parser-merge-keys))

    ;;------------------------------
    ;; Clean & Tidy
    ;;------------------------------
    ;; defaults, restrictions/exclusive keywords, implicit keywords...

    ;; Add default values for keywords not specified, when applicable.
    (cl-dolist (spec imp-parser-defaults)
      (when (let ((func (nth 2 spec)))
              (if (and func (functionp func))
                  (funcall func name args)
                (eval func)))
        (setq args (imp-parser-plist-maybe-put
                    args (nth 0 spec)
                    (let ((func (nth 1 spec)))
                      (if (and func (functionp func))
                          (funcall func name args)
                        (eval func)))))))

    ;; TODO: Add validation (eg keyword conflict resolution) here.
    ;;
    ;; example:
    ;; ;; The :load keyword overrides :no-require
    ;; (when (and (plist-member args :load)
    ;;            (plist-member args :no-require))
    ;;   (setq args (imp-parser-plist-delete args :no-require)))

    ;; TODO: Add implicit keywords here?
    ;;
    ;; example:
    ;; ;; If at this point no :load, :defer or :no-require has been seen, then
    ;; ;; :load the package itself.
    ;; (when (and (not (plist-member args :load))
    ;;            (not (plist-member args :defer))
    ;;            (not (plist-member args :no-require)))
    ;;   (setq args (append args `(:load (,name)))))

    ;; Sort the list of keywords based on the order of `imp-parser-keywords'.
    (imp-parser-sort-keywords args)))

(defun imp-parser-process-keywords (name plist &optional state)
  "Process the next keyword in the free-form property list PLIST.
The values in the PLIST have each been normalized by the function
imp-parser-normalize/KEYWORD.

STATE is a property list that the function may modify and/or
query.  This is useful if a package defines multiple keywords and
wishes them to have some kind of stateful interaction.

Unless the KEYWORD being processed intends to ignore remaining
keywords, it must call this function recursively, passing in the
plist with its keyword and argument removed, and passing in the
next value for the STATE."
  (declare (indent 1))
  (if (null plist)
      ;; No more keywords to process; do the thing!
      (imp-parser-load name state)

    ;; Process the next keyword.
    (let* ((keyword (car plist))
           (arg (cadr plist))
           (rest (cddr plist)))
      (unless (keywordp keyword)
        (imp-parser-error (format "%s is not a keyword" keyword)))
      (let* ((handler (concat "imp-parser-handler/" (symbol-name keyword)))
             (handler-sym (intern handler)))
        (if (functionp handler-sym)
            (funcall handler-sym name keyword arg rest state)
          (imp-parser-error
           (format "Keyword handler not defined: %s" handler)))))))

;; TODO: WTF does this do? Why `(declare (indent 1))` in the func
;; and then immediately back to indent of `defun`?!
(put 'imp-parser-process-keywords 'lisp-indent-function 'defun)

(defun imp-parser-load (name state)
  "Actually load the file, maybe."
  (let* ((funcname "imp-parser-load") ; for debug & errors
         ;; Handle STATE: `:path'
         (path (plist-get state :path)))

      (when imp--debugging?
        (imp--debug funcname
                    "load '%s'"
                    ;; Don't love expanded path, but it does match what Emacs `load' outputs:
                    ;; > "Loading /home/work/.config/emacs-sn004/core/modules/emacs/imp/init.el (source)... done"
                    path-load))

      ;; Time the minimum; only load, ideally.
      ;; TODO: make imp-timing take just a path, not separated path & filename.
      (imp-timing
          name
          (imp--path-filename path)
          (imp-path-parent path)

        ;; Actually do the load.
        ;; Skip erroring out if STATE says so.
        ;; Return the results of `load'.
        (load path
              ;; Handle STATE: `:error', `:optional'
              (not (or (plist-get state :error)
                       (plist-get state :optional)))
              'nomessage))))

;; (defun imp-parser-list-insert (elem xs &optional anchor after test)
;;   "Insert ELEM into the list XS.
;; If ANCHOR is also a keyword, place the new KEYWORD before that
;; one.
;; If AFTER is non-nil, insert KEYWORD either at the end of the
;; keywords list, or after the ANCHOR if one has been provided.
;; If TEST is non-nil, it is the test used to compare ELEM to list
;; elements.  The default is `eq'.
;; The modified list is returned.  The original list is not modified."
;;   (let (result)
;;     (dolist (k xs)
;;       (if (funcall (or test #'eq) k anchor)
;;           (if after
;;               (setq result (cons k result)
;;                     result (cons elem result))
;;             (setq result (cons elem result)
;;                   result (cons k result)))
;;         (setq result (cons k result))))
;;     (if anchor
;;         (nreverse result)
;;       (if after
;;           (nreverse (cons elem result))
;;         (cons elem (nreverse result))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Argument Processing
;;

(defun imp-parser-only-one (label args f)
  "Call F on the first member of ARGS if it has exactly one element."
  (declare (indent 1))
  (cond
   ((and (listp args) (listp (cdr args))
         (= (length args) 1))
    (funcall f label (car args)))
   (t
    (imp-parser-error
     (concat label " wants exactly one argument")))))

(put 'imp-parser-only-one 'lisp-indent-function 'defun)

(defun imp-parser-as-one (label args f &optional allow-empty)
  "Call F on the first element of ARGS if it has one element, or all of ARGS.
If ALLOW-EMPTY is non-nil, it's OK for ARGS to be an empty list."
  (declare (indent 1))
  (if (if args
          (and (listp args) (listp (cdr args)))
        allow-empty)
      (if (= (length args) 1)
          (funcall f label (car args))
        (funcall f label args))
    (imp-parser-error
     (concat label " wants a non-empty list"))))

(put 'imp-parser-as-one 'lisp-indent-function 'defun)

(defun imp-parser-memoize (f arg)
  "Ensure the macro-expansion of F applied to ARG evaluates ARG
no more than once."
  (let ((loaded (cl-gentemp "imp-parser--loaded"))
        (result (cl-gentemp "imp-parser--result"))
        (next   (cl-gentemp "imp-parser--next")))
    `((defvar ,loaded nil)
      (defvar ,result nil)
      (defvar ,next #'(lambda () (if ,loaded ,result
                              (setq ,loaded t ,result ,arg))))
      ,@(funcall f `((funcall ,next))))))

(defsubst imp-parser-normalize-value (_label arg)
  "Normalize the Lisp value given by ARG.
The argument LABEL is ignored."
  (cond ((null arg) nil)
        ((eq t arg) t)
        ((imp-parser-non-nil-symbolp arg)
         `(symbol-value ',arg))
        ((functionp arg)
         `(funcall #',arg))
        (t arg)))

(defun imp-parser-normalize-eval (arg)
  "Eval ARG if a form/function. Otherwise return arg."
  (cond ((null arg) nil)
        ((eq t arg) t)
        ((stringp arg) arg)
        ((symbolp arg) arg)
        (t
         (eval arg))))

(defsubst imp-parser-normalize-symbol-or-string (_label arg)
  "Normalize the Lisp value given by ARG.
The argument LABEL is ignored."
  (cond ((null arg) nil)
        ((eq t arg) t)
        ((stringp arg) arg)
        ((functionp arg)
         `(funcall #',arg))
        (t arg)))

(defun imp-parser-normalize-symbols (label arg &optional recursed)
  "Normalize a list of symbols."
  (cond
   ((imp-parser-non-nil-symbolp arg)
    (list arg))
   ((and (not recursed) (listp arg) (listp (cdr arg)))
    (mapcar #'(lambda (x) (car (imp-parser-normalize-symbols label x t))) arg))
   (t
    (imp-parser-error
     (concat label " wants a symbol, or list of symbols")))))

(defun imp-parser-normalize-symlist (_name keyword args)
  (imp-parser-as-one (symbol-name keyword) args
    #'imp-parser-normalize-symbols))

(defun imp-parser-normalize-only-one-value-or-flag (name keyword args)
  (if (null args)
      ;; keyword used as a flag (by itself; no value), so normalize to true.
      '(t)
    (imp-parser-only-one (symbol-name keyword) args
      #'imp-parser-normalize-value)))

(defun imp-parser-normalize-only-one-symbol-or-string (_name keyword args)
  (imp-parser-only-one (symbol-name keyword)
    args
    #'imp-parser-normalize-value))

(defun imp-parser-normalize-only-one-value (_name keyword args)
  (imp-parser-only-one (symbol-name keyword) args
    #'imp-parser-normalize-value))

;; (defun imp-parser-normalize-recursive-symbols (label arg)
;;   "Normalize a list of symbols."
;;   (cond
;;    ((imp-parser-non-nil-symbolp arg)
;;     arg)
;;    ((and (listp arg) (listp (cdr arg)))
;;     (mapcar #'(lambda (x) (imp-parser-normalize-recursive-symbols label x))
;;             arg))
;;    (t
;;     (imp-parser-error
;;      (concat label " wants a symbol, or nested list of symbols")))))

;; (defun imp-parser-normalize-recursive-symlist (_name keyword args)
;;   (imp-parser-as-one (symbol-name keyword) args
;;     #'imp-parser-normalize-recursive-symbols))

(defun imp-parser-normalize-predicate (_name keyword args)
  (if (null args)
      t
    (imp-parser-only-one (symbol-name keyword) args
      #'imp-parser-normalize-value)))

;; (defun imp-parser-normalize-form (label args)
;;   "Given a list of forms, return it wrapped in `progn'."
;;   (unless (listp (car args))
;;     (imp-parser-error (concat label " wants a sexp or list of sexps")))
;;   (mapcar #'(lambda (form)
;;               (if (and (consp form)
;;                        (memq (car form)
;;                              '(imp-parser bind-key bind-key*
;;                                 unbind-key bind-keys bind-keys*)))
;;                   (macroexpand form)
;;                 form)) args))

;; (defun imp-parser-normalize-forms (_name keyword args)
;;   (imp-parser-normalize-form (symbol-name keyword) args))

;; (defun imp-parser-normalize-pairs
;;     (key-pred val-pred name label arg &optional recursed)
;;   "Normalize a list of pairs.
;; KEY-PRED and VAL-PRED are predicates recognizing valid keys and
;; values, respectively.
;; If RECURSED is non-nil, recurse into sublists."
;;   (cond
;;    ((funcall key-pred arg)
;;     (list (cons arg (imp-parser-as-symbol name))))
;;    ((imp-parser-is-pair arg key-pred val-pred)
;;     (list arg))
;;    ((and (not recursed) (listp arg) (listp (cdr arg)))
;;     (let (last-item)
;;       (mapcar
;;        #'(lambda (x)
;;            (prog1
;;                (let ((ret (imp-parser-normalize-pairs
;;                            key-pred val-pred name label x t)))
;;                  (if (and (listp ret)
;;                           (not (keywordp last-item)))
;;                      (car ret)
;;                    ret))
;;              (setq last-item x))) arg)))
;;    (t arg)))

;; (defun imp-parser-recognize-function (v &optional binding additional-pred)
;;   "A predicate that recognizes functional constructions:
;;   nil
;;   sym
;;   \\='sym
;;   (quote sym)
;;   #\\='sym
;;   (function sym)
;;   (lambda () ...)
;;   \\='(lambda () ...)
;;   (quote (lambda () ...))
;;   #\\='(lambda () ...)
;;   (function (lambda () ...))"
;;   (or (if binding
;;           (symbolp v)
;;         (imp-parser-non-nil-symbolp v))
;;       (and (listp v)
;;            (memq (car v) '(quote function))
;;            (imp-parser-non-nil-symbolp (cadr v)))
;;       (if binding (commandp v) (functionp v))
;;       (and additional-pred
;;            (funcall additional-pred v))))

;; (defun imp-parser-normalize-function (v)
;;   "Reduce functional constructions to one of two normal forms:
;;   sym
;;   #\\='(lambda () ...)"
;;   (cond ((symbolp v) v)
;;         ((and (listp v)
;;               (memq (car v) '(quote function))
;;               (imp-parser-non-nil-symbolp (cadr v)))
;;          (cadr v))
;;         ((and (consp v)
;;               (eq 'lambda (car v)))
;;          v)
;;         ((and (listp v)
;;               (memq (car v) '(quote function))
;;               (eq 'lambda (car (cadr v))))
;;          (cadr v))
;;         (t v)))

;; (defun imp-parser-normalize-commands (args)
;;   "Map over ARGS of the form ((_ . F) ...), normalizing functional F's."
;;   (mapcar #'(lambda (x)
;;               (if (consp x)
;;                   (cons (car x) (imp-parser-normalize-function (cdr x)))
;;                 x))
;;           args))

;; (defun imp-parser-normalize-mode (name keyword args)
;;   "Normalize arguments for keywords which add regexp/mode pairs to an alist."
;;   (imp-parser-as-one (symbol-name keyword) args
;;     (apply-partially #'imp-parser-normalize-pairs
;;                      #'imp-parser-regex-p
;;                      #'imp-parser-recognize-function
;;                      name)))

;; (defun imp-parser-autoloads-mode (_name _keyword args)
;;   (mapcar
;;    #'(lambda (x)
;;        (cond
;;         ((consp (cdr x))
;;          (cons (cddr x) 'command))
;;         ((consp x)
;;          (cons (cdr x) 'command))))
;;    (cl-remove-if-not #'(lambda (x)
;;                          (or (and (consp x)
;;                                   (imp-parser-non-nil-symbolp (cdr x)))
;;                              (and (consp x)
;;                                   (consp (cdr x))
;;                                   (imp-parser-non-nil-symbolp (cddr x)))))
;;                      args)))

;; (defun imp-parser-handle-mode (name alist args rest state)
;;   "Handle keywords which add regexp/mode pairs to an alist."
;;   (imp-parser-concat
;;    (imp-parser-process-keywords name rest state)
;;    (mapcar
;;     #'(lambda (thing)
;;         `(add-to-list
;;           ',alist
;;           ',(cons (imp-parser-normalize-regex (car thing))
;;                   (cdr thing))))
;;     (imp-parser-normalize-commands args))))

(defun imp-parser-handle-state (name keyword arg rest state)
  "Save KEYWORD & ARG into STATE plist for later use."
  (setq state (imp-parser-plist-maybe-put state keyword arg))
  (imp-parser-process-keywords name rest state))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Statistics
;;

(defun imp-parser-reset-statistics ()
  "Reset statistics for `imp-parser'.
See also `imp-parser-statistics'."
  (interactive)
  (setq imp-parser-statistics (make-hash-table)))

;; (defun imp-parser-statistics-status (package)
;;   "Return loading configuration status of PACKAGE statistics."
;;   (cond ((gethash :config package)      "Configured")
;;         ((gethash :init package)        "Initialized")
;;         ((gethash :preface package)     "Prefaced")
;;         ((gethash :imp-parser package) "Declared")))

;; (defun imp-parser-statistics-last-event (package)
;;   "Return the date when PACKAGE's status last changed.
;; The date is returned as a string."
;;   (or (gethash :config package)
;;       (gethash :init package)
;;       (gethash :preface package)
;;       (gethash :imp-parser package)))

;; (defun imp-parser-statistics-time (package)
;;   "Return the time is took for PACKAGE to load."
;;   (+ (float-time (gethash :config-secs package '(0 0 0 0)))
;;      (float-time (gethash :init-secs package '(0 0 0 0)))
;;      (float-time (gethash :preface-secs package '(0 0 0 0)))
;;      (float-time (gethash :imp-parser-secs package '(0 0 0 0)))))

;; (defun imp-parser-statistics-convert (package)
;;   "Return information about PACKAGE.

;; The information is formatted in a way suitable for
;; `imp-parser-statistics-mode'."
;;   (let ((statistics (gethash package imp-parser-statistics)))
;;     (list
;;      package
;;      (vector
;;       (symbol-name package)
;;       (imp-parser-statistics-status statistics)
;;       (format-time-string
;;        "%H:%M:%S.%6N"
;;        (imp-parser-statistics-last-event statistics))
;;       (format "%.2f" (imp-parser-statistics-time statistics))))))

;; (defun imp-parser-report ()
;;   "Show current statistics gathered about `imp-parser' declarations.
;; In the table that's generated, the status field has the following
;; meaning:
;;   Configured        :config has been processed (the package is loaded!)
;;   Initialized       :init has been processed (load status unknown)
;;   Prefaced          :preface has been processed
;;   Declared          the imp-parser declaration was seen"
;;   (interactive)
;;   (with-current-buffer (get-buffer-create "*imp-parser statistics*")
;;     (setq tabulated-list-entries
;;           (mapcar #'imp-parser-statistics-convert
;;                   (hash-table-keys imp-parser-statistics)))
;;     (imp-parser-statistics-mode)
;;     (tabulated-list-print)
;;     (display-buffer (current-buffer))))

;; (defvar imp-parser-statistics-status-order
;;   '(("Declared"    . 0)
;;     ("Prefaced"    . 1)
;;     ("Initialized" . 2)
;;     ("Configured"  . 3)))

;; (define-derived-mode imp-parser-statistics-mode tabulated-list-mode
;;   "imp-parser statistics"
;;   "Show current statistics gathered about `imp-parser' declarations."
;;   (setq tabulated-list-format
;;         ;; The sum of column width is 80 characters:
;;         [("Package" 25 t)
;;          ("Status" 13
;;           (lambda (a b)
;;             (< (assoc-default
;;                 (imp-parser-statistics-status
;;                  (gethash (car a) imp-parser-statistics))
;;                 imp-parser-statistics-status-order)
;;                (assoc-default
;;                 (imp-parser-statistics-status
;;                  (gethash (car b) imp-parser-statistics))
;;                 imp-parser-statistics-status-order))))
;;          ("Last Event" 23
;;           (lambda (a b)
;;             (< (float-time
;;                 (imp-parser-statistics-last-event
;;                  (gethash (car a) imp-parser-statistics)))
;;                (float-time
;;                 (imp-parser-statistics-last-event
;;                  (gethash (car b) imp-parser-statistics))))))
;;          ("Time" 10
;;           (lambda (a b)
;;             (< (imp-parser-statistics-time
;;                 (gethash (car a) imp-parser-statistics))
;;                (imp-parser-statistics-time
;;                 (gethash (car b) imp-parser-statistics)))))])
;;   (setq tabulated-list-sort-key '("Time" . t))
;;   (tabulated-list-init-header))

(defun imp-parser-statistics-gather (keyword name after)
  (let* ((hash (gethash name imp-parser-statistics
                        (make-hash-table)))
         (before (and after (gethash keyword hash (current-time)))))
    (puthash keyword (current-time) hash)
    (when after
      (puthash (intern (concat (symbol-name keyword) "-secs"))
               (time-subtract (current-time) before) hash))
    (puthash name hash imp-parser-statistics)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Handlers
;;

;;------------------------------
;;;; `:disabled'
;;------------------------------
;; No handlers; handled by `imp-parser' personally.

;; ;; Don't alias this to `ignore', as that will cause the resulting
;; ;; function to be interactive.
;; (defun imp-parser-normalize/:disabled (_name _keyword _arg)
;;   "Do nothing, return nil.")

;; (defun imp-parser-handler/:disabled (name _keyword _arg rest state)
;;   (imp-parser-process-keywords name rest state))


;;------------------------------
;;;; `:error'
;;------------------------------

(defalias 'imp-parser-normalize/:error 'imp-parser-normalize-only-one-value-or-flag)
(defalias 'imp-parser-handler/:error   'imp-parser-handle-state)

;; (imp-parser foo :optional t :error t)

;;------------------------------
;;;; `:optional'
;;------------------------------

(defalias 'imp-parser-normalize/:optional 'imp-parser-normalize-only-one-value-or-flag)
(defalias 'imp-parser-handler/:optional   'imp-parser-handle-state)

;;------------------------------
;;;; `:if' `:when' `:unless'
;;------------------------------

(defalias 'imp-parser-normalize/:if     'imp-parser-normalize-only-one-value)
(defalias 'imp-parser-normalize/:when   'imp-parser-normalize-only-one-value)
(defalias 'imp-parser-normalize/:unless 'imp-parser-normalize-only-one-value)

(defun imp-parser-handler/:if (name _keyword pred rest state)
  (let ((body (imp-parser-process-keywords name rest state)))
    `((when ,pred ,@body))))

(defalias 'imp-parser-handler/:when 'imp-parser-handler/:if)

(defun imp-parser-handler/:unless (name _keyword pred rest state)
  (let ((body (imp-parser-process-keywords name rest state)))
    `((unless ,pred ,@body))))


;;------------------------------
;;;; `:requires'
;;------------------------------

(defalias 'imp-parser-normalize/:requires 'imp-parser-normalize-symlist)

(defun imp-parser-handler/:requires (name _keyword requires rest state)
  (let ((body (imp-parser-process-keywords name rest state)))
    (if (null requires)
        body
      `((when ,(if (> (length requires) 1)
                   `(not (member nil (mapcar #'featurep ',requires)))
                 `(featurep ',(car requires)))
          ,@body)))))


;;------------------------------
;;;; `:path'
;;------------------------------

(defun imp-parser-normalize-path-symbol (feature label arg)
  "Convert a path symbol (eg `relative') into a path string."
  (when (symbolp arg)
    (cond
     ;; path == working directory
     ;; NOTE: Careful with `pwd'; it's already a function.
     ((memq arg '(pwd cwd current-dir current-directory working-dir working-directory))
      (imp-path-current-dir))

     ;; path == rooted @ FEATURE
     ((memq arg '(feature root feature-root from-feature relative-to-feature))
      (if (imp--path-root-dir feature :no-error)
          (imp--path-root-dir feature :no-error)
        (imp--error funcname
                    '("`%s' wants to be feature root of `%s'. "
                      "Cannot find feature root for: '%s'")
                    label
                    feature
                    arg)))

     ;; path == user emacs root
     ((memq arg '(emacs .emacs .emacs.d))
      user-emacs-directory)

     ;; path is not a placeholder symbol.
     ;; Let someone else deal with whatever it is.
     (t nil))))
;; (imp-parser-normalize-path-symbol :user :path 'pwd)
;; (imp-parser-normalize-path-symbol :user :path 'user-emacs-directory)

(defun imp-parser-normalize-path-string (feature keyword arg)
  "Convert path prefixes ('./', ':/') in the path arg string."
  (when (stringp arg)
    (cond
     ;; path == pwd
     ((string-prefix-p "./" arg)
      (imp-path-join (imp-path-current-dir)
                     (string-remove-prefix "./" arg)))

     ;; path == rooted @ FEATURE
     ((string-prefix-p ":/" arg)
      (imp-path-join (imp--path-root-dir feature :no-error)
                     (string-remove-prefix ":/" arg)))

     ;; TODO?: look for "FEATURE-ROOT:/..."
     ;; TODO?: get root of FEATURE

     ;; path as-is
     (t arg))))
;; (imp-parser-normalize-path-string :user :path "/foo/bar")
;; (imp-parser-normalize-path-string :user :path "./foo/bar")
;; (imp-parser-normalize-path-string :user :path ":/foo/bar")

(defun imp-parser-normalize-path-arg (feature keyword arg)
  "Normalize a (list of?) filesystem paths to abs and/or rel paths."
   (cond
    ;; Simple/base Cases:
    ;;-------------------
    ((imp-parser-normalize-path-symbol feature keyword arg))

    ((imp-parser-normalize-path-string feature keyword arg))

    ;; Recursive Cases:
    ;; ARG is a function or something.
    ;;----------------------------
    ((and arg (or (imp-parser-non-nil-symbolp arg) (functionp arg)))
     (let ((value (imp-parser-normalize-value keyword arg)))
       (condition-case _
           (imp-parser-normalize-path-arg feature keyword (eval value))
         (error
          (imp--error 'imp-parser-normalize-path-arg
                      "`%s' couldn't evaluate %s: '%s'"
                      keyword
                      (type-of arg)
                      arg)))))

    ((and (listp arg) (listp (cdr arg)))
     (mapcar #'(lambda (x)
                 (imp-parser-normalize-path-arg feature keyword x))
             arg))

    ;; Error: No idea what ARG is.
    (t
     (imp--error 'imp-parser-normalize-path-arg
                 "`%s' wants a path (or list of paths). "
                 "Don't know what to do with %s: '%s'"
                 keyword
                 (type-of arg)
                 arg))))
;; (imp-parser-normalize-path-arg :user :path "/foo/bar")
;; (imp-parser-normalize-path-arg :user :path "/foo/bar/")
;; (imp-parser-normalize-path-arg :user :path "foo/bar")
;; (imp-parser-normalize-path-arg :user :path '("/foo/bar" "baz/qux/"))
;; (imp-parser-normalize-path-arg :user :path 'pwd)
;; (imp-parser-normalize-path-arg :user :path 'user-emacs-directory)
;; (imp-parser-normalize-path-arg :user :path 'foo)

(defun imp-parser-normalize-path (feature keyword arg)
  "Normalize a (list of?) filesystem paths to absolute, canonical paths."
  ;; Normalize
  (let ((path (imp-parser-normalize-path-arg feature keyword arg)))
    ;; Canonicalize
    (cond ((stringp path)
           (imp-path-canonical path))

        ((and (listp path) (listp (cdr path)))
         (mapcar #'(lambda (x)
                     (imp-parser-normalize-path feature keyword x))
                 path))

        ;; Error: No idea what PATH is.
        (t
         (imp--error 'imp-parser-normalize-path
                     "`%s' wants a path (or list of paths). "
                     "Don't know what to do with %s: '%s'"
                     keyword
                     (type-of path)
                     path)))))
;; (imp-parser-normalize-path :user :path "/foo/bar")
;; (imp-parser-normalize-path :user :path "foo/bar")
;; (imp-parser-normalize-path :user :path '("/foo/bar" "baz/qux/"))
;; (imp-parser-normalize-path :user :path 'pwd)
;; (imp-parser-normalize-path :user :path 'user-emacs-directory)
;; (imp-parser-normalize-path :user :path 'foo)

(defun imp-parser-normalize/:path (feature keyword arg)
  "Normalize `:path' ARG to absolute & canonical path string.

If ARG is `guess', make a guess at where the path to FEATURE probably is:
  1) Using FEATURE's root path.
  2) Using Emacs's current working directory.
Else ARG will be used as-is for the path.

If the path is relative, root it in one of:
  1) FEATURE's root path
  2) `user-emacs-directory'"
  (let ((funcname 'imp-parser-normalize/:path)
        ;; Normalize to canonical, absolute path(s).
        (path (imp-parser-normalize-path feature keyword arg)))

    ;; Normalize to one path.
    ;; `imp-parser-normalize-path' allows lists of paths.
    ;; We want just one; take the first.
    (when (proper-list-p path)
      (setq path (car path)))

    ;; Double Check: We got an absolute path, right?
    (unless (file-name-absolute-p path)
      (imp--error funcname
                  "`:path' should end up as absolute: got: '%s' -> '%s'"
                  arg
                  path))

    ;; Add (some of) FEATURE to end of path?


    ;;---
    ;; TODO: remove root from feature if needed?
    ;; TODO: don't want .../source/user/user/init.el for FEATURE `:user/init'
    ;;---
    ;; See TODO: file:path.el::;; TODO: move this to new func... `imp-path-exists' or something?
    ;; (pcase (imp-path-file-exists? ...)
    ;;   (:file
    ;;    ...)
    ;;   (:file:load
    ;;    ...)
    ;;   (:dir
    ;;    ...)
    ;;   (_
    ;;    ...))

    path))
;; (imp-parser-normalize/:path :user :path "/foo/bar")
;; (imp-parser-normalize/:path :user :path "foo/bar")
;; (imp-parser-normalize/:path :user :path '("/foo/bar" "baz/qux"))
;; (imp-parser-normalize/:path :user :path 'pwd)
;; (imp-parser-normalize/:path :user :path 'user-emacs-directory)
;; (imp-parser-normalize/:path :user :path 'foo)

(defalias 'imp-parser-handler/:path 'imp-parser-handle-state)


;;------------------------------
;;;; `:catch'
;;------------------------------
;; TODO: Add this keyword back in?
;; TODO: is this better than `imp-parser-verbose' == `errors'?
;; TODO: is this better than `imp-parser-verbose' == `debug'?

;; (defvar imp-parser--form)
;; (defvar imp-parser--hush-function #'(lambda (_keyword body) body))

;; (defsubst imp-parser-hush (context keyword body)
;;   `((condition-case-unless-debug err
;;         ,(macroexp-progn body)
;;       (error (funcall ,context ,keyword err)))))

;; (defun imp-parser-normalize/:catch (_name keyword args)
;;   (if (null args)
;;       t
;;     (imp-parser-only-one (symbol-name keyword) args
;;       imp-parser--hush-function)))

;; (defun imp-parser-handler/:catch (name keyword arg rest state)
;;   (let* ((context (cl-gentemp "imp-parser--warning")))
;;     (cond
;;      ((not arg)
;;       (imp-parser-process-keywords name rest state))
;;      ((eq arg t)
;;       `((defvar ,context
;;           #'(lambda (keyword err)
;;               (let ((msg (format "%s/%s: %s" ',name keyword
;;                                  (error-message-string err))))
;;                 ,@(when (eq imp-parser-verbose 'debug)
;;                     `((with-current-buffer
;;                           (get-buffer-create "*imp-parser*")
;;                         (goto-char (point-max))
;;                         (insert "-----\n" msg ,imp-parser--form)
;;                         (emacs-lisp-mode))
;;                       (setq msg
;;                             (concat msg
;;                                     " (see the *imp-parser* buffer)"))))
;;                 (display-warning 'imp-parser msg :error))))
;;         ,@(let ((imp-parser--hush-function
;;                  (apply-partially #'imp-parser-hush context)))
;;             (funcall imp-parser--hush-function keyword
;;                      (imp-parser-process-keywords name rest state)))))
;;      ((functionp arg)
;;       `((defvar ,context ,arg)
;;         ,@(let ((imp-parser--hush-function
;;                  (apply-partially #'imp-parser-hush context)))
;;             (funcall imp-parser--hush-function keyword
;;                      (imp-parser-process-keywords name rest state)))))
;;      (t
;;       (imp-parser-error "The :catch keyword expects 't' or a function")))))


;;------------------------------
;;;; `:defer'
;;------------------------------
;; TODO: Add this keyword back in?

;; (defalias 'imp-parser-normalize/:defer 'imp-parser-normalize-predicate)

;; (defun imp-parser-handler/:defer (name _keyword arg rest state)
;;   (let ((body (imp-parser-process-keywords name rest state)))
;;     (imp-parser-concat
;;      ;; Load the package after a set amount of idle time, if the argument to
;;      ;; `:defer' was a number.
;;      (when (numberp arg)
;;        `((run-with-idle-timer ,arg nil #'require
;;                               ',(imp-parser-as-symbol name) nil t)))
;;      (if (or (not arg) (null body))
;;          body
;;        `((eval-after-load ',name ',(macroexp-progn body)))))))


;;------------------------------
;;;; `:after'
;;------------------------------

(defun imp-parser-normalize/:after (name keyword args)
  (setq args (imp-parser-normalize-recursive-symlist name keyword args))
  (if (consp args)
      args
    (list args)))

(defun imp-parser-after-count-uses (features*)
  "Count the number of time the body would appear in the result."
  (cond ((imp-parser-non-nil-symbolp features*)
         1)
        ((and (consp features*)
              (memq (car features*) '(:or :any)))
         (let ((num 0))
           (cl-dolist (next (cdr features*))
             (setq num (+ num (imp-parser-after-count-uses next))))
           num))
        ((and (consp features*)
              (memq (car features*) '(:and :all)))
         (apply #'max (mapcar #'imp-parser-after-count-uses
                              (cdr features*))))
        ((listp features*)
         (imp-parser-after-count-uses (cons :all features*)))))

(defun imp-parser-require-after-load (features* body)
  "Generate `eval-after-load' statements to represents FEATURES*.
FEATURES* is a list containing keywords `:and' and `:all', where
no keyword implies `:all'."
  (cond
   ((imp-parser-non-nil-symbolp features*)
    `((eval-after-load ',features* ',(macroexp-progn body))))
   ((and (consp features*)
         (memq (car features*) '(:or :any)))
    (cl-mapcan #'(lambda (x) (imp-parser-require-after-load x body))
               (cdr features*)))
   ((and (consp features*)
         (memq (car features*) '(:and :all)))
    (cl-dolist (next (cdr features*))
      (setq body (imp-parser-require-after-load next body)))
    body)
   ((listp features*)
    (imp-parser-require-after-load (cons :all features*) body))))

(defun imp-parser-handler/:after (name _keyword arg rest state)
  (let ((body (imp-parser-process-keywords name rest state))
        (uses (imp-parser-after-count-uses arg)))
    (if (or (null uses) (null body))
        body
      (if (<= uses 1)
          (imp-parser-require-after-load arg body)
        (imp-parser-memoize
         (apply-partially #'imp-parser-require-after-load arg)
         (macroexp-progn body))))))


;;------------------------------
;;;; `:custom'
;;------------------------------
;; TODO: Add this keyword back in?

;; (defun imp-parser-normalize/:custom (_name keyword args)
;;   "Normalize imp-parser custom keyword."
;;   (imp-parser-as-one (symbol-name keyword) args
;;     #'(lambda (label arg)
;;         (unless (listp arg)
;;           (imp-parser-error
;;            (concat label " a (<symbol> <value> [comment])"
;;                    " or list of these")))
;;         (if (imp-parser-non-nil-symbolp (car arg))
;;             (list arg)
;;           arg))))

;; (defun imp-parser-handler/:custom (name _keyword args rest state)
;;   "Generate imp-parser custom keyword code."
;;   (imp-parser-concat
;;    (if (bound-and-true-p imp-parser-use-theme)
;;        `((let ((custom--inhibit-theme-enable nil))
;;            ;; Declare the theme here so imp-parser can be required inside
;;            ;; eval-and-compile without warnings about unknown theme.
;;            (unless (memq 'imp-parser custom-known-themes)
;;              (deftheme imp-parser)
;;              (enable-theme 'imp-parser)
;;              (setq custom-enabled-themes (remq 'imp-parser custom-enabled-themes)))
;;            (custom-theme-set-variables
;;             'imp-parser
;;             ,@(mapcar
;;                #'(lambda (def)
;;                    (let ((variable (nth 0 def))
;;                          (value (nth 1 def))
;;                          (comment (nth 2 def)))
;;                      (unless (and comment (stringp comment))
;;                        (setq comment (format "Customized with imp-parser %s" name)))
;;                      `'(,variable ,value nil () ,comment)))
;;                args))))
;;      (mapcar
;;       #'(lambda (def)
;;           (let ((variable (nth 0 def))
;;                 (value (nth 1 def))
;;                 (comment (nth 2 def)))
;;             (unless (and comment (stringp comment))
;;               (setq comment (format "Customized with imp-parser %s" name)))
;;             `(customize-set-variable (quote ,variable) ,value ,comment)))
;;       args))
;;    (imp-parser-process-keywords name rest state)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; The main macro
;;

(defmacro imp-parser-core (name args)
  `(let* ((args* (imp-parser-normalize-keywords ,name ,args))
          (imp-parser--form
           (if (eq imp-parser-verbose 'debug)
               (concat "\n\n"
                       (pp-to-string `(imp-parser ,name ,@,args))
                       "\n  -->\n\n"
                       (pp-to-string `(imp-parser ,name ,@args*))
                       "\n  ==>\n\n"
                       (pp-to-string
                        (macroexp-progn
                         (let ((imp-parser-verbose 'errors)
                               (imp-parser-expand-minimally t))
                           (imp-parser-process-keywords name args*)))))
             "")))
     (when (eq imp-parser-verbose 'debug)
       (message (make-string 40 ?━))
       (message "%s" name)
       (message (make-string 40 ?━))
       (message "%s" imp-parser--form))

     (imp-parser-process-keywords name args*)
     ))


;;;###autoload
(defmacro imp-parser (name &rest args)
  "Load NAME.

Usage:

  (imp-parser package-name
     [:keyword [option]]...)

:disabled      Flag. Nothing happens; NAME is ignored completely if this keyword
               is present.

:error ERR     Value (aka ERROR) can be:
                 - nil
                 - non-nil (default)
                 - A form that should evaluate to one of the above.
               If ERROR is nil, the function will not raise an error if:
                 - The file doesn't exist.
                 - NAME isn't provided after loading the file.
               It will still raise an error if:
                 - It cannot parse the inputs.
                 - It cannot determine where to /look/ for the file.

:optional OPT  Load NAME if it exists; do nothing if it does not.
               Basically, it is a more specific `:error nil'.
               OPT can be:
                 - nil (default)
                 - non-nil
                 - A form that should evaluate to one of the above.

:requires REQ  Assert that a feature already exists in `features'.
               If assert fails, handle according to ERROR/OPTIONAL.
               REQ can be:
                 - nil
                 - symbol
                 - A form that should evaluate to one of the above.

:if EXPR       Initialize and load only if EXPR evaluates to a non-nil value.
:when EXPR     See `:if'.
:unless EXPR   Opposite of `:if'.
               Initialize and load only if EXPR evaluates to a nil value.

:path PATH     Absolute, relative, or rooted path to file.
               PATH can be:
                 - A path string.
                 - A list of strings to join into a path.
                 - A form that should evaluate to one of the above.

:after AFTER   Delay the effect of the imp-parser declaration
               until after the named libraries have loaded.
               Before they have been loaded, no other keyword
               has any effect at all, and once they have been
               loaded it is as if `:after' was not specified."
  (declare (indent defun))
  (unless (memq :disabled args)
    (macroexp-progn
     (imp-parser-concat
      ;; (when imp-parser-compute-statistics
      ;;   `((imp-parser-statistics-gather :imp-parser ',name nil)))
      (if (memq imp-parser-verbose '(errors debug))
          (imp-parser-core name args)
        (condition-case-unless-debug err
            (imp-parser-core name args)
          (error
           (ignore
            (display-warning
             'imp-parser
             (format "Failed to parse package %s: %s"
                     name (error-message-string err))
             :error)))))
      ;; (when imp-parser-compute-statistics
      ;;   `((imp-parser-statistics-gather :imp-parser ',name t)))
      ))
    ))
;; (imp-parser foo)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide :imp 'parser)
