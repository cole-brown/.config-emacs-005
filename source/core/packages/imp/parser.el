;;; imp/parser.el --- Plist Parsing -*- lexical-binding: t; -*-
;;
;; Author:     John Wiegley <johnw@newartisans.com>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2025-09-22
;; Timestamp:  2025-10-28
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

(require 'cl-lib)

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
the semantics of `imp-parser' declarations and may necessitate
changing where you had inserted a new keyword earlier.

NOTE: `:disabled' is special in this list, as it causes
nothing at all to happen, even if the rest of the `imp-parser'
declaration is incorrect."
  :type '(repeat symbol)
  :group 'imp-parser)

(defcustom imp-parser-ignore-unknown-keywords nil
  "If non-nil, warn instead of signaling error for unknown keywords.
The unknown keyword and its associated arguments will be ignored
in the `imp-parser' expansion."
  :type 'boolean
  :group 'imp-parser)

;; TODO: use imp's debugging flag? merge with it?
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

(defcustom imp-parser-defaults
  '(;; (KEYWORD DEFAULT-VALUE USAGE-PREDICATE)
    ;; TODO(stats): uncomment
    ;; (:stats t t)
    (:path
     (lambda (feature args) (imp-parser-normalize/:path feature :path nil))
     (lambda (feature args) (not (plist-member args :path))))
    (:error    t   t)
    (:optional nil t))
  "Default values for specified `imp-parser' keywords.
Each entry in the alist is a list of three elements:
The first element is the `imp-parser' keyword.

The second is a form that can be evaluated to get the default
value.  It can also be a function that will receive the FEATURE from
the `imp-parser' declaration and the keyword plist given to
`imp-parser', in normalized form.  The value it returns should
also be in normalized form (which is sometimes *not* what one
would normally write in a `imp-parser' declaration, so use
caution).

The third element is a form that can be evaluated to determine
whether or not to assign a default value; if it evaluates to nil,
then the default value is not assigned even if the keyword is not
present in the `imp-parser' form.  This third element may also be
a function, in which case it receives the FEATURE from `imp-parser' (as
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

(defcustom imp-parser-merge-key-alist
  '((:if    . (lambda (new old) `(and ,new ,old)))
    (:after . (lambda (new old) `(:all ,new ,old))))
  "Alist of keys and the functions used to merge multiple values.
For example, if the following form is provided:

  (imp-parser foo :if pred1 :if pred2)

Then based on the above defaults, the merged result will be:

  (imp-parser foo :if (and pred1 pred2))

This is done so that, at the stage of invoking handlers, each
handler is called only once."
  :type `(repeat
          (cons (choice :tag "Keyword"
                        ,@(mapcar #'(lambda (k) (list 'const k))
                                  imp-parser-keywords)
                        (const :tag "Any" t))
                function))
  :group 'imp-parser)

;; TODO(stats): use any of predecessor's stats?
;; (defcustom imp-parser-minimum-reported-time 0.1
;;   "Minimal load time that will be reported.
;; Note that `imp-parser-verbose' has to be set to a non-nil value
;; for anything to be reported at all."
;;   :type 'number
;;   :group 'imp-parser)

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
  :group 'imp-parser)

;; TODO: figure out wtf this regex soup is doing & adapt to imp?
;; (defconst imp-parser-font-lock-keywords
;;   '(("(\\(imp-parser\\)\\_>[ \t']*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
;;      (1 font-lock-keyword-face)
;;      (2 font-lock-constant-face nil t))))
;;
;; (font-lock-add-keywords 'emacs-lisp-mode imp-parser-font-lock-keywords)

;; TODO(stats): use any of predecessor's stats?
;; (defcustom imp-parser-compute-statistics nil
;;   "If non-nil, compute statistics concerned `imp-parser' declarations.
;; View the statistical report using `imp-parser-report'.  Note that
;; if this option is enabled, you must require `imp-parser' in your
;; user init file at loadup time, or you will see errors concerning
;; undefined variables."
;;   :type 'boolean
;;   :group 'imp-parser)

;; TODO(stats): use any of predecessor's stats?
(defvar imp-parser-statistics (make-hash-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Utility functions
;;

;; TODO: Switch to imp--error
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
  (if (symbolp string-or-symbol)
      string-or-symbol
    (intern string-or-symbol)))

(defsubst imp-parser-as-string (string-or-symbol)
  "If STRING-OR-SYMBOL is already a string, return it.
Otherwise convert it to a string and return that."
  (if (stringp string-or-symbol)
      string-or-symbol
    (symbol-name string-or-symbol)))

;; (defsubst imp-parser-is-pair (x car-pred cdr-pred)
;;   "Return non-nil if X is a cons satisfying the given predicates.
;; CAR-PRED and CDR-PRED are applied to X's `car' and `cdr',
;; respectively."
;;   (and (consp x)
;;        (funcall car-pred (car x))
;;        (funcall cdr-pred (cdr x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Property Lists
;;

(defun imp-parser-plist-delete (plist property)
  "Delete PROPERTY from PLIST.
This is in contrast to merely setting it to 0."
  (let (p)
    (while plist
      (if (not (eq property (car plist)))
          (setq p (plist-put p (car plist) (nth 1 plist))))
      (setq plist (cddr plist)))
    p))

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

(defun imp-parser-normalize-plist (feature input &optional plist merge-function)
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
           (args-norm (and (functionp normalizer)
                     (funcall normalizer feature keyword args)))
           (error-string (format "Unrecognized keyword: %s" keyword)))
      (if (memq keyword imp-parser-keywords)
          (progn
            (setq plist (imp-parser-normalize-plist
                         feature tail plist merge-function))
            (plist-put plist keyword
                       (if (plist-member plist keyword)
                           (funcall merge-function keyword args-norm
                                    (plist-get plist keyword))
                         args-norm)))
        (if imp-parser-ignore-unknown-keywords
            (progn
              (display-warning 'imp-parser error-string)
              (imp-parser-normalize-plist
               feature tail plist merge-function))
          (imp-parser-error error-string))))))

(defun imp-parser-unalias-keywords (feature args)
  "Convert `:when' and `:unless' in ARGS to `:if'."
  (setq args (cl-nsubstitute :stats :statistics args))
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

(defun imp-parser-normalize-keywords (feature args)
  (let* ((feature-symbol (if (stringp feature) (intern feature) feature))
         (feature-string (symbol-name feature-symbol)))

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
    (setq args (imp-parser-unalias-keywords feature-symbol args))

    ;;------------------------------
    ;; Per-Keyword Normalization
    ;;------------------------------

    ;; Normalize keyword values, coalescing multiple occurrences.
    (setq args (imp-parser-normalize-plist feature-symbol args nil
                                           #'imp-parser-merge-keys))

    ;;------------------------------
    ;; Clean & Tidy
    ;;------------------------------
    ;; defaults, restrictions/exclusive keywords, implicit keywords...

    ;; Add default values for keywords not specified, when applicable.
    (cl-dolist (spec imp-parser-defaults)
      (when (let ((func (nth 2 spec)))
              (if (and func (functionp func))
                  (funcall func feature args)
                (eval func)))
        (setq args (imp-parser-plist-maybe-put
                    args (nth 0 spec)
                    (let ((func (nth 1 spec)))
                      (if (and func (functionp func))
                          (funcall func feature args)
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
    ;;   (setq args (append args `(:load (,feature)))))

    ;; Sort the list of keywords based on the order of `imp-parser-keywords'.
    (imp-parser-sort-keywords args)))

(defun imp-parser-process-keywords (feature plist &optional state)
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
  (if (null plist)
      ;; No more keywords to process; do the thing!
      (imp-parser-load feature state)

    ;; Process the next keyword.
    (let* ((keyword (car plist))
           (arg (cadr plist))
           (rest (cddr plist)))
      (unless (keywordp keyword)
        (imp-parser-error (format "%s is not a keyword" keyword)))
      (let* ((handler (concat "imp-parser-handler/" (symbol-name keyword)))
             (handler-sym (intern handler)))
        (if (functionp handler-sym)
            (funcall handler-sym feature keyword arg rest state)
          (imp-parser-error
           (format "Keyword handler not defined: %s" handler)))))))

(defun imp-parser-load (feature state)
  "Actually load the file, maybe."
  (let* ((funcname 'imp-parser-load)
         ;; Handle STATE: `:path'
         (path (plist-get state :path))
         (path-load (plist-get state :path-load))
         (feature-root (imp-feature-root feature)))

    ;; We really should have a `path' of some sorts now.
    ;; NOTE: We may not have a `path-load' if eg file does not exist.
    (unless (and (stringp path)
                 (file-name-absolute-p path))
      ;; TODO(stats): some imp timing thing to say that this thing errored?
      (imp--error funcname
                  `("Path is invalid or not absolute! "
                    "path:'%s'")
                  path))

    ;; Deal with non-existent file.
    (if (null path-load)
        ;; Handle STATE: `:optional'
        (if (plist-get state :optional)
            ;; Optional load and file does not exist.
            ;; Return sexprs that will skip the file
            `((progn
                ;; Skip w/ optional timing message.
                (imp-timing-skip-optional-dne ',feature ,path)
                ;; Return `nil' for load result.
                nil))
          ;; Else requried, so error?
          ;; TODO(stats): Make some imp timing thing to say that this thing errored?
          (imp--error funcname
                      "Cannot find a file to load. path:'%s' -> load-path:'%s'"
                      path
                      path-load))

      ;; Have a valid load path. Try loading it.

      (when imp--debugging?
        ;; `load' outputs:
        ;; > "Loading /home/work/.config/emacs-sn004/core/modules/emacs/imp/init.el (source)... done"
        (imp--debug funcname
                    "load '%s' => '%s'"
                    path
                    path-load))

      ;; Return sexprs that will time & load the file.
      `((imp-timing
            ',feature
            ,path-load

            ;; Actually do the load.
            ;; Skip erroring out if STATE says so.
            ;; Return the results of `load'.
            (load ,path
                  ;; Handle STATE: `:error'
                  ',(when (not (plist-get state :error))
                      'noerror)
                  'nomessage))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Argument Processing
;;

;; TODO: make 'em all start with `imp-parser-args-'
;; TODO: and try to standardize the naming of the 2 or 3 different kinds of arg processing funcs.

(defun imp-parser-args-only-one (keyword args)
  "Return the first member of ARGS if it has exactly one element.
Else error."
  (cond
   ((and (listp args) (listp (cdr args))
         (= (length args) 1))
    (car args))
   (t
    (imp-parser-error
     (concat (imp-parser-as-string keyword) " wants exactly one argument")))))

(defun imp-parser-only-one (keyword args func)
  "Call FUNC on the first member of ARGS if it has exactly one element.
Else error."
  (cond
   ((and (listp args) (listp (cdr args))
         (= (length args) 1))
    (funcall func keyword (car args)))
   (t
    (imp-parser-error
     (concat (imp-parser-as-string keyword) " wants exactly one argument")))))

(defun imp-parser-as-one (keyword args func &optional allow-empty)
  "Call FUNC on the first element of ARGS if it has one element, or all of ARGS.
If ALLOW-EMPTY is non-nil, it's OK for ARGS to be an empty list."
  (if (if args
          (and (listp args) (listp (cdr args)))
        allow-empty)
      (if (= (length args) 1)
          (funcall func keyword (car args))
        (funcall func keyword args))
    (imp-parser-error
     (concat (imp-parser-as-string keyword) " wants a non-empty list"))))

(defun imp-parser-memoize (func arg)
  "Ensure the macro-expansion of FUNC applied to ARG evaluates ARG
no more than once."
  (let ((loaded (cl-gentemp "imp-parser--loaded"))
        (result (cl-gentemp "imp-parser--result"))
        (next   (cl-gentemp "imp-parser--next")))
    `((defvar ,loaded nil)
      (defvar ,result nil)
      (defvar ,next #'(lambda () (if ,loaded ,result
                                   (setq ,loaded t ,result ,arg))))
      ,@(funcall func `((funcall ,next))))))

(defsubst imp-parser-normalize-value (keyword arg)
  "Normalize the Lisp value given by ARG."
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

(defsubst imp-parser-normalize-symbol-or-string (keyword arg)
  "Normalize the Lisp value given by ARG."
  (cond ((null arg) nil)
        ((eq t arg) t)
        ((stringp arg) arg)
        ((functionp arg)
         `(funcall #',arg))
        (t arg)))

(defun imp-parser-normalize-symbols (keyword args &optional recursed)
  "Normalize a list of symbols."
  (cond
   ((imp-parser-non-nil-symbolp args)
    (list args))
   ((and (not recursed) (listp args) (listp (cdr args)))
    (mapcar #'(lambda (x) (car (imp-parser-normalize-symbols keyword x t))) args))
   (t
    (imp-parser-error
     (concat (imp-parser-as-string keyword) " wants a symbol, or list of symbols")))))

(defun imp-parser-normalize-symlist (feature keyword args)
  (imp-parser-as-one keyword args
    #'imp-parser-normalize-symbols))

(defun imp-parser-normalize-flag (feature arg)
  "Converts ARGS to '(t) if there are no args."
  (if (null arg)
      '(t)
    arg))

(defun imp-parser-normalize-only-one-symbol (feature keyword args)
  (imp-parser-only-one keyword args
    #'imp-parser-normalize-symbols))

(defun imp-parser-normalize-only-one-value-or-flag (feature keyword args)
  (if (null args)
      ;; keyword used as a flag (by itself; no value), so normalize to true.
      '(t)
    (imp-parser-only-one keyword args
      #'imp-parser-normalize-value)))

(defun imp-parser-normalize-only-one-symbol-or-string (feature keyword args)
  (imp-parser-only-one keyword
    args
    #'imp-parser-normalize-value))

(defun imp-parser-normalize-only-one-value (feature keyword args)
  (imp-parser-only-one keyword args
    #'imp-parser-normalize-value))

(defun imp-parser-normalize-recursive-symbols (keyword args)
  "Normalize a list of symbols."
  (cond
   ((imp-parser-non-nil-symbolp args)
    args)
   ((and (listp args) (listp (cdr args)))
    (mapcar #'(lambda (x) (imp-parser-normalize-recursive-symbols keyword x))
            args))
   (t
    (imp-parser-error
     (concat (imp-parser-as-string keyword) " wants a symbol, or nested list of symbols")))))

(defun imp-parser-normalize-recursive-symlist (feature keyword args)
  (imp-parser-as-one keyword args
    #'imp-parser-normalize-recursive-symbols))

(defun imp-parser-normalize-predicate (feature keyword args)
  (if (null args)
      t
    (imp-parser-only-one keyword args
      #'imp-parser-normalize-value)))

(defun imp-parser-normalize-form (keyword args)
  "Given a list of forms, return it wrapped in `progn'."
  (unless (listp (car args))
    (imp-parser-error (concat (imp-parser-as-string keyword) " wants a sexp or list of sexps")))
  (mapcar #'(lambda (form)
              (if (and (consp form)
                       (memq (car form)
                             '(imp-parser bind-key bind-key*
                                unbind-key bind-keys bind-keys*)))
                  (macroexpand form)
                form)) args))

(defun imp-parser-normalize-forms (feature keyword args)
  "Given a list of forms, return it wrapped in `progn'."
  (imp-parser-normalize-form keyword args))

;; (defun imp-parser-normalize-pairs
;;     (key-pred val-pred feature keyword arg &optional recursed)
;;   "Normalize a list of pairs.
;; KEY-PRED and VAL-PRED are predicates recognizing valid keys and
;; values, respectively.
;; If RECURSED is non-nil, recurse into sublists."
;;   (cond
;;    ((funcall key-pred arg)
;;     (list (cons arg (imp-parser-as-symbol feature))))
;;    ((imp-parser-is-pair arg key-pred val-pred)
;;     (list arg))
;;    ((and (not recursed) (listp arg) (listp (cdr arg)))
;;     (let (last-item)
;;       (mapcar
;;        #'(lambda (x)
;;            (prog1
;;                (let ((ret (imp-parser-normalize-pairs
;;                            key-pred val-pred feature keyword x t)))
;;                  (if (and (listp ret)
;;                           (not (keywordp last-item)))
;;                      (car ret)
;;                    ret))
;;              (setq last-item x))) arg)))
;;    (t arg)))

;; TODO: use this in path normalizer for the `eval' case?
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
;;   "Map over ARGS of the form ((_ . FUNC) ...), normalizing FUNCs."
;;   (mapcar #'(lambda (x)
;;               (if (consp x)
;;                   (cons (car x) (imp-parser-normalize-function (cdr x)))
;;                 x))
;;           args))

(defun imp-parser-handle-state (feature keyword arg rest state)
  "Save KEYWORD & ARG into STATE plist for later use."
  (setq state (imp-parser-plist-maybe-put state keyword arg))
  (imp-parser-process-keywords feature rest state))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Statistics
;;
;; TODO(stats): use any of predecessor's stats?

;; (defun imp-parser-reset-statistics ()
;;   "Reset statistics for `imp-parser'.
;; See also `imp-parser-statistics'."
;;   (interactive)
;;   (setq imp-parser-statistics (make-hash-table)))

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

;; (defun imp-parser-statistics-gather (keyword feature after)
;;   (let* ((hash (gethash feature imp-parser-statistics
;;                         (make-hash-table)))
;;          (before (and after (gethash keyword hash (current-time)))))
;;     (puthash keyword (current-time) hash)
;;     (when after
;;       (puthash (intern (concat (symbol-name keyword) "-secs"))
;;                (time-subtract (current-time) before) hash))
;;     (puthash feature hash imp-parser-statistics)))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide imp parser)
