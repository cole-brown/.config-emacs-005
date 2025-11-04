;;; namespaced/elisp/functions.el --- Functions for Functions -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2020-12-17
;; Timestamp:  2025-11-03
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Functions for functions.
;; Functions for function args.
;; Functions that create functions.
;;
;;; Code:

;;-!-!-!-!-!-!-!-!-!-!-!-!-!-!-
;; `ns' Imports Allowed:
;; ----------------
;;   - :elisp
;;-!-!-!-!-!-!-!-!-!-!-!-!-!-!-

(imp-require-assert :elisp 'types)


;;------------------------------------------------------------------------------
;; Symbols
;;------------------------------------------------------------------------------
;; TODO: Make & move to `symbol.el'

(defmacro elisp:symbol/lexical:bound-and-true? (symbol)
  "Return SYMBOL's value if it exists, else nil.

NOTE: SYMBOL's value can be nil; will return nil in that case.
See also `elisp:symbol/lexical:bound?'."
  ;; If it has a value, return it.
  ;; If not, who cares and we get to return `nil' for free.
  `(ignore-error void-variable ,symbol))
;; (let ((testing 42)) (elisp:symbol/lexical:bound-and-true? testing))
;; (elisp:symbol/lexical:bound-and-true? testing)


(defmacro elisp:symbol/lexical:bound? (symbol)
  "Return t if SYMBOL exists as a variable, else nil.

NOTE: If SYMBOL exists and is nil, return t.
See also `elisp:symbol/lexical:bound-and-true?'."
  ;; Don't care about `debug-on-error'; that would ruin this function's reason
  ;; for existance.
  `(condition-case nil
       ;; Return true if it exist...
       (when ,symbol t)
     ;; Return nil if it caused the "does not exist" error.
     (void-variable nil)))
;; (let ((testing 42)) (elisp:symbol/lexical:bound? testing))
;; (elisp:symbol/lexical:bound? testing)


;;------------------------------------------------------------------------------
;; Delete
;;------------------------------------------------------------------------------
;; TODO: move to `elisp/elisp.el'

(defun elisp:delete (symbol)
  "Delete a SYMBOL by calling:
1. `fmakunbound' - Remove the function definition.
2. `makunbound'  - Remove the variable binding.
3. `unintern'    - Remove the symbol name from the symbol table."
  (fmakunbound symbol)
  (makunbound symbol)
  (unintern symbol nil))


;;------------------------------------------------------------------------------
;; List Functions
;;------------------------------------------------------------------------------
;; TODO: Move to `ns/list'!!!!
;;   - I think I have other list util functions hiding... somewhere.
;;     - Maybe in `alist'...

(defun elisp:list:flatten (args)
  "Return ARGS as a single, flat list.

If ARGS is a list, it must be a proper list (no circular lists or conses)."
  (declare (pure t) (side-effect-free t))
  (cond
   ;; If it's a list, flatten it.
   ((elisp:list/proper? args)
    (seq-mapcat #'elisp:list:flatten args))
   ;; If it's a cons, convert into a list and flatten.
   ((elisp:cons? args)
    (elisp:list:flatten (list (car args) (cdr args))))
   ;; Else it's now a list.
   (t
    (list args))))
;; (elisp:list:flatten 1)
;; (elisp:list:flatten '(1 . 2))
;; (elisp:list:flatten '(1 2 3))
;; (elisp:list:flatten '(1 (2 3)))


;;   - I think I have other list util functions hiding... somewhere.
;;     - Maybe in `alist'...
(defun elisp:list:listify (args)
  "Ensure ARGS is a list.

Return ARGS wrapped in a list, or as-is if already a list.

From Doom's `doom-enlist'."
  (declare (pure t) (side-effect-free t))
  (if (proper-list-p args)
      args
    (list args)))


;;------------------------------------------------------------------------------
;; Ensure Quote/Unquote
;;------------------------------------------------------------------------------
;; TODO: IDK where these go but likely not here. in `symbol.el'?
;;   - Well... you can unquote more than just symbols.
;;   - Maybe just put in `elisp/elisp.el'

(defun elisp:unquote (arg)
  "Return ARG unquoted.

Removes both `quote' ('foo) and `function' (#'foo) style quoting.

Originaly from `doom-unquote'."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe arg) '(quote function))
    (setq arg (cadr arg)))
  arg)
;; (elisp:unquote '''foo)


(defun elisp:quote (arg)
  "Return ARG quoted.

Ensure that ARG is quoted exactly once.
  ''foo -> 'foo
  'foo  -> 'foo
  foo   -> 'foo

Check both `quote' ('foo) and `function' (#'foo) style quoting."
  (declare (pure t) (side-effect-free t))
  `(quote ,(elisp:unquote arg)))
;; (elisp:quote '''foo)


;;------------------------------------------------------------------------------
;; Interactive Commands
;;------------------------------------------------------------------------------
;; TODO: Move to `elisp:command'? `elisp:lambda'?

(defmacro elisp:cmd (&rest body)
  "Return (lambda () (interactive) ,@BODY)

A factory for quickly producing interaction commands, particularly for keybinds
or aliases.

Proudly nicked from Doom's `cmd!' in \"core/core-lib.el\"."
  (declare (doc-string 1) (pure t) (side-effect-free t))
  `(lambda (&rest _) (interactive) ,@body))


(defmacro elisp:cmd/prefix (command prefix-param)
  "Return lambda for calling COMMAND with prefix arg of PREFIX-PARAM.

A factory for quickly producing interaction commands, particularly for keybinds
or aliases.

For \"just the standard prefix arg please\" PREFIX-PARAM should be one of:
  - `:prefix/always'
  - `prefix/always'
  - `(list 4)'
    - This is what `universal-argument' sets `current-prefix-arg' to for the
      `interactive' \"P\" spec.

For \"maybe a prefix, maybe not\" PREFIX-PARAM sholud be:
  - nil

Like `elisp:cmd', but allows you to change `current-prefix-arg'arguments to
COMMAND. This macro is meant to be used as a target for keybinds (e.g. with
`define-key' or `general-def').

Originally from Doom's `cmd!!' in \"core/core-lib.el\"."
  (declare (doc-string 1)
           (pure t)
           (side-effect-free t))
  `(lambda (&optional arg &rest _) (interactive "P")
     (let* ((prefix ,prefix-param)
            (current-prefix-arg (cond
                                 ;; Translate pretty PREFIX-PARAM to something Emacs understands?
                                 ((memq prefix '(:prefix/always prefix/always))
                                  '(4))
                                 ;; PREFIX-PARAM exists; use it.
                                 (prefix)
                                 ;; No PREFIX-PARAM, so this is just a
                                 ;; "prefix-maybe" command; use whatever
                                 ;; was supplied with this command
                                 ;; invocation.
                                 (t
                                  arg))))
       (call-interactively ,command))))
;; (let ((cmd (elisp:cmd/prefix (lambda (&optional x) (interactive "P") (message "prefix: %S" x)) :prefix))) (call-interactively cmd))


(defmacro elisp:cmd/args (command &rest args)
  "Return lambda for calling COMMAND with ARGS.

A factory for quickly producing interaction commands, particularly for keybinds
or aliases.

Like `elisp:cmd', but allows you to pass arguments to COMMAND. This macro is
meant to be used as a target for keybinds (e.g. with `define-key' or
`general-def').

Originally from from Doom's `cmd!!' in \"core/core-lib.el\"."
  (declare (doc-string 1) (pure t) (side-effect-free t))
  `(lambda (&rest _) (interactive)
     (,(if args
           #'funcall-interactively
         #'call-interactively)
      ,command ,@args)))


;;------------------------------------------------------------------------------
;; Functions for argument parsing.
;;------------------------------------------------------------------------------
;; TODO: Move to `elisp:parse'?

(defun elisp:parse:args+kwargs (input &rest claims)
  "Parse INPUT into args (list) and keyword args (plist).

Expects INPUT to be a list of:
  - Some args.
  - Followed by some keyword args.

That is, INPUT should be something like:
'((list of args) \"whatever\" ... et al :k0 v0 :k1 v1 ... :kn vn)

CLAIMS should be keywords that the caller expects. It will be
flattened to a single list of keywords to look for.

Splits INPUT list into the args and the keyword args, where
`:k0' to `:kn' are keywords in CLAIMS.

Once any of the keyword args in CLAIMS is found, that and the
rest of INPUT are assumed to be the keyword args. Everything
before will be the returned args, and that keyword and everything
after will be the returned keyword args.

So this:
  '((list of args) \"whatever\" ... et al :k0 v0 :k1 v1 ... :kn vn)
Becomes this:
  '(((list of args) \"whatever\" ... et al)
    (:k0 v0 :k1 v1 ... :kn vn))

It's sort of like a function signature of:
  '&rest args &keys k0 k1 ... kn'

Example:
  (defun example (&rest args)
    ...
    (-let*/-let (((arg-list kw-plist)
                  (elisp:parse:args+kwargs args
                                           :jeff :zort :vogon)))
    ...

Return a cons of lists: '(args-list . kwargs-plist)

NOTE: Not compatible with `dash' let functions like `-let', `-let*' when using
keywords that can have keyword values.
For example:
  1. This is  fine:
     (let* ((args '(id email :namespace :work :default failed-to-find-thing))
            (parsed (elisp:parse:args+kwargs args
                                             :namespace
                                             :timestamp
                                             :comment
                                             :default))
            (type (car parsed))
            (keys (cdr parsed)))
       (list (cons \"type:\" type)
             (cons \"keys:\" keys)))

  2. This loses the keyword `:namespace' value of `:work':
     (-let* ((args '(id email :namespace :work :default failed-to-find-thing))
             ((type keys) (elisp:parse:args+kwargs args
                                                   :namespace
                                                   :timestamp
                                                   :comment
                                                   :default)))

       (list (cons \"type:\" type)
             (cons \"keys:\" keys)))"
  (let ((leading-args nil)
        (rest-as-keywords nil)
        (keywords nil)
        ;; Flatten so callers don't have to use `apply' to get CLAIMS in correct format.
        (claims (elisp:list:flatten claims)))
    ;; Sort args into args/kwargs.
    (dolist (arg input)
      ;; Once we hit the first keyword arg, the rest are always all keywords.
      (if (not (or rest-as-keywords
                   (memq arg claims)))
        ;; Still in the args.
        (push arg leading-args)

        ;; Rest are keywords.
        (setq rest-as-keywords t)
        (push arg keywords)))

    ;; Done processing list, but our lists to return are backwords right now.
    (cons (nreverse leading-args) (nreverse keywords))))
;; (elisp:parse:args+kwargs '(jeff jefferson :namespace :work) :namespace)
;; (elisp:parse:args+kwargs '(jeff jefferson :namespace nil :value 42) :namespace :value)
;; (elisp:parse:args+kwargs '((jeff (jefferson)) :namespace nil :value 42) :namespace :value)
;;
;; `dash.el' ain't happy with values that are keywords:
;; (-let* ((args '(id email :namespace :work :default failed-to-find-thing))
;;         ((type keys) (elisp:parse:args+kwargs args :namespace :timestamp :comment :default)))
;;   (list (cons "type:" type)
;;         (cons "keys:" keys)))
;; (-let* ((args '(id email :namespace :work :default failed-to-find-thing))
;;         ((type keys) (elisp:parse:args+kwargs args :namespace :timestamp :comment :default))
;;         ((&plist :namespace :timestamp :comment :default) keys))
;;   (pp (list (cons "type:" type)
;;             (cons "keys:" keys)
;;             (cons :namespace namespace)
;;             (cons :timestamp timestamp)
;;             (cons :comment comment)
;;             (cons :default default))))
;; This does work, but... meh?
;; (-let* ((args '(id email :namespace :work :default failed-to-find-thing))
;;         (parsed (elisp:parse:args+kwargs args :namespace :timestamp :comment :default))
;;         (type (car parsed))
;;         (keys (cdr parsed))
;;         ((&plist :namespace :timestamp :comment :default) keys))
;;   (pp (list (cons "type:" type)
;;             (cons "keys:" keys)
;;             (cons :namespace namespace)
;;             (cons :timestamp timestamp)
;;             (cons :comment comment)
;;             (cons :default default))))


(defun elisp/cl:parse:filter-kwargs (args &rest claims)
  "Filter CLAIMS keywords & values out of ARGS and return.

ARGS should be a list.

CLAIMS should be all the keywords that the caller expects. It will be flattened
to a single list of keywords to look for.

Example:
  (cl-defun example (&rest args &key jeff &allow-other-keys)
  ...)
  (example :jeff \"Jeffory Jefferson\"
          :jill :jack :jim)

`jeff' will be:
  \"Jeffory Jefferson\"

`args' will be:
  '(:jeff \"Jeffory Jefferson\" :jill :jack :jim)

So you have `jeff' polluting your args.
To filter it out:
  (elisp/cl:parse:filter-kwargs
   (example :jeff \"Jeffory Jefferson\"
            :jill :jack :jim)
   :jeff)
Return value:
  '(:jill :jack :jim)"
  ;;------------------------------
  ;; Filter keys from ARGS
  ;;------------------------------
  (let (key
        args/filtered
        ;; Flatten so callers don't have to use `apply' to get CLAIMS in correct format.
        (claims (elisp:list:flatten claims)))
    (dolist (arg args)
      ;; If this is one of our keywords, save that fact for the next `arg', which is the value.
      (cond ((memq arg claims)
             (setq key arg))

            ;; Last `arg' was one of our keywords, so this one is its value.
            (key
             ;; We don't care; so just reset for next `arg'.
             (setq key nil))

            ;; Just an arg; save to the filtered list.
            (t
             (push arg args/filtered))))

    ;;------------------------------
    ;; Build & Return Filtered Args
    ;;------------------------------
    (nreverse args/filtered)))
;; (elisp/cl:parse:filter-kwargs '(:sudo? t :a 'a :b "bee" 'rest 'of 'the "args") :sudo?)
;; (elisp/cl:parse:filter-kwargs '(:sudo? t :a 'a :b "bee" 'rest 'of 'the "args") :sudo? :b)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide elisp functions)
