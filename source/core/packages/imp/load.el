;;; imp/load.el --- Load Files -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-05-07
;; Timestamp:  2025-10-29
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;                                 ──────────
;; ╔════════════════════════════════════════════════════════════════════════╗
;; ║                               Load Files                               ║
;; ╚════════════════════════════════════════════════════════════════════════╝
;;                                   ──────
;;                      This is, like, the whole point.
;;                                 ──────────
;;
;; Special thanks/apologies/condolences to Doom's `load!' macro,
;; which this was originally based off of, but is not anymore.
;;
;; Flagrantly stolen from `use-package-core.el',
;;  - https://github.com/jwiegley/use-package/blob/cba23de38d0a0c361f591be450d97dfc277469d3/use-package-core.el
;; And lovingly beaten to death.
;;   (apologies to John Wiegley)
;;
;;; Code:

(require 'cl-lib)

;;------------------------------------------------------------------------------
;;; Keyword Processing
;;------------------------------------------------------------------------------

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
          (imp--error 'imp-parser-normalize-plist error-string))))))

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
        (imp--error 'imp-parser-process-keywords
                    "%s is not a keyword"
                    keyword))
      (let* ((handler (concat "imp-parser-handler/" (symbol-name keyword)))
             (handler-sym (intern handler)))
        (if (functionp handler-sym)
            (funcall handler-sym feature keyword arg rest state)
          (imp--error 'imp-parser-process-keywords
                      "Keyword handler not defined: %s"
                      handler))))))

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


;;------------------------------------------------------------------------------
;;; Keyword Handlers
;;------------------------------------------------------------------------------

;;------------------------------
;;;; `FEATURE' arg
;;------------------------------

(defun imp-parser-normalize-feature (feature)
  (imp-feature-normalize feature))

;;------------------------------
;;;; `:disabled'
;;------------------------------
;; No handlers; handled by `imp-parser' personally.

;;------------------------------
;;;; `:error'
;;------------------------------

(defalias 'imp-parser-normalize/:error 'imp-parser-normalize-only-one-value-or-flag)
(defalias 'imp-parser-handler/:error   'imp-parser-handle-state)

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

(defun imp-parser-handler/:if (feature keyword pred rest state)
  (let ((body (imp-parser-process-keywords feature rest state)))
    `((when ,pred ,@body))))

(defalias 'imp-parser-handler/:when 'imp-parser-handler/:if)

(defun imp-parser-handler/:unless (feature keyword pred rest state)
  (let ((body (imp-parser-process-keywords feature rest state)))
    `((unless ,pred ,@body))))

;;------------------------------
;;;; `:requires'
;;------------------------------

(defalias 'imp-parser-normalize/:requires 'imp-parser-normalize-symlist)

(defun imp-parser-handler/:requires (feature keyword requires rest state)
  (let ((body (imp-parser-process-keywords feature rest state)))
    (if (null requires)
        body
      `((when ,(if (> (length requires) 1)
                   `(not (member nil (mapcar #'featurep ',requires)))
                 `(featurep ',(car requires)))
          ,@body)))))

;;------------------------------
;;;; `:path'
;;------------------------------

(defun imp-parser-normalize-path-symbol (feature keyword arg)
  "Convert a path symbol (eg `relative') into a path string."
  (when (symbolp arg)
    (cond
     ;; Symbol Itself (Placeholder Symbols)
     ;;------------------------------------

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
                    keyword
                    feature
                    arg)))

     ;; path == user emacs root
     ((memq arg '(emacs .emacs .emacs.d))
      user-emacs-directory)

     ;; path starts with ":/" or "./"
     ((or (string-prefix-p ":/" (symbol-name arg))
          (string-prefix-p "./" (symbol-name arg)))
      ;; redirect to the func that already knows how to deal with it.
      (imp-parser-normalize-path-string feature keyword (symbol-name arg)))

     ;; Symbol Value (ya know... a variable)
     ;;-------------------------------------
     ;; Don't use `bound-and-true-p'.
     ((when-let ((value (and (boundp arg) arg)))
        (when (stringp value)
          value)))

     ;; Unknown; Do Not Error
     ;;----------------------
     ;; path is not anything we know about.
     ;; Let someone else deal with whatever it is.
     (t nil))))
;; (imp-parser-normalize-path-symbol :user :path 'pwd)
;; (imp-parser-normalize-path-symbol :user :path 'user-emacs-directory)
;; (imp-parser-normalize-path-symbol :user :path 'foo)

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
      (if-let ((root (imp--path-root-dir feature :no-error)))
          (imp-path-join root (string-remove-prefix ":/" arg))
        (imp--error 'imp-parser-normalize-path-string
                    "`%S' has no root; `%S' doesn't know what to do with %S in %S"
                    feature
                    keyword
                    ":/"
                    arg)))

     ;; TODO?: look for "FEATURE-ROOT:/..."
     ;; TODO?: get root of FEATURE

     ;; otherwise, leave path string as-is
     (t arg))))
;; (imp-parser-normalize-path-string :user :path "/foo/bar")
;; (imp-parser-normalize-path-string :user :path "./foo/bar")
;; (imp-parser-normalize-path-string 'foo/bar/init :path ":/bar/")

(defun imp-parser-normalize-path-one-arg (feature keyword arg &optional no-recurse)
  ;; Don't handle lists of things. That is handled above us.
  (cond
   ;; Simple/Base Cases:
   ;;-------------------
   ;; A known shortcut symbol. example: `.emacs', `pwd', etc
   ((imp-parser-normalize-path-symbol feature keyword arg))

   ;; A string with optional prefix. example: "./", ":/"
   ((imp-parser-normalize-path-string feature keyword arg))

   ;; Recursive Cases:
   ;;----------------------------
   (no-recurse
    (imp--error 'imp-parser-normalize-path-one-arg
                "`%S': `%S' doesn't know what to do %S: %S"
                feature keyword (type-of arg) arg))

   ;; A func or form? example: (imp-path-join user-emacs-directory "path/to/imp")
   ((condition-case _
        (let ((arg-eval (eval arg)))
          ;; Require that the eval'd whatever returns a string.
          (when (stringp arg-eval)
            (imp-parser-normalize-path-one-arg feature
                                               keyword
                                               arg-eval
                                               :no-recurse)))
      (error nil)))

   ;; Unknown/Error Case:
   ;;----------------------------
   (t
    (imp--error 'imp-parser-normalize-path-one-arg
                "`%S': `%S' cannot parse %S: %S"
                feature keyword (type-of arg) arg))))
;; symbol: (imp-parser-normalize-path-one-arg 'test :path 'emacs)
;; string: (imp-parser-normalize-path-one-arg 'test :path "/path/to/foo")
;; form:   (imp-parser-normalize-path-one-arg 'test :path '(imp-path-join user-emacs-directory "path/to/imp"))
;; symbol-value: (imp-parser-normalize-path-one-arg 'user :path 'user-emacs-directory)
;; unknown: (imp-parser-normalize-path-one-arg 'test :path 'invalid)

(defun imp-parser-normalize-path-args-list (feature keyword args)
  "Determine what each arg in ARGS lits is, exactly.

ARGS should be the raw args list from func `imp-parser-normalize-keywords'."
  (if (sequencep args)
      (seq-map (apply-partially #'imp-parser-normalize-path-one-arg feature keyword)
               args)
    (imp--error 'imp-parser-normalize-path-args-list
                "`%S': `%S' expected a list of args. Got %S: %S"
                feature keyword (type-of args) args)))
;; symbol: (imp-parser-normalize-path-args-list 'test :path '(emacs))
;; string: (imp-parser-normalize-path-args-list 'test :path '("/path/to/foo"))
;; form:   (imp-parser-normalize-path-args-list 'test :path '((imp-path-join user-emacs-directory "path/to/imp")))
;; symbol-value: (imp-parser-normalize-path-args-list 'user :path '(user-emacs-directory))
;; unknown: (imp-parser-normalize-path-args-list 'test :path '(invalid))
;; not a list: (imp-parser-normalize-path-args-list 'test :path 'emacs)
;; (imp-parser-normalize-path-args-list 'user :path '("/foo/bar"))
;; (imp-parser-normalize-path-args-list 'user :path '("/foo/bar/"))
;; (imp-parser-normalize-path-args-list 'user :path '("foo/bar"))
;; (imp-parser-normalize-path-args-list 'user :path '("/foo/bar" "baz/qux/" pwd user-emacs-directory))
;; (imp-parser-normalize-path-args-list 'user :path '(pwd))
;; (imp-parser-normalize-path-args-list 'user :path '(foo))

(defun imp-parser-normalize-paths (feature keyword args)
  "Normalize ARGS (list) to absolute, canonical paths."
  ;; Normalize args list into list of paths.
  (let ((paths (imp-parser-normalize-path-args-list feature keyword args)))

    ;; Validate normalization.
    (when (seq-find (lambda (p) (not (stringp p))) paths)
      (imp--error 'imp-parser-normalize-paths
                  "`%S': `%S' couldn't normalize path(s). Got a non-string: %S -> S%"
                  feature
                  keyword
                  args
                  paths))

    ;; Canonicalize the list of paths.
    (seq-map #'imp-path paths)))
;; (imp-parser-normalize-paths :user :path '("/foo/bar"))
;; (imp-parser-normalize-paths :user :path '("foo/bar"))
;; (imp-parser-normalize-paths :user :path '("/foo/bar" "baz/qux/"))
;; (imp-parser-normalize-paths :user :path '(pwd))
;; (imp-parser-normalize-paths :user :path '(user-emacs-directory))
;; (imp-parser-normalize-paths :user :path '(foo))

(defun imp-parser-normalize/:path (feature keyword args)
  "Normalize `:path' ARGS to absolute & canonical path string.

If ARGS is `guess', make a guess at where the path to FEATURE probably is:
  1) Using FEATURE's root path.
  2) Using Emacs's current working directory.
Else ARGS will be used as-is for the path.

If the path is relative, root it in one of:
  1) FEATURE's root path
  2) `user-emacs-directory'"
  (let* ((funcname 'imp-parser-normalize/:path)
         ;; Normalize to canonical, absolute path(s).
         (paths (imp-parser-normalize-paths feature keyword args))
         (feature-path (imp-feature-split feature))
         ;; Normalize path list to one path.
         ;; `imp-parser-normalize-paths' works on and returns lists.
         (path (car paths)))

    ;; If no path supplied and feature is rooted, use feature root's path.
    (when (and (not path)
               (imp-feature-root feature))
      (setq path (imp-path-root-get (imp-feature-root feature)))
      ;; Take FEATURE out of PATH since we used it just now.
      (setq feature-path (imp-feature-unrooted feature)))

    ;; Double Check: We got an absolute path, right?
    (unless (and (stringp path)
                 (file-name-absolute-p path))
      (imp--error funcname
                  "%S: `%S' should end up with an absolute path: got: %S -> %S -> %S"
                  feature
                  keyword
                  args
                  paths
                  path))

    ;; We're done if path itself is a full, loadable path.
    (unless (imp-path-load-file path)
      ;; Add rest of FEATURE to path; is a relative path.
      (setq path (apply #'imp-path-join
                        path
                        ;; FEATURE, with or without root.
                        feature-path)))

    path))
;; (imp-parser-normalize/:path 'user :path '("/foo/bar"))
;; (imp-parser-normalize/:path 'user :path '("foo/bar"))
;; (imp-parser-normalize/:path 'user :path '("/foo/bar" "baz/qux"))
;; (imp-parser-normalize/:path 'user :path '(pwd))
;; (imp-parser-normalize/:path 'user :path '(user-emacs-directory))
;; (imp-parser-normalize/:path 'user :path '(foo))

(defun imp-parser-handler/:path (feature keyword arg rest state)
  "Put `:path' and `:path-load' into state."
  (setq state (imp-parser-plist-maybe-put state keyword arg))

  (if-let ((path-load (imp-path-load-file arg)))
      (setq state (imp-parser-plist-maybe-put state :path-load path-load))
    (imp--error 'imp-parser-handler/:path
                '("Cannot find a load path from path. "
                  "Is path pointing to a file (with or without ext)? "
                  "path: %S")
                arg))

  ;; TODO(stats): Add path & load-path to stats?
  (imp-parser-process-keywords feature rest state))

;;------------------------------
;;;; `:root'
;;------------------------------

(defun imp-parser-normalize/:root (feature keyword args)
  (let ((root (imp-parser-only-one
                feature
                (imp-parser-normalize-flag feature args)
                #'imp-parser-normalize-symbol-or-string)))
    (imp-feature-first (if (eq t root) feature root))))

(defun imp-parser-handler/:root (feature keyword arg rest state)
  ;; TODO(stats): Add root to stats?
  (imp-parser-concat
   ;; Add root before load happens so that subfeatures can use their root.
   ;; Example: Loading `imp/init.el' will load all of imp's files, some of which
   ;; expect imp's root path to exist.
   `((imp-path-root-set ',arg
                        ,(imp-path-parent (plist-get state :path-load))))
   (imp-parser-process-keywords feature rest state)))

;; TODO(stats): uncomment
;; ;;------------------------------
;; ;;;; `:stats'
;; ;;------------------------------
;;
;; (defun imp-parser-normalize/:stats (feature keyword args)
;;   (imp-parser-normalize-symbols feature
;;                                 (imp-parser-normalize-flag feature args)))
;; ;; (imp-parser-normalize/:stats 'testing :stats nil)
;; ;; (imp-parser-normalize/:stats 'testing :stats '(t))
;; ;; (imp-parser-normalize/:stats 'testing :stats '(autoshow))
;; ;; (imp-parser-normalize/:stats 'testing :stats '(timing autoshow))
;; ;; (imp-parser-normalize/:stats 'testing :stats '("hello"))
;;
;; (defun imp-parser-handler/:stats (feature keyword arg rest state)
;;   ;; ARG is a list of symbols.
;;   (let (denormalized)
;;     ;; Denormalize stats into stat flags.
;;     (when (or (memq t         arg)
;;               (memq 'default  arg)
;;               (memq 'defaults arg))
;;       ;; Expand t into default stat flags.
;;       ;; TODO(stats): Put default flags in a const/var/custom.
;;       (setq denormalized (append denormalized '(timing))))
;;
;;     ;; Add known stat flags.
;;     ;; TODO(stats): Put valid flags in a const/var/custom.
;;     (when-let ((valid (cl-intersection arg '(timing autoshow debug))))
;;       (setq denormalized (append denormalized valid)))
;;
;;     ;; TODO(stats): error for unknown flags?
;;
;;     ;; Deduplicate flags.
;;     (setq denormalized (seq-uniq denormalize))
;;
;;     ;; Add stat flags into state & continue to next handler.
;;     (imp-parser-handle-state feature keyword denormalized rest state)))

;;------------------------------
;;;; `:catch'
;;------------------------------
;; TODO: Add this keyword back in?
;; TODO: is this better than `imp-parser-verbose' == `errors'?
;; TODO: is this better than `imp-parser-verbose' == `debug'?

;; (defvar imp-parser--form)
;; (defvar imp-parser--hush-function #'(lambda (keyword body) body))

;; (defsubst imp-parser-hush (context keyword body)
;;   `((condition-case-unless-debug err
;;         ,(macroexp-progn body)
;;       (error (funcall ,context ,keyword err)))))

;; (defun imp-parser-normalize/:catch (feature keyword args)
;;   (if (null args)
;;       t
;;     (imp-parser-only-one keyword args
;;       imp-parser--hush-function)))

;; (defun imp-parser-handler/:catch (feature keyword arg rest state)
;;   (let* ((context (cl-gentemp "imp-parser--warning")))
;;     (cond
;;      ((not arg)
;;       (imp-parser-process-keywords feature rest state))
;;      ((eq arg t)
;;       `((defvar ,context
;;           #'(lambda (keyword err)
;;               (let ((msg (format "%s/%s: %s" ',feature keyword
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
;;                      (imp-parser-process-keywords feature rest state)))))
;;      ((functionp arg)
;;       `((defvar ,context ,arg)
;;         ,@(let ((imp-parser--hush-function
;;                  (apply-partially #'imp-parser-hush context)))
;;             (funcall imp-parser--hush-function keyword
;;                      (imp-parser-process-keywords feature rest state)))))
;;      (t
;;       (imp--error 'imp-parser-handler/:catch "The :catch keyword expects 't' or a function")))))


;;------------------------------
;;;; `:defer'
;;------------------------------
;; TODO: Add this keyword back in?
;; TODO: Make it able to defer for N seconds after emacs init.
;; TODO: make it able to defer until N seconds of emacs being idle?

;; (defalias 'imp-parser-normalize/:defer 'imp-parser-normalize-predicate)

;; (defun imp-parser-handler/:defer (feature keyword arg rest state)
;;   (let ((body (imp-parser-process-keywords feature rest state)))
;;     (imp-parser-concat
;;      ;; Load the package after a set amount of idle time, if the argument to
;;      ;; `:defer' was a number.
;;      (when (numberp arg)
;;        `((run-with-idle-timer ,arg nil #'require
;;                               ',(imp-parser-as-symbol feature) nil t)))
;;      (if (or (not arg) (null body))
;;          body
;;        `((eval-after-load ',feature ',(macroexp-progn body)))))))


;;------------------------------
;;;; `:after'
;;------------------------------

(defun imp-parser-normalize/:after (feature keyword args)
  (setq args (imp-parser-normalize-recursive-symlist feature keyword args))
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

(defun imp-parser-handler/:after (feature keyword arg rest state)
  (let ((body (imp-parser-process-keywords feature rest state))
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
;; TODO: Add something similar for `let' vars?

;; (defun imp-parser-normalize/:custom (feature keyword args)
;;   "Normalize imp-parser custom keyword."
;;   (imp-parser-as-one keyword args
;;     #'(lambda (keyword arg)
;;         (unless (listp arg)
;;           (imp--error 'imp-parser-normalize/:custom
;;                       "%S expects a (<symbol> <value> [comment])"
;;                       " or list of these"
;;                       keyword))
;;         (if (imp-parser-non-nil-symbolp (car arg))
;;             (list arg)
;;           arg))))

;; (eval-and-compile
;;   ;; Declare a synthetic theme for :custom variables.
;;   ;; Necessary in order to avoid having those variables saved by custom.el.
;;   (deftheme imp-parser))

;; (enable-theme 'imp-parser)
;; ;; Remove the synthetic imp-parser theme from the enabled themes, so
;; ;; iterating over them to "disable all themes" won't disable it.
;; (setq custom-enabled-themes (remq 'imp-parser custom-enabled-themes))

;; (defcustom imp-parser-use-theme t
;;   "If non-nil, use a custom theme to avoid saving :custom
;; variables twice (once in the Custom file, once in the imp-parser
;; call)."
;;   :type 'boolean
;;   :group 'imp-parser)

;; (defun imp-parser-handler/:custom (feature keyword args rest state)
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
;;                        (setq comment (format "Customized with imp-parser %s" feature)))
;;                      `'(,variable ,value nil () ,comment)))
;;                args))))
;;      (mapcar
;;       #'(lambda (def)
;;           (let ((variable (nth 0 def))
;;                 (value (nth 1 def))
;;                 (comment (nth 2 def)))
;;             (unless (and comment (stringp comment))
;;               (setq comment (format "Customized with imp-parser %s" feature)))
;;             `(customize-set-variable (quote ,variable) ,value ,comment)))
;;       args))
;;    (imp-parser-process-keywords feature rest state)))


;;------------------------------------------------------------------------------
;;; The Main Macro(es)
;;------------------------------------------------------------------------------

(defmacro imp-core (feature args)
  `(let* ((feature* (imp-parser-normalize-feature ,feature))
          (args* (imp-parser-normalize-keywords feature* ,args))
          (imp-parser--form
           (if (eq imp-parser-verbose 'debug)
               (concat "\n\n"
                       (pp-to-string `(imp feature* ,@,args))
                       "\n  -->\n\n"
                       (pp-to-string `(imp feature* ,@args*))
                       "\n  ==>\n\n"
                       (pp-to-string
                        (macroexp-progn
                         (let ((imp-parser-verbose 'errors)
                               (imp-parser-expand-minimally t))
                           (imp-parser-process-keywords feature* args*)))))
             "")))
     (when (eq imp-parser-verbose 'debug)
       (message (make-string 40 ?━))
       (message "%s" feature*)
       (message (make-string 40 ?━))
       (message "%s" imp-parser--form))

     (imp-parser-process-keywords feature* args*)))


;;;###autoload
(defmacro imp (feature &rest args)
  "Load FEATURE.

Usage:

  (imp feature
     [:keyword [option]]...)

:disabled      Flag. Nothing happens; FEATURE is ignored completely if this keyword
               is present.

:path PATH     Absolute, relative, or rooted path to file.
               PATH can be:
                 - A path string.
                   - absolute or relative
                   - prefixes:
                     - \"./\" - relative to `imp-path-current-dir'
                     - \":/\" - relative to FEATURE's root
                 - A symbol whose value is a path string.
                   - example: `user-emacs-directory'
                 - A symbol whose name is known.
                   - examples:
                     - `pwd' - `imp-path-current-dir'
                     - `.emacs' - `user-emacs-directory'
                     - `root' - `imp-feature-root'
                   - See func `imp-parser-normalize-path-symbol'
                 - A form/function that evaluates to a path string.
                   - (imp-path-join user-emacs-directory \"path/to/imp\")
TODO(path): Do we want this to be the solution for lists?
                 - A list of the above to join into a path.
TODO(path): Alternative is a list of paths to try for locating the file.

:root ROOT     Create an imp feature root for ROOT at PATH given in `:path'.
               See var `imp-path-roots' and func `imp-path-root-set'.
               If ROOT is t or a flag (arg-less), use first part of FEATURE.
               Example:
                 (imp imp/init
                   :path (imp-path-join user-emacs-directory
                                        \"path/to/imp\")
                   :root)
                 => imp-path-roots: '((imp \"~/.config/emacs/path/to/imp\") ...)

:error ERR     Value (aka ERROR) can be:
                 - nil
                 - non-nil (default)
                 - A form that should evaluate to one of the above.
               If ERROR is nil, the function will not raise an error if:
                 - The file doesn't exist.
                 - FEATURE isn't provided after loading the file.
               It will still raise an error if:
                 - It cannot parse the inputs.
                 - It cannot determine where to /look/ for the file.

:optional OPT  Load FEATURE if it exists; do nothing if it does not.
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

:after AFTER   Delay the effect of the imp declaration until after the
               named features have loaded. Before they have been loaded, no
               other keyword has any effect at all, and once they have been
               loaded it is as if `:after' was not specified."
  (declare (indent defun))
  (unless (memq :disabled args)
    (macroexp-progn
     (imp-parser-concat
      ;; (when imp-parser-compute-statistics
      ;;   `((imp-parser-statistics-gather :imp ',feature nil)))

      ;; Let errors bubble up for now. They're more useful for debugging
      ;; and I don't have `imp-parser' integrated with imp's preexisting
      ;; debug/error stuff.
      (imp-core feature args)

      ;; (if (memq imp-parser-verbose '(errors debug))
      ;;     (imp-core feature args)
      ;;   (condition-case-unless-debug err
      ;;       (imp-core feature args)
      ;;     (error
      ;;      (ignore
      ;;       (display-warning
      ;;        'imp
      ;;        (format "Failed to parse package %s: %s"
      ;;                feature (error-message-string err))
      ;;        :error)))))

      ;; (when imp-parser-compute-statistics
      ;;   `((imp-parser-statistics-gather :imp ',feature t)))
      ))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide imp load)
