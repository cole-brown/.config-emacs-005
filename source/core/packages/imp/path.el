;;; imp/path.el --- Path Functions -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-05-07
;; Timestamp:  2026-10-01
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;                                 ──────────
;; ╔════════════════════════════════════════════════════════════════════════╗
;; ║                                  Path                                  ║
;; ╚════════════════════════════════════════════════════════════════════════╝
;;                                   ──────
;;         Don't forget to use Special Relativity for relative paths.
;;                                 ──────────
;;
;; Paths. File, directory, relative, absolute...
;;
;; ---
;;
;; Path construction operates on filenames; the named files and directories
;; need not exist.
;;
;; During macro expansion, path construction must only inspect syntax and
;; generate code. It must not evaluate argument forms, invoke file-name
;; handlers, or access the filesystem. Invalid syntax may signal an error.
;;
;; At runtime, path construction may evaluate arguments and use standard
;; filename operations, including file-name handlers. It must not explicitly
;; check existence, inspect file attributes, or resolve symlinks.
;;
;; Filesystem access belongs to explicit runtime operations: validating
;; directories, resolving symlinks, and locating files to load. Their
;; documentation must describe that access.
;;
;;; Code:

(require 'seq)


;;------------------------------------------------------------------------------
;; Error Handling
;;------------------------------------------------------------------------------

(defvar imp-path-error? t
  "Should imp path functions raise errors?

nil     - raise errors
non-nil - ignore errors; return nil")


(defun imp--path-error (caller string &rest args)
  "Error function that respects `imp-path-error?'."
  (apply #'imp--error-if imp-path-error? caller string args))

;; TODO: instead of `imp-path-error?', make a macro that puts FORMS inside
;; of a `condition-case-unless-debug'? `imp-error-ignore'?


;;------------------------------------------------------------------------------
;; Path Builders
;;------------------------------------------------------------------------------

(defun imp--path-segment-normalize (input)
  "Ensure INPUT is a string.

INPUT should be a string, keyword, or symbol.
  - If it's a string, use as-is.
  - If it's a keyword/symbol, use the symbol's name sans \":\".
    - 'foo -> \"foo\"
    - :foo -> \"foo\"

Return a string."
  (declare (side-effect-free t))
  (cond ((null input) ;; Let nil through so `imp-path-join` functions correctly.
         nil)

        ((stringp input) ;; String good. Want string.
         input)

        ;; Keyword? Use its name.
        ((keywordp input)
         ;; But strip the keyword's leading colon.
         (string-remove-prefix ":" (symbol-name input)))

        ;; Symbol? Use its name.
        ((symbolp input)
         (symbol-name input))

        (t
         (imp--path-error 'imp--path-segment-normalize
                          "INPUT must be string or keyword/symbol. Got %S: %S"
                          (type-of input)
                          input))))
;; (imp--path-segment-normalize nil)
;; (imp--path-segment-normalize :foo)
;; (imp--path-segment-normalize :f:o:o)
;; (imp--path-segment-normalize :D:/foo)
;; (imp--path-segment-normalize 'foo)
;; (imp--path-segment-normalize "foo")
;; (imp--path-segment-normalize :/bar)
;; (imp--path-segment-normalize '/bar)
;; (imp--path-segment-normalize "/bar")


(defun imp--path-segment-append (parent next)
  "Append NEXT element to PARENT, adding dir separator if needed."
  (declare (side-effect-free t))
  (let ((parent (imp--path-segment-normalize parent))
        (next   (imp--path-segment-normalize next)))
    ;; Error checks first.
    (cond ((and parent
                (not (stringp parent)))
           (imp--path-error 'imp--path-segment-append
                            "Paths to append must be strings. PARENT is: %S"
                            parent))
          ((or (null next)
               (not (stringp next)))
           (imp--path-error 'imp--path-segment-append
                            "Paths to append must be strings. NEXT is: %S"
                            next))

          ;;---
          ;; Append or not?
          ;;---
          ;; Expected initial case for appending: nil parent, non-nil next.
          ((null parent)
           next)

          (t
           (concat (file-name-as-directory parent) next)))))
;; (imp--path-segment-append :/foo 'bar)
;; (imp--path-segment-append nil nil)
;; (let (imp-path-error?) (imp--path-segment-append nil nil))


(defun imp-path-join (&rest path)
  "Combine PATH segments together into a path.

(imp-path-join \"jeff\" \"jill.el\")
  ->\"jeff/jill.el\""
  (declare (side-effect-free t))
  (if-let ((flattened (imp--list-flatten path)))
    (seq-reduce #'imp--path-segment-append
                flattened
                nil)
    (imp--path-error 'imp-path-join
                     "Cannot join nothing. PATH = %S => %S"
                     path
                     flattened)))
;; (imp--list-flatten '(:foo bar))
;; (imp--list-flatten '(nil nil))
;; (imp-path-join "/foo" "bar.el")
;; (imp-path-join '("/foo" ("bar.el")))
;; (imp-path-join "foo" "bar.el")
;; (imp-path-join "foo")
;; (imp-path-join nil nil)
;; (let (imp-path-error?) (imp-path-join nil))


(defun imp-path-split (path)
  "Split PATH into a list of dir/file names.

(imp-path-split \"/path/to/some/where.txt\")
  => (\"path\" \"to\" \"some\" \"where.txt\")

Split on forward or backward slash if `system-type' is `windows-nt'.
Else split on forward slash only."
  (declare (side-effect-free t))
  (if (stringp path)
      (string-split path
                    ;; Only backslashes if Windows path.
                    (if (eq system-type 'windows-nt)
                        (rx (any "/" "\\"))
                      (rx "/"))
                    t)
    (imp--path-error 'imp-path-split
                     "PATH must be a string; got '%s'. PATH = %S"
                     (type-of path)
                     path)))
;; (imp-path-split (imp-path-current-file))
;; (imp-path-split "/path/to/some/where.txt")
;; (imp-path-split "C:\\path\\to\\some\\where.txt")
;; (imp-path-split :foo)
;; (let (imp-path-error?) (imp-path-split :foo))


;;------------------------------------------------------------------------------
;; Path Validation
;;------------------------------------------------------------------------------

(defun imp--path-validate (path)
  "PATH is an absolute path string."
  (cond ((not (stringp path))
         (imp--path-error 'imp--path-validate
                          "PATH must be string. Got a %s: '%s'"
                          (type-of path)
                          path))

        ((not (file-name-absolute-p path))
         (imp--path-error 'imp--path-validate
                          "PATH string must be an absolute path. Got: '%s'"
                          path))

        ;; OK: PATH is string and absolute path
         (t
          path)))

(defun imp--path-validate-root (path)
  "Check that PATH is a vaild root path."
  ;; Is it a valid path?
  (when (imp--path-validate path)
    ;; Is it a valid /root/ path?
    (unless (file-directory-p path)
      (imp--path-error 'imp--path-validate-root
                       "Path does not exist or is not a directory: %s"
                       path))
    path))


;;------------------------------------------------------------------------------
;; Path Normalization
;;------------------------------------------------------------------------------

(defun imp-path-normalize (path)
  "Normalize PATH string.

1. Fully expand path.
2. Follow symlinks.
3. Abbreviate path.

Return nil when PATH is not absoulte path string."
  (declare (side-effect-free t))
  (if (and (stringp path)
           (file-name-absolute-p path))
      ;; Convert "/home/USER/" to "~/".
      (abbreviate-file-name
       ;; Follow symlinks, remove ".."
       (file-truename
        ;; Remove trailing slash.
        (directory-file-name
         ;; Get absolute path.
         ;; Ignore default-directory for `expand-file-name'.
         (expand-file-name path nil))))

    (imp--path-error 'imp-path-normalize
                     "PATH must be absoulte path string. Got %S: %S"
                     (type-of path)
                     path)))
;; (imp-path-normalize "/foo/bar/baz")
;; (imp-path-normalize "bar/baz")
;; (imp-path-normalize nil)


(defun imp-path-of-feature (feature)
  "Convert FEATURE into a path string.

Path string will be absolute if FEATURE:
  - has a root in `imp-path-roots'
  - starts with `./'
Else path string will be relative."
  (declare (side-effect-free t))
  (let ((feature (imp-feature-normalize feature)))
    ;; Does FEATURE have a root path?
    (if-let* ((feature-root (imp-feature-root feature))
              (path-root (imp-path-root-get feature-root)))
        ;; Join feature's root path with the rest of feature.
        (apply #'imp-path-join
               path-root
               (imp-feature-unrooted feature))

      ;; Is FEATURE rooted "here"?
      (if (string-prefix-p "./" (symbol-name feature))
          (apply #'imp-path-join (imp-path-current-dir)
                 (imp-feature-split (imp-feature-rest feature)))

        ;; No root; make relative path.
        (apply #'imp-path-join (imp-feature-split feature))))))
;; (imp-path-of-feature 'imp:/foo/bar)
;; (imp-path-of-feature 'imp)
;; (imp-path-of-feature './foo/bar)
;; (imp-path-of-feature 'foo/bar)


(defun imp-path-relative (root path)
  "Get segment of PATH that is relative to ROOT.

ROOT should be:
  - feature - Something that `imp-feature-normalize' can handle.
    - Returned path will be relative to the entry in `imp-roots'.
    - Will raise an error if the feature does not have a path root.
  - string - an absolute path
    - Returned path will be relative to this absolute path.

PATH should be an absolute path string."
  (declare (side-effect-free t))
  ;; Both ROOT and PATH must be valid (absolute) paths.
  (when-let* ((root (file-name-as-directory ; add trailing slash so regex replace is cleaner
                     (imp-path-normalize
                      (imp--path-validate (if (and (not (null root))
                                                   (symbolp root))
                                              (imp-path-of-feature root)
                                            root)))))
              (path (imp-path-normalize (imp--path-validate path)))
              ;; Don't like `file-relative-name' as it can return weird things
              ;; when it goes off looking for actual directories and files...
              ;; This path library is for theoretical paths.
              (path-relative (replace-regexp-in-string
                              ;; Make sure root dir has ending slash.
                              root ;; Look for root directory path...
                              ""        ;; Replace with nothing to get a relative path.
                              path
                              :fixedcase
                              :literal)))

    ;; End up with the same thing? Not a relative path - signal error?
    (if (string= path-relative path)
        (imp--path-error 'imp-path-relative
                         '("PATH is not relative to ROOT!\n"
                           "  PATH: %S\n"
                           "  ROOT: %S\n"
                           "---> result:    %s")
                         path
                         root
                         path-relative)
      path-relative)))
;; (imp-path-relative 'imp:/path (imp-path-join (imp-path-current-dir) "path/to/thing"))
;; (imp-path-relative (imp-path-current-dir) (imp-path-join (imp-path-current-dir) "path/to/thing"))
;; (imp-path-relative nil (imp-path-join (imp-path-current-dir) "path/to/thing"))


;;------------------------------------------------------------------------------
;; File Helpers
;;------------------------------------------------------------------------------

(defun imp-file-name (path &optional no-ext)
  "Return the filename component of PATH.

If NO-EXT is non-nil, remove one file extenstion."
  (declare (pure t) (side-effect-free t))
  (funcall (if no-ext #'imp-path-sans-extension #'identity)
           (file-name-nondirectory path)))
;; (imp-file-name "/foo/bar/")
;; (imp-file-name "/foo/bar.el")
;; (imp-file-name "/foo/bar.el" t)


(defun imp-file-current (&optional no-ext)
  "Return the filename (no path, just filename) that this is called from."
  (imp-file-name (imp-path-current-file) no-ext))
;; (imp-file-current)
;; (imp-file-current t)

(defun imp-path-sans-extension (path ext)
  "Remove EXT from PATH, if present.

EXT should be a string or one of these symbols:
  `t', `any', `:any'
If EXT is string, only remove PATH's extension if it matches EXT.
If EXT is a valid symbol, remove whatever extension that PATH has.

(imp-path-sans-extension \"jeff/jill.el\" \".el\")
  ->\"jeff/jill\""
  (let ((any-ext '(t any :any)))
    ;; NOTE: This should work with just about any path/file string,
    ;; so go easy on input validation.
    (cond ((and (not (stringp ext))
                (not (memq ext any-ext)))
           (imp--path-error 'imp-path-sans-extension
                            "EXT must be a string. Got %S: %S"
                            (type-of ext)
                            ext))
          ((not (stringp path))
           (imp--path-error 'imp-path-sans-extension
                            "PATH must be a string. Got %S: %S"
                            (type-of path)
                            path))
          ;; Remove whatever EXT.
          ((memq ext any-ext)
           (file-name-sans-extension path))
          ;; Remove specific EXT.
          ((string= (file-name-extension path)
                    (string-remove-prefix "." ext))
           (file-name-sans-extension path))
          ;; EXT not found to remove; return original PATH.
          (t
           path))))
;; (imp-path-sans-extension "foo/bar/" ".el")
;; (imp-path-sans-extension "foo/bar/" t)
;; (imp-path-sans-extension "foo/bar/baz.el" ".el")
;; (imp-path-sans-extension "foo/bar/baz.el" "el")
;; (imp-path-sans-extension "foo/bar/baz.el" :any)


;;------------------------------------------------------------------------------
;; Path Helpers
;;------------------------------------------------------------------------------

(defun imp-path-parent (path)
  "Return the parent directory component of PATH."
  (cond
   ;;------------------------------
   ;; Validation
   ;;------------------------------
   ((not (stringp path))
    (imp--path-error 'imp-path-parent
                     "PATH must be a string. Got %S: %S"
                     (type-of path)
                     path))

   ;;------------------------------
   ;; Figure out path type so we can figure out parent.
   ;;------------------------------
   ;; Directory path?
   ((string= (file-name-as-directory path) path)
    ;; First get the dirname:      "/foo/bar/" -> "/foo/bar"
    ;; Then get its parent dir:    "/foo/bar"  -> "/foo/"
    ;; Then get the parent's name: "/foo/"     -> "/foo"
    (directory-file-name
     (file-name-directory
      (directory-file-name path))))

   ;; File path?
   (t
    ;; First get its parent dir:   "/foo/bar.el" -> "/foo/"
    ;; Then get the parent's name: "/foo/"       -> "/foo"
    (directory-file-name (file-name-directory path)))))
;; (imp-path-parent "/foo/bar/")
;; (imp-path-parent "/foo/bar.el")


(defun imp-path-current-file ()
  "Return the path of the file this function is called from."
  (cond
   ;;------------------------------
   ;; Look for a valid "current file" variable.
   ;;------------------------------
   ((bound-and-true-p byte-compile-current-file))

   ((bound-and-true-p load-file-name))

   ((stringp (car-safe current-load-list))
    (car current-load-list))

   ;; Indirect buffers don't have a `buffer-file-name'; you need to get their
   ;; base buffer first. But direct buffers have a `nil' base buffer, so... this
   ;; works for both direct and indirect buffers:
   ((buffer-file-name (buffer-base-buffer)))

   ;;------------------------------
   ;; Error: Didn't find anything valid.
   ;;------------------------------
   (t
    (imp--path-error 'imp-path-current-file
                     "Cannot get this file-path"))))
;; (imp-path-current-file)


(defun imp-path-current-dir ()
  "Return the directory path of the file this is called from."
  (when-let (path (imp-path-current-file))
    (directory-file-name (file-name-directory path))))
;; (imp-path-current-dir)


;;------------------------------------------------------------------------------
;; `load' Paths
;;------------------------------------------------------------------------------

(defun imp-path-has-load-extension (path)
  "Return non-nil if PATH ends with a known load extension.

See func `get-load-suffixes' for known load extenstions."
  (seq-reduce (lambda (result ext)
                (or result
                    (string-suffix-p ext path)))
              (get-load-suffixes)
              nil))


(defun imp-path-load-file (path-absolute)
  "Return string path to existing file or nil."
  ;; return nil if not a string.
  (when (and path-absolute
             (stringp path-absolute))
    ;; Use the same function `load' uses to find its files: `locate-file'
    (locate-file path-absolute
                 '("/") ; Don't use `load-paths'; we have an absolute path.
                 (unless (imp-path-has-load-extension path-absolute)
                   (get-load-suffixes)))))


;;------------------------------------------------------------------------------
;; /The/ Path Macro
;;------------------------------------------------------------------------------

(eval-and-compile
  (defun imp--path-expand (segments)
    "Return a standard Elisp expression joining flat path SEGMENTS.

Strings, keywords, and quoted symbols are literal segments.  Other symbols
and forms are expressions whose values must be strings at execution time.

Expansion uses only syntax and string operations. It must not evaluate
argument forms, invoke file-name handlers, or access the filesystem.
Invalid segment syntax may signal an error."
    (unless segments
      (error "imp-path requires at least one segment"))
    (let ((segments
           (mapcar
            (lambda (segment)
              (cond
               ((stringp segment) segment)
               ((keywordp segment) (substring (symbol-name segment) 1))
               ((eq (car-safe segment) 'quote)
                (unless (and (consp (cdr segment))
                             (null (cddr segment))
                             (or (stringp (cadr segment))
                                 (and (symbolp (cadr segment))
                                      (cadr segment))))
                  (error "imp-path expects a quoted string or symbol: %S" segment))
                (let ((literal (cadr segment)))
                  (cond ((stringp literal) literal)
                        ((keywordp literal) (substring (symbol-name literal) 1))
                        (t (symbol-name literal)))))
               ((and segment (not (eq segment t))
                     (or (symbolp segment) (consp segment)))
                segment)
               (t (error "Invalid imp-path segment: %S" segment))))
            segments)))
      ;; Build from the right so adjacent literals become a single suffix.
      ;; Each expression still appears once, in left-to-right evaluation order.
      (let* ((reversed (reverse segments))
             (joined (car reversed)))
        (dolist (segment (cdr reversed))
          (setq joined
                (if (and (stringp segment) (stringp joined))
                    ;; Never dispatch to a file-name handler while folding.
                    (let ((directory
                           (if (memq system-type '(windows-nt ms-dos))
                               (subst-char-in-string ?\\ ?/ segment)
                             segment)))
                      (when (and (eq system-type 'windows-nt)
                                 (bound-and-true-p w32-downcase-file-names))
                        (setq directory (downcase directory)))
                      (concat directory
                              (cond ((equal directory "") "./")
                                    ((string-suffix-p "/" directory) "")
                                    (t "/"))
                              joined))
                  `(concat (file-name-as-directory ,segment) ,joined))))
        joined))))


(defmacro imp-path (&rest segments)
  "Join flat SEGMENTS and expand to an absolute path without a trailing slash.

Strings, keywords, and quoted symbols denote literal path segments:
  (imp-path user-emacs-directory \='source :user)

Bare variables and forms are evaluated once, from left to right, and must
return strings.  Relative paths use `default-directory' at execution time.
Nested segment lists are not supported; pass each segment separately.

The expansion uses only standard Elisp calls.  It does not follow symlinks
or abbreviate the resulting path."
  (declare (debug (&rest form)))
  `(directory-file-name
    (expand-file-name ,(imp--path-expand segments))))
;; (macroexpand-1 '(imp-path user-emacs-directory 'source :user))
;; (macroexpand-1 '(imp-path (locate-user-emacs-file "init.el")))


;;------------------------------------------------------------------------------
;; Root Paths
;;------------------------------------------------------------------------------

(defun imp-path-root-set (feature dirpath)
  "Set the root path(s) of FEATURE for future `imp-require' calls.

DIRPATH is the directory under which all of FEATURE exist."
  (let ((funcname 'imp-path-root-set)
        ;; normalize inputs
        (feature-base (imp-feature-first feature))
        (path-root (imp-path dirpath)))

    (imp--error-if (not feature-base)
                   funcname
                   "Could not normalize FEATURE. %S -> %S"
                   feature
                   feature-base)

    (cond ((imp-path-root-get feature-base :no-error)
           ;; ignore exact duplicates.
           (unless (string= (imp-path-root-get feature-base :no-error)
                            path-root)
             (imp--path-error funcname
                              '("Feature is already an imp root. "
                                "FEATURE: %S "
                                "feature base: %S "
                                "existing path: %s "
                                "requested path: %s")
                              feature
                              feature-base
                              (imp-path-root-get feature-base :no-error)
                              path-root)))

          ;; `imp--path-validate-root' will error with better reason, so the
          ;; error here isn't actually triggered... I think?
          ((not (imp--path-validate-root path-root))
           (imp--path-error funcname
                            "Path must be a valid directory: %s" path-root))

          ;; Ok; set the feature's root to the path.
          (t
           (push (list feature-base path-root) imp-roots)))))
;; (imp-path-root-set 'imp (imp-path-current-dir))


(defun imp-path-root-get (feature)
  "Get the root directory from `imp-roots' for FEATURE.

Return path string from `imp-roots' or nil."
  ;; TODO(path): need to be able to tell `imp-feature-normalize' to error or not
  (if-let* ((feature-norm (imp-feature-normalize feature))
            (dir (nth 0 (imp--alist-get-value feature
                                              imp-roots))))
      (imp-path dir)
    ;; this returns nil if we're not erroring.
    (imp--path-error 'imp-path-root-get
                     "FEATURE is unknown: %S -> %S"
                     feature
                     feature-norm)))
;; (imp-path-root-get 'imp)
;; (imp-path-root-get 'imp 'no-error)
;; (imp-path-root-get 'dne)
;; (imp-path-root-get 'dne t)


(defun imp-path-root-delete (feature)
  "Delete the root path for FEATURE."
  (imp--alist-delete (imp-feature-first feature) imp-roots))
;; imp-roots
;; (imp-path-root-delete 'imp)


;;------------------------------------------------------------------------------
;; The Init.
;;------------------------------------------------------------------------------
;; Set `imp' root idempotently.
;;   - Might as well automatically fill ourself in.
;;     - dogfood, etc.
(let ((imp-path-error? nil))
  (unless (imp-path-root-get 'imp)
    (imp-path-root-set 'imp
                       (imp-path-current-dir))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
