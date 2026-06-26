;;; imp/path.el --- Path Functions -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-05-07
;; Timestamp:  2026-06-26
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
;; Path Canons.
;;
;; This path library is for theoretical paths. It should never ask the actual
;; filesystem for anything.
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
  (when-let* ((root (file-name-directory ; add trailing slash so regex replace is cleaner
                     (imp-path-normalize
                      (imp--path-validate (if (and (not (null root))
                                                   (symbolp root))
                                              (imp-path-of-feature root))))))
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
;; (imp-path-relative nil (imp-path-join (imp-path-current-dir) "path/to/thing"))

;; TODO U R HERE

;;------------------------------------------------------------------------------
;; File Helpers
;;------------------------------------------------------------------------------

;; TODO: rename `imp-file-name'
(defun imp--path-filename (path)
  "Return the filename component of PATH."
  (declare (pure t) (side-effect-free t))
  (file-name-nondirectory path))
;; (imp--path-filename "/foo/bar/")
;; (imp--path-filename "/foo/bar.el")


(defun imp-file-current (&optional no-ext)
  "Return the filename (no path, just filename) this is called from."
  (funcall (if no-ext #'imp-path-sans-extension #'identity)
           (file-name-nondirectory (imp-path-current-file))))
;; (imp-file-current)
;; (imp-file-current t)


;;------------------------------------------------------------------------------
;; Path Helpers
;;------------------------------------------------------------------------------

(defun imp-path-parent (path)
  "Return the parent directory component of PATH."
  (cond
   ;;------------------------------
   ;; Errors
   ;;------------------------------
   ((not (stringp path))
    (imp--path-error 'imp-path-parent
                     "PATH is not a string! %S"
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
    ;; Then get its parent dir:    "/foo/bar.el" -> "/foo/"
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


;; TODO(path): Unused. Delete?
;; (defun imp-path-current-file-relative (&optional root)
;;   "Return the relative path of the file this function is called from.

;; ROOT should be:
;;   - keyword - the `imp' feature's keyword
;;     - Returned path will be relative to the root directory of the keyword.
;;     - Will raise an error if the feature does not have a path root.
;;   - string  - an absolute path
;;     - Returned path will be relative to this absolute path.
;;   - nil
;;     - Returned path will be relative to `user-emacs-directory'.

;; Will raise an error if `imp-path-current-file' (i.e. the absolute path)
;; has no relation to the determined root path.

;; Example (assuming `:dot-emacs' has root path initialized as \"~/.config/emacs\"):
;;   ~/.config/emacs/foo/bar.el:
;;     (imp-path-current-file)
;;       -> \"/home/<username>/.config/emacs/foo/bar.el\"
;;     (imp-path-current-file-relative)
;;       -> \"foo/bar.el\"
;;     (imp-path-current-file-relative :dot-emacs)
;;       -> \"foo/bar.el\"
;;     (imp-path-current-file-relative \"/home/<username>/.config/emacs/foo\")
;;       -> \"bar.el\""
;;   (imp--path-relative root
;;                       (imp-path-current-file)
;;                       :error))
;; ;; (imp-path-current-file)
;; ;; (imp-path-current-file-relative)
;; ;; (imp-path-root-set :test (imp-path-current-dir))
;; ;; (imp-path-current-file-relative :test)


(defun imp-path-current-dir ()
  "Return the directory path of the file this is called from."
  (when-let (path (imp-path-current-file))
    (directory-file-name (file-name-directory path))))
;; (imp-path-current-dir)


;; TODO(path): Unused. Delete?
;; (defun imp-path-current-dir-relative (feature/base)
;;   "Return the relative path from feature's path root to the dir this is called.

;; Path will be relative to FEATURE/BASE. If FEATURE/BASE is nil, use
;; `user-emacs-directory' as the base path.

;; Will raise an error if non-nil FEATURE/BASE does not have a path root.

;; Will raise an error if `imp-path-current-dir' (i.e. the absolute path)
;; has no relation to FEATURE/BASE's root path.

;; Example (assuming `:dot-emacs' has root path initialized as \"~/.config/emacs\":
;;   ~/.config/emacs/foo/bar.el:
;;     (imp-path-current-dir)
;;       -> \"/home/<username>/.config/emacs/foo/\"
;;     (imp-path-current-dir-relative :dot-emacs)
;;       -> \"foo\"
;;     (imp-path-current-dir-relative)
;;       -> \"foo\""
;;   ;; Make sure both paths are equivalent (directory paths) for the regex replace.
;;   (let* ((path-root (file-name-as-directory
;;                      (expand-file-name (if feature/base
;;                                            (imp-path-root-get feature/base)
;;                                          user-emacs-directory))))
;;          (path/here (file-name-as-directory (imp-path-current-dir)))
;;          ;; Don't like `file-relative-name' as it can return weird things when it
;;          ;; goes off looking for actual directories and files...
;;          (path/relative (replace-regexp-in-string
;;                          ;; Make sure root dir has ending slash.
;;                          path-root ;; Look for root directory path...
;;                          ""        ;; Replace with nothing to get a relative path.
;;                          path/here
;;                          :fixedcase
;;                          :literal)))
;;     ;; End up with the same thing? Not a relative path - signal error.
;;     (when (string= path/relative path/here)
;;       ;; Error message gets truncated to oblivion, so... hello again:
;;       ;; (message (mapconcat #'identity
;;       ;;                     '("Current directory is not relative to FEATURE/BASE!"
;;       ;;                       "  FEATURE/BASE: %S"
;;       ;;                       "  root path:    %s"
;;       ;;                       "  curr path:    %s"
;;       ;;                       "---> result:    %s")
;;       ;;                     "\n")
;;       ;;          feature/base
;;       ;;          path-root
;;       ;;          path/here
;;       ;;          path/relative)
;;       (imp--path-error 'imp-path-current-dir-relative
;;                   '("Current directory is not relative to FEATURE/BASE!\n"
;;                     "  FEATURE/BASE: %S\n"
;;                     "  root path:    %s\n"
;;                     "  curr path:    %s\n"
;;                     "---> result:    %s")
;;                   feature/base
;;                   path-root
;;                   path/here
;;                   path/relative))
;;     ;; Return relative path, sans final slash.
;;     (directory-file-name path/relative)))
;; ;; Should be "" since we're at the root dir for imp:
;; ;;   (imp-path-current-dir-relative :imp)


;; TODO(path): Unused. Delete? Wait til `imp` hits the Win10VM first.
(defvar imp--path-path-platform-case-insensitive
  '(;; Windows
    cygwin windows-nt ms-dos
    ;; MacOS
    darwin)
  "These operating systems have case-insensitive paths.")


;; TODO(path): Unused. Delete? Wait til `imp` hits the Win10VM first.
(defun imp--path-platform-agnostic (path)
  "Convert PATH string into a standardized path for the platform.

Replaces backslash with forward slash.
Downcases path on case-insensitive OSes."
  ;; Convert backslashes to forward slashes.
  (replace-regexp-in-string
   (rx "\\")
   "/"
   ;; Convert path to lowercase if on a case-insensitive OS.
   (funcall
    (if (memq system-type imp--path-path-platform-case-insensitive)
        #'downcase
      #'identity)
    path)
   path))
;; (imp--path-platform-agnostic "/foo/bar")
;; (imp--path-platform-agnostic "/FOO/BAR")
;; (imp--path-platform-agnostic "/Foo/Bar")
;; (imp--path-platform-agnostic "C:/Foo/Bar")
;; (imp--path-platform-agnostic "C:\\Foo\\Bar")


;; TODO(path): change to pass in EXT, check for EXT, remove if matching.
(defun imp-path-sans-extension (&rest path)
  "Join PATH elements together and then remove any extension.

(imp-path-sans-extension \"jeff\" \"jill.el\")
  ->\"jeff/jill\""
  (file-name-sans-extension (imp-path-join path)))
;; (imp-path-sans-extension "foo" "bar/")
;; (imp-path-sans-extension "foo" "bar/" "baz.el")


(defun imp-path-with-extension (path ext)
  "Ensure PATH has an extension of EXT.

(imp-path-with-extension \"jeff/jill.el\" \".el\")
  ->\"jeff/jill.el\"

(imp-path-with-extension \"jeff/jill\" \"el\")
  ->\"jeff/jill.el\"

(imp-path-with-extension \"jeff/jill..\" \"...el\")
  ->\"jeff/jill.el\""
  (if (string-suffix-p ext path 'ignore-case)
      ;; PATH has suffix already. Do nothing.
      path
    ;; Clear all periods so we can just glue 'em together with one regardless.
    (file-name-with-extension
     (replace-regexp-in-string (rx (one-or-more ".") string-end)
                               ""
                               path)
     (replace-regexp-in-string (rx (one-or-more ".") string-end)
                               ""
                               ext))))
;; (imp-path-with-extension "foo/bar/baz" ".el")
;; (imp-path-with-extension "foo/bar/baz.el" "el")
;; (imp-path-with-extension nil "el")


;;------------------------------------------------------------------------------
;; Load Paths
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
;; Public API: Feature Root Directories
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
;; /The/ Path Function
;;------------------------------------------------------------------------------

;; TODO(path): make this use all the shenanigans that `imp-parser-normalize/:path' uses?
(defun imp-path (&rest path)
  "Get an imp standard path.

1. Join PATH into a path string.
2. Get the absolute path.
   - If path is relative, root with func `imp-path-current-dir'.
3. Follow symlinks to 'true' file path.
4. Remove trailing slashes.
5. Abbreviate path (for '~/' paths instead of '/home/user/')."
  (declare (pure t) (side-effect-free t))
  (let ((default-directory (imp-path-current-dir)))
    (convert-standard-filename
     (apply #'imp-path-abbreviate path))))


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
