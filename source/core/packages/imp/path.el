;;; imp/path.el --- Path Functions -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-05-07
;; Timestamp:  2025-11-17
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
;;; Code:


(require 'seq)


;;------------------------------------------------------------------------------
;; Normalize Paths
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


(defun imp-path-canonical (path &optional root)
  "Expand PATH to a full/absolute/canonical path, based off of ROOT if relative.

Follow symlinks to find true path.

If ROOT is nil, `default-directory' is used if needed."
  (declare (pure t) (side-effect-free t))
  ;; remove trailing slash
  (directory-file-name
   ;; follow symlinks, remove ".."
   (file-truename
    ;; get absolute path
    (expand-file-name path root))))


(defun imp-path-abbreviate (&rest path)
  "Join & canonicalize PATH, then shortened using `abbreviate-file-name'.

Return an absolute path."
  (declare (pure t) (side-effect-free t))
  (abbreviate-file-name (imp-path-canonical (apply #'imp-path-join path))))


(defun imp--path-relative (root path &optional error?)
  "Get PATH, relative to ROOT.

ROOT should be:
  - feature - Something that `imp-feature-normalize' can handle.
    - Returned path will be relative to the entry in `imp-roots'.
    - Will raise an error if the feature does not have a path root.
  - string - an absolute path
    - Returned path will be relative to this absolute path.
  - nil
    - Returned path will be relative to `user-emacs-directory'.

PATH should be an absolute path string.

If PATH has no relation to the determined root path from ROOT:
  - If ERROR? is nil, just return (canonicalized) PATH.
  - If ERROR? is non-nil, signal an error."
  (let ((func-name "imp--path-relative")
        path-root)

    ;;------------------------------
    ;; Input Validation
    ;;------------------------------
    ;; Does ROOT make sense?
    (cond ((null root) nil) ; nil is valid

          ((and (stringp root) ; absolute path string is valid
                (file-name-absolute-p root))
           (setq path-root root))

          ((and (stringp root) ; other strings are not valid
                (not (file-name-absolute-p root)))
           (imp--error-if error?
                          func-name
                          "ROOT must be an absolute path if a string. Got: '%s'"
                          root)
           (setq root 'error))

          ((condition-case _ ;; can this be understood as a feature name?
               (imp-feature-normalize root)
             (error nil))
           ;; It's either:
           ;;   - symbol or something that `imp-feature-first' can resolve
           ;;   - an error
           ;; TODO(path): Relative to whole of feature, not just the root bit?
           (setq path-root (imp-path-root-get (imp-feature-first root)))
           (unless (and path-root
                        (stringp path-root)
                        (file-name-absolute-p path-root))
             (imp--error-if error?
                            func-name
                            '("Cannot get a path from ROOT. "
                              "ROOT is not a feature in `imp-roots'. "
                              "ROOT: %S")
                            root)
             (setq root 'error)))

          (t
           (imp--error-if error?
                          func-name
                          '("Don't know how to handle ROOT. "
                            "Not a feature, a string, or nil. "
                            "Got a '%S': %S")
                          (type-of root)
                          root)
           (setq root 'error)))

    ;; Does PATH make sense?
    (if (stringp path)
        (setq path (imp-path path)) ; normalize path
      (imp--error-if error?
                     func-name
                     "PATH must be a string! Got '%S': '%S'"
                     (type-of path)
                     path)
      (setq path 'error))

    (setq path-root
          (if (eq root 'error)
              ;; Don't have a valid ROOT but don't want to error out.
              ;; So do something that'll end up returning absolute PATH.
              ""
            ;; Add trailing slash so we don't have to deal with it
            ;; in `replace-regexp-in-string'.
            (file-name-as-directory
             ;; normalize path
             (imp-path (if (and path-root
                               (stringp path-root))
                          path-root
                        user-emacs-directory))))) ; default/fallback

    ;;------------------------------
    ;; Get Relative Path
    ;;------------------------------
    (let (;; Don't like `file-relative-name' as it can return weird things
          ;; when it goes off looking for actual directories and files...
          (path-relative (replace-regexp-in-string
                          ;; Make sure root dir has ending slash.
                          path-root ;; Look for root directory path...
                          ""        ;; Replace with nothing to get a relative path.
                          path
                          :fixedcase
                          :literal)))

      ;; End up with the same thing? Not a relative path - signal error?
      (when (and error?
                 (string= path-relative path))
        (imp--error func-name
                    '("PATH is not relative to ROOT!\n"
                      "  PATH: %S\n"
                      "  ROOT: %S\n"
                      "  root path:    %s\n"
                      "---> result:    %s")
                    path
                    root
                    path-root
                    path-relative))

      ;; Return relative path.
      path-relative)))
;; (imp--path-relative 'imp:/path (imp-path-join (imp-path-current-dir) "path/to/thing") t)


(defun imp-path-relative (root &rest path)
  "Join & canonicalize PATH, then make relative to ROOT.

ROOT should be:
  - feature - Something that `imp-feature-normalize' can handle.
    - Returned path will be relative to the entry in `imp-roots'.
    - Will raise an error if the feature does not have a path root.
  - string - an absolute path
    - Returned path will be relative to this absolute path.
  - nil
    - Returned path will be relative to `user-emacs-directory'.

PATH should be an absolute path string.

Error ff PATH has no relation to the determined root path from ROOT."
  (imp--path-relative root
                      (imp-path-canonical (apply #'imp-path-join path))
                      :error))


;;------------------------------------------------------------------------------
;; `imp-roots' Getters
;;------------------------------------------------------------------------------

;; TODO(path): Unused. Delete? Make it a public func? Needs better params.
;; (defun imp--path-file-exists? (root &rest paths)
;;   "Return canonical path to file if it exists, else nil.
;;
;; Join ROOT and PATHS together into a filepath and canonicalize it.
;; Then checks that:
;;   1) It exists as a file.
;;   2) The file is readable."
;;   ;; Join PATHS, canonicalize at ROOT.
;;   (let ((filepath (imp-path-canonical (imp-path-join paths) root)))
;;     ;; Check that it exists.
;;     (if (and (file-regular-p  filepath)
;;              (file-readable-p filepath))
;;         filepath
;;       nil)))
;; ;; (imp--path-file-exists? default-directory "init.el")


(defun imp--path-root-valid? (caller path &rest kwargs)
  "Check that PATH is a vaild root path.

CALLER should be a string of calling function's name or file's path.

KWARGS should be a plist. All default to t:
  - :exists - Path must exist.
  - :dir    - Path must be a directory (implies :exists)."
  (let ((func-name "imp--path-root-valid?")
        (exists (if (and kwargs
                         (plist-member kwargs :exists))
                    (plist-get kwargs :exists)
                  t))
        (dir    (if (and kwargs
                         (plist-member kwargs :dir))
                    (plist-get kwargs :dir)
                  t))
        (result t))

    (imp--debug func-name "caller:   %s" caller)
    (imp--debug func-name "path:     %s" path)
    (imp--debug func-name "kwargs:   %S" kwargs)
    (imp--debug func-name "  exists: - %S" exists)
    (imp--debug func-name "  dir:    - %S" dir)

    ;;---
    ;; Validity Checks
    ;;---
    (if (not (or exists dir))  ; :dir implies :exists
        (imp--debug func-name
                    "Existance not required. (or exists(%S) dir(%S)) -> %S"
                    exists
                    dir
                    (or exists dir))

      ;; Path is required to exist.
      (imp--debug func-name
                  "Existance required! (or exists(%S) dir(%S)) -> %S"
                  exists
                  dir
                  (or exists dir))

      (cond ((null path)
             (imp--error caller
                         "Null `path'?! path: %s"
                         path)
             (setq result nil))

            ((not (file-exists-p path))
             (imp--error caller
                         "Path does not exist: %s"
                         path)
             (setq result nil))

            (t
             (imp--debug func-name
                         "Path exists!"
                         path)
             nil)))

    (if (not dir)
        (imp--debug func-name
                    "Path can be any type. dir(%S)"
                    dir)

      ;; Make sure path is a directory.
      (imp--debug func-name
                  "Path must be directory. dir(%S)"
                  dir)

      (if (file-directory-p path)
          (imp--debug func-name
                      "  -> Path is a directory!")

        (imp--error caller
                    "Path is not a directory: %s"
                    path)
        (setq result nil)))

    ;;---
    ;; Return valid
    ;;---
    (imp--debug func-name "->result: %S" result)
    result))


;;------------------------------------------------------------------------------
;; Path Helpers
;;------------------------------------------------------------------------------

;; TODO(path): Used once. Delete? The name is terrible.
(defun imp--path-dir? (path)
  "Return non-nil if PATH is a directory path."
  ;; Directory path if the PATH is equal to its path-as-a-directory.
  (declare (pure t) (side-effect-free t))
  (string= (file-name-as-directory path)
           path))
;; (imp--path-dir? "/foo")
;; (imp--path-dir? "/foo/")
;; (imp--path-dir? nil)


(defun imp-path-parent (path)
  "Return the parent directory component of PATH."
  (cond
   ;;------------------------------
   ;; Errors
   ;;------------------------------
   ((not (stringp path))
    (imp--error "imp-path-parent"
                "PATH is not a string! %S"
                path))

   ;;------------------------------
   ;; Figure out path type so we can figure out parent.
   ;;------------------------------
   ;; Directory path?
   ((imp--path-dir? path)
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


(defun imp--path-filename (path)
  "Return the filename component of PATH."
  (declare (pure t) (side-effect-free t))
  (file-name-nondirectory path))
;; (imp--path-filename "/foo/bar/")
;; (imp--path-filename "/foo/bar.el")


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
    (imp--error "imp-path-current-file"
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


(defun imp-file-current (&optional no-ext)
  "Return the filename (no path, just filename) this is called from."
  (funcall (if no-ext #'imp-path-sans-extension #'identity)
           (file-name-nondirectory (imp-path-current-file))))
;; (imp-file-current)
;; (imp-file-current t)


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
;;       (imp--error "imp-path-current-dir-relative"
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


;; TODO(path):
;; TODO(path): Used in one func. Delete? Use imp-parser func or imp-feature func?
(defun imp--path-to-str (input)
  "Ensure INPUT is a string.

INPUT should be a string, keyword, or symbol.
  - If it's a string, use as-is.
  - If it's a keyword/symbol, use the symbol's name sans \":\".
    - 'foo -> \"foo\"
    - :foo -> \"foo\"

Return a string."
  (cond ((null input) ;; Let nil through so `imp-path-join` functions correctly.
         nil)

        ((stringp input) ;; String good. Want string.
         input)

        ;; Symbol? Use its name.
        ((symbolp input)
         ;; But strip the keyword colon.
         (replace-regexp-in-string ":" "" (symbol-name input)))

        (t
         (imp--error "imp--path-to-str"
                     "INPUT must be string or keyword/symbol. Got %S: %S"
                     (type-of input)
                     input))))
;; (imp--path-to-str nil)
;; (imp--path-to-str :foo)
;; (imp--path-to-str 'foo)
;; (imp--path-to-str "foo")
;; (imp--path-to-str :/bar)
;; (imp--path-to-str '/bar)
;; (imp--path-to-str "/bar")


;; TODO(path): Only used in `imp-path-join`. Roll this into that func then delete.
(defun imp--path-append (parent next)
  "Append NEXT element to PARENT, adding dir separator if needed."
  (let ((parent (imp--path-to-str parent))
        (next   (imp--path-to-str next)))
    ;; Error checks first.
    (cond ((and parent
                (not (stringp parent)))
           (imp--error "imp--path-append"
                       "Paths to append must be strings. Parent is: %S"
                       parent))
          ((or (null next)
               (not (stringp next)))
           (imp--error "imp--path-append"
                       "Paths to append must be strings. Next is: %S"
                       next))

          ;;---
          ;; Append or not?
          ;;---
          ;; Expected initial case for appending: nil parent, non-nil next.
          ((null parent)
           next)

          (t
           (concat (file-name-as-directory parent) next)))))
;; (imp--path-append :/foo 'bar)


(defun imp-path-join (&rest path)
  "Combine PATH elements together into a path.

(imp-path-join \"jeff\" \"jill.el\")
  ->\"jeff/jill.el\""
  (seq-reduce #'imp--path-append
              ;; flatten and convert to strings.
              (imp--list-flatten path)
              nil))
;; (imp-path-join "/foo" "bar.el")
;; (imp-path-join '("/foo" ("bar.el")))
;; (imp-path-join "foo" "bar.el")
;; (imp-path-join "foo")


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
             (imp--error funcname
                         '("Feature is already an imp root. "
                           "FEATURE: %S "
                           "feature base: %S "
                           "existing path: %s "
                           "requested path: %s")
                         feature
                         feature-base
                         (imp-path-root-get feature-base :no-error)
                         path-root)))

          ;; `imp--path-root-valid?' will error with better reason, so the
          ;; error here isn't actually triggered... I think?
          ((not (imp--path-root-valid? "imp-root" path-root))
           (imp--error funcname
                       "Path must be a valid directory: %s" path-root))

          ;; Ok; set the feature's root to the path.
          (t
           (push (list feature-base path-root) imp-roots)))))
;; (imp-path-root-set 'imp (imp-path-current-dir))


(defun imp-path-root-get (feature &optional no-error?)
  "Get the root directory from `imp-roots' for FEATURE.

If NO-ERROR? is nil and FEATURE is not in `imp-roots',
signals an error.

Return path string from `imp-roots' or nil."
  ;; TODO(path): need to be able to tell `imp-feature-normalize' to error or not
  (if-let* ((feature-norm (imp-feature-normalize feature))
            (dir (nth 0 (imp--alist-get-value feature
                                              imp-roots))))
      (imp-path dir)
    ;; this returns nil if we're not erroring.
    (imp--error-if (not no-error?)
                   'imp-path-root-get
                   "FEATURE is unknown: %S -> %S"
                   feature
                   feature-norm)))
;; (imp-path-root-get 'imp)
;; (imp-path-root-get 'imp 'no-error)
;; (imp-path-root-get 'dne)
;; (imp-path-root-get 'dne t)


(defun imp-path-root-delete (feature &optional no-error?)
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
(unless (imp-path-root-get 'imp 'no-error)
  (imp-path-root-set 'imp
                     (imp-path-current-dir)))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
