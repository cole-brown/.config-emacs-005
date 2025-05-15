;;; core/modules/emacs/imp/path.el --- Path Functions -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-05-07
;; Timestamp:  2023-08-18
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;                                 ──────────
;; ╔════════════════════════════════════════════════════════════════════════╗
;; ║                                   Path                                 ║
;; ╚════════════════════════════════════════════════════════════════════════╝
;;                                   ──────
;;                       Filepaths and Directory Roots
;;                                 ──────────
;;
;;; Code:


(require 'seq)


;;------------------------------------------------------------------------------
;; Constants & Variables
;;------------------------------------------------------------------------------

(defconst int<imp>:path:replace:rx
  `(;;------------------------------
    ;; Default/Any/All
    ;;------------------------------
    (default
      ;;---
      ;; Valid, but...
      ;;---
      ;; We are going to disallow some valids just to make life easier.
      ;; E.g. regex "^:" is not allowed so that keywords can be used.
      ,(list (rx-to-string `(sequence string-start (or ":" "~"))
                           :no-group)
             "")
      ,(list (rx-to-string `(sequence string-start ":")
                           :no-group)
             "")
      ;;---
      ;; Disallowed by all:
      ;;---
      ("/"
       "-")
      ,(list (rx-to-string `control)
             "")
      ;; Unit test name -> filename that doesn't require quoting if in shell.
      ,(list (rx-to-string `(sequence (or "<" ">" ":")) ;; "/" already done.
                           :no-group)
             "-"))

    ;;------------------------------
    ;; Linux/Unix/Etc.
    ;;------------------------------
    ;; We'll just assume all the unixy systems are the same...
    ;;
    ;; Linux has these restrictions:
    ;;   1. Invalid/reserved file system characters:
    ;;      - / (forward slash)
    ;;      - Integer 0 (1-31: technically legal, but we will not allow).
    (gnu
     ;; Just the defaults, thanks.
     nil)
    (gnu/linux
     ;; Just the defaults, thanks.
     nil)
    (gnu/kfreebsd
     ;; Just the defaults, thanks.
     nil)
    (cygwin
     ;; Just the defaults, thanks.
     nil)

    ;;------------------------------
    ;; Windows
    ;;------------------------------
    ;; Windows has these restrictions:
    ;;   1. Invalid/reserved file system characters:
    ;;      - < (less than)
    ;;      - > (greater than)
    ;;      - : (colon)
    ;;      - " (double quote)
    ;;      - / (forward slash)
    ;;      - \ (backslash)
    ;;      - | (vertical bar or pipe)
    ;;      - ? (question mark)
    ;;      - * (asterisk)
    ;;      - Integers 0 through 31.
    ;;      - "Any other character that the target file system does not allow.
    ;;        - Very useful; thanks.
    ;;   2. Invalid as filenames (bare or with extensions):
    ;;      - CON, PRN, AUX, NUL COM1, COM2, COM3, COM4, COM5, COM6, COM7, COM8,
    ;;        COM9, LPT1, LPT2, LPT3, LPT4, LPT5, LPT6, LPT7, LPT8, LPT9
    ;;   3. Other rules:
    ;;      - Filenames cannot end in: . (period/dot/full-stop)
    (windows-nt
     ,(list (rx-to-string `(sequence (or "<"
                                         ">"
                                         ":"
                                         "\""
                                         "/" ; Also in defaults.
                                         "\\"
                                         "|"
                                         "?"
                                         "*")))
            "")
     ,(list (rx-to-string `(sequence string-start
                                     (or "CON"  "PRN"  "AUX"  "NUL"  "COM1"
                                         "COM2" "COM3" "COM4" "COM5" "COM6"
                                         "COM7" "COM8" "COM9" "LPT1" "LPT2"
                                         "LPT3" "LPT4" "LPT5" "LPT6" "LPT7"
                                         "LPT8" "LPT9")
                                     (or "." string-end)))
            "")
     ,(list (rx-to-string `(sequence "." string-end))
            ""))

    ;;------------------------------
    ;; Mac
    ;;------------------------------
    ;; Mac has these restrictions:
    ;;   1. Invalid/reserved file system characters:
    ;;      - / (forward slash)
    ;;      - : (colon)
    ;;      - Technically that's all for HFS+, but usually you can't get away with
    ;;        NUL (integer 0), et al.
    (darwin
     (":" ""))

    ;;------------------------------
    ;; Unsupported/Only Defaults
    ;;------------------------------
    (ms-dos
     nil))
  "Alist of regexs to replace and their replacement strings.

Used symbol-by-symbol in `imp:feature:normalize:imp->emacs' when
translating an imp symbol chain into one symbol for Emacs.

Alist format in `defcustom' language:
  :type '(alist :key-type (choice (string :tag \"Regex String\")
                                  (sexp :tag \"Expression that returns a string.\"))
                :value-type (choice (list string :tag \"Replacement Value\")
                                    (list symbol :tag \"Symbol whose value is the replacement value\")))")
;; (pp-display-expression int<imp>:path:replace:rx "*int<imp>:path:replace:rx*")
;; (makunbound 'int<imp>:path:replace:rx)


(defvar imp:path:roots nil
  "Alist of require/provide root keywords to a cons of: (root-dir . root-file).

Example:
  `:imp' entry is: '(:imp \"/path/to/imp/\" \"/path/to/imp/init.el\")")
;; imp:path:roots
;; (setq imp:path:roots nil)


(defconst imp:path:filename:init "imp-init.el"
  "Default filename for your imp init file.")


(defconst imp:path:filename:features "imp-features.el"
  "Default filename for your imp features file.")


;;------------------------------------------------------------------------------
;; Normalize Paths
;;------------------------------------------------------------------------------

(defun imp:path:canonical (path &optional root)
  "Expand PATH to a full/absolute/canonical path, based off of ROOT if relative.

If ROOT is nil, `default-directory' is used if needed."
  (expand-file-name path root))


(defun imp:path:join:canonical (&rest path)
  "Combines PATH elements together into an absolute/canonical path.

If PATH is relative, canonicalize to be under `default-directory'.

(imp:path:join \"/foo/bar/\" \"jeff\" \"jill.el\")
  ->\"/foo/bar/jeff/jill.el\"

(imp:path:join \"/foo/bar/\" \"jeff\" \"jill.el\" \"..\")
  ->\"/foo/bar/jeff\""
  (imp:path:canonical (apply #'imp:path:join path)))
;; (imp:path:join:canonical "foo" "bar")
;; (imp:path:join:canonical nil)


(defun imp:path:abbreviate (&rest path)
  "Join & canonicalize PATH, then shortened using `directory-abbrev-alist'.

Return an absolute path."
  (abbreviate-file-name (imp:path:canonical (apply #'imp:path:join path))))


(defun int<imp>:path:relative (feature-or-root path &optional error?)
  "Get PATH, relative to FEATURE-OR-ROOT.

FEATURE-OR-ROOT should be:
  - keyword - the `imp' feature's keyword
    - Returned path will be relative to the root directory of the keyword.
    - Will raise an error if the feature does not have a path root.
  - string  - an absolute path
    - Returned path will be relative to this absolute path.
  - nil
    - Returned path will be relative to `user-emacs-directory'.

PATH should be an absolute path string.

If PATH has no relation to the determined root path from FEATURE-OR-ROOT:
  - If ERROR? is nil, just return (canonicalized) PATH.
  - If ERROR? is non-nil, signal an error.

Example (given `:dot-emacs' has root path initialized as \"~/.config/emacs\"):
  ~/.config/emacs/foo/bar.el:
    (imp:path:relative :dot-emacs \"~/.config/emacs/foo/bar.el\")
      -> \"foo/bar.el\"
    (imp:path:relative :dot-emacs
                       \"/home/<username>\" \".config\" \"emacs/foo/bar.el\")
      -> \"foo/bar.el\""
  (let ((func/name "int<imp>:path:relative"))
    ;;------------------------------
    ;; Error Checking
    ;;------------------------------
    (cond ((null feature-or-root)
           nil)
          ((keywordp feature-or-root)
           nil)
          ((and (stringp feature-or-root)
                (not (file-name-absolute-p feature-or-root)))
           (int<imp>:error/if error?
                              func/name
                              "FEATURE-OR-ROOT must be an absolute path if a string! Got: '%s'"
                              feature-or-root)
           (setq feature-or-root 'error))
          (t
           (int<imp>:error/if error?
                              func/name
                              "Don't know how to handle FEATURE-OR-ROOT! Not a keyword, a string, or nil. Got a '%S': %S"
                              (type-of feature-or-root)
                              feature-or-root)
           (setq feature-or-root 'error)))

    (unless (stringp path)
      (int<imp>:error/if error?
                         func/name
                         "PATH must be a string! Got '%S': '%S'"
                         (type-of path)
                         path)
           (setq path 'error))

    ;;------------------------------
    ;; Get Relative Path
    ;;------------------------------
    (let* ((path/root (if (eq feature-or-root 'error)
                          ;; Don't have a valid FEATURE-OR-ROOT but don't want to error out.
                          ;; So do something that'll end up returning absolute PATH.
                          ""
                        ;; canonical directory path
                        (file-name-as-directory
                         (expand-file-name (cond ((keywordp feature-or-root)
                                                  (int<imp>:path:root/dir feature-or-root))
                                                 ((stringp feature-or-root)
                                                  feature-or-root)
                                                 (t
                                                  user-emacs-directory))))))
           ;; Don't like `file-relative-name' as it can return weird things when it
           ;; goes off looking for actual directories and files...
           (path/relative (replace-regexp-in-string
                           ;; Make sure root dir has ending slash.
                           path/root ;; Look for root directory path...
                           ""        ;; Replace with nothing to get a relative path.
                           path
                           :fixedcase
                           :literal)))

      ;; End up with the same thing? Not a relative path - signal error?
      (when (and error?
                 (string= path/relative path))
        (int<imp>:error func/name
                        '("Current directory is not relative to FEATURE-OR-ROOT!\n"
                          "  FEATURE-OR-ROOT: %S\n"
                          "  root path:    %s\n"
                          "  curr path:    %s\n"
                          "---> result:    %s")
                        feature-or-root
                        path/root
                        path/here
                        path/relative))

      ;; Return relative path.
      path/relative)))


(defun imp:path:relative (feature-or-root &rest path)
  "Join & canonicalize PATH, then make relative to FEATURE-OR-ROOT.

FEATURE-OR-ROOT should be:
  - keyword - the `imp' feature's keyword
    - Returned path will be relative to the root directory of the keyword.
    - Will raise an error if the feature does not have a path root.
  - string  - an absolute path
    - Returned path will be relative to this absolute path.
  - nil
    - Returned path will be relative to `user-emacs-directory'.

Will raise an error if `imp:path:current:file' (i.e. the absolute path)
has no relation to the determined root path.

Example (given `:dot-emacs' has root path initialized as \"~/.config/emacs\"):
  ~/.config/emacs/foo/bar.el:
    (imp:path:relative :dot-emacs \"~/.config/emacs/foo/bar.el\")
      -> \"foo/bar.el\"
    (imp:path:relative :dot-emacs
                       \"/home/<username>\" \".config\" \"emacs/foo/bar.el\")
      -> \"foo/bar.el\""
  (int<imp>:path:relative feature-or-root
                          (apply #'imp:path:join:canonical path)
                          :error))


;;------------------------------------------------------------------------------
;; `imp:path:roots' Getters
;;------------------------------------------------------------------------------

(defun int<imp>:path:root/dir (feature:base &optional no-error?)
  "Get the root directory from `imp:path:roots' for FEATURE:BASE.

If NO-ERROR? is nil and FEATURE:BASE is not in `imp:path:roots',
signals an error."
  (if-let ((dir (nth 0 (int<imp>:alist:get/value feature:base
                                                 imp:path:roots))))
      (imp:path:canonical "" dir)
    (if no-error?
        nil
      (int<imp>:error "int<imp>:path:root/dir"
                      "FEATURE:BASE '%S' unknown."
                      feature:base))))
;; (int<imp>:path:root/dir :imp)
;; (int<imp>:path:root/dir :dne)
;; (int<imp>:path:root/dir :dne t)


(defun int<imp>:path:file:exists? (root &rest paths)
  "Return canonical path to file if it exists, else nil.

Join ROOT and PATHS together into a filepath and canonicalize it.
Then checks that:
  1) It exists as a file.
  2) The file is readable."
  ;; Join PATHS, canonicalize at ROOT.
  (let ((filepath (imp:path:canonical (imp:path:join paths) root)))
    ;; Check that it exists.
    (if (and (file-regular-p  filepath)
             (file-readable-p filepath))
        filepath
      nil)))
;; (int<imp>:path:file:exists? default-directory "init.el")


(defun int<imp>:path:strings? (root &rest paths)
  "Return canonical path to file if ROOT and PATHS are strings, else nil."
  (if (and (stringp root)
           (seq-every-p #'stringp paths))
      (imp:path:canonical (imp:path:join paths) root)
    nil))


(defun int<imp>:path:root/file/init (feature:base &optional no-exist-check)
  "Get the init file from `imp:path:roots' for FEATURE:BASE.

Looks for the init file's name/path in `imp:path:roots' first, then if that is
nil looks for the default init filename in the root directory.

Raises an error signal if no init file exists.
Or if NO-EXIST-CHECK is non-nil, skips file existance check."
  (let ((func/name "int<imp>:path:root/file/init"))
    (int<imp>:debug func/name
                    '("inputs:\n"
                      "  - feature:base:   %S\n"
                      "  - no-exist-check: %S")
                    feature:base
                    no-exist-check)
    (if-let ((paths (int<imp>:alist:get/value feature:base imp:path:roots)))
        (let ((root    (nth 0 paths))
              (init    (or (nth 1 paths) ""))
              (verify-fn (if no-exist-check
                             #'int<imp>:path:strings?
                           #'int<imp>:path:file:exists?)))
          (int<imp>:debug func/name
                          '("paths: %S\n"
                            "  - root: %S\n"
                            "  - init: %S\n"
                            "verify-fn: %S")
                          paths
                          root
                          init
                          verify-fn)
          (cond
           ;; Do we have an entry?
           ((funcall verify-fn root init))
           ;; Not found; check for default.
           ((funcall verify-fn root imp:path:filename:init))
           ;; Still not found; error.
           (t
            (int<imp>:error "int<imp>:path:root/file/init"
                            "No imp init file found for `%S'!"
                            feature:base))))

      ;; Error when no entry in `imp:path:roots'.
      (int<imp>:error "int<imp>:path:root/file/init"
                      "FEATURE:BASE '%S' unknown."
                      feature:base))))
;; (int<imp>:path:root/file/init :imp)
;; (int<imp>:path:root/file/init :module)


(defun int<imp>:path:root/file/features (feature:base &optional no-exist-check)
  "Get the features file from `imp:path:roots' for FEATURE:BASE.

Looks for the features file's name/path in `imp:path:roots' first, then if that
is nil looks for the default features filename in the root directory.

Raises an error signal if no features file exists.
Or if NO-EXIST-CHECK is non-nil, skips file existance check."
  (if-let ((paths (int<imp>:alist:get/value feature:base imp:path:roots)))
      (let ((root     (nth 0 paths))
            (features (or (nth 2 paths) ""))
            (verify-fn (if no-exist-check
                           #'int<imp>:path:strings?
                         #'int<imp>:path:file:exists?)))

        (cond
         ;; Do we have an entry?
         ((funcall verify-fn root features))
         ;; Not found; check for default.
         ((funcall verify-fn root imp:path:filename:features))
         ;; Still not found; error.
         (t
          (int<imp>:error "int<imp>:path:root/file/features"
                          "No imp features file found for `%S'!"
                          feature:base))))

    ;; Error when no entry in `imp:path:roots'.
    (int<imp>:error "int<imp>:path:root/file/features"
                    "FEATURE:BASE '%S' unknown."
                    feature:base)))
;; (int<imp>:path:root/file/features :imp)
;; (int<imp>:path:root/file/features :module)


(defun int<imp>:path:root/contains? (feature:base)
  "Return bool based on if `imp:path:roots' contains FEATURE:BASE."
  (not (null (int<imp>:alist:get/value feature:base imp:path:roots))))


(defun int<imp>:path:root/valid? (caller path &rest kwargs)
  "Check that PATH is a vaild root path.

CALLER should be a string of calling function's name or file's path.

KWARGS should be a plist. All default to t:
  - :exists - Path must exist.
  - :dir    - Path must be a directory (implies :exists)."
  (let ((func/name "int<imp>:path:root/valid?")
        (exists (if (and kwargs
                         (plist-member kwargs :exists))
                    (plist-get kwargs :exists)
                  t))
        (dir    (if (and kwargs
                         (plist-member kwargs :dir))
                    (plist-get kwargs :dir)
                  t))
        (result t))

    (int<imp>:debug func/name "caller:   %s" caller)
    (int<imp>:debug func/name "path:     %s" path)
    (int<imp>:debug func/name "kwargs:   %S" kwargs)
    (int<imp>:debug func/name "  exists: - %S" exists)
    (int<imp>:debug func/name "  dir:    - %S" dir)

    ;;---
    ;; Validity Checks
    ;;---
    (if (not (or exists dir))  ; :dir implies :exists
        (int<imp>:debug func/name
                        "Existance not required. (or exists(%S) dir(%S)) -> %S"
                        exists
                        dir
                        (or exists dir))

      ;; Path is required to exist.
      (int<imp>:debug func/name
                      "Existance required! (or exists(%S) dir(%S)) -> %S"
                      exists
                      dir
                      (or exists dir))

      (cond ((null path)
             (int<imp>:error caller
                             "Null `path'?! path: %s"
                             path)
             (setq result nil))

            ((not (file-exists-p path))
             (int<imp>:error caller
                             "Path does not exist: %s"
                             path)
             (setq result nil))

            (t
             (int<imp>:debug func/name
                             "Path exists!"
                             path)
             nil)))

    (if (not dir)
        (int<imp>:debug func/name
                        "Path can be any type. dir(%S)"
                        dir)

      ;; Make sure path is a directory.
      (int<imp>:debug func/name
                      "Path must be directory. dir(%S)"
                      dir)

      (if (file-directory-p path)
          (int<imp>:debug func/name
                          "  -> Path is a directory!")

        (int<imp>:error caller
                        "Path is not a directory: %s"
                        path)
        (setq result nil)))

    ;;---
    ;; Return valid
    ;;---
    (int<imp>:debug func/name "->result: %S" result)
    result))


;;------------------------------------------------------------------------------
;; Safing Paths
;;------------------------------------------------------------------------------

(defun int<imp>:path:safe:string (symbol-or-string)
  "Translate SYMBOL-OR-STRING to a path string.

Use `int<imp>:path:replace:rx' translations."
  (let ((name (if (symbolp symbol-or-string)
                  (symbol-name symbol-or-string)
                symbol-or-string))
        regex
        replacement)
    ;; Defaults first.
    (int<imp>:debug "int<imp>:path:safe:string" "defaults:")
    (dolist (pair
             (int<imp>:alist:get/value 'default int<imp>:path:replace:rx)
             name)
      (setq regex (nth 0 pair)
            replacement (if (symbolp (nth 1 pair))
                            (symbol-value (nth 1 pair))
                          (nth 1 pair)))
      (int<imp>:debug "int<imp>:path:safe:string" "  rx: %S" regex)
      (int<imp>:debug "int<imp>:path:safe:string" "  ->: %S" replacement)
      (setq name (replace-regexp-in-string regex replacement name)))

    ;; Now the system-specifics, if any. Return `name' from `dolist' because
    ;; we're done.
    (int<imp>:debug "int<imp>:path:safe:string" "system(%S):" system-type)
    (dolist (pair
             (int<imp>:alist:get/value system-type int<imp>:path:replace:rx)
             name)
      (setq regex (nth 0 pair)
            replacement (if (symbolp (nth 1 pair))
                            (symbol-value (nth 1 pair))
                          (nth 1 pair)))
      (unless (null regex)
        (int<imp>:debug "int<imp>:path:safe:string" "  rx: %S" regex)
        (int<imp>:debug "int<imp>:path:safe:string" "  ->: %S" replacement)
        (setq name (replace-regexp-in-string regex replacement name))))))
;;   (int<imp>:path:safe:string :imp)
;; Should lose both slashes and ~:
;;   (int<imp>:path:safe:string "~/emacs.d/")
;; Should remain the same:
;;   (int<imp>:path:safe:string "config")
;; Test func name -> valid path:
;;   (int<imp>:path:safe:string "test<imp/feature/at>::int<imp>:feature:paths")


(defun int<imp>:path:safe:list (feature)
  "Normalize FEATURE (a list of symbols/keywords) to a list of strings.

Returns the list of normalized string."
  (mapcar #'int<imp>:path:safe:string feature))
;; (int<imp>:path:safe:list '(:root test feature))


;;------------------------------------------------------------------------------
;; Path Helpers
;;------------------------------------------------------------------------------

(defun int<imp>:path:dir? (path)
  "Return non-nil if PATH is a directory path."
  ;; Directory path if the PATH is equal to its path-as-a-directory.
  (string= (file-name-as-directory path)
           path))
;; (int<imp>:path:dir? "/foo")
;; (int<imp>:path:dir? "/foo/")
;; (int<imp>:path:dir? nil)


(defun imp:path:parent (path)
  "Return the parent directory component of PATH."
  (cond
   ;;------------------------------
   ;; Errors
   ;;------------------------------
   ((not (stringp path))
    (int<imp>:error "imp:path:parent"
                    "PATH is not a string! %S"
                    path))

   ;;------------------------------
   ;; Figure out path type so we can figure out parent.
   ;;------------------------------
   ;; Directory path?
   ((int<imp>:path:dir? path)
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
;; (imp:path:parent "/foo/bar/")
;; (imp:path:parent "/foo/bar.el")


(defun int<imp>:path:filename (path &optional dirname?)
  "Return the filename component of PATH.

If DIRNAME? is nil, returns \"\" for the filename of a directory path.
  example:
    (int<imp>:path:filename \"/foo/bar/\")
      -> \"\"

If DIRNAME? is non-nil, returns the directory's name for the filename of
a directory path.
  example:
    (int<imp>:path:filename \"/foo/bar/\" :dir)
      -> \"bar\""
  (let ((path:filename path))
    ;; Should we clean a directory path up to a directory-name path first?
    (when (and dirname?
               (int<imp>:path:dir? path:filename))
      (setq path:filename (directory-file-name path:filename)))

    ;; Return the filename segment of the path.
    (file-name-nondirectory path:filename)))
;; (int<imp>:path:filename "/foo/bar/" nil)
;; (int<imp>:path:filename "/foo/bar/" t)
;; (int<imp>:path:filename "/foo/bar.el" nil)
;; (int<imp>:path:filename "/foo/bar.el" t)


(defun imp:path:current:file ()
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
    (int<imp>:error "imp:path:current:file"
                    "Cannot get this file-path"))))
;; (imp:path:current:file)


(defun imp:path:current:file/relative (&optional feature-or-root)
  "Return the relative path of the file this function is called from.

FEATURE-OR-ROOT should be:
  - keyword - the `imp' feature's keyword
    - Returned path will be relative to the root directory of the keyword.
    - Will raise an error if the feature does not have a path root.
  - string  - an absolute path
    - Returned path will be relative to this absolute path.
  - nil
    - Returned path will be relative to `user-emacs-directory'.

Will raise an error if `imp:path:current:file' (i.e. the absolute path)
has no relation to the determined root path.

Example (assuming `:dot-emacs' has root path initialized as \"~/.config/emacs\"):
  ~/.config/emacs/foo/bar.el:
    (imp:path:current:file)
      -> \"/home/<username>/.config/emacs/foo/bar.el\"
    (imp:path:current:file/relative)
      -> \"foo/bar.el\"
    (imp:path:current:file/relative :dot-emacs)
      -> \"foo/bar.el\"
    (imp:path:current:file/relative \"/home/<username>/.config/emacs/foo\")
      -> \"bar.el\""
  (int<imp>:path:relative feature-or-root
                          (imp:path:current:file)
                          :error))
;; (imp:path:current:file/relative)
;; (imp:path:root/set :test (imp:path:current:dir))
;; (imp:path:current:file/relative :test)


(defun imp:file:current ()
  "Return the filename (no path, just filename) this is called from."
  (file-name-nondirectory (imp:path:current:file)))


(defun imp:path:current:dir ()
  "Return the directory path of the file this is called from."
  (when-let (path (imp:path:current:file))
    (directory-file-name (file-name-directory path))))
;; (imp:path:current:dir)


(defun imp:path:current:dir/relative (feature/base)
  "Return the relative path from feature's path root to the dir this is called.

Path will be relative to FEATURE/BASE. If FEATURE/BASE is nil, use
`user-emacs-directory' as the base path.

Will raise an error if non-nil FEATURE/BASE does not have a path root.

Will raise an error if `imp:path:current:dir' (i.e. the absolute path)
has no relation to FEATURE/BASE's root path.

Example (assuming `:dot-emacs' has root path initialized as \"~/.config/emacs\":
  ~/.config/emacs/foo/bar.el:
    (imp:path:current:dir)
      -> \"/home/<username>/.config/emacs/foo/\"
    (imp:path:current:dir/relative :dot-emacs)
      -> \"foo\"
    (imp:path:current:dir/relative)
      -> \"foo\""
  ;; Make sure both paths are equivalent (directory paths) for the regex replace.
  (let* ((path/root (file-name-as-directory
                     (expand-file-name (if feature/base
                                           (int<imp>:path:root/dir feature/base)
                                         user-emacs-directory))))
         (path/here (file-name-as-directory (imp:path:current:dir)))
         ;; Don't like `file-relative-name' as it can return weird things when it
         ;; goes off looking for actual directories and files...
         (path/relative (replace-regexp-in-string
                         ;; Make sure root dir has ending slash.
                         path/root ;; Look for root directory path...
                         ""        ;; Replace with nothing to get a relative path.
                         path/here
                         :fixedcase
                         :literal)))
    ;; End up with the same thing? Not a relative path - signal error.
    (when (string= path/relative path/here)
      ;; Error message gets truncated to oblivion, so... hello again:
      ;; (message (mapconcat #'identity
      ;;                     '("Current directory is not relative to FEATURE/BASE!"
      ;;                       "  FEATURE/BASE: %S"
      ;;                       "  root path:    %s"
      ;;                       "  curr path:    %s"
      ;;                       "---> result:    %s")
      ;;                     "\n")
      ;;          feature/base
      ;;          path/root
      ;;          path/here
      ;;          path/relative)
      (int<imp>:error "imp:path:current:dir/relative"
                      '("Current directory is not relative to FEATURE/BASE!\n"
                        "  FEATURE/BASE: %S\n"
                        "  root path:    %s\n"
                        "  curr path:    %s\n"
                        "---> result:    %s")
                      feature/base
                      path/root
                      path/here
                      path/relative))
    ;; Return relative path, sans final slash.
    (directory-file-name path/relative)))
;; Should be "" since we're at the root dir for imp:
;;   (imp:path:current:dir/relative :imp)


(defvar int<imp>:path:path:platform:case-insensitive
  '(;; Windows
    cygwin windows-nt ms-dos
    ;; MacOS
    darwin)
  "These operating systems have case-insensitive paths.")


(defun int<imp>:path:platform-agnostic (path)
  "Convert PATH string into a standardized path for the platform.

Replaces backslash with forward slash.
Downcases path on case-insensitive OSes."
  ;; Convert backslashes to forward slashes.
  (replace-regexp-in-string
   (rx "\\")
   "/"
   ;; Convert path to lowercase if on a case-insensitive OS.
   (funcall
    (if (memq system-type int<imp>:path:path:platform:case-insensitive)
        #'downcase
      #'identity)
    path)
   path))
;; (int<imp>:path:platform-agnostic "/foo/bar")
;; (int<imp>:path:platform-agnostic "/FOO/BAR")
;; (int<imp>:path:platform-agnostic "/Foo/Bar")
;; (int<imp>:path:platform-agnostic "C:/Foo/Bar")
;; (int<imp>:path:platform-agnostic "C:\\Foo\\Bar")


(defun int<imp>:path:input->str (input)
  "Ensure INPUT is a string.

INPUT should be a string, keyword, or symbol.
  - If it's a string, use as-is.
  - If it's a keyword/symbol, use the symbol's name sans \":\".
    - 'foo -> \"foo\"
    - :foo -> \"foo\"

Return a string."
  (cond ((null input) ;; Let nil through so `imp:path:join` functions correctly.
         nil)

        ((stringp input) ;; String good. Want string.
         input)

        ;; Symbol? Use its name.
        ((symbolp input)
         ;; But strip the keyword colon.
         (replace-regexp-in-string ":" "" (symbol-name input)))

        (t
         (int<imp>:error "int<imp>:path:input->str"
                         "INPUT must be string or keyword/symbol. Got %S: %S"
                         (type-of input)
                         input))))
;; (int<imp>:path:input->str nil)
;; (int<imp>:path:input->str :foo)
;; (int<imp>:path:input->str 'foo)
;; (int<imp>:path:input->str "foo")
;; (int<imp>:path:input->str :/bar)
;; (int<imp>:path:input->str '/bar)
;; (int<imp>:path:input->str "/bar")


(defun int<imp>:path:append (parent next)
  "Append NEXT element as-is to PARENT, adding dir separator if needed.

NEXT and PARENT are expected to be strings."
  (let ((parent (int<imp>:path:input->str parent))
        (next   (int<imp>:path:input->str next)))
  ;; Error checks first.
  (cond ((and parent
              (not (stringp parent)))
         (int<imp>:error "int<imp>:path:append"
                         "Paths to append must be strings. Parent is: %S"
                         parent))
        ((or (null next)
             (not (stringp next)))
         (int<imp>:error "int<imp>:path:append"
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
;; (int<imp>:path:append :/foo 'bar)


(defun imp:path:join (&rest path)
  "Combine PATH elements together into a path.

(imp:path:join \"jeff\" \"jill.el\")
  ->\"jeff/jill.el\""
   (seq-reduce #'int<imp>:path:append
               ;; flatten and convert to strings.
               (int<imp>:list:flatten path)
               nil))
;; (imp:path:join "/foo" "bar.el")
;; (imp:path:join '("/foo" ("bar.el")))
;; (imp:path:join "foo" "bar.el")
;; (imp:path:join "foo")


(defun int<imp>:path:sans-extension (&rest path)
  "Join PATH elements together and then remove any extension.

(int<imp>:path:sans-extension \"jeff\" \"jill.el\")
  ->\"jeff/jill\""
  (file-name-sans-extension (imp:path:join path)))
;; (int<imp>:path:sans-extension "foo" "bar/")
;; (int<imp>:path:sans-extension "foo" "bar/" "baz.el")


(defun int<imp>:path:canonical (root relative &optional assert-exists sans-extension)
  "Join ROOT and RELATIVE paths, normalize, and return the path string.

If ASSERT-EXISTS is `:file', raises an error if normalized path is not an
existing, readable file.

If ASSERT-EXISTS is `:file:load', raises an error if normalized path
plus an extension glob (\".*\") is not an existing, readable file.

If ASSERT-EXISTS is `:dir', raises an error if normalized path is not an
existing directory.

If SANS-EXTENSION is non-nil, returns a path without an extension
\(e.g. suitable for loading '.elc' or '.el' files.).

Returns normalized path."
  (let ((func/name "int<imp>:path:canonical")
        (valid:assert-exists '(nil :file :file:load :dir)))
    (int<imp>:debug func/name
                    '("inputs:\n"
                      "  - root: %s\n"
                      "  - relative: %s\n"
                      "  - assert-exists: %S\n"
                      "  - sans-extension: %S")
                    root
                    relative
                    assert-exists
                    sans-extension)

    ;;------------------------------
    ;; Error Check Inputs
    ;;------------------------------
    (unless (stringp root)
      (int<imp>:error func/name
                      "ROOT must be a string; got: %S"
                      root))
    (unless (stringp relative)
      (int<imp>:error func/name
                      "RELATIVE must be a string; got: %S"
                      relative))
    (unless (memq assert-exists valid:assert-exists)
      (int<imp>:error func/name
                      "ASSERT-EXISTS must be one of %S; got: %S"
                      valid:assert-exists
                      relative))

    ;;------------------------------
    ;; Normalize & check path.
    ;;------------------------------
    (let ((path (imp:path:join root relative))) ;; Assumes ROOT is already normalized.
      (int<imp>:debug func/name
                      "path: %s"
                      path)

      ;;------------------------------
      ;; Signal error?
      ;;------------------------------
      (cond
       ;;---
       ;; Sanity Check?
       ;;---
       ((not (stringp path))
        (int<imp>:error func/name
                        '("Error creating path from inputs (expected a string result)! "
                          "'%s' & '%s' -> %S")
                        root
                        relative
                        path))

       ;;---
       ;; ASSERT-EXISTS != nil
       ;;---
       ((eq assert-exists :file)
        ;; Signal an error if file doesn't exist or is not readable.
        (if (and (file-regular-p path)
                 (file-readable-p path))
            path
          (int<imp>:error func/name
                          "File does not exist: %s"
                          path)))

       ((eq assert-exists :file:load)
        (let ((file:valid? (lambda (ext)
                             "Is FILEPATH a file and readable?"
                             (let ((check (concat path ext)))
                               (and (file-regular-p  check)
                                    (file-readable-p check))))))
          ;; Check in the ordering that `load' checks: .elc, .el, <order of `load-suffixes'>, no-suffix
          (cond ((funcall file:valid? ".elc")
                 path)
                ((funcall file:valid? ".el")
                 path)
                ((and load-suffixes
                      (seq-some file:valid? load-suffixes))
                 path)
                ((funcall file:valid? "")
                 path)
                ;; Assert failed; signal error.
                (t
                 (int<imp>:error func/name
                                 "No (readable) files exist to load: %s"
                                 path)))))

       ((eq assert-exists :dir)
        ;; Signal an error if directory doesn't exist.
        (if (file-directory-p path)
            path
          (int<imp>:error func/name
                          "Directory does not exist: %s"
                          path)))

       ;;---
       ;; ASSERT-EXISTS == `nil'
       ;;---
       (t
        ;; Do not signal an error; just return path.
        path))

      ;;------------------------------
      ;; Normalize/Canonicalize path.
      ;;------------------------------
      (if (not (file-name-absolute-p path))
          (int<imp>:error func/name
                          "Path is not absolute: %s"
                          path)
        ;; Actually normalize it before returning.
        (imp:path:canonical (if sans-extension
                                     ;; Take out the extension if requested.
                                     (file-name-sans-extension path)
                                   path))))))
;; Just Normalize:
;;   (int<imp>:path:canonical "/path/to/imp/test/loading" "dont-load")
;;   (int<imp>:path:canonical "/path/to/imp/test/loading" "dont-load.el")
;;   (int<imp>:path:canonical "loading" "dont-load.el")
;; Normalize w/o extension:
;;   (int<imp>:path:canonical "/path/to/imp/test/loading" "dont-load.el" nil t)
;;   (int<imp>:path:canonical "/path/to/imp/test/loading" "dont-load" nil t)
;; Error:
;;   (int<imp>:path:canonical "/path/to/imp/test/loading" "dont-load" :file)
;; Ok:
;;   (int<imp>:path:canonical "/path/to/imp/test/loading" "dont-load" :file:load)
;;   (int<imp>:path:canonical "/path/to/imp/test/loading" "dont-load.el" :file:load)


;;------------------------------------------------------------------------------
;; Public API: Feature Root Directories
;;------------------------------------------------------------------------------

(defun imp:path:root/set (feature:base path:dir:root &optional path:file:init path:file:features)
  "Set the root path(s) of FEATURE:BASE for future `imp:require' calls.

PATH:DIR:ROOT is the directory under which all of FEATURE:BASE's features exist.

PATH:FILE:INIT is nil or the file to load if only FEATURE:BASE is used in an
`imp:require', and the feature isn't loaded, AND we have the entry... somehow...
in `imp:path:roots'.
  - This can be either an absolute or relative path. If relative, it will be
    relative to PATH:DIR:ROOT.

PATH:FILE:FEATURES is nil or a minimal file with a call to `imp:feature:at'.
  - If a sub-feature (that isn't provided) is requested for your feature and there
    is no entry in `imp:features', this file will be loaded in order to populate
    `imp:features' so the feature can be looked for."
  (cond ((int<imp>:path:root/contains? feature:base)
         (int<imp>:error "imp:root"
                         '("FEATURE:BASE '%S' is already an imp root.\n"
                           "path: %s\n"
                           "file: %s")
                         feature:base
                         (int<imp>:path:root/dir feature:base)
                         (int<imp>:path:root/file/init feature:base)))

        ((not (keywordp feature:base))
         (int<imp>:error "imp:root"
                         "FEATURE:BASE must be a keyword (e.g. `:foo' `:bar' etc)"))

        ;; int<imp>:path:root/valid? will error with better reason, so the error here
        ;; isn't actually triggered... I think?
        ((not (int<imp>:path:root/valid? "imp:root" path:dir:root))
         (int<imp>:error "imp:root"
                         "Path must be a valid directory: %s" path:dir:root))

        ;; Ok; set keyword to path.
        (t
         (push (list feature:base
                     path:dir:root
                     path:file:init
                     path:file:features)
               imp:path:roots))))


(defun imp:path:root/get (feature:base &optional no-error?)
  "Get the root directory from `imp:path:roots' for FEATURE:BASE.

If NO-ERROR? is nil and FEATURE:BASE is not in `imp:path:roots',
signals an error.

Return path string from `imp:path:roots' or nil."
  (int<imp>:path:root/dir feature:base no-error?))


;;------------------------------------------------------------------------------
;; Internal API: Initialization
;;------------------------------------------------------------------------------
;; We are loaded before 'provide.el', but we have public functions that people
;; may want, so we want to call:
;;   (imp:provide:with-emacs :imp 'path)
;;
;; Instead of calling directly when this file is loaded/eval'd, we'll depend on
;; 'init.el' to call this function.

(defun int<imp>:path:init ()
  "Initialize imp's path functions/variables.

This will:
  - Call `imp:path:root' for setting imp's root dir & file.
  - Provide 'path.el' feature to imp & emacs.

Must be called after 'provide.el' is loaded."
  ;;---
  ;; Set `imp' root.
  ;;   - Might as well automatically fill ourself in.
  ;;---
  (imp:path:root/set :imp
                     ;; root dir
                     (file-name-directory (if load-in-progress
                                              load-file-name
                                            (buffer-file-name)))
                     ;; root file - just provide relative to dir/imp
                     "init.el")

  ;;---
  ;; Provide feature symbol for 'path.el'.
  ;;---
  (imp:provide:with-emacs :imp 'path))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
