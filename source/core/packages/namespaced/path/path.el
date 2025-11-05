;;; namespaced/path/path.el --- Path Functions -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2020-10-28
;; Timestamp:  2025-11-03
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Path Functions
;;
;;; Code:


(imp-require str)
(imp-require elisp:/functions)

;; TODO: defaliases for emacs's path functions?
;;   - file-name-as-directory == path->dir ?
;;     - and some inverse: path->file ?

;; TODO: truenames?
;;   - E.g. Funcs to follow symlinks.
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Truenames.html
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/File-Name-Expansion.html
;; And I guess TODO symlinks in general?

;; TODO: `ignore-case' non-nil on windows et al?


;;--------------------------------------------------------------------------------
;; Settings
;;--------------------------------------------------------------------------------

(defcustom path:name:truncate "…"
  "String to use as a replacement in paths that need truncation."
  :group 'path
  :type '(string))


;;------------------------------------------------------------------------------
;; Constants
;;------------------------------------------------------------------------------

(defconst path:types
  '(;;---
    ;; Real Actual Types
    ;;---

    ;; Directory
    :dir

    ;; File
    :file

    ;; Symbolic Link
    :symlink

    ;;---
    ;; Error / Failure Types?
    ;;---
    ;; Used when we don't have enough path types yet to figure out what this is...
    :unknown)
  "Types of files that Emacs can reason about.

https://www.gnu.org/software/emacs/manual/html_node/elisp/Kinds-of-Files.html")


(defconst _:path:system:types
  '((:windows . windows-nt)
    (:linux   . gnu/linux)
    (:wsl     . nil)

    ;; TODO: This should be like `:wsl' except "/cygdrive/<drive>/" instead of "/mnt/<drive>/"
    ;; (:cygwin  . cygwin)

    ;; NOTE: Not an actual system type; should be figured out.
    (:auto    . nil))
  "Alist of supported system types to `system-type' value.

NOTE: `:wsl' doesn't know it's `:wsl'; so it can't have a `system-type' value.")


(defconst path:rx:names:ignore
  (list
   ;; Ignore "." and ".." entries.
   (rx-to-string '(sequence
                   string-start
                   (repeat 1 2 ".")
                   string-end)
                 :no-group))
  "What to ignore when traversing paths, getting children, etc..

NOTE: These should be compiled regex strings.")


(defconst path:rx:dirs:not-parent-or-current-dot
  directory-files-no-dot-files-regexp
  "Regexp matching any (non-empty) file name except \".\" and \"..\".

Useful for `path:children' and `path:children:types'.")


;;------------------------------------------------------------------------------
;; Predicates
;;------------------------------------------------------------------------------

(defun _:path:type:valid? (caller type &optional nil-invalid?)
  "Error if TYPE is invalid. Return TYPE if valid (can be nil).

CALLER should be the name of the calling function, for use in the error messages."
  ;;---
  ;; Error/OK: nil may or may not be valid...
  ;;---
  (cond ((null type)
         ;; Should nil be allowed as a type?
         (if nil-invalid?
             (error "%s: Invalid TYPE `%S'. TYPE must be one of: %S"
                caller
                type
                path:types)

           ;; nil is allowed.
           type))

         ;;---
         ;; Error: Not a keyword?
         ;;---
         ((not (keywordp type))
          (error "%s: Invalid TYPE `%S'. TYPE must be a keyword! Valid TYPEs: %S"
                caller
                type
                path:types))

         ;;---
         ;; Error: Not a valid keyword.
         ;;---
         ((not (memq type path:types))
          (error "%s: Unknown TYPE `%S'. TYPE must be one of: %S"
                 caller
                 type
                 path:types))

         ;;---
         ;; OK: Anything else? Think we've checked everything.
         ;;---
         (t
          type)))


(defun path:directory? (path)
  "Return non-nil if PATH is a directory name path."
  (directory-name-p path))


(defun path:file? (path)
  "Return non-nil if PATH is a file name path."
  (not (path:directory? path)))


(defun path:absolute? (path)
  "Return non-nil if PATH is an absolute path.

`file-name-absolute-p' only cares about current system; it is not able to
say \"we're on linux but yeah, 'C:/' is absolute.\"

This understands paths regardless of current system type.

See `_:path:system:types' for system types it can handle."
  ;; Check for various absolute paths, return truthy.
  ;; Failure to absolute means relative.

  ;; `file-name-absolute-p' only cares about current system; it is not able to
  ;; say "we're on linux but yeah, 'C:/' is absolute."
  ;; But check it anyways; it's probably smarter for all the edge cases?
  (cond ((file-name-absolute-p path)
         t)

        ;; Assume "/" is the root of a Linuxy/Unixy path.
        ((string-prefix-p "/" path)
         t)

        ;; Check for Windows drive prefixes.
        ((string-match-p (rx-to-string '(and string-start letter ":")
                                       :no-group)
                         path)
         t)

        ;; ¯\_(ツ)_/¯
        ;; Guess it's relative then?
        (t
         nil)))
;; (path:absolute? "C:/")
;; (path:absolute? "/")


(defun path:relative? (path)
  "Return non-nil if PATH is a relative path.

`file-name-absolute-p' only cares about current system; it is not able to
say \"we're on linux but yeah, 'C:/' is absolute.\"

This understands paths regardless of current system type.

See `_:path:system:types' for system types it can handle."
  (not (path:absolute? path)))


(defun path:exists? (path &optional type)
  "Return non-nil if PATH exists.

TYPE can be: :dir, :file, :symlink, or nil.
If TYPE is provided, PATH must exist \and\ be the correct type."
  ;;------------------------------
  ;; Error Cases
  ;;------------------------------
  ;; Errors on invaild type.
  (_:path:type:valid? "path:exists?" type)

  ;; Sanity check path...
  (cond ((not (stringp path))
         (error "path:exists?: PATH must be a string: %S"
                path))

        ;;------------------------------
        ;; Check Existance
        ;;------------------------------
        ((null type)
         (file-exists-p path))

        ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Kinds-of-Files.html
        ((eq :dir type)
         (file-directory-p path))

        ((eq :file type)
         (file-regular-p path))

        ((eq :symlink type)
         (file-symlink-p path))

        ;; Why & how are you here?
        (t
         (error "path:exists?: Unhandled TYPE `%S'!!!"
                type))))
;; (path:exists? (path:current:file))
;; (path:exists? (path:current:file) :file)
;; (path:exists? (path:current:file) :dir)
;; (path:exists? (path:current:dir) :dir)
;; (path:exists? (path:current:dir))


(defun path:type? (path)
  "What type of thing is PATH?

Return a `path:types' keyword if PATH exists or nil if PATH does not."
  ;; No attributes mean it doesn't exist means we can just return nil.
  (when-let ((attrs (file-attributes path)))
    ;; Have attributes; figure out what this path is based off them:
    (cond ((eq (file-attribute-type attrs) t)    ; t == directory
           :dir)
          ((eq (file-attribute-type attrs) nil)  ; nil == file
           :file)
          ((stringp (file-attribute-type attrs)) ; string == symlink
           :symlink)
          ;; Well...
          ;; PATH exists but we don't know exactly _what_ it is...
          (t
           :unknown))))
;; (path:type? (path:current:file))
;; (path:type? (path:current:dir))
;; (path:type? "does-not-exist.el")


(defun path:readable? (path)
  "Return non-nil if PATH exists and is readable."
  (file-readable-p path))


(defun path:descendant? (descendant ancestor)
  "Return non-nil if DESCENDANT path is a descendant of ANCESTOR path.

DESCENDANT and ANCESTOR cannot be the same path.

Will convert the paths to absolute/canonical values before comparing."
  (let ((descendant/abs (path:canonicalize:dir descendant))
        (ancestor/abs (path:canonicalize:dir ancestor)))
    (unless (string= descendant/abs ancestor/abs)
      (string-prefix-p ancestor/abs
                       descendant/abs))))
;; (path:descendant? "/foo/bar" "/foo")


(defun path:ancestor? (ancestor descendant)
  "Return non-nil if ANCESTOR path is an ancestor of DESCENDANT path.

DESCENDANT and ANCESTOR cannot be the same path.

Will convert the paths to absolute/canonical values before comparing."
  ;; Just translate into the thing we already have.
  (path:descendant? descendant ancestor))
;; (path:ancestor? "/foo/" "/foo/bar")


(defun path:child? (child parent)
  "Return non-nil if CHILD path is a direct child of PARENT path.

CHILD and PARENT cannot be the same path.

Will convert the paths to absolute/canonical values before comparing."
  ;; Convert both to file paths so that a dir and a file path work correctly.
  ;; (File paths because that's what `path:parent' returns).
  ;; e.g.:
  ;;   (path:child? "/foo/" "/foo")
  ;;     -> nil
  (let ((child/abs (path:canonicalize:file child))
        (parent/abs (path:canonicalize:file parent)))
    (unless (string= child/abs parent/abs)
      (string= (path:parent child/abs)
               parent/abs))))
;; (path:child? "/" "/")
;; (path:child? "/foo" "/")
;; (path:child? "/foo/bar" "/foo")


(defun path:equal? (a b)
  "Return non-nil if paths A and B are the same.

Will convert the paths to absolute/canonical values before comparing."
  ;; Convert both to dir or file paths so that comparing a dir and a file path work correctly.
  (string= (path:canonicalize:file a)
           (path:canonicalize:file b)))



;;------------------------------------------------------------------------------
;; Traversal
;;------------------------------------------------------------------------------

(defun path:parent (path)
  "Return the parent directory of PATH.

Special Cases:
  1. If PATH is the root of the file system, return the root.

  2. If PATH is the top level of a relative path (e.g. \"this-dir\"), return
     empty string."
  (directory-file-name
   ;; `file-name-directory' returns nil for parent of top-level relative path:
   ;;   (file-name-directory "relative")
   ;;     -> nil
   ;; Avoid that; use empty string instead of nil.
   (or (file-name-directory
        ;; But first make sure PATH doesn't have a trailing slash.
        ;; Otherwise all we do is strip the slash.
        ;; Example: "/path/to/foo/" should have a parent of:
        ;; "/path/to" aka "/path/to/", not "/path/to/foo".
        (path:file path))
       "")))
;; (path:parent "relative/path/to/foo.test")
;; (path:parent "relative")
;; (path:parent "/")
;; (path:parent "/path/to/foo/")


;; TODO: Move to regex.el?
(defun path:ignore? (path regexes)
  "Return non-nil if PATH matches any of the REGEXES."
  (let ((func/name "path:ignore?")
        (regex:list regexes) ;; Shallow copy so we can pop without possibly changing caller's list.
        ignore?)
    (unless (stringp path)
      (error "%s: PATH must be a string! Got: %S"
             func/name
             path))
    (unless (listp regexes)
      (error "%s: REGEXES must be a list of regex strings! Got: %S"
             func/name
             regexes))

    (while (and (not ignore?) ;; Found a reason to ignore already?
                regex:list)   ;; Finished the regexes?
      (let ((regex (pop regex:list)))
        (unless (stringp regex)
          (error "%s: Regex must be a string! Got: %S"
                 func/name
                 regex))

        (when (string-match regex path)
          ;; Set our return & stop-looping-early value.
          (setq ignore? t))))

    ignore?))
;; (path:ignore? "x.y" path:rx:names:ignore)
;; (path:ignore? "." path:rx:names:ignore)
;; (path:ignore? ".." path:rx:names:ignore)
;; (path:ignore? "..." path:rx:names:ignore)


(defun path:children:types (path:dir &optional absolute-paths? regex &rest types)
  "Return immediate children of PATH:DIR directory.

Return an alist of children by type:
  '((:file . (\"child.ext\" ...))
    ...)

If ABSOLUTE-PATHS? is non-nil, return absolute paths to the children.
Else return names of children.

If REGEX is non-nil, return only children whose filename matches the REGEX.

TYPES should be nil or a list of keywords from `path:types'.
If TYPES is non-nil, return only children of those types."
  (let ((types (if types
                   ;; Remove nils & flatten '(nil) to just nil.
                   (seq-filter (lambda (t) (not (null t)))
                               types)
                 ;; nil is any/all types.
                 nil)))
    ;;------------------------------
    ;; Error Checking
    ;;------------------------------
    ;; Errors on invaild type.
    (dolist (type types)
      (_:path:type:valid? "path:children:types" type))

    (let ((func/name "path:children:types"))
      (unless (stringp path:dir)
        (error "%s: PATH:DIR must be a string! Got: %S"
               func/name
               path:dir))

      (let ((path:root     (path:canonicalize:dir path:dir))
            (save:dirs     (if types (memq :dir     types) t))
            (save:files    (if types (memq :file    types) t))
            (save:symlinks (if types (memq :symlink types) t))
            children)
        (unless (path:exists? path:root :dir)
          (error "%s: PATH:DIR does not exist: %s"
                 func/name
                 path:root))

        ;;------------------------------
        ;; Find Children
        ;;------------------------------
        ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Contents-of-Directories.html
        ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/File-Attributes.html#Definition-of-file_002dattributes
        (dolist (child (directory-files-and-attributes path:root nil regex))
          (let* ((child:name  (car child))
                 (child:path  (path:join path:root child:name))
                 (child:attrs (cdr child))
                 type)

            ;;------------------------------
            ;; Ignore / Determine Type
            ;;------------------------------
            ;; Explicitly ignore?
            (cond ((path:ignore? child:name path:rx:names:ignore)
                   (setq type :ignore))

                  ;;---
                  ;; Save / Ignore by Type
                  ;;---
                  ((eq (file-attribute-type child:attrs) t) ;; t is the attr type for directories.
                   (setq type (if save:dirs :dir :ignore)))

                  ((eq (file-attribute-type child:attrs) nil) ;; nil is the attr type for files.
                   (setq type (if save:files :file :ignore)))

                  ((stringp (file-attribute-type child:attrs)) ;; The attr type for symlinks is a string of the path they point to.(
                   (setq type (if save:symlinks :symlink :ignore)))

                  ;;---
                  ;; Error: How did you get here?
                  ;;---
                  (t
                   (error "%s: '%s': Unhandled file-attribute-type: %S"
                          func/name
                          child:path
                          (file-attribute-type child:attrs))))

            ;;------------------------------
            ;; Save Child?
            ;;------------------------------
            (when (and type
                       (not (eq type :ignore)))
              (let ((children:type (alist-get type children)))
                ;; Update type's list of children with this child's path.
                (push (if absolute-paths? child:path child:name) children:type)
                (setf (alist-get type children) children:type)))))

        ;;------------------------------
        ;; Return list we've built.
        ;;------------------------------
        children))))
;; (path:children:types user-emacs-directory)
;; (path:children:types user-emacs-directory nil)
;; (path:children:types user-emacs-directory t)
;; (path:children:types user-emacs-directory nil :file)
;; (path:children:types user-emacs-directory nil :dir)
;; (path:children:types (path:current:dir) nil nil)


;; TODO: Follow symlinks or no?
(defun path:children (path:dir &optional absolute-paths? type regex)
  "Return immediate children of PATH:DIR directory.

If ABSOLUTE-PATHS? is non-nil, return a list of absolute paths to the children.
Else return a list of names of children.

If TYPE is supplied, return only children of that type.

If REGEX is non-nil, return only children whose filename matches the REGEX."
  ;; Get by TYPE, then flatten to a single list of all children.
  (let ((by-types (path:children:types path:dir absolute-paths? regex type))
        children)
    (dolist (type:assoc by-types)
      (setq children (if children
                         ;; Append to children.
                         (cons children (cdr type:assoc))
                       ;; Create children.
                       (cdr type:assoc))))
    children))
;; (path:children (path:current:dir))
;; (path:children (path:current:dir) t)
;; (path:children (path:current:dir) nil :file)
;; (path:children (path:current:dir) nil :dir)


(defun _:path:walk (root dir callback)
  "Helper for walking a directory tree.

Gets children from ROOT subdirectory DIR, calls CALLBACK for each child.

CALLBACK should accept params:
  1) string - root (absolute)
  2) string - directory (relative path from root)
  3) string - child (a file/dir/etc in directory)

CALLBACK should be a predicate for \"Continue walking?\"; it should return
non-nil to continue and nil to halt the walk."
  (let ((children:type (path:children:types (path:canonicalize:dir root dir)))
        (continue t)
        (child:dirs nil)) ;; Walk down into these dirs.

    ;;------------------------------
    ;; Deal with children based on how likely to be a directory.
    ;;------------------------------
    (when-let ((continue continue) ;; Already done?
               (files (alist-get :file children:type)))
      (while (and continue
                  files)
        (when-let ((child (pop files)))
          (setq continue (funcall callback
                                  root
                                  dir
                                  child)))))

    (when-let ((continue continue) ;; Already done?
               (symlinks (alist-get :symlink children:type)))
      (while (and continue
                  symlinks)
        (when-let ((child (pop symlinks)))
          ;; TODO: Push dir symlinks (not file symlinks) into `child:dirs'?
          (setq continue (funcall callback
                                  root
                                  dir
                                  child)))))

    (when-let ((continue continue) ;; Already done?
               (dirs (alist-get :dir children:type)))
      (while (and continue
                  dirs)
        (when-let ((child (pop dirs)))
          (push child child:dirs)
          (setq continue (funcall callback
                                  root
                                  dir
                                  child)))))

    ;;------------------------------
    ;; Return all our children dirs to walk.
    ;;------------------------------
    (cons continue
          (if child:dirs
              (cons dir child:dirs)
            nil))))
;; (_:path:walk (path:current:dir) "." (lambda (x) (message "hi %S" x)))
;; (_:path:walk (path:current:dir) nil (lambda (x) (message "hi %S" x)))
;; (_:path:walk (path:current:dir) ".." (lambda (x) (message "hi %S" x)) t)


(defun path:walk (root callback)
  "Walk the directory tree starting at ROOT, call CALLBACK for each child.

CALLBACK should accept params:
  1) string - path of child, absolute
  2) string - path of child, relative to ROOT

CALLBACK should be a predicate for \"Continue walking?\"; it should return
non-nil to continue and nil to halt the walk."
  ;;------------------------------
  ;; Validate input.
  ;;------------------------------
  (cond ((null callback)
         (error "_:path:walk: Must have a CALLBACK function to walk directory tree! Got: %S"
                callback))
        ((not (functionp callback))
         (error "_:path:walk: CALLBACK must be a `functionp' to walk directory tree! Got: %S --functionp?--> %S"
                callback
                (functionp callback)))
        (t
         ;; ok
         nil))

  ;;------------------------------
  ;; Walk dirs.
  ;;------------------------------
  ;; Start our walk.
  (let* ((path-root (path:canonicalize:dir root))
         ;; walked is (continue . (dir . children))
         (walked   (_:path:walk root
                                   nil
                                   callback))
         (continue (car walked))
         (next     (list (cdr walked))))

    ;; Walk while we still have entries in `next'.
    (while (and continue
                next)
      (let* ((current  (pop next))
             (dir      (car current))
             (children (cdr current)))
        ;; Walk a child directory.
        (while (and continue
                    children)
          ;; Walk a child directory.
          (setq walked (_:path:walk root
                                       (path:join dir (pop children))
                                       callback)
                continue (car walked))

          ;; _Append_ to next; don't set/overwrite it.
          (when (cddr walked) ;; Only append if we have more children directories to walk.
            ;; Don't push `continue' flag.
            (push (cdr walked) next)))))))
;; (path:walk (path:current:dir) (lambda (x) (message "hi %S" x)))
;; (path:walk (path:parent (path:current:dir)) (lambda (x) (message "hi %S" x)) t)


;;------------------------------------------------------------------------------
;; Join
;;------------------------------------------------------------------------------

;; TODO: `path:validate' function?

(defun _:path:append (parent next)
  "Append NEXT element to PARENT, adding dir separator between them.

PARENT & NEXT are normalized via `str:normalize:any', so keywords or symbol
names can be used as well as strings."
  (cond ((and (null parent)
              (null next))
         ;; Change this to just return nil so `path:join' can be a little more robust?
         nil
         ;; (error "_:path:append: Cannot append nulls! parent: %S, next: %S"
         ;;        parent
         ;;        next)
         )
        ((null next)
         (str:normalize:any parent))
        ((null parent)
         (str:normalize:any next))
        (t
         (concat (file-name-as-directory (str:normalize:any parent))
                 (str:normalize:any next)))))
;; (_:path:append nil "jeff")
;; (_:path:append "jeff" "jill")
;; (_:path:append "jeff/" "jill")
;; (_:path:append 'jeff :jill)
;; (_:path:append 'jeff nil)
;; (_:path:append nil nil)


;; TODO: Make `list` module, move there?
(defun _:path:flatten (list)
  "Flatten LIST to a single, non-nested list.

Filter out nil."
  (declare (pure t) (side-effect-free t))

  (if (and (listp list) (listp (cdr list)))
      (mapcan #'_:path:flatten list)
    (list list)))


(defun path:join (&rest path)
  "Flatten & combine PATH elements together into a path platform-agnostically.

(path:join \"jeff\" \"jill.el\")
  ->\"jeff/jill.el\""
  (seq-reduce #'_:path:append
              (apply #'str:normalize:each (_:path:flatten path))
              nil))
;; (path:join "jeff" "jill")
;; (path:join "jeff" "jill/")
;; (path:join "jeff" nil)
;; (path:join "" nil)
;; (path:join nil)
;; (path:join '("jeff" :jill) "hill")


;;------------------------------------------------------------------------------
;; Split
;;------------------------------------------------------------------------------

(defvar _:path:separators:rx (rx-to-string '(one-or-more (or ?/ ?\\)) ;; '(or ?/ ?\\)
                                              :no-group)
  "Separators for Windows and Linux paths.")


;;------------------------------
;; Split on Dir Separators
;;------------------------------

(defun path:split (&rest path)
  "Split all PATH strings by directory separators, return one list."
  (let* ((path (apply #'path:join path))
         (split (split-string path
                              "[/\\]"
                              :omit-nulls)))

    ;; Unix rooted paths should keep "/" as the root directory.
    (if (string-prefix-p "/" path)
        (cons "/" split)
      ;; Relative paths and Windows drive-letter rooted paths don't need to do that.
      split)))
;; (path:split "/foo/bar" "/baz")
;; (path:split "c:/foo/bar" "/baz")
;; (path:split "c:\\foo\\bar" "\\baz")


;;------------------------------
;; Path Segments
;;------------------------------

(defun _:path:normalize:system-type (type &optional type-of-auto)
  "Normalize TYPE.

TYPE should be a valid value of `system-type', which see.
  - Also acceptable: `:auto', `:windows', `:linux', `:mac'

Return value will be a keyword & valid key of `_:path:system:types'.

NOTE: TYPE `:auto' will be normalized to TYPE-OF-AUTO, or to system's type if
TYPE-OF-AUTO is nil."
  (let ((keyword (if-let ((assoc (rassoc type _:path:system:types)))
                     ;; Got a `system-type' we support; return the keyword.
                     (car assoc)
                   ;; Is it a supported keyword already?
                   (if (assoc type _:path:system:types)
                       type
                     ;; Dunno; give up.
                     (error "_:path:normalize:system-type: Unknown/unsupported system type: %S"
                            type)))))

    ;; Figured out the type keyword; do we need to resolve `:auto'?
    (when (eq keyword :auto)
      (setq keyword (or type-of-auto
                        (car (rassoc system-type _:path:system:types))))
      (unless keyword
        (error "_:path:normalize:system-type: Unsupported system type for `:auto': %S"
               (or type-of-auto system-type))))

    keyword))
;; (_:path:normalize:system-type :auto)
;; (_:path:normalize:system-type :windows)
;; (_:path:normalize:system-type 'windows-nt)
;; (_:path:normalize:system-type 'gnu/linux)


(defun path:segments (type &rest path)
  "Split PATH by directory separators, return a plist.

TYPE should be a valid value of `system-type', which see.
  - Also acceptable: `:auto', `:windows', `:linux', `:mac'

Returned PLIST will have these keys (if their values are non-nil).
  :drive   - Only if TYPE is `:windows'.
           - Drive letter / name
           - \"C:/foo/bar\" -> \"C:\"
  :root    - Root of the path (\"/\", \"C:\\\", etc.)
           - \"C:/foo/bar\" -> \"/\"
           - \"/foo/bar\" -> \"/\"
  :parents - Parent directory ancestors of PATH.
           - \"C:/foo/bar\" -> '(\"foo\" \"bar\")
           - \"/foo/bar/baz\" -> '(\"foo\" \"bar\")
  :name    - Name of the final path element.
           - \"/foo/bar/baz\" -> \"baz\"
           - \"/foo/bar/\" -> \"bar\"
           - \"/foo/bar.tar.gz\" -> \"bar.tar.gz\""
  (setq path (apply #'path:join path))
  (let ((type (_:path:normalize:system-type type))
        drive
        root
        ;; `segments' will get split into `parents' and `name'.
        segments
        parents
        name
        output
        ;; Normalize inputs into a list of path components.
        (paths (path:split path)))

    (unless paths
      (error "path:segments: PATH has no strings to split: %S"
             path))

  ;;------------------------------
  ;; Parse input.
  ;;------------------------------
  ;;---
  ;; Root & Drive
  ;;---
  ;; Path is absolute if first/only segment is absolute.
  (when (path:absolute? path)

    ;; If absolute, set the root directory.
    (setq root "/")

    ;; If a Windows path, set the root drive.
    (when (eq type :windows)
      (save-match-data
        (when (string-match (rx-to-string '(and string-start (group letter) ":")
                                          :no-group)
                            (car paths))
          (setq drive (match-string 1 (car paths))
                root  (format "%s:/" drive)
                paths (cdr paths)))))) ;; Drop drive ("C:") off of list of dirs.

  ;;---
  ;; Parents & Name
  ;;---
  ;; Split each input into segments.
  (dolist (path paths)
    (dolist (segment (split-string path
                                   _:path:separators:rx
                                   t
                                   split-string-default-separators))
      (push segment segments)))

  ;; `segments' is backwards, so first item is file/dir `name',
  ;; rest need to be reversed into the `parents'.
  (setq name    (car segments)
        parents (nreverse (cdr segments)))

  ;;------------------------------
  ;; Build output plist (in reverse).
  ;;------------------------------
  (when name
    (push name output)
    (push :name output))

  (when parents
    (push parents output)
    (push :parents output))

  (when root
    (push root output)
    (push :root output))

  (when drive
    (push drive output)
    (push :drive output))

  ;; Return segments plist.
  output))
;; (path:segments :linux "/foo/bar" "/baz")
;; (path:segments :windows "C:/foo/bar" "/baz")


;;------------------------------------------------------------------------------
;; Normalize / Canonicalize
;;------------------------------------------------------------------------------

(defun path:directory (path &rest segment)
  "Join PATH and SEGMENTs, then ensure it is a directory path."
   (file-name-as-directory (apply #'path:join path segment)))
;; (path:directory "/path/to/dir.ectory")
;; (path:dir "/path/to/dir.ectory")

(defalias 'path:dir 'path:directory)


(defun path:file (path &rest segment)
  "Join PATH and SEGMENTs, then ensure it is _NOT_ a directory path."
  ;; Force the return value to be a file path by ensuring trailing "/" and then
  ;; removing it.
  (directory-file-name (apply #'path:directory path segment)))
;; (path:file "/path/to/file.txt/")


(defun path:canonicalize:file (path &rest segment)
  "Canonicalize/normalize a file PATH and path SEGMENTS.

Return an absolute path.

Does not fix or validate PATH or SEGMENT components; they are expected to be
valid."
  (path:file
   ;; Get a normalized path.
   (expand-file-name (apply #'path:join
                            path
                            segment))))
;; (path:canonicalize:file "~" "personal" "something.exe" "zort.txt")
;; (path:canonicalize:file "~" "personal" "something.exe" ".")
;; (path:canonicalize:file "~" "personal" "something.exe" "..")


(defalias 'path:absolute:file 'path:canonicalize:file)
(defalias 'path:abs:file      'path:canonicalize:file)


(defun path:canonicalize:dir (path &rest segment)
  "Canonicalize/normalize a directory PATH and path SEGMENTS.

Return an absolute path.

Does not fix or validate PATH or SEGMENT components; they are expected to be
valid."
  ;; Fully qualify base as start of return value.
  (path:directory (apply #'path:canonicalize:file path segment)))
;; (path:canonicalize:dir "~" "personal" "something" "zort")


(defalias 'path:absolute:dir 'path:canonicalize:dir)
(defalias 'path:abs:dir      'path:canonicalize:dir)


(defun path:canonicalize:absolute (path &rest segment)
  "Canonicalize/normalize a file PATH and path SEGMENTS.

Will attempt to preserve file/directory-ness of PATH - that is, try to preserve
the final slash if it exists.

Return an absolute path.

Does not fix or validate PATH or SEGMENT components; they are expected to be
valid."
  (let ((path/joined (apply #'path:join path segment)))
    (funcall (if (path:directory? path/joined)
                 #'path:canonicalize:dir
               #'path:canonicalize:file)
             path/joined)))
;; (path:canonicalize:absolute "/foo" "bar")
;; (path:canonicalize:absolute "/foo" "bar/")


(defalias 'path:canonicalize       'path:canonicalize:absolute)
(defalias 'path:absolute           'path:canonicalize:absolute)
(defalias 'path:abs                'path:canonicalize:absolute)


(defun path:abbreviate:file (path &rest segment)
  "Create file path from PATH & SEGMENTs, shortened using `directory-abbrev-alist'.

Return an absolute path.

Does not fix or validate PATH or SEGMENT components; they are expected to be
valid."
  (abbreviate-file-name
   (apply #'path:canonicalize:file path segment)))


(defalias 'path:abbrev:file        'path:abbreviate:file)


(defun path:abbreviate:dir (path &rest segment)
  "Create dir path from PATH & SEGMENTs, shortened using `directory-abbrev-alist'.

Return an absolute path.

Does not fix or validate PATH or SEGMENT components; they are expected to be
valid."
  (abbreviate-file-name
   (apply #'path:canonicalize:dir path segment)))


(defalias 'path:abbrev:dir         'path:abbreviate:dir)


(defun path:abbreviate:absolute (path &rest segment)
  "Create path from PATH & SEGMENTs, shortened using `directory-abbrev-alist'.

Will attempt to preserve file/directory-ness of PATH - that is, try to preserve
the final slash if it exists.

Return an absolute path.

Does not fix or validate PATH or SEGMENT components; they are expected to be
valid."
  (let ((path/joined (apply #'path:join path segment)))
    (funcall (if (path:directory? path/joined)
                 #'path:abbreviate:dir
               #'path:abbreviate:file)
             path/joined)))
;; (path:abbreviate:absolute "/foo" "bar")
;; (path:abbreviate:absolute "/foo" "bar/")


(defalias 'path:abbreviate         'path:abbreviate:absolute)
(defalias 'path:abbrev             'path:abbreviate:absolute)


;;------------------------------------------------------------------------------
;; Relative Paths
;;------------------------------------------------------------------------------

(defun path:canonicalize:relative (path root)
  "Return a file path to PATH relative to ROOT.

Could just return PATH if it has no relation to ROOT.

Raises an error if PATH is not a string.
Raises an error if ROOT is not nil and not a string."
  (unless (stringp path)
    (error "path:canonicalize:relative: PATH must be a string! Got: path: %S, root: %S"
           path root))
  (unless (or (null root)
              (stringp root))
    (error "path:canonicalize:relative: ROOT must be nil or a string! Got: path: %S, root: %S"
           path root))

  ;; Translate nil ROOT to empty string if needed.
  ;; And canonicalize our paths.
  (let ((root (or (path:canonicalize:dir root) ""))
        (path (path:canonicalize:file path)))
    ;; Don't like `file-relative-name' as it can return weird things when it
    ;; goes off looking for actual directories and files...
    (replace-regexp-in-string
     root ;; Look for ROOT directory path...
     ""   ;; Replace with nothing to get a relative path.
     path ;; Ensure
     :fixedcase
     :literal)))
;; (path:canonicalize:relative "/path/to/a/file/location.txt" "/path/to/a/")
;; (path:canonicalize:relative "/path/to/a/dir/location/" "/path/to/a/")
;; (path:canonicalize:relative "/path/to/a/dir/location/" "/path/to/a")


(defalias 'path:relative           'path:canonicalize:relative)
(defalias 'path:rel                'path:canonicalize:relative)


(defun path:ancestor/common (&rest paths)
  "Return canonicalized common path between PATHS strings."
  ;; Normalize & split paths.
  (let ((paths (seq-map (lambda (path) (path:split (path:canonicalize:file path)))
                        paths)))
    ;; Finally, combine the common ancestor path segments back together again.
    (path:join
     (seq-reduce
      ;; Find common ancestor path of ACCUM and PATH.
      ;; PATH is a list of path segments to compare against ACCUM.
      ;; ACCUM is our common path segments (so far)."
      (lambda (accum path)
        ;; Compare path lists ACCUM and PATH, root-first. Stop when we
        ;; don't have a matching path segment between them.
        (seq-take-while (lambda (segment)
                          ;; If we have a common path (ACCUM), and if the (current)
                          ;; frontest piece (dir) of ACCUM matches this segment, take this
                          ;; segment as common between ACCUM and PATH.
                          (and accum
                               (equal (pop accum) segment)))
                        path))
      ;; Run rest of the paths through to find actual common ancestor.
      (seq-drop paths 1)
      ;; Start with first path as the presumed common ancestor.
      (pop paths)))))
;; (path:ancestor/common "~/foo/bar/baz"
;;                       "~/foo/bar/qux"
;;                       "~/foo/bazola/ztesch"
;;                       "~/foo/basket/case")


;;------------------------------------------------------------------------------
;; Current Paths
;;------------------------------------------------------------------------------

(defun path:current:file (&optional no-error?)
  "Return the Emacs Lisp file this function is called from.

Works when:
  - byte compiling
    - `byte-compile-current-file'
  - loading
    - `load-file-name'
    - `current-load-list'
  - visiting/evaluating
    - function `buffer-file-name'

Raises an error signal if it cannot find a file path.
If NO-ERROR? is non-nil, just return nil instead of raising an error signal."
  (cond
   ;;------------------------------
   ;; Look for a valid "current file" variable.
   ;;------------------------------
   ((bound-and-true-p byte-compile-current-file))

   ((bound-and-true-p load-file-name))

   ((stringp (car-safe current-load-list))
    (car current-load-list))

   ;; Opened (direct & indirect) buffers.
   ((buffer-file-name (buffer-base-buffer)))

   ;;------------------------------
   ;; Error: Didn't find anything valid.
   ;;------------------------------
   (no-error? nil)
   ((error "path:current:file: Cannot get the current file's path"))))
;; (path:current:file)


(defun path:current:dir ()
  "Return the directory of the Emacs Lisp file this function is called from.

Uses `path:current:file' and just chops off the filename."
  (when-let (path (path:current:file))
    (path:parent path)))
;; (path:current:dir)


;;------------------------------------------------------------------------------
;; Translations Between OSes
;;------------------------------------------------------------------------------

;; There are some existing packages for dealing with windows->unix or unix->windows paths...
;;   Windows emacs, Cygwin paths: https://www.emacswiki.org/emacs/cygwin-mount.el
;;   Cygwin/WSL emacs, win paths: https://github.com/victorhge/windows-path
;; but..: aren't on melpa, haven't been updated in years, etc.
(defun path:translate (from to translate-path)
  "Translate a path style, e.g. from Windows to WSL.

FROM and TO should be one of: (:auto :windows :wsl :linux)
If FROM is `:auto', attempt to determine TRANSLATE-PATH's OS automatically.
If TO is `:auto', it will be:
  - `:linux' if FROM is `:windows'
  - `:windows' if FROM is `:wsl' or `:linux'

TRANSLATE-PATH should be a string.

For `:windows' -> `:wsl':
  - Translates '<drive>:' to '/mnt/<drive>'.
  - Translates '\\' to '/'."
  ;;------------------------------
  ;; Resolve `:auto'?
  ;;------------------------------
  (setq from (if (eq from :auto)
                 (_:path:type translate-path)
               (_:path:normalize:system-type from)))

  (setq to (if (eq to :auto)
               (if (eq from :windows)
                   :linux
                 :windows)
             (_:path:normalize:system-type to)))

  ;;------------------------------
  ;; Translate path.
  ;;------------------------------
  (let ((segments (path:segments from translate-path))
        ;; Initial assumption: no translation needed.
        (path/translated translate-path))

    ;;---
    ;; Relative -> Relative?
    ;;---
    (cond ((not (path:absolute? translate-path))
           (setq path/translated translate-path))

          ;;---
          ;; `:windows' -> `:wsl'
          ;;---
          ((and (eq from :windows)
                (eq to   :wsl))
           (setq path/translated (path:join "/mnt"
                                  (downcase (plist-get segments :drive))
                                  (plist-get segments :parents)
                                  (plist-get segments :name))))

          ;;---
          ;; `:wsl' -> `:windows'
          ;;---
          ((and (eq from :wsl)
                (eq to   :windows))
           ;; Replace '/mnt/<drive>' with '<drive>:'.
           (if (string= (nth 0 (plist-get segments :parents))
                        "mnt")
               (setq path/translated (path:join
                                      ;; Drive letter is right after "mnt".
                                      (concat (upcase (nth 1 (plist-get segments :parents)))
                                                        ":")
                                      ;; Next, everything after drive letter:
                                      (cddr (plist-get segments :parents))
                                      (plist-get segments :name)))
             ;; No match - no translation.
             (error "path:translate: Cannot translate `%S' -> `%S'. Path is not in a WSL \"/mnt/<windows-drive>\" directory: %s"
                    from to translate-path)))

          ;;---
          ;;`:linux' -> `:windows'
          ;;---
          ((and (eq from :linux)
                (eq to   :windows))
           ;; Best we can do is guess at a Windows drive?
           (setq path/translated (path:join "C:"
                                            (plist-get segments :parents)
                                            (plist-get segments :name))))

          ;;---
          ;;`:windows' -> `:linux'
          ;;---
          ((and (eq from :windows)
                (eq to   :linux))
           ;; Just drop the Windows drive?
           (setq path/translated (path:join "/"
                                            (plist-get segments :parents)
                                            (plist-get segments :name))))

          ;;---
          ;; Unsupported Translation; Error Out
          ;;---
          (t
           (error "path:translate: Unsuppoprted translation: `%S' -> `%S' for path %s"
                  from to translate-path)))

    ;; Return the translation.
    path/translated))
;; (path:translate :windows :wsl "D:/path/to/somewhere.txt")
;; (path:translate :windows :auto "D:/path/to/somewhere.txt")
;; (path:translate :auto :wsl "D:/path/to/somewhere.txt")
;; (path:translate :wsl :windows "/mnt/d/path/to/somewhere.txt")
;; Should not be able to translate so should return "".
;; (path:translate :windows :wsl "~/path/to/somewhere.txt")


(defun _:path:type (path)
  "Try to guess a PATH type.

Return:
   :wsl     - Linux path to a windows drive?
   :windows - Windows path?
   :linix   - Linux path?"
  ;; Start guessing...
  ;; If it has a backslash, it's probably windows.
  (cond ((str:contains? "\\" path)
         :windows)

        ;; Does it start with a drive-letter and colon?
        ;; Probably windows.
        ((string-match (rx string-start
                           letter
                           ":")
                       path)
         :windows)

        ((string-match (rx string-start
                           "/mnt/"
                           letter
                           "/")
                       path)
         :wsl)

        (t
         :linux)))


;; TODO: "desired type" param?
(defun path:cmd:translate (path)
  "Try to auto-guess PATH type and then translate the path."
  (interactive "sPath: ")
  (let* ((source (_:path:type path))
         (dest (if (eq source :windows)
                   ;; WSL should work for translating to Linux too?
                   :wsl
                 :windows))
         (translated (path:translate source
                            dest
                            path)))
    ;; Copy to kill-ring...
    (kill-new translated)
    ;; Return it.
    translated))
;; (path:cmd:translate "D:/")


;;------------------------------------------------------------------------------
;; Project Paths
;;------------------------------------------------------------------------------

;; TODO: move to some `:project' module?
(defun path:project:root (&optional dir)
  "Return the project root of DIR (default: `default-directory').
Return nil if not in a project.

Borrowed from Doom's `doom-project-root' in \"core/autoload/projects.el\"."
  (let ((projectile-project-root
         (unless dir (bound-and-true-p projectile-project-root)))
        projectile-require-project-root)
    (projectile-project-root dir)))



;; TODO: move to some `:project' module?
(defun path:project? (&optional dir)
  "Return t if DIR (default: `default-directory') is a valid project directory.

Borrowed from Doom's `doom-project-p' in \"core/autoload/projects.el\""
  (and (path:project:root dir)
       t))


;;------------------------------------------------------------------------------
;; Dired
;;------------------------------------------------------------------------------

;;;###autoload
(defun path:cmd:dired (arg)
  "Open a directory in Dired.

If prefix ARG is non-nil, prompt for a known project to open in Dired.

Proudly nicked from Doom's \"modules/config/default/autoload/files.el\"."
  (interactive "P")
  (apply #'dired
         (if arg
             (list (completing-read "Open dired in project: " projectile-known-projects))
           (dired-read-dir-and-switches ""))))


;;------------------------------------------------------------------------------
;; Tramp
;;------------------------------------------------------------------------------

;; TODO: Make rest of `:path' and `:file' Tramp-aware?
;;   - remote path part
;;   - sudo part
;;   - method part
;;   - all the parts
;;   - parsing paths with the tramp junk in 'em...
;;     - Windows vs Linux Tramp paths...
(cl-defun path:tramp (&rest args
                      &key  sudo?
                            ;; user
                            ;; host
                      &allow-other-keys)
  "Return a Tramp-style path for ARGS & keys.

If SUDO? is non-nil, include 'sudo:root@<host>:' in the returned Tramp path so
Emacs will, e.g., open as super user.

Original from Doom's `doom--sudo-file-path' in \"core/autoload/files.el\"."
  ;; Must filter keyword args out of ARGS.
  (let* ((args (elisp/cl:parse:filter-kwargs args
                                             ;;---
                                             ;; Expected Tramp Keys
                                             ;;---
                                             ;; NOTE: This should be a superset of this function's `&key'!
                                             :sudo?))
         (path (path:join args))
         (user (file-remote-p path 'user))
         (host (or (file-remote-p path 'host) "localhost"))
         (remote? (file-remote-p path)))
    (if (or remote? sudo?)
        ;;------------------------------
        ;; Path Style: Tramp
        ;;------------------------------
        ;; Include at least one Tramp path component.
        (concat "/"

                ;;---
                ;; Remote user/host?
                ;;---
                (when remote?
                  (concat (file-remote-p path 'method) ":"
                          (if user
                              (concat user "@" host)
                            host)))

                (when (and remote? sudo?)
                  "|")

                ;;---
                ;; Super user?
                ;;---
                (when sudo?
                  (concat "sudo:root@"
                          host))

                ":"

                ;;---
                ;; Actual file path.
                ;;---
                (or (file-remote-p path 'localname)
                    path))

      ;;------------------------------
      ;; Path Style: Standard
      ;;------------------------------
      ;; Not remote; not sudo... just path.
      path)))
;; (path:tramp (buffer-file-name))
;; (path:tramp :sudo? t (buffer-file-name))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide path path)
