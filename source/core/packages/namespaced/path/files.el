;;; namespaced/path/files.el --- Filename & files functions. -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-07-29
;; Timestamp:  2025-11-03
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Filename & files functions.
;;
;;; Code:


(require 'cl-lib)

(imp-require path:/path)


;;------------------------------------------------------------------------------
;; File Name
;;------------------------------------------------------------------------------

(defun file:name (path &rest segment)
  "Return the file name (with extension) from PATH & SEGMENT(s).

Example:
  (file:name \"/foo\" \"bar.tar.gz\")
  (file:name \"/foo/bar.tar.gz\")
  (file:name \"c:/foo/bar.tar.gz\")
    -> \"bar.tar.gz\""
  (file-name-nondirectory (apply #'path:file path segment)))
;; (file:name "/foo" "bar.tar.gz")
;; (file:name "/path/to/foo/")


(defun file:name:base (path &rest segment)
  "Return the file name (sans extension) from PATH & SEGMENT(s).

Only removes one extension if there are multiple.

Examples:
  (file:name \"/foo\" \"bar.baz\")
  (file:name \"/foo/bar.baz\")
  (file:name \"c:/foo/bar.baz\")
    -> \"bar\"

  (file:name \"/foo\" \"bar.tar.gz\")
  (file:name \"/foo/bar.tar.gz\")
  (file:name \"c:/foo/bar.tar.gz\")
    -> \"bar.tar\""
  (file-name-sans-extension (file:name path segment)))
;; (file:name "c:/foo/bar.baz")


;;------------------------------------------------------------------------------
;; Filters / Ignore Regexes
;;------------------------------------------------------------------------------

(defun files:ignore/regex (rx/path/partial &optional ignore)
  "Create a regex for ignoring RX/PATH/PARTIAL.

RX/PATH/PARTIAL should be a regex string. It can be either absolute or relative.
If relative, it can start with or without a path separator. Both Unix and
Windows type paths are supported.

IGNORE can be:
  - nil - No extra options.
  - :dir - Also ignore a trailing directory separator character.
  - :children - Like `:dir', and also ignore all of its children."
  (let* ((dir-sep '(or "/" "\\"))
         ;; Root of file system can be a few things, depending...
         (root (list 'or
                     (list 'and 'alphabetic ":") ;; Windows drive.
                     "~" ;; Home directory.
                     "/"))) ;; Unix root.
    (concat
     ;; First RX: the optional leading-up-to-`rx/path/partial' stuff.
     (eval `(rx string-start
                ;; Allow path to start at root, or with a directory separator.
                (optional (or ,root ,dir-sep))
                ;; Allow optional parent directories in path.
                (zero-or-more
                 (and (one-or-more printing)
                      ,dir-sep))))
     ;; Second RX: The Input.
     ;;   - Our path we're actually looking for.
     rx/path/partial
     ;; Third RX: The optional ending stuff.
     (eval `(rx
             ;; No path after our match - we want to match the end of the path.
             ;; Do, however, allow for optional trailing directory separator.
             ,(cond ((eq ignore :dir)
                     (list 'zero-or-one dir-sep))
                    ;; Path allowed after our match - ignore the dir and all its children.
                    ((eq ignore :children)
                     (list
                      ;; Allow path after the `rx/path/partial' dir.
                      'zero-or-more
                      (list 'and
                            dir-sep
                            '(one-or-more printing))
                      ;; Allow the optional trailing directory separator.
                      (list 'optional dir-sep)))
                    ;; Nothing is allowed after our match.
                    (t
                     ""))
             string-end)))))
;; (files:ignore/regex ".git" :children)
;; (files:ignore/regex "init.el")


(defun files:ignore/string (string/path/partial &optional ignore)
  "Create a regex for ignoring STRING/PATH/PARTIAL.

STRING/PATH/PARTIAL should NOT be a regex string. It can be either absolute or relative.
If relative, it can start with or without a path separator. Both Unix and
Windows type paths are supported.

IGNORE can be:
  - nil - No extra options.
  - :dir - Also ignore a trailing directory separator character.
  - :children - Like `:dir', and also ignore all of its children."
  (files:ignore/regex (regexp-quote string/path/partial) ignore))
;; (files:ignore/string ".git" :children)


(defun files:ignore? (path ignores)
  "Return non-nil if PATH should be ignored based on IGNORES (list of regex strings)."
  (declare (pure t) (side-effect-free t))

  (let ((ignores ignores) ;; Give ourself an `ignores' to modify.
        path/rx/ignore
        regex)
    (while (and (not path/rx/ignore)
                ignores)
      (setq regex (car ignores)
            ignores (cdr ignores))
       (when (string-match-p regex path)
        (setq path/rx/ignore t)))

    ;; Return whether to ignore or not.
    path/rx/ignore))
;; (files:ignore? "~/mnt/d/somewhere/.git" (list (files:ignore/string ".git" :children)))
;; Doesn't care about "." or "..":
;; (files:ignore? "~/mnt/d/somewhere/." (list (files:ignore/string ".git" :children)))
;; Not a regex, but still works:
;; (files:ignore? "d:/somewhere/modules/spy/buffer/init.el" (list "init.el"))
;; "Regex" version of above:
;; (files:ignore? "d:/somewhere/modules/spy/buffer/init.el" (list (files:ignore/string "init.el")))
;; (files:ignore? "~/mnt/d/somewhere/init.el" (list (files:ignore/string "init.el")))



;;------------------------------------------------------------------------------
;; Files in directory
;;------------------------------------------------------------------------------

(defun files:in-directory (dir &optional recursive types ignores)
  "Get all files in directory DIR (never returns \".\" or \"..\").

If RECURSIVE is non-nil, looks in DIR and all children directories.

If TYPES is nil, all file types are vaild for returning.
TYPES can also be a list with these valid members:
  - :dir - allow directories
  - :file - allow files
  - :symlink - symbolic links
Any file type matching a member of TYPES will be returned if not ignored.

If IGNORES is non-nil, it should be a list of regex strings to use. Any absolute
path matching any of the filters will not be included in return values."
  (let ((dir/root dir)
        dir/current
        dirs/to-process
        paths/return)
    ;; Add root dir to processing list and output (if desired) now.
    (push dir/root dirs/to-process)
    (when (or (null types)
              (memq :dir types))
      (push dir/root paths/return))

    ;; Get/process one dir at a time from our processing list.
    (while dirs/to-process
      (setq dir/current (pop dirs/to-process))

      (dolist (entry (directory-files-and-attributes dir/current 'full))
        (let ((path (car entry))
              (attrs (cdr entry)))
          ;; Force ignore "." and ".." to avoid infinite loop.
          (cond ((string-suffix-p "." path)
                 nil)

                ;; Ignore anything that matches one of our ignore regex.
                ((and ignores
                      (files:ignore? path ignores))
                 nil)

                ;; A directory?
                ((eq (file-attribute-type attrs) t) ;; t == directory
                 ;; Save if TYPES allows.
                 (when (or (not types)
                           (memq :dir types))
                   (push path paths/return))
                 ;; If RECURSIVE, add to list to process.
                 (when recursive
                   (push path dirs/to-process)))

                ;; A file?
                ((eq (file-attribute-type attrs) nil) ;; nil == file
                 ;; Save if TYPES allows.
                 (when (or (not types)
                           (memq :file types))
                   (push path paths/return)))

                ;; A symlink?
                ((stringp (file-attribute-type attrs)) ;; string == symlink
                 ;; Save if TYPES allows.
                 (when (or (not types)
                           (memq :symlink types))
                   (push path paths/return)))

                ;; Default case - ehm... dunno how you got here.
                (t
                 (error "files:in-directory: Unsupported file-attribute-type ('%S') for path: %s"
                        (file-attribute-type attrs)
                        path))))))

    ;; Return the collected paths.
    paths/return))
;; (files:in-directory "..")
;; (files:in-directory ".." t)
;; (files:in-directory ".." t '(:dir))
;; (files:in-directory ".." t '(:dir :file))
;; (files:in-directory ".." t nil (list (files:ignore/string "init.el")))


(defun file:find:in-ancestors (path filename &optional root)
  "Search for FILENAME in PATH's directory and parent directories up to ROOT.

If ROOT is nil, search up to the root of the filesystem.
If ROOT is non-nil and not an ancestor of PATH, raise an error signal.

Return absolute filepath string of found FILENAME, or nil."
  (let* ((path/current "placeholder for first loop :;',.@!#$%&*(){}][")
         (path/next (if (eq (path:type? path) :file)
                        (path:parent path)
                      path))
         path/found)

    ;;------------------------------
    ;; Error Checks
    ;;------------------------------
    (when root
      (cond ((not (eq (path:type? root) :dir))
             (error "file:find:in-ancestors: Expected a directory as ROOT! Got '%S' for root: %S"
                    (path:type? root)
                    root))
            ((not (path:ancestor? root path/next))
             (error "file:find:in-ancestors: PATH is not a descendant of ROOT! path: %S, root: %S"
                    path
                    root))
            (t
             ;; ok good!
             nil)))

    ;;------------------------------
    ;; Search for FILENAME
    ;;------------------------------
    ;; Did we find it or do we keep searching?
    (while (and
            ;; Continue if we haven't found it yet.
            (not path/found)
            ;; Continue if we have no explicit root...
            (or (not root)
                ;; ...or if we do and we haven't reached it yet.
                (and root
                     (path:ancestor? root path/next)))
            ;; Continue if we haven't reached the filesystem root yet.
            (not (string= path/current path/next)))

      ;; Prep for this loop.
      (setq path/current path/next)

      ;; Search in this directory's files for a matching filename.
      (setq path/found (seq-find (lambda (filepath)
                                   "Is this directory file the one we're searching for?"
                                   (string= filename (file:name filepath)))
                                 (files:in-directory path/current)))

      ;; Prep for next loop.
      (setq path/next (path:parent path/current)))

    ;; Return path of thing we found, or nil.
    path/found))
;; (file:find:in-ancestors (path:current:file) (file:name (path:current:file)))
;; (file:find:in-ancestors (path:current:dir) (file:name (path:current:file)))
;; (file:find:in-ancestors (path:current:dir) "str")
;; (file:find:in-ancestors (path:current:dir) "shouldnt-exist-probably")
;; (file:find:in-ancestors (path:current:dir) "shouldnt-exist-probably" (path:project:root))


;;------------------------------------------------------------------------------
;; File Operations
;;------------------------------------------------------------------------------

(defun file:update-files (&rest files)
  "Ensure FILES are updated in `recentf', `magit' and `save-place'.

Proudly nicked from Doom's `doom--update-files' in \"core/autoload/files.el\"."
  (let (toplevels)
    (dolist (file files)
      (when (featurep 'vc)
        (vc-file-clearprops file)
        (when-let (buffer (get-file-buffer file))
          (with-current-buffer buffer
            (vc-refresh-state))))

      (when (featurep 'magit)
        (when-let (default-directory (magit-toplevel (file-name-directory file)))
          (cl-pushnew default-directory toplevels)))

      (unless (file-readable-p file)
        (when (bound-and-true-p recentf-mode)
          (recentf-remove-if-non-kept file))

        (when (and (bound-and-true-p projectile-mode)
                   (path:project?)
                   (projectile-file-cached-p file (path:project:root)))
          (projectile-purge-file-from-cache file))))

    (dolist (default-directory toplevels)
      (magit-refresh))

    (when (bound-and-true-p save-place-mode)
      (save-place-forget-unreadable-files))))


;;------------------------------------------------------------------------------
;; Commands
;;------------------------------------------------------------------------------

;;;###autoload
(defun file:cmd:copy/this-buffer-file (path/new &optional force?)
  "Copy current buffer's file to PATH/NEW.

If FORCE?, overwrite the destination file if it exists, without confirmation.

Proudly nicked from Doom's \"core/autoload/files.el\"."
  (interactive
   (list (read-file-name "Copy file to: ")
         current-prefix-arg))

  (unless (and buffer-file-name (file-exists-p buffer-file-name))
    (user-error "Buffer is not visiting any file"))

  (let ((path/old (buffer-file-name (buffer-base-buffer)))
        (path/new (path:canonicalize:absolute path/new)))
    (make-directory (path:parent path/new) :make-parents)
    (copy-file path/old path/new (or force? 1))
    (file:update-files path/old path/new)
    (message "File copied to %S" (path:abbreviate:file path/new))))


;;;###autoload
(defun file:cmd:delete (&optional path force?)
  "Delete PATH, kill its buffers and expunge it from vc/magit cache.

If PATH is not specified, default to the current buffer's file.

If FORCE?, delete without confirmation.

Proudly nicked from Doom's \"core/autoload/files.el\"."
  (interactive (list (buffer-file-name (buffer-base-buffer))
                     current-prefix-arg))

  (let* ((path (or path (buffer-file-name (buffer-base-buffer))))
         (path/short (path:abbreviate:file path)))
    ;;------------------------------
    ;; Error Checks
    ;;------------------------------
    (unless (and path (file-exists-p path))
      (user-error "file:cmd:delete: Buffer is not visiting any file"))
    (unless (file-exists-p path)
      (error "file:cmd:delete: File doesn't exist: %s" path))
    (unless (or force? (y-or-n-p (format "Really delete %S?" path/short)))
      (user-error "file:cmd:delete: Aborted"))

    ;;------------------------------
    ;; Delete File
    ;;------------------------------
    (let ((buf (current-buffer)))
      (unwind-protect
          ;; Delete file?
          (progn (delete-file path :trash) t)

        ;; `delete-file' above failed. Do we need to do any clean up?
        (if (path:exists? path)
            ;; Still exists so just complain.
            (error "file:cmd:delete: Failed to delete %S" path/short)
          ;; File doesn't exist, so... ¯\_(ツ)_/¯
          (buffer:cmd:kill buf :dont-save)
          (file:update-files path)
          (message "Deleted %S" path/short))))))


;; TODO: A `file:delete` function.

;; TODO: Should I have file ops for `path:` where it makes sense?
;; (defalias 'path:cmd:delete 'file:cmd:delete)


;;;###autoload
(defun file:cmd:move/this (path/new &optional force?)
  "Move current buffer's file to PATH/NEW.

If FORCE?, overwrite the destination file if it exists, without confirmation.

Originally from Doom's `doom/move-this-file' in \"core/autoload/files.el\"."
  (interactive
   (list (read-file-name "Move file to: ")
         current-prefix-arg))

  ;;------------------------------
  ;; Error Checks
  ;;------------------------------
  (unless (and buffer-file-name (file-exists-p buffer-file-name))
    (user-error "file:cmd:move/this: Buffer is not visiting any file"))

  ;;------------------------------
  ;; Move File
  ;;------------------------------
  (let ((path/old (buffer-file-name (buffer-base-buffer)))
        (path/new (path:canonicalize path/new)))
    (when (path:directory? path/new)
      (setq path/new (path:join path/new (file:name path/old))))

    (make-directory (path:parent path/new) :make-parents)
    (rename-file path/old
                 path/new
                 ;; An integer for the third arg means "ask user for confirmation".
                 (or force? 1))
    (set-visited-file-name path/new :no-query :along-with-file)
    (file:update-files path/old path/new)
    (message "File moved to: %S" (path:abbreviate:file path/new))))


;;;###autoload
(defun file:cmd:find (&optional dir)
  "Find a file starting at DIR.

DIR, if nil, will default to `default-directory'.

Uses:
  - `counsel-find-file' if in `ivy-mode'
  - `helm-find-file' if in `helm-mode'
  - `find-file' otherwise"
  (interactive)
  (let ((default-directory (file-truename (path:canonicalize:dir (if (and (stringp dir)
                                                                          (not (string-empty-p dir)))
                                                                     dir
                                                                   default-directory)))))
    (call-interactively
     ;; What `find-file' function should be used anyways?
     (cond ((and (bound-and-true-p ivy-mode)
                 (fboundp 'counsel-find-file))
            #'counsel-find-file)
           ((and (bound-and-true-p helm-mode)
                 (fboundp 'helm-find-files))
            #'helm-find-files)
           (#'find-file)))))


;;;###autoload
(defun file:cmd:find/sudo (file)
  "Find FILE using a TRAMP 'sudo' path."
  (interactive "FSudo open file: ")
  (find-file (path:tramp :sudo? t file)))


;;;###autoload
(defun file:cmd:find/sudo/this ()
  "Open the current buffer's file/dir as root.

Copy/pasted from Doom's `doom/sudo-this-file' in \"core/autoload/files.el\"."
  (interactive)
  (funcall-interactively #'file:cmd:find/sudo
                         (or buffer-file-name
                             (when (or (derived-mode-p 'dired-mode)
                                       (derived-mode-p 'wdired-mode))
                               default-directory))))


;; TODO: Make a `:project' module and move there? Make this `project:cmd:find-file'?
;;;###autoload
(defun file:cmd:project:find-file (dir)
  "Jump to a file in DIR (searched recursively).

If DIR is not a project, it will be indexed (but not cached).

On loan from Doom's `doom-project-find-file' in \"core/autoload/projects.el\"."
  (interactive "DFind File In Project Directory: ")
  (let ((dir (path:canonicalize:dir dir)))
    ;;------------------------------
    ;; Error Checks
    ;;------------------------------
    (unless (file-directory-p dir)
      (error "file:cmd:project:find-file: Directory does not exist: %S" dir))
    (unless (file-readable-p dir)
      (error "file:cmd:project:find-file: Directory isn't readable: %S" dir))

    ;;------------------------------
    ;; Go to dir using...
    ;;------------------------------
    (let* ((default-directory (file-truename dir))
           (projectile-project-root (path:project:root dir))
           (projectile-enable-caching projectile-enable-caching))

      ;;---
      ;; Projectile?
      ;;---
      (cond (projectile-project-root
             (unless (path:project? default-directory)
               ;; Disable caching if this is not a real project; caching
               ;; non-projects easily has the potential to inflate the projectile
               ;; cache beyond reason.
               (setq projectile-enable-caching nil))
             (call-interactively
              ;; Intentionally avoid `helm-projectile-find-file', because it runs
              ;; asynchronously, and thus doesn't see the lexical
              ;; `default-directory'
              (if (and (bound-and-true-p ivy-mode)
                       (fboundp 'counsel-projectile-find-file))
                  #'counsel-projectile-find-file
                #'projectile-find-file)))

            ;;---
            ;; Ivy & Consel?
            ;;---
            ((and (bound-and-true-p ivy-mode)
                  (fboundp 'counsel-file-jump))
             (call-interactively #'counsel-file-jump))


            ;;---
            ;; Helm?
            ;;---
            ((and (bound-and-true-p helm-mode)
                  (fboundp 'helm-find-files))
             (call-interactively #'helm-find-files))

            ;; ;;---
            ;; ;; Project (not Projectile)?
            ;; ;;---
            ;; ((project-current nil dir)
            ;;  ;;---
            ;;  ;; ERROR: I'm... not sure how to make this...work?
            ;;  ;;---
            ;;  ;; Debugger entered--Lisp error: (cl-no-applicable-method project-root "/home/user/repos/proj-root/some-subdir/")
            ;;  ;; [...]
            ;;  ;;   project-root("/home/user/repos/proj-root/some-subdir/")
            ;;  ;;   #f(compiled-function (project &optional dirs) #<bytecode -0x1ac7ca1e5ef6e4f9>)("/home/user/repos/proj-root/some-subdir/" nil)
            ;;  ;;   apply(#f(compiled-function (project &optional dirs) #<bytecode -0x1ac7ca1e5ef6e4f9>) "/home/user/repos/proj-root/some-subdir/" nil)
            ;;  ;;   project-files("/home/user/repos/proj-root/some-subdir/" nil)
            ;;  ;;   project-find-file-in(nil nil "/home/user/repos/proj-root/some-subdir/")
            ;;  (project-find-file-in nil nil dir))

            ;;---
            ;; Fallback: Ye olde `find-file'.
            ;;---
            ((call-interactively #'find-file))))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide path files)
