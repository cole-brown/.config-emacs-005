;;; core/modules/emacs/path/buffer.el --- Buffer-Related Path Functions -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2020-12-04
;; Timestamp:  2023-06-22
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Buffer-Related Path Functions
;;
;;; Code:


(require 'cl-lib)
(require 'dired)
(require 'project)

(imp:require :path 'path)
(imp:require :path 'git)


;;--------------------------------------------------------------------------------
;; Buffer Path
;;--------------------------------------------------------------------------------

(defun path:buffer (&optional buffer)
  "Get buffer's file path, if any.

BUFFER should be a buffer object, a buffer name string, or nil for
`current-buffer'.

Return nil if no backing file.

Some buffers have trouble knowing where they came from.

Example 1: An indirect buffer will be called \"foo.el<2>\", be visiting
\"foo.el\", but `buffer-file-truename` will be nil, `buffer-file-name` will
return nil... I mean... WTF?

Example 2: `dired' buffers... Sigh."
  (let ((buffer (if buffer
                    (get-buffer buffer)
                  (current-buffer))))
    (with-current-buffer buffer
      (cond (buffer-file-truename) ;; Is it already all figured out for us?
            (list-buffers-directory)

            ;; `dired' buffer? Use it's dir because... why does it not have a buffer file name?
            ;; TODO: Why would the original version use `string-equal' instead of `eq'?!
            ;;  > (string-equal major-mode 'dired-mode)
            ((eq major-mode 'dired-mode)
             default-directory)

            ;; Not a Dired buffer: Return the buffer's file name, if it has one.
            ((buffer-file-name (or (buffer-base-buffer buffer) ; indirect buffer?
                                   buffer)))

            ;; No wild guessing allowed.
            (t
             nil)))))
;; (path:buffer)


;;--------------------------------------------------------------------------------
;; Buffer Path Relative to Project
;;--------------------------------------------------------------------------------

(defun path:buffer:project/alist (&optional buffer)
  "Get an alist about BUFFER's filepath relative to its project root.

BUFFER should be a buffer object, a buffer name string, or nil for
`current-buffer'.

If in a project, return an alist:
  '((:project/type . symbol)                 ; e.g. `projectile'
    (:project/name . \"dir-name\")           ; e.g. \".emacs.d\"
    (:path         . \"path/to/buffer.el\")) ; relative to `:project/name' root

If not in a project, return nil.

If not a file-backed buffer, return nil."
  (when-let* ((path/buffer      (path:buffer buffer)) ;; Does buffer have a file and what is its actual name?
              (path/project     (cdr-safe (project-current))))
    ;; Ok; have the pieces. What's the relative path now?
    (list (cons :project/type (car-safe (project-current)))
          (cons :project/name (dir:name path/project))
          (cons :path  (path:canonicalize:relative path/buffer path/project)))))
;; (path:buffer:project/alist)


(defun path:buffer:project (&optional buffer pretty?)
  "Return BUFFER's filepath relative to the project root.

If not in a project, return nil.
If not a file-backed buffer, return nil.

If in a project, return:
  - If PRETTY? is non-nil:
    \"<project-name>:/relative/path/to/file.el\"
  - If PRETTY? is nil:
    \"<project-name>/relative/path/to/file.el\""
  (when-let* ((alist (path:buffer:project/alist buffer))
              (root (alist-get :project/name alist))
              (path (alist-get :path alist))
              (branch (path:vc/git:branch (path:buffer buffer))))
    ;; Return the fancy/pretty version, or just the relative path string?
    (concat (if pretty? "" "")
            (if pretty?
                (concat
                 (propertize root 'face 'underline)
                 (if branch
                     (concat "⎇(" branch ")")
                   nil))
              root)
            ;; Git uses ":/" as "the root of this git repo.
            (if pretty?
                path:vc/git:rooted
              "/")
            path)))
;; (path:buffer:project)
;; (path:buffer:project nil :pretty)


;;------------------------------------------------------------------------------
;; Copy Buffer File/Dir Name Functions
;;------------------------------------------------------------------------------

(cl-defun path:buffer:copy (&key parent? relative?)
  "Copy the buffer's current path to `kill-ring'.

If in a file buffer, copy the file's path.

If in Dired buffer, copy the file/dir cursor is on, or marked files.

If a buffer is not file and not Dired, copy value of `default-directory' (which
is usually the \"current\" dir when that buffer was created).

Optional keys:
  - `:parent?' (PARENT?)
    - If non-nil, copy the buffer's parent directory path instead of the
      buffer's own path.
  - `:relative?' (RELATIVE?)
    - If non-nil, copy the buffer's path relative to & including the project
      root, instead of the absolute path.

Return a string: either a path name or a newline separated list of path names.

Originally `xah-file-path' from
http://xahlee.info/emacs/emacs/emacs_copy_file_path.html"
  ;;------------------------------
  ;; Find Path(s)
  ;;------------------------------
  (let ((paths
         ;; In dired mode? Figure out what exactly to return.
         ;; TODO: Why would the original version use `string-equal' instead of `eq'?!
         ;;  > (string-equal major-mode 'dired-mode)
         (if (and (eq major-mode 'dired-mode)
                  ;; Have marked files? Use all those instead.
                  (> (length (dired-get-marked-files)) 0))
             (dired-get-marked-files) ; a list of strings
           (path:buffer))) ; a string
        ;; Need to normalize `paths' first.
        root)

    ;; Normalize to a list if we only got one path.
    (unless (listp paths)
      (setq paths (list paths)))

    ;; Now we can figure out project root.
    (when relative?
      (setq root (path:project:root (nth 0 paths)))
      (unless root
        (error "path:buffer:copy: No project root for path! Cannot determine relative path(s) for: %s"
               paths)))

    ;;------------------------------
    ;; What Part of Path(s) to Copy?
    ;;------------------------------
    (when parent?
      ;; Swap `paths' out for their parents.
      (let (paths/parent)
        (dolist (path paths)
          (push (path:parent path)
                paths/parent))
        (setq paths paths/parent)))

    (when relative?
      ;; Swap `paths' out for relative paths.
      (let ((project (file:name root))
            paths/rel)
        (dolist (path paths)
          (push (path:vc/git:rooted project
                                    (path:canonicalize:relative path root))
                paths/rel))
        (setq paths paths/rel)))

    ;;------------------------------
    ;; Copy Path(s)
    ;;------------------------------
    (let ((paths/len (length paths)))
      (if (= paths/len 1)
          (message "%s%sPath Copied: 「%s」"
                   (if relative? "Relative " "")
                   (if parent?   "Parent "   "")
                   (nth 0 paths))
        (message "%d %s%sPaths Copied: 「%s, ...」"
                 (format "%d " paths/len)
                 (if relative? "Relative " "")
                 (if parent?   "Parent "   "")
                 (nth 0 paths)))

      (kill-new (mapconcat 'identity paths "\n")))))


(defun path:cmd:buffer:copy:absolute (&optional parent?)
  "Copy the buffer's current path to `kill-ring'.

If in a file buffer, copy the file's path.

If in Dired buffer, copy the file/dir cursor is on, or marked files.

If a buffer is not file and not Dired, copy value of `default-directory' (which
is usually the \"current\" dir when that buffer was created).

If PARENT? is non-nil (`universal-argument' is called first), copy the buffer's
parent directory path instead of the buffer's own path.

Return a path string."
  (interactive "P")
  (path:buffer:copy :parent? parent?))


(defun path:cmd:buffer:copy:project (&optional parent?)
  "Copy the buffer's path, relative to/including project root, to `kill-ring'.

If in a file buffer, copy the file's path.

If in Dired buffer, copy the file/dir cursor is on, or marked files.

If a buffer is not file and not Dired, copy value of `default-directory' (which
is usually the \"current\" dir when that buffer was created).

If PARENT? is non-nil (`universal-argument' is called first), copy the buffer's
parent directory path instead of the buffer's own path.

Return a path string."
  (interactive "P")
  (path:buffer:copy :parent?   parent?
                    :relative? t))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :path 'buffer)
