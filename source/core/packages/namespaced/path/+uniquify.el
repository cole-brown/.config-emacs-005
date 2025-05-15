;;; core/modules/emacs/path/+uniquify.el --- Project-Based Unique Buffer Names -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-10-20
;; Timestamp:  2023-08-29
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Unique Buffer Names
;;
;; `uniquify' only gives you a very little bit of context to use if you want to
;; try to create your own buffer uniquifying function. It does this because it
;; expects to be in control of what is unique about the buffer's name.
;;
;; `path:uniquify' uniquify off of a project / version control root directory
;; and relative path, so we have to just ignore `uniquify' and wander out on our
;; own.
;;
;;; Code:


(require 'cl-lib)

(imp:require :path 'path)
(imp:require :path 'git)
(imp:require :path 'buffer)


;;--------------------------------------------------------------------------------
;; Nub - Errors & Debugging
;;--------------------------------------------------------------------------------

(defun int<path>:uniquify:nub:init ()
  "Initialize nub user & settings for `path:uniquify'."
  ;; Defaults for all the settings.
  (nub:vars:init :path:uniquify))
;; (int<path>:uniquify:nub:init)


(defun path:uniquify:debug:toggle ()
  "Toggle debugging for `path:uniquify'."
  (interactive)
  (nub:debug:toggle :path:uniquify))


;;--------------------------------------------------------------------------------
;; Settings
;;--------------------------------------------------------------------------------

;;------------------------------
;; Customizable
;;------------------------------

(defcustom path:uniquify:directory/end-in-slash? t
  "If non-nil, add a path separator to end of names that are dirs (eg `Dired')."
  :group 'path:group
  :type  'boolean)


(defconst path:uniquify:ignore/buffer:name/rx:defaults
  '(or
    ;;---
    ;; Emacs
    ;;---
    ;; Special buffers start with "*", optionally with leading space.
    (group
     (optional " ")
     "*" ;; literal asterisk
     (one-or-more printing)
     "*")

    ;;---
    ;; Magit Buffers
    ;;---
    (group
     "magit"
     (optional "-"
               (one-or-more alphanumeric))
     ": "
     (one-or-more printing))))


(defcustom path:uniquify:ignore/buffer:name/rx
  (list 'and
        'string-start
        path:uniquify:ignore/buffer:name/rx:defaults
        'string-end)
  "`rx' Regular Expression Sexpr for buffer names that we should _not_ mess with.

Will get `string-start' and `string-end' added to it before being compiled to a
regex string.

Add on to this variable like so:
\(customize-set-variable
 path:uniquify:ignore/buffer:name/rx
 (list 'and
       'string-start
       (list 'or
             ;; The defaults...
             path:uniquify:ignore/buffer:name/rx:defaults
             ;; Or my special buffers.
             '(group
               (optional \" \")
               (or \"§-\" \"§!\" \"ⓘ-\")
               (optional \" \")
               (one-or-more printing)
               (optional \" \")
               (or \"-§\" \"!§\" \"-ⓘ\"))
             buffer:regex:bookend)
       'string-end))

Use function `path:uniquify:ignore/buffer:name/rx' to see if what you created
compiles to a regex string or not."
  :group 'path:group
  :type  'sexp)


(defun path:uniquify:ignore/buffer:name/rx ()
  "Compile variable `path:uniquify:ignore/buffer:name/rx' to a regex string."
  (rx-to-string path:uniquify:ignore/buffer:name/rx :no-group))
;; path:uniquify:ignore/buffer:name/rx
;; (pp path:uniquify:ignore/buffer:name/rx)
;; (path:uniquify:ignore/buffer:name/rx)


(defcustom path:uniquify:ignore/buffer:mode/major nil
  "List of major modes that we should not mess with."
  :group 'path:group
  :type  '(repeat symbol))


(defcustom path:uniquify:directory:modes '(dired-mode cvs-mode vc-dir-mode)
  "List of modes that we should use `list-buffers-directory'.

Some buffers have no variable `buffer-file-name', but do have a
`list-buffers-directory'that describes the directory they represent (e.g.
`Dired' buffers). Use `list-buffers-directory' instead for them."
  :group 'path:group
  :type  '(repeat symbol))


;;------------------------------
;; Constants
;;------------------------------

(defconst path:uniquify:doom-modeline-buffer-file-name-style/valid
  '(path:uniquify:project/truncate-to-file
    path:uniquify:project/truncate-to-unique)
  "Valid `path:uniquify' styles for `doom-modeline-buffer-file-name-style'.")


(defun int<path>:uniquify:doom-modeline/style? ()
  "Are we in charge of styling name for `doom-modeline'?"
  (not (null ; cast to bool
        (and (bound-and-true-p doom-modeline-buffer-file-name-style)
             (memq doom-modeline-buffer-file-name-style
                   path:uniquify:doom-modeline-buffer-file-name-style/valid)))))
;; (int<path>:uniquify:doom-modeline/style?)


;;------------------------------
;; Variables
;;------------------------------

(defvar-local path:uniquify:settings/local nil
  "Non-nil if the name of this buffer is managed by `path:uniquify'.

Value should be an alist-tree of keywords:
  - `:path'    - alist
    - `:parent'   - string: absolute path to parent directory
    - `:filepath' - string: absolute path to file(/directory) itself
    - `:filename' - string: basename of `:filepath' (with extension)
  - `:project' - alist: return value of `path:project:current/alist'
  - `:name'    - alist
    - `:requested'          - string: desired/proposed buffer name from Emacs
    - `:buffer'             - string: actual buffer name decided by us
    - `:buffer/propertized' - string: propertized NAME string")
(put 'path:uniquify:settings/local 'permanent-local t) ; Always a buffer-local variable.
;; (get 'path:uniquify:settings/local 'permanent-local)


(defun int<path>:uniquify:settings/path/normalize (path &rest segments)
  "Normalize PATH and SEGMENTS strings into a path string."
  ;; Always use "abbreviations" in path (i.e. "~" for user's home), and default
  ;; to file path...
  (let ((path (apply #'path:abbreviate:file path segments)))
    ;; Do we actually want this to be a directory path?
    (if (and path:uniquify:directory/end-in-slash?
             ;; Is this, like, really actually a directory? Go find out.
             (file-directory-p path))
        (path:directory path)
      path)))
;; (int<path>:uniquify:settings/path/normalize (path:current:file))
;; (int<path>:uniquify:settings/path/normalize (path:current:dir))
;; (setq path:uniquify:directory/end-in-slash? nil)
;; (setq path:uniquify:directory/end-in-slash? t)


(defun int<path>:uniquify:settings/build (path/absolute/directory
                                           name/requested
                                           buffer)
  "Create and return settings alist from args.
NOTE: Does not set `path:uniquify:settings/local'!

NAME/REQUESTED should be a string of the buffer name that Emacs (via
 `rename-buffer') desires.

PATH/ABSOLUTE/DIRECTORY should be a string of the absolute path to the file's
parent directory."
  (let ((func/name "int<path>:uniquify:settings/create")
        (func/tags '(:settings)))
    (with-current-buffer buffer
      (nub:debug:func/start
          :path:uniquify
          func/name
          func/tags
        (cons 'path/absolute/directory path/absolute/directory)
        (cons 'name/requested          name/requested)
        (cons 'buffer                  buffer))

      ;; Figure out new buffer name.
      (let* ((path/absolute/file (int<path>:uniquify:settings/path/normalize path/absolute/directory
                                                                             name/requested))
             (project            (path:project:current/alist path/absolute/file))
             ;; Assuming we don't have enough info, the name will just be the path.
             (name/buffer/naked       path/absolute/file)
             (name/buffer/propertized path/absolute/file))

        ;;------------------------------
        ;; Name the buffer.
        ;;------------------------------
        ;; If we have a `project', we can figure out our name for the buffer.
        (when project
          (setq name/buffer/naked
                ;; Create the buffer name from the project settings.
                (int<path>:uniquify:name:propertize
                 :buffer         buffer
                 :filepath-abs   path/absolute/file
                 ;; Do not provide any text properteries in these!
                 :project-props  (list (alist-get :project/name project))
                 :filepath-props (list (file:name (alist-get :path project)))
                 :truncated?     t))

          (setq name/buffer/propertized
                ;; Create the buffer name from the project settings.
                (int<path>:uniquify:name:propertize
                 :buffer         buffer
                 :filepath-abs   path/absolute/file
                 ;; Do propertize these as desired.
                 :project-props  (list (alist-get :project/name project)
                                       'face 'underline)
                 :filepath-props (list (file:name (alist-get :path project)))
                 :truncated?     t)))

        ;;------------------------------
        ;; Create our settings (alist tree structure).
        ;;------------------------------
        (nub:debug:func/return
            :path:uniquify
            func/name
            func/tags
          ;; Create & Return
          (list (list :managed?
                      ;; We only create the settings; someone else should set their flag when they set the name.
                      (cons :buffer   nil)  ;; Name in `buffer-name'.
                      (cons :modeline nil)  ;; Name in `mode-line-format' or `doom-modeline'.
                      (cons :title    nil)) ;; Name in `frame-title-format'.
                (list :path
                      (cons :parent    path/absolute/directory)
                      (cons :filepath  path/absolute/file)
                      (cons :filename  (file:name path/absolute/file)))
                (cons :project         project)
                (list :name
                      ;; Name Emacs asked for.
                      (cons :requested name/requested)
                      ;; And the actual `buffer-name'.
                      (cons :buffer             name/buffer/naked)
                      (cons :buffer/propertized name/buffer/propertized)
                      ;; These will be set when created.
                      ;; (cons :modeline                "...")
                      ;; (cons :modeline/propertized    "...")
                      )))))))
;; path:uniquify:settings/local
;; (int<path>:uniquify:settings/build (path:parent (path:current:file)) (file:name (path:current:file)) (current-buffer))


(defun int<path>:uniquify:settings/create (path/absolute/directory
                                           name/requested
                                           buffer)
  "Create settings alist from args.

NAME/REQUESTED should be a string of the buffer name that Emacs (via
 `rename-buffer') desires.

PATH/ABSOLUTE/DIRECTORY should be a string of the absolute path to the file's
parent directory."
  (with-current-buffer buffer
    (setq path:uniquify:settings/local
          (int<path>:uniquify:settings/build path/absolute/directory
                                             name/requested
                                             buffer))))
;; (int<path>:uniquify:settings/create (path:parent (path:current:file)) (file:name (path:current:file)) (current-buffer))
;; path:uniquify:settings/local


(defun int<path>:uniquify:settings/clear (buffer)
  "Clear `path:uniquify:settings/local' by setting it to nil.

BUFFER should be a buffer object."
  (let ((func/name "int<path>:uniquify:settings/clear")
        (func/tags '(:settings))
        (special? (string-match-p
                   ;; Special buffers start with "*", optionally with leading space.
                   (rx (optional " ")
                       "*" ;; literal asterisk
                       (one-or-more printing)
                       "*")
                   (buffer-name buffer))))
    (with-current-buffer buffer
      (unless special?
        (nub:debug
            :path:uniquify
            func/name
            func/tags
          "before clear: %S"
          path:uniquify:settings/local))

      (setq path:uniquify:settings/local nil)

      (unless special?
        (nub:debug
            :path:uniquify
            func/name
            func/tags
          "after clear: %S"
          path:uniquify:settings/local)))))


(defun int<path>:uniquify:settings/get (keywords settings)
  "Get KEYWORDS value from the SETTINGS alist tree.

KEYWORDS should be:
  - a keyword: `:project'
  - a list of keywords: `(:path :filepath)'

SETTINGS should be `path:uniquify:settings/local' or a sub-alist thereof."
  (let ((func/name "int<path>:uniquify:settings/get")
        (func/tags '(:settings)))
    (nub:debug:func/start
        :path:uniquify
        func/name
        func/tags
      (cons 'keywords keywords)
      (cons 'settings settings))

    (cond ((null keywords)
           (nub:error
               :path:uniquify
               func/name
             '("Cannot get value in settings; no keyword? "
               "keywords: %S, buffer: %S, settings: %S")
             keywords
             buffer
             settings))

          ;; Actually get the value.
          ((keywordp keywords)
           (nub:debug:func/return
               :path:uniquify
               func/name
               func/tags
             ;; Return value.
             (alist-get keywords settings)))

          ((and (listp keywords)
                (= 1 (length keywords)))
           (nub:debug:func/return
               :path:uniquify
               func/name
               func/tags
             ;; Return value.
             (alist-get (car keywords) settings)))

          ;; Recurse into settings a level.
          ((listp keywords)
           (let ((keyword (pop keywords)))
             (int<path>:uniquify:settings/get keywords
                                              (alist-get keyword settings))))

          ;; ???
          (t
           (nub:error
               :path:uniquify
               func/name
             '("Cannot get value in settings; don't know what to do with keywords! "
               "keywords: %S, settings: %S")
             keywords
             settings)))))


(defun path:uniquify:settings/get (keywords buffer)
  "Get KEYWORDS value from the local variable `path:uniquify:settings/local'.

BUFFER should be a buffer object.

KEYWORDS should be:
  - a keyword: `:project'
  - a list of keywords: `(:path :filepath)'"
  (with-current-buffer buffer
    (int<path>:uniquify:settings/get keywords path:uniquify:settings/local)))
;; path:uniquify:settings/local
;; (path:uniquify:settings/get '(:path :parent) (current-buffer))


(defun int<path>:uniquify:settings/set (keywords value settings)
  "Set KEYWORDS to VALUE in the local variable `path:uniquify:settings/local'.

KEYWORDS should be:
  - a keyword: `:project'
  - a list of keywords: `(:path :filepath)'

SETTINGS should be `path:uniquify:settings/local' or a sub-alist.

NOTE: Must be call in context of buffer that owns SETTINGS."
  (let ((func/name "int<path>:uniquify:settings/set"))
    (cond ((null keywords)
           (nub:error
               :path:uniquify
               func/name
             '("Cannot set value in settings; no keyword? "
               "keywords: %S, value: %S, settings: %S")
             keywords
             value
             settings))

          ;; Actually set the value.
          ((keywordp keywords)
           ;; Return this level of settings.
           (setf (alist-get keywords settings)
                 value))

          ((and (listp keywords)
                (= 1 (length keywords)))
           (setf (alist-get (car keywords) settings)
                 value))

          ;; Recurse into settings a level.
          ((listp keywords)
           (let ((keyword (pop keywords)))
             (setf (alist-get keyword settings)
                   ;; Recurse into w/ rest of keywords to update/set value.
                   (int<path>:uniquify:settings/set keywords
                                                    value
                                                    (alist-get keyword settings)))))

          ;; ???
          (t
           (nub:error
               :path:uniquify
               func/name
             '("Cannot set value in settings; don't know what to do with keywords! "
               "keywords: %S, value: %S, settings: %S")
             keywords
             value
             settings)))

    ;; Return this level of settings.
    settings))
;; path:uniquify:settings/local


(defun path:uniquify:settings/set (keywords value buffer)
  "Set KEYWORDS to VALUE in the local variable `path:uniquify:settings/local'.

BUFFER should be a buffer object.

KEYWORDS should be:
  - a keyword: `:project'
  - a list of keywords: `(:path :filepath)'"
  (with-current-buffer buffer
    (int<path>:uniquify:settings/set keywords value path:uniquify:settings/local)))
;; (int<path>:uniquify:settings/create (path:parent (path:current:file)) (file:name (path:current:file)) (current-buffer))
;; (pp path:uniquify:settings/local)
;; (path:uniquify:settings/get '(:path :filename) (current-buffer))
;; (path:uniquify:settings/set '(:path :filename) "jeff" (current-buffer))
;; (path:uniquify:settings/set '(:managed? :buffer) t (current-buffer))


(defun int<path>:uniquify:settings/set:name/buffer (buffer)
  "Update BUFFER's name with data from `path:uniquify:settings/local'.

BUFFER should be a buffer object.
`path:uniquify:settings/local' should be up-to-date."
  (with-current-buffer buffer
    (let ((func/name "int<path>:uniquify:settings/set:name/buffer")
          (func/tags '(:settings))
          (name (path:uniquify:settings/get '(:name :buffer) buffer)))
      ;;------------------------------
      ;; Error Checks
      ;;------------------------------
      (cond ((null name)
             ;; Error for now so we can work on hunting this down.
             ;; Ideally no erroring? Maybe?
             (nub:error
                 :path:uniquify
                 func/name
               '(:line:each
                 "Cannot name this buffer; no name in settings?!"
                 "`%s': %S")
               (symbol-name 'path:uniquify:settings/local)
               path:uniquify:settings/local))
            ((not (stringp name))
             (nub:error
                 :path:uniquify
                 func/name
               '(:line:each
                 "Cannot name this buffer; no name in settings?!"
                 "`%s': %S")
               (symbol-name 'path:uniquify:settings/local)
               path:uniquify:settings/local))
            ;;------------------------------
            ;; Buffer Naming (Or Not)
            ;;------------------------------
            ((string= name (buffer-name buffer))
             ;; Nothing to do; name already set?
             nil)
            (t
             (nub:debug
                 :path:uniquify
                 func/name
                 func/tags
               "Set buffer name via `rename-buffer'.")
             ;; Use non-nil UNIQUE arg in order to avoid infinite loop recursion
             ;; due to our advising of `rename-buffer'.
             (rename-buffer name :path:uniquify)
             ;; Mark buffer's name as managed.
             (int<path>:uniquify:buffer/manage buffer :buffer t))))))
;; (int<path>:uniquify:settings/set:name/buffer (current-buffer))


(defun int<path>:uniquify:settings/set:name/modeline (buffer path/unique truncated?)
  "Set BUFFER's modeline name and mark BUFFER as managed for modeline.

Use PATH/UNIQUE for the path & filename part of BUFFER's name.

If TRUNCATED? is non-nil, add truncated string `path:name:truncate' before
PATH/UNIQUE."
  (let ((func/name "int<path>:uniquify:settings/set:name/modeline")
        (func/tags '(:settings :modeline)))
    (with-current-buffer buffer
      (nub:debug:func/start
          :path:uniquify
          func/name
          func/tags
        (cons 'buffer      buffer)
        (cons 'path/unique path/unique)
        (cons 'truncated?  truncated?)
        (cons 'path:uniquify:settings/local path:uniquify:settings/local))

      ;;------------------------------
      ;; Error Checks
      ;;------------------------------
      ;; Settings should already be created.
      (unless path:uniquify:settings/local
        (nub:error
            :path:uniquify
            func/name
          "Buffer has no `path:uniquify:settings/local'! Cannot set modeline name."))

      ;;------------------------------
      ;; Set the modeline settings.
      ;;------------------------------
      (let ((project/alist (path:uniquify:settings/get :project buffer))
            (filepath      (path:uniquify:settings/get '(:path :filepath) buffer)))
        ;; No func or anything to set the modeline name; just save to settings for
        ;; `doom-modeline' or whoever to use.

        ;; No properties supplied.
        (path:uniquify:settings/set
         '(:name :modeline)
         (int<path>:uniquify:name:propertize :buffer         buffer
                                             :filepath-abs   filepath
                                             :project-props  (list (alist-get :project/name project/alist))
                                             :filepath-props (list path/unique)
                                             :truncated?     truncated?
                                             :modeline?      nil) ;; Don't add modeline properties.
         buffer)

        ;; Yes properties supplied.
        (path:uniquify:settings/set
         '(:name :modeline/propertized)
         (int<path>:uniquify:name:propertize :buffer         buffer
                                             :filepath-abs   filepath
                                             :project-props  (list (alist-get :project/name project/alist)
                                                                   'face 'underline)
                                             :filepath-props (list path/unique)
                                             :truncated?     truncated?
                                             :modeline?      t) ;; Do add modeline properties.
         buffer)

        ;; Mark modeline's name as managed.
        (int<path>:uniquify:buffer/manage buffer :modeline t)

        (nub:debug:func/end
            :path:uniquify
            func/name
            func/tags
          (cons 'path:uniquify:settings/local path:uniquify:settings/local))))))
;; path:uniquify:settings/local
;; (int<path>:uniquify:settings/set:name/modeline (current-buffer) "+uniquify.el" t)


;;--------------------------------------------------------------------------------
;; Buffers
;;--------------------------------------------------------------------------------

(defun path:uniquify:buffer/managed? (buffer type)
  "Is the TYPE name of BUFFER already managed by `path:uniquify'?

TYPE should be:
  - `:buffer'
  - `:modeline'
  - `:title'
  - `:any'

Return nil/non-nil."
  (with-current-buffer buffer
    ;; If our local var is non-nil, we are probably managing something about this buffer...
    (and path:uniquify:settings/local
         ;; TEMP! Check if new `:managed?' is in settings.
         (or (null (path:uniquify:settings/get '(:managed?) buffer))
             ;; Check if TYPE is being managed in this buffer.
             (if (eq type :any)
                 (or (path:uniquify:settings/get '(:managed? :buffer)   buffer)
                     (path:uniquify:settings/get '(:managed? :modeline) buffer)
                     (path:uniquify:settings/get '(:managed? :title)    buffer))
               (path:uniquify:settings/get (list :managed? type) buffer))))))
;; (path:uniquify:buffer/managed? (current-buffer) :buffer)
;; (path:uniquify:buffer/managed? (current-buffer) :modeline)
;; (path:uniquify:buffer/managed? (current-buffer) :title)
;; (path:uniquify:buffer/managed? (current-buffer) :any)


(defun int<path>:uniquify:buffer/manage (buffer type managed?)
  "Update BUFFER's `:managed?' settings for TYPE to MANAGED?."
  (path:uniquify:settings/set (list :managed? type)
                              ;; cast to bool
                              (not (null managed?))
                              buffer))
;; path:uniquify:settings/local
;; (int<path>:uniquify:buffer/manage (current-buffer) :modeline t)
;; (int<path>:uniquify:buffer/manage (current-buffer) :modeline nil)
;; (int<path>:uniquify:buffer/manage (current-buffer) :title t)
;; (int<path>:uniquify:buffer/manage (current-buffer) :title nil)


(defun int<path>:uniquify:buffer/should-manage? (buffer filepath)
  "Should we be managing BUFFER's name?

BUFFER should be a buffer object.
FILEPATH should be the absolute path to the file.

Will check settings:
  - `path:uniquify:ignore/buffer:name/rx'
  - `path:uniquify:ignore/buffer:mode/major'"
  ;;------------------------------
  ;; Unable to manage this buffer?
  ;;------------------------------
  ;; Want to short-circuit out for nil filepaths & projects.

  ;; Nothing we can do for non-file buffers.
  (cond ((not filepath) ; nil -> should not manage
         nil)

        ;; No project? Can't figure out a name given current project-based naming schemes.
        ((not (path:project:current/alist filepath)) ; nil -> should not manage
         nil)

        ;;------------------------------
        ;; Ignore this buffer?
        ;;------------------------------
        ;; Our settings are of the "ignore this?" variety, so figure out "should we
        ;; ignore this?", then invert for "should we manage this?".
        ((not (or
                ;; Should we ignore due to buffer name regexes?
                (and path:uniquify:ignore/buffer:name/rx
                     (string-match (path:uniquify:ignore/buffer:name/rx) (buffer-name buffer)))
                ;; Should we ignore due to buffer's mode?
                (memq major-mode path:uniquify:ignore/buffer:mode/major))))))
;; (int<path>:uniquify:buffer/should-manage? (current-buffer) (path:current:file))


;;--------------------------------------------------------------------------------
;; Buffers - Uniquify Management
;;--------------------------------------------------------------------------------

(defun int<path>:uniquify:modeline:doom-modeline (buffer filepath)
  "Set BUFFER modeline name based on `doom-modeline-buffer-file-name-style'.

FILEPATH should be absolute path to BUFFER's file.

Check `doom-modeline-buffer-file-name-style' for custom styles. If not a custom
style, set settings to \"modeline not managed\". If a known custom style, set
modeline to managed and figure out a name for the buffer.

Given:
  1. A project root of `~/Projects/FOSS/emacs/'.
  2. Another buffer open to `~/Projects/FOSS/emacs/lisp/comint-hacks/comint.el'.
  3. \"This\" buffer open to `~/Projects/FOSS/emacs/lisp/comint/comint.el'.

Custom styles for `doom-modeline-buffer-file-name-style' are:
  `path:uniquify:project/truncate-to-file'   => emacs:/…/comint.el
  `path:uniquify:project/truncate-to-unique' => emacs:/…/comint/comint.el

In order for this to actually do anything:
  1. `doom-modeline' must be in use,
  2. `doom-modeline-buffer-file-name' must be advised.
      example:
      (advice-add 'doom-modeline-buffer-file-name
                  :around
                  #'path:advice:uniquify:doom-modeline-buffer-file-name)
  3. `doom-modeline-buffer-file-name-style' must be one of our custom styles.
     example:
     (setq doom-modeline-buffer-file-name-style
           'path:uniquify:project/truncate-to-unique)"
  (let ((func/name "int<path>:uniquify:modeline:doom-modeline")
        (func/tags '(:settings :modeline :doom-modeline)))
    (with-current-buffer buffer
      (nub:debug:func/start
          :path:uniquify
          func/name
          func/tags
        (cons 'buffer   buffer)
        (cons 'filepath filepath)
        (cons 'path:uniquify:settings/local         path:uniquify:settings/local)
        (cons 'doom-modeline-buffer-file-name-style doom-modeline-buffer-file-name-style))

      ;; Only do something if `doom-modeline' exists and we should be styling it.
      (when (int<path>:uniquify:doom-modeline/style?)
        ;;------------------------------
        ;; Error Checks
        ;;------------------------------
        ;; Settings should already be created.
        (unless path:uniquify:settings/local
          (nub:error
              :path:uniquify
              func/name
            "Buffer has no `path:uniquify:settings/local'! Cannot set modeline name."))

        ;;------------------------------
        ;; Set Buffer's Name for Modeline
        ;;------------------------------
        (pcase doom-modeline-buffer-file-name-style
          ;;------------------------------
          ;; Custom Style?
          ;;------------------------------
          ('path:uniquify:project/truncate-to-file
           ;; Name this buffer; no need to uniquify.
           (int<path>:uniquify:settings/set:name/modeline
            buffer
            (file:name filepath)
            t))

          ('path:uniquify:project/truncate-to-unique
           ;; First name it, assuming it's already unique.
           (nub:debug
               :path:uniquify
               func/name
               func/tags
             "truncate to unique")

           (int<path>:uniquify:settings/set:name/modeline buffer
                                                          (file:name filepath)
                                                          t)

           (nub:debug
               :path:uniquify
               func/name
               func/tags
             "modeline name set to: %S"
             (path:uniquify:settings/get '(:name :modeline) buffer))

           ;; Now we can uniquify all buffers.
           (int<path>:uniquify:buffers/refresh :modeline)

           (nub:debug
               :path:uniquify
               func/name
               func/tags
             "modeline name refreshed to: %S"
             (path:uniquify:settings/get '(:name :modeline) buffer)))

          ;;------------------------------
          ;; Standard Style?
          ;;------------------------------
          ;; Otherwise dunno what that style is.
          (_
           ;; Unset `(:managed? :modeline)' flag if set.
           (when (path:uniquify:buffer/managed? buffer :modeline)
             (int<path>:uniquify:buffer/manage buffer :modeline nil))))

        (nub:debug:func/end
            :path:uniquify
            func/name
            func/tags
          (cons 'path:uniquify:settings/local path:uniquify:settings/local))))))
;; path:uniquify:settings/local
;; (int<path>:uniquify:modeline:doom-modeline (current-buffer))


(defun int<path>:uniquify:buffers/refresh (type)
  "Refresh all buffer name TYPEs managed by `path:uniquify'.

TYPE should be:
  - `:buffer'
  - `:modeline'
  - `:title'
  - `:any'

Update any that need extra uniquification in e.g. modeline buffer name."
  (let ((func/name "int<path>:uniquify:buffers/refresh")
        (func/tags '(:buffer :refresh))
        (type/valid '(:buffer :modeline :title :any))
        ;; managed/all is an alist-tree:
        ;;   '(("project-dir-name" . (("filename.el" (<buffer> <buffer>)))))
        managed/all)
    (nub:debug:func/start
        :path:uniquify
        func/name
        func/tags
      (cons 'type type))

    ;;------------------------------
    ;; Error Checks
    ;;------------------------------
    (unless (memq type type/valid)
      (nub:error
          :path:uniquify
          func/name
        "Unknown TYPE! Got `%S', expected one of: %S"
        type
        type/valid))

    ;;------------------------------
    ;; Look For Naming Conflicts
    ;;------------------------------
    ;; Build alist-tree of managed buffers ("project-dir-name" -> "filename.el" -> buffers).
    (dolist (buffer (buffer-list))
      (when (path:uniquify:buffer/managed? buffer type)
        ;; Add this buffer to the alist.
        (push buffer (alist-get (path:uniquify:settings/get '(:path :filename) buffer)
                                (alist-get (path:uniquify:settings/get '(:project :project/name) buffer)
                                           managed/all
                                           nil nil #'string=)
                                nil nil #'string=))))

    (nub:debug
        :path:uniquify
        func/name
        func/tags
      "managed/all: %S"
      managed/all)

    (dolist (managed/project managed/all)
      (let ((managed/project/name (car managed/project))
            (managed/project/files (cdr managed/project)))
        (dolist (managed/file managed/project/files)
          (let ((managed/file/name (car managed/file))
                (managed/file/buffers (cdr managed/file)))
            ;;------------------------------
            ;; Resolve Conflicts
            ;;------------------------------
            (when (> (length managed/file/buffers) 1)
              (nub:debug
                  :path:uniquify
                  func/name
                  func/tags
                "name conflicts exist; uniquify...")

              ;; Found some buffers that have the same project & filename.
              ;; Need to uniquify their display name for the TYPE.

              ;; Get the common path of all these buffers. Unique bit will be "the rest".
              (let ((path/common (apply #'path:ancestor/common
                                        ;; Need list of buffer's filepaths from the list of buffers.
                                        (seq-map (lambda (buffer) (path:uniquify:settings/get '(:path :filepath) buffer)) managed/file/buffers))))
                (nub:debug
                    :path:uniquify
                    func/name
                    func/tags
                  "path/common: %S"
                  path/common)

                (dolist (buffer managed/file/buffers)
                  (with-current-buffer buffer
                    ;; Uniquify by removing common parts of path.
                    (let ((path/unique (path:relative (path:uniquify:settings/get '(:path :filepath) buffer)
                                                      path/common)))
                      (nub:debug
                          :path:uniquify
                          func/name
                          func/tags
                        '(:line:each
                          "buffer:      %S"
                          "path/unique: %S")
                        buffer
                        path/common)

                      ;; Set whatever TYPE says to.
                      (pcase type
                        (:modeline
                         ;; NOTE: No display update needed; modeline will pick up new name when it redisplays.
                         (int<path>:uniquify:settings/set:name/modeline
                          buffer
                          path/unique
                          ;; Truncated if not equal to its relative-from-project-root path.
                          (not (path:equal? path/unique
                                            (path:uniquify:settings/get '(:project :path) buffer)))))

                        (:buffer
                         ;; TODO: `:buffer' uniquity.
                         ;; TODO: Force update of anything in order to get new name displayed?
                         (nub:error
                             :path:uniquify
                             func/name
                           "TYPE `%S' not implemented yet..."
                           type))

                        (:title
                         ;; TODO: `:title' uniquity.
                         ;; TODO: Force update of anything in order to get new name displayed?
                         (nub:error
                             :path:uniquify
                             func/name
                           "TYPE `%S' not implemented yet..."
                           type))

                        (:any
                         ;; Do all three.
                         ;; TODO: `:buffer' uniquity.
                         ;; TODO: `:title' uniquity.
                         (nub:error
                             :path:uniquify
                             func/name
                           "TYPE `%S' not implemented yet..."
                           type)

                         ;; `:modeline'
                         (int<path>:uniquify:settings/set:name/modeline
                          buffer
                          path/unique
                          (not
                           (path:equal? path/unique
                                        (path:uniquify:settings/get '(:project :path) buffer)))))

                        (_
                         (nub:error
                             :path:uniquify
                             func/name
                           "TYPE `%S' not understood?"
                           type))))))))))))

    (nub:debug:func/end
        :path:uniquify
        func/name
        func/tags
      ;; TODO:nub: Make `nub:debug:func/end' not print "  <--[RETURN]--" when we don't supply anything to it here.
      )))
;; (int<path>:uniquify:settings/set:name/modeline (current-buffer) "a/+uniquify.el" t)
;; (int<path>:uniquify:settings/set:name/modeline (get-buffer "+uniquify.el") "b/+uniquify.el" t)
;; (int<path>:uniquify:buffers/refresh :modeline)


(cl-defun int<path>:uniquify:name:propertize (&key buffer
                                                   filepath-abs
                                                   project-props
                                                   filepath-props
                                                   truncated?
                                                   modeline?)
  "Return a buffer name string based off of params.

Keyword Parameters:
  - BUFFER - Buffer object, buffer name string, or nil for `current-buffer'.
  - conses - (string . properties):
    - PROJECT-PROPS  - project name and properties
                     - e.g. (list \".emacs.d\" 'face 'underline)
    - FILEPATH-PROPS - relative path from PROJECT and properties
                     - e.g. (list \"path/to/file.el\")
  - FILEPATH-ABS - absolute path to file
                 - like `buffer-file-truename' which can't always be used here
  - Flags / Options:
    - TRUNCATED? - nil     - FILEPATH is relative to PROJECT dir.
                 - non-nil - FILEPATH has been truncated.
    - MODELINE?  - nil     - Do nothing.
                 - non-nil - Add modeline properties to returned name string."
  (let ((func/name "int<path>:uniquify:name:propertize")
        (func/tags '(:settings))
        (buffer (if buffer
                    (get-buffer buffer)
                  (current-buffer)))
        name)
    (nub:debug:func/start
        :path:uniquify
        func/name
        func/tags
      (cons 'buffer                       buffer)
      (cons 'filepath-abs                 filepath-abs)
      (cons 'project-props                project-props)
      (cons 'filepath-props               filepath-props)
      (cons 'truncated?                   truncated?)
      (cons 'modeline?                    modeline?))

    (with-current-buffer buffer
      ;;------------------------------
      ;; Propertize Buffer Name
      ;;------------------------------
      ;; Project
      (setq name (concat (apply #'str:propertize (car project-props) (cdr project-props))
                         path:vc/git:rooted
                         ;; Truncation Indicator?
                         (when truncated?
                           (path:dir path:name:truncate))
                         ;; Filepath
                         (apply #'str:propertize
                                (car filepath-props)
                                (cdr filepath-props))))

      ;;------------------------------
      ;; Propertize for Modeline?
      ;;------------------------------
      (nub:debug:func/return
          :path:uniquify
          func/name
          func/tags
        ;; Returned value:
        (if modeline?
            (let ((filepath (path:abbreviate:file filepath-abs)))
              ;; Shouldn't overwrite any propertize, as these are non-dispaly properties for modeline help.
              (apply #'str:propertize
                     name
                     ;; Smush list of property pair lists into just a list of properties.
                     (flatten-list
                      ;; Keep all non-nil property pairs.
                      (seq-filter (lambda (x) (not (null x)))
                                  (list
                                   ;; (Required): Add the usual modeline extras.
                                   (list 'mouse-face 'mode-line-highlight)
                                   (list 'help-echo (concat filepath
                                                            (unless (string= (file-name-nondirectory filepath)
                                                                             (buffer-name buffer))
                                                              (concat "\n" (buffer-name buffer)))
                                                            "\nmouse-1: Previous buffer\nmouse-3: Next buffer"))
                                   (list 'local-map mode-line-buffer-identification-keymap)

                                   ;; (Optional): Add stuff that would be responsibility of `doom-modeline-buffer-file-name'?
                                   (when (int<path>:uniquify:doom-modeline/style?)
                                     (list 'face 'doom-modeline-buffer-file)))))))

          ;; No modeline properties.
          name)))))
;; (int<path>:uniquify:name:propertize :filepath-abs "/projects/hello/path/to/file.txt" :project-props '("hello" face underline) :filepath-props '("path/to/file.txt"))
;; (int<path>:uniquify:name:propertize :filepath-abs "/projects/hello/path/to/file.txt" :project-props '("hello" face underline) :filepath-props '(     "to/file.txt") :truncated? t)
;; (int<path>:uniquify:name:propertize :filepath-abs "/projects/hello/path/to/file.txt" :project-props '("hello" face underline) :filepath-props '("path/to/file.txt") :modeline? t)


;;--------------------------------------------------------------------------------
;; Commands
;;--------------------------------------------------------------------------------

(defun path:cmd:uniquify:buffer/test ()
  "Find out what we would name the current buffer if we were naming buffers."
  (interactive)
  (let* ((buffer (current-buffer))
         (name (buffer-name buffer)))
    ;; Say what we're already doing:
    (cond ((path:uniquify:buffer/managed? buffer :any)
           (message "Buffer is managed; name: %S"
                    (path:uniquify:settings/get '(:name :buffer) buffer)))

          ;; Say why we wouldn't do anything.
          ((not (int<path>:uniquify:buffer/should-manage? buffer
                                                          (buffer-file-name (buffer-base-buffer buffer))))
           ;; Ignored due to buffer name regexes?
           (cond ((and path:uniquify:ignore/buffer:name/rx
                       (string-match (path:uniquify:ignore/buffer:name/rx) name))
                  (message "Ignore buffer due to name regex: %S"
                           name))
                 ;; Ignored due to buffer's mode?
                 ((memq major-mode path:uniquify:ignore/buffer:mode/major)
                  (message "Ignore buffer due to name regex: %S"
                           name))
                 ;; ???
                 (t
                  (message "Ignore buffer due to... unknown circumstances???: %S"
                           name))))

          ;; Say what we would do if we were doing things.
          (t
           (let ((settings (int<path>:uniquify:settings/create (path:parent (path:current:file))
                                                               (file:name (path:current:file)))))
             (message "Would name buffer: %S"
                      (int<path>:uniquify:settings/get '(:name :buffer) settings))))))
  nil)
;; (path:cmd:uniquify:buffer/test)


;;--------------------------------------------------------------------------------
;; Advice
;;--------------------------------------------------------------------------------

;;------------------------------
;; Special 'desktop.el' Hand-Holding
;;------------------------------

(defun path:uniquify:get:name/requested ()
  "Return `path:uniquify' saved `:name :requested' setting for this buffer.

Indended as alias for `uniquify-buffer-base-name'."
  ;; Return something if we have something, nil otherwise.
  (and (path:uniquify:buffer/managed? (current-buffer) :any)
       (path:uniquify:settings/get '(:name :requested) (current-buffer))))


(defun path:advice:uniquify:uniquify-buffer-base-name (func &rest args)
  "Return `path:uniquify' saved `:name :requested' setting for this buffer.

Indended as `:around' advice for FUNC `rename-buffer'. Prefer returning
`uniquify-buffer-base-name' result; return ours if they have nothing.

ARGS are just for future-proofing call to `uniquify-buffer-base-name'."
  (if-let ((name/base (apply func args)))
      name/base
    (path:uniquify:get:name/requested)))


;;------------------------------
;; Buffer (Re)Naming
;;------------------------------

(defun path:advice:uniquify:rename-buffer (func name/requested &optional unique? &rest args)
  "Uniquify file-backed buffer names with parts their path.

Indended as `:around' advice for FUNC `rename-buffer'.

NAME/REQUESTED should be a string of the buffer's intended new name.
UNIQUE should be nil/non-nil.
ARGS are just for future-proofing call to `rename-buffer'.

Return a string of the name actually given to the buffer."
  (let* ((func/name "path:advice:uniquify:rename-buffer")
         (func/tags '(:advice))
         ;; Find out what Emacs wants to call this buffer.
         (name/proposed (apply func name/requested unique? args))
         (buffer (current-buffer))
         ;; TODO: Use `buffer:special?'? I don't want to rely on too many
         ;; things, but `:buffer' is available to `:path'...
         (special? (string-match-p
                    ;; Special buffers start with "*", optionally with leading space.
                    (rx (optional " ")
                        "*" ;; literal asterisk
                        (one-or-more printing)
                        "*")
                    (buffer-name buffer)))
         name/actual)
    (with-current-buffer buffer
      ;; Don't do the debug message for special buffers.
      (unless special?
        (nub:debug:func/start
            :path:uniquify
            func/name
            func/tags
          ;; (cons 'func           func)
          (cons 'name/requested name/requested)
          (cons 'unique?        unique?)
          (cons 'args           args)
          (cons '-----          "-----")
          (cons 'name/proposed  name/proposed)
          (cons 'buffer         buffer)
          (cons 'path:uniquify:settings/local path:uniquify:settings/local)))

      ;;------------------------------
      ;; Do we do anything or..?
      ;;------------------------------
      (cond
       ;;------------------------------
       ;; Do Not Uniquify
       ;;------------------------------
       (unique?
        (unless special?
          (nub:debug
              :path:uniquify
              func/name
              func/tags
            "Do nothing; leave settings alone.")

          (nub:debug
              :path:uniquify
              func/name
              func/tags
            "path:uniquify:settings/local: %S"
            path:uniquify:settings/local))

        ;; Just ignore the buffer rename this time; could be we already
        ;; renamed it in e.g. `path:advice:uniquify:create-file-buffer'.
        (setq name/actual name/proposed)

        (unless special?
          (nub:debug
              :path:uniquify
              func/name
              func/tags
            "name (proposed & actual): %S"
            name/proposed)))

       ;; NOTE: `path:current:file' is ok here since this is a buffer rename and
       ;; the buffer definitely already exists. Just don't let it error out; it
       ;; errors when it can't find a file name and that happens (e.g. special
       ;; buffers).
       ((not (int<path>:uniquify:buffer/should-manage? buffer (path:current:file :no-error)))
        (unless special?
          (nub:debug
              :path:uniquify
              func/name
              func/tags
            "Do nothing & clear settings!")

          (nub:debug
              :path:uniquify
              func/name
              func/tags
            "path:uniquify:settings/local: %S"
            path:uniquify:settings/local))

        ;; Don't mess with it; mark this buffer as not-named-by-us and leave its name alone.
        (int<path>:uniquify:settings/clear buffer)
        (setq name/actual name/proposed)

        (unless special?
          (nub:debug
              :path:uniquify
              func/name
              func/tags
            "name (proposed & actual): %S"
            name/proposed)))

       ;;------------------------------
       ;; Uniquify Buffer Name
       ;;------------------------------
       (t
        (nub:debug
            :path:uniquify
            func/name
            func/tags
          "Uniquify buffer name!")

        ;; Clear any old settings.
        (int<path>:uniquify:settings/clear buffer)

        ;; Figure (& save) out /our/ name for this buffer.
        ;;---
        ;; NOTE: `path:current:file' is ok here since this is a buffer rename
        ;; and the buffer definitely already exists.
        ;;---
        ;; NOTE: If `path:current:file' is erroring here due to no file path,
        ;; then _probably_ it's a buffer that /should/ be caught by
        ;; `int<path>:uniquify:buffer/should-manage?' in the `cond` case above
        ;; and /should not/ get into this `cond` case.
        (int<path>:uniquify:settings/create (path:parent (path:current:file))
                                            name/proposed
                                            buffer)

        ;; Actually set the buffer's new name.
        ;; NOTE: This "recurses" back into `rename-buffer', which will call this
        ;; advice, which we ignore because UNIQUE? is non-nil.
        (int<path>:uniquify:settings/set:name/buffer buffer)
        (setq name/actual (buffer-name buffer))

        (nub:debug
            :path:uniquify
            func/name
            func/tags
          "path:uniquify:settings/local: %S"
          path:uniquify:settings/local)
        (nub:debug
            :path:uniquify
            func/name
            func/tags
          "settings->filepath: %S"
          (path:uniquify:settings/get '(:path :filepath) buffer))

        ;; Should we be doing anything else for this buffer?
        ;; Manage its modeline name separate from buffer name maybe?
        (int<path>:uniquify:modeline:doom-modeline
         buffer
         (path:uniquify:settings/get '(:path :filepath) buffer))))

      ;;------------------------------
      ;; Return Name
      ;;------------------------------
      ;; Ignore debugging for special buffers.
      (if special?
          name/actual
        (nub:debug:func/return
            :path:uniquify
            func/name
            func/tags
          ;; Returned value:
          name/actual)))))


;;------------------------------
;; Buffer / File Creation
;;------------------------------

(defun path:advice:uniquify:create-file-buffer (func filepath &rest args)
  "Uniquify buffer names with parts of directory name.

Intended as `:around' advice for FUNC `create-file-buffer'.

FILEPATH should be the path to the file being created.
ARGS are just for future-proofing call to `create-file-buffer'.

Return the buffer created by `create-file-buffer'."
  (let ((func/name "path:advice:uniquify:create-file-buffer")
        (func/tags '(:advice))
        ;; Get the actual buffer from `create-file-buffer'...
        (buffer (apply func filepath args)))
    (with-current-buffer buffer
      (nub:debug:func/start
          :path:uniquify
          func/name
          func/tags
        (cons 'func     func)
        (cons 'filepath filepath)
        (cons 'args     args)
        (cons '-----    "-----")
        (cons 'buffer   buffer)
        ;; Should be nil right now?
        (cons 'path:uniquify:settings/local path:uniquify:settings/local))

      (if (int<path>:uniquify:buffer/should-manage? buffer filepath)
          ;;------------------------------
          ;; Tweak Buffer Name
          ;;------------------------------
          (progn
            ;; Figure out a buffer name.
            (int<path>:uniquify:settings/create (path:parent filepath)
                                                (file:name filepath)
                                                buffer)

            ;; Actually set the buffer's new name (& return it).
            ;; TODO: Do we want `rename-buffer' to get involved here?
            (int<path>:uniquify:settings/set:name/buffer buffer)

            ;; Should we be doing anything else for this buffer?
            ;; Manage its modeline name separate from buffer name maybe?
            (int<path>:uniquify:modeline:doom-modeline
             buffer
             (path:uniquify:settings/get '(:path :filepath) buffer)))

        ;;------------------------------
        ;; Ignore This Buffer
        ;;------------------------------
        ;; Don't mess  with it; mark this buffer as not-named-by-us and leave its name alone.
        (int<path>:uniquify:settings/clear buffer))

      ;;------------------------------
      ;; Return Buffer Object
      ;;------------------------------
      (nub:debug:func/end
          :path:uniquify
          func/name
          func/tags
        (cons 'buffer buffer)
        (cons 'path:uniquify:settings/local path:uniquify:settings/local))

      buffer)))


;;------------------------------
;; Modeline
;;------------------------------

(defun path:advice:uniquify:doom-modeline-buffer-file-name (func &rest args)
  "Create advice for package `doom-modeline' so it can this style buffer name.

Propertize file name based on `doom-modeline-buffer-file-name-style'.

Check `doom-modeline-buffer-file-name-style' for custom styles; pass on to
original function if not custom.

Given:
  1. A project root of `~/Projects/FOSS/emacs/'.
  2. Another buffer open to `~/Projects/FOSS/emacs/lisp/comint-hacks/comint.el'.
  3. \"This\" buffer open to `~/Projects/FOSS/emacs/lisp/comint/comint.el'.

Custom styles for `doom-modeline-buffer-file-name-style' are:
  `path:uniquify:project/truncate-to-file'   => emacs:/…/comint.el
  `path:uniquify:project/truncate-to-unique' => emacs:/…/comint/comint.el

FUNC & ARGS are for advice type `:around'. Will call
`doom-modeine-buffer-file-name' for any `doom-modeline-buffer-file-name-style'
not recognized as `path:uniquify' custom styles."
  (let ((func/name "path:advice:uniquify:doom-modeline-buffer-file-name")
        (func/tags '(:advice))
        (buffer (current-buffer)))
    (nub:debug:func/start
        :path:uniquify
        func/name
        func/tags
      (cons 'func     func)
      (cons 'args     args)
      (cons '-----    "-----")
      (cons 'buffer   buffer)
      (cons 'path:uniquify:settings/local path:uniquify:settings/local))

    ;;------------------------------
    ;; NOTE: This is called a lot (every frame) to populate the modeline.
    ;; Try to avoid doing (too) much?
    ;; All the figuring out of stuff should maybe happen elsewhere?
    ;;------------------------------
    (nub:debug
        :path:uniquify
        func/name
        func/tags
      "modeline name ours? %S & %S => %S"
      (int<path>:uniquify:doom-modeline/style?)
      (path:uniquify:buffer/managed? buffer
                                     :modeline)
      (and (int<path>:uniquify:doom-modeline/style?)
           (path:uniquify:buffer/managed? buffer
                                          :modeline)))

    (if (and (int<path>:uniquify:doom-modeline/style?)
             (path:uniquify:buffer/managed? buffer
                                            :modeline))
        ;;------------------------------
        ;; Custom Style
        ;;------------------------------
        ;; Just get our name, which should already be set based on the style.
        (nub:debug:func/return
            :path:uniquify
            func/name
            func/tags
          ;; Return Value:
          (path:uniquify:settings/get '(:name :modeline) buffer))

      ;;------------------------------
      ;; Standard Style
      ;;------------------------------
      ;; Otherwise dunno what that style is; pass through to original function.
      (nub:debug:func/return
          :path:uniquify
          func/name
          func/tags
        ;; Return Value:
        (apply func args)))))
;; doom-modeline-buffer-file-name
;; doom-modeline-buffer-file-name-style
;; path:uniquify:settings/local
;; (setq doom-modeline-buffer-file-name-style 'path:uniquify:project/truncate-to-unique)
;; (advice-add 'doom-modeline-buffer-file-name :around #'path:advice:uniquify:doom-modeline-buffer-file-name)
;; (advice-remove 'doom-modeline-buffer-file-name #'path:advice:uniquify:doom-modeline-buffer-file-name)


;;--------------------------------------------------------------------------------
;; Set-Up
;;--------------------------------------------------------------------------------

(defun path:uniquify:set-up ()
  "Set up hooks and advice for `path:uniquify'."
  (advice-add 'rename-buffer :around #'path:advice:uniquify:rename-buffer)
  (advice-add 'create-file-buffer :around #'path:advice:uniquify:create-file-buffer)

  ;; Alias if `uniquify' isn't loaded (yet).
  (unless (functionp 'uniquify-buffer-base-name)
    (defalias 'uniquify-buffer-base-name 'path:uniquify:get:name/requested))

  ;; Advice if `uniquify' is loaded (now or later).
  (eval-after-load 'uniquify
    ;; ...but only advise if we're enabled (still) by then...
    (when (advice-member-p #'path:advice:uniquify:rename-buffer #'rename-buffer)
      (advice-add 'uniquify-buffer-base-name :around #'path:advice:uniquify:uniquify-buffer-base-name))))
;; (path:uniquify:set-up)


;; TODO:emacs-29: Delete when upgraded to Emacs 29!
;; TODO:package: Or depend on compat package for this function?
(if (version< emacs-version "29")
    (unless (functionp 'function-alias-p)
      (defun function-alias-p (func &optional noerror)
        "Return nil if FUNC is not a function alias.
If FUNC is a function alias, return the function alias chain.

If the function alias chain contains loops, an error will be
signalled.  If NOERROR, the non-loop parts of the chain is returned."
        (declare (side-effect-free t))
        (let ((chain nil)
              (orig-func func))
          (nreverse
           (catch 'loop
             (while (and (symbolp func)
                         (setq func (symbol-function func))
                         (symbolp func))
               (when (or (memq func chain)
                         (eq func orig-func))
                 (if noerror
                     (throw 'loop chain)
                   (signal 'cyclic-function-indirection (list orig-func))))
               (push func chain))
             chain)))))
  (nub:warning
      :path:uniquify
      caller
    '("Emacs %s: This `if' block for `function-alias-p' can be deleted once "
      "Emacs 29+ is on all systems.")
    emacs-version))


(defun path:uniquify:tear-down ()
  "Remove `path:uniquify' from advice, etc. and reset buffer names."
  (save-current-buffer
    (let (buffers/managed)
      ;;------------------------------
      ;; Prep
      ;;------------------------------
      ;; Gather a list of buffers to revert from all buffers.
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (path:uniquify:buffer/managed? buffer :any)
            (push (cons buffer (path:uniquify:settings/get '(:name :requested) buffer))
                  buffers/managed))))

      ;;------------------------------
      ;; Clean-Up
      ;;------------------------------
      (advice-remove 'rename-buffer             #'path:advice:uniquify:rename-buffer)
      (advice-remove 'create-file-buffer        #'path:advice:uniquify:create-file-buffer)
      ;; Delete `uniquify-buffer-base-name' if it's our alias.
      (when (and (functionp 'uniquify-buffer-base-name)
                 (memq 'path:uniquify:get:name/requested
                       (function-alias-p 'uniquify-buffer-base-name :no-error)))
        ;; Delete it with extreme predjudice.
        (fmakunbound 'uniquify-buffer-base-name)
        (makunbound 'uniquify-buffer-base-name))
      ;; Unadvise `uniquify-buffer-base-name' if it's not our alias.
      (advice-remove 'uniquify-buffer-base-name #'path:advice:uniquify:uniquify-buffer-base-name)

      ;; Extract ourself from `doom-modeline', if being used.
      (advice-remove 'doom-modeline-buffer-file-name #'path:advice:uniquify:doom-modeline-buffer-file-name)
      (when (int<path>:uniquify:doom-modeline/style?)
        ;; `auto' might be a better value to revert to, in general?
        (setq doom-modeline-buffer-file-name-style 'buffer-name))

      ;; Revert all the buffers we renamed to their original/desired name.
      (dolist (managed buffers/managed)
        (with-current-buffer (car managed)
          (rename-buffer (cdr managed) t)))

      ;; Clear settings in separate step in case we're having bugs.
      (dolist (managed buffers/managed)
        (int<path>:uniquify:settings/clear (car managed)))))

  nil)
;; (path:uniquify:tear-down)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :path '+uniquify)
