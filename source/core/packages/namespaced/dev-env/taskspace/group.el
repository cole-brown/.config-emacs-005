;;; modules/dev-env/taskspace/group.el --- Taskspace Groups -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-07-06
;; Timestamp:  2023-09-13
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Taskspace Groups
;;
;;; Code:


;;---------------------------------taskspace------------------------------------
;;--                   Simple Taskspace / Task Management                     --
;;------------------------------------------------------------------------------

(require 'dash)

(imp:require :nub)
(imp:require :dlv)


;;------------------------------------------------------------------------------
;; Custom Variables Group
;;------------------------------------------------------------------------------

(defgroup taskspace nil
  "Extremely Simple Taskspace/Workspace Management.

Tool for creating/using simple taskspaces (workspaces) for short tasks,
bug investigation, log munging, or whatever."
  :group 'convenience)


;;------------------------------------------------------------------------------
;; Multiple Separate Taskspaces
;;------------------------------------------------------------------------------

(defcustom taskspace:groups
  '((:default "Taskspace" taskspace:group:default))
  "Definitions for multiple task spaces. Each will have its own settings.
Each entry in the alist is a list of: (keyword string settings-variable)

'keyword' is used internally to identify the taskspace groups.

'string' is a name used for display purposes.

'settings-variable' should be the big settings alist (see
`taskspace:group:default' for the default's settings)."
  :group 'taskspace
  :type '(alist :key-type symbol
                :value-type string))


;;------------------------------------------------------------------------------
;; Per-Taskspace Settings
;;------------------------------------------------------------------------------

(defcustom taskspace:group:default
  '((:type/notes :self-contained
     (concat
      "What kind of taskspace to create by default.\n"
      "\n"
      "It can 'deal' with these kind of taskspaces:\n"
      "  - `:self-contained' - Self-Contained (taskspace contains everything)\n"
      "    - Taskspace dir contains:\n"
      "      - notes,\n"
      "      - data,\n"
      "      - etc.\n"
      "   - No other directories or files.\n"
      "  - `:noteless' - Split (taskspace notes are separate from rest)\n"
      "    - Taskspace dir contains:\n"
      "      - data,\n"
      "      - etc.\n"
      "   - Notes file exists separately.\n"))

    (:format/datetime "%Y-%m-%d"
     "Date format for parsing/naming taskspace directory names.")

    (:function/shell  #'shell
     (concat
      "Function to call to open shell buffer. `shell' and `eshell' work. "
      "Opens the current taskspace's top dir in an emacs shell buffer."))

    (:dir/tasks (path:absolute:dir user-emacs-directory "taskspace")
     (concat
      "User's root taskspace folder for small work tasks. "
      "All taskspaces for this group will be created here."))

    (:dir/notes (path:absolute:dir user-emacs-directory "taskspace")
     (concat
      "User's folder for all notes that are in `:noteless' taskspaces. "
      "Unused in `:self-contained' taskspaces."))

    (:file/new/copy (path:absolute:dir (int<taskspace>:config :default :dir/tasks)
                                       "taskspace-new")
     "User's folder for files to copy into new taskspaces.")

    (:file/new/generate (((int<taskspace>:config :default :file/notes) "") ;; empty file
                         (".projectile" "")) ;; also empty
     (concat
      "Files to generate for new taskspaces. Expects an alist like:\n"
      "(('file1.name' 'contents') ('file2.name' #'your-gen-function))\n"
      "\n"
      "Note: `group', `taskname', `taskpath', and `notepath' are supplied as the args\n"
      "to the generator functions.\n"
      "\n"
      "`taskname' is the full taskname.\n"
      "  example: \"2023-07-21_0_lunch-n-learn-all-about-3d\"\n"
      "\n"
      "`taskpath' is the full path to the task folder.\n"
      "  example: \"/home/user/taskspace/work/2023-07-21_0_lunch-n-learn-all-about-3d/\"\n"
      "`notepath' is the full path to the notes file, which can be in a separate directory.\n"
      "  example: \"/home/user/org/taskspace/work/2023-07-21_0_lunch-n-learn-all-about-3d.notes.org\"\n"
      "\n"
      "Should return a string of the file's contents.\n"
      "e.g.: (defun my/taskspace/gen-org-notes (group taskname taskpath filename)\n"
      "        (format ...))\n"))

    (:file/notes "_notes.org" "File for storing/recording notes about a task.")

    ;; TODO: REGEX
    ;; TODO: regex all these or somethinng?
    ;; TODO: REGEX
    (:dir/tasks/ignore
     ;; TODO: how do I even do string and/or regexes? Different alist entries
     ;; probably since regexes are just strings?
     ("." ".."
      "00_archive"
      (path:absolute:file (int<taskspace>:config :default :file/new/copy)))
     (concat
      "Always ignore these when getting/determining a taskspace directory. "
      "Can be strings or functions."))

    (:naming/separator "_"
     "Split directory name on this to extract date, number, and description.")

    ;; TODO: Turn these into regexes w/ capture groups, I think?..
    ;; Have make-name and split-name use the regexes to make/split.
    ;; http://ergoemacs.org/emacs/elisp_string_functions.html
    ;; TODO: use this nice regex builder (elisp sexprs)?:
    ;;   https://www.reddit.com/r/emacs/comments/cf8r83/easier_editing_of_elisp_regexps/eu84ob1/
    (:naming/parts-alists
     (
      ;; Three part. Code does this all the time?
      ((date . 0)
       (number . 1)
       (description . 2))
      ;; Two part. Human does this most of the time...
      ((date . 0)
       (description . 2))
      )
     (concat
      "Order of items in task's descriptive directory name. List of alists. "
      "First one of the correct length is used currently."))

    ;; §-TODO-§ [2020-08-18]: Turn this into an `(rx ...)'.
    (:naming/description/rx/valid "^[[:alnum:]_\\-]\\{3,\\}$"
     "Letters, numbers, underscore, and hypen are valid.")

    (:dir/tasks/org/keyword "TASKSPACE"
     (concat
      "Name of the Property/Keyword that might hold a pointer to the "
      "taskspace directory in a taskspace's org notes file.\n"
      "\n"
      "e.g.: if this line is in the '_notes.org':"
      "#+TASKSPACE: ~/path/to/taskspace/2020-03-17_0_example"
      "...then `int<taskspace>:org:keyword:get' can be used to get it for"
      "`taskspace:dwim:dir'.")))

  "Alist of custom settings (name/value/docstr) for default taskspace."
  :group 'taskspace
  ;; :type no idea so I'm leaving it out... sexp?
  )


;;------------------------------------------------------------------------------
;; Per-Group Config/Settings Helpers
;;------------------------------------------------------------------------------

(defun int<taskspace>:config (group key &optional field)
  "Get the FIELD of KEY from GROUP's settings alist in `taskspace:groups'.
If GROUP doesn't exist in `taskspace:groups', this will look for `:default'.
Errors if nothing is found in GROUP or `:default' settings.

If FIELD is nil or `:setting', gets the setting's value.

If FIELD is `:docstr', gets the setting's docstr.

If FIELD is `:key', gets the setting's key, which is KEY. Almost
useless, but does validate entry's exists."
  (let ((entry nil)
        (settings (int<taskspace>:config:group:get/settings group)))
    ;; First, try to get from group's settings (if we found group's settings).
    (when settings
      (setq entry (int<taskspace>:config:get key settings)))

    ;; Second, if needed, try to get from default/fallback settings.
    (when (null entry)
      ;; Didn't find the requested group... use defaults.
      (setq entry
            (int<taskspace>:config:get
             key
             (int<taskspace>:config:group:get/settings :default))))

    ;; Now we can finally either reduce down to the value or error out.
    (if (null entry)
        ;; Not found anywhere; error out.
        (nub:error
            :taskspace
            "int<taskspace>:config"
          "Taskspace: Could not find setting '%S' in group '%S' or default"
          key group)

      ;; Got group; return value of requested field.
      ;; Entry should be: (key value docstr)
      ;; Now figure out which is requested, what it actually is, and return it.
      (cond ((eq field :key)
             (int<taskspace>:config:entry:get/key entry))

            ((eq field :docstr)
             (int<taskspace>:config:entry:get/docstr entry))

            (t
             (int<taskspace>:config:entry:get/setting entry))))))
;; (int<taskspace>:config :home :format/datetime)
;; (int<taskspace>:config :home :format/datetime :setting)
;; (int<taskspace>:config :home :format/datetime :docstr)
;; (int<taskspace>:config :home :format/datetime :key)
;; (int<taskspace>:config :home :type/notes)
;; (int<taskspace>:config :home :dir/tasks)
;; (int<taskspace>:config :home :dir/tasks/ignore)
;; (int<taskspace>:config :home :naming/parts-alists)
;; (int<taskspace>:config :home :file/new/copy)


(defun int<taskspace>:config:entry:get/key (entry)
  "Helper for int<taskspace>:config.

Given an ENTRY from a group's settings alist, returns its key or nil."
  (nth 0 entry))


(defun int<taskspace>:config:entry:get/setting (entry)
  "Helper for int<taskspace>:config.

Given an ENTRY from a group's settings alist, turn it into an actual setting.

ENTRY is the alist tuple of (taskspace-keyword thing-to-figure-out docstr).

Thing-to-figure-out could be: a symbol that needs evaluated, a string, a
function that needs called, etc."
  (let ((setting (nth 1 entry)))
    (cond
     ;; Function: Check before listp. If `setting' is #'ignore then I guess it's
     ;; actually (function ignore), which is a list.
     ;; There's probably a better way to do this...
     ((and (listp setting)
           (eq (nth 0 setting) 'function)
           (functionp (nth 1 setting)))
      (funcall (nth 1 setting)))

     ;; Just a function: Call it.
     ((or (functionp setting)
          (and (listp setting)
               (eq (nth 0 setting) 'function)
               (functionp (nth 1 setting))))
      (funcall setting))

     ;; If setting is a list, figure out what to do with it before returning.
     ((listp setting)
      (cond ((functionp (nth 0 setting))
             ;; List starts with a function; eval list and return.
             (eval setting))

            ;; Any other things to do with a list?

            ;; Default to just returning the list.
            (t setting)))

     ;; Symbol: Return value of symbol; use lexical scope.
     ((symbolp setting)
      (condition-case-unless-debug err
          (eval setting t)
        ;; If it's void, just return symbol itself.
        (void-variable setting)
        ;; Let other errors through?
        ;; (error 'setting)
        ))

     ;; Else just return it.
     (t
      setting))))
;; (int<taskspace>:config:entry:get/setting '(jeff (+ 4 1) "list function"))
;; (int<taskspace>:config:entry:get/setting '(jeff (4 1)   "just a list"))
;; (int<taskspace>:config:entry:get/setting '(jeff #'ignore "a function"))
;; (int<taskspace>:config:entry:get/setting '(jeff jeff "symbol, no value"))
;; (let ((a 42)) (int<taskspace>:config:entry:get/setting '(jeff a "symbol w/ value")))


(defun int<taskspace>:config:entry:get/docstr (entry)
  "Helper for int<taskspace>:config.

Given an ENTRY from a group's settings alist, returns its docstr or nil."
  (nth 2 entry))


(defun int<taskspace>:config:group:get/settings (group)
  "Helper for int<taskspace>:config.

Get settings from `taskspace:groups' using GROUP as alist key.
Return just the settings - not the full assoc value.
Can return nil."
  ;; Group entry is: (keyword display-name settings)
  ;; Return only settings, or just nil if assoc/nth don't find anything.
  (let ((settings (int<taskspace>:group:settings group)))
    (cond ((listp settings)
           settings)

          ((symbolp settings)
           (eval settings))

          (t
           nil))))
;; (int<taskspace>:config:group:get/settings :default)


(defun int<taskspace>:config:get (key settings)
  "Helper for int<taskspace>:config.

Gets value for KEY from settings. Returns nil if not found.
Returns assoc value if found (key's full entry in SETTINGS alist)."
  (assoc key settings))


;;------------------------------------------------------------------------------
;; Validation
;;------------------------------------------------------------------------------

;; TODO: use this to validate group in code places.
(defun int<taskspace>:group:valid? (group)
  "Return non-nil if GROUP is a valid group keyword/name."
  (keywordp group))


;;------------------------------------------------------------------------------
;; Group DLV
;;------------------------------------------------------------------------------

(defun taskspace:group:dlv (group directory)
  "Create a directory-local-variable for GROUP and DIRECTORY.
This sets the automatic group for that dir (and sub-dirs) to GROUP."
  (if (imp:provided? :dlv)
      (dlv:set directory
               nil ;; global mode
               (list 'int<taskspace>:dlv:group
                     group
                     :safe))
    (nub:error
        :taskspace
        "taskspace:group:dlv"
      "Requires `dlv' feature/package/module; didn't find them. Group: %s, Directory: %s"
      group directory)))


(defun int<taskspace>:group:dlv ()
  "Try to get the directory-local-variable `taskspace:group:dlv'.

`taskspace:group:dlv' is not expected to exist, generally.
This will return its value or nil."
  ;; Cannot use `condition-case-unless-debug' here... it just doesn't catch
  ;; `void-variable' signal.
  (condition-case nil
      int<taskspace>:dlv:group
    ;; `int<taskspace>:dlv:group' does not exist; return nil.
    (void-variable nil)
    ;; Generic error signal...?
    ;; (error nil)
    ;; All signals?
    ;; (t nil)
    ))
;; (int<taskspace>:group:dlv)


;;------------------------------------------------------------------------------
;; Group Getters
;;------------------------------------------------------------------------------

(defun int<taskspace>:group:current (&optional quiet)
  "Try to figure out current group given currently visited buffer.

Set QUIET to non-nil for nil return on error, else will signal an error.
QUIET also suppresses the \"Current Taskspace Group: ...\" message."
  ;; `int<taskspace>:path:current' can return nil, so make sure to account for it.
  (let ((path (int<taskspace>:path:current))
        (current-root nil)
        (current-group nil))

    ;; Do we have enough to get started?
    (when path
      ;; Search through our groups for something to match to that.
      (setq current-root
            (car
             ;; Start by getting our group keywords...
             (->> (-map #'car taskspace:groups)
                  ;; and turning into task and notes dirs...
                  (-map (lambda (g) (list
                                     (int<taskspace>:config g :dir/tasks)
                                     (int<taskspace>:config g :dir/notes))))
                  ;; Have list of tuple lists now; want flat list.
                  (-flatten)
                  ;; Figure out which one is our current root.
                  (-map (lambda (root)
                          ;; If a child or the same dir as root, keep root.
                          ;; Otherwise return nil for a "nope, not this one".
                          (if (or
                               (path:descendant? path root)
                               (path:equal? path root))
                              root
                            nil)))
                  ;; Reduce down to non-nil answer.
                  ;; Assumes there is only one non-nil answer.
                  (-remove #'null)))))

    ;; How'd we do?
    (if (not (stringp current-root))
        ;; Complain if interactive; return nil if not.
        (if quiet
            nil
          (nub:error
              :taskspace
              "int<taskspace>:group:current"
            "Could not find a taskspace root for currently visited buffer dir: %s"
            path))

      ;; Normalize the path.
      (setq current-root (path:absolute:dir current-root))

      ;; Now, finally... We're not done.
      ;; Got to translate some taskspace or notes root dir into its group.
      (setq current-group
            (car ;; Not sure what to do if there's more than one choice left...
             ;; Reduce down to whatever is non-nil.
             (-remove
              #'null
              (-map (lambda (entry)
                      ;; If we match one of this group's roots, return the
                      ;; group keyword.
                      (if (or
                           (path:equal? (int<taskspace>:config (nth 0 entry)
                                                               :dir/tasks)
                                        current-root)
                           (path:equal? (int<taskspace>:config (nth 0 entry)
                                                               :dir/notes)
                                        current-root))
                          (nth 0 entry)
                        nil))
                    taskspace:groups))))
      ;; Inform it and return it.
      (unless quiet
        (message "Current Taskspace Group: %s" current-group))
      current-group)))
;; (int<taskspace>:group:current)


(defun int<taskspace>:group:auto (&optional quiet)
  "Try to get either the auto-group or the current-group.

Prefer the auto-group.

Set QUIET to non-nil for nil return on error, else will signal an error.
QUIET also suppresses the \"Current Taskspace Group: ...\" message."
  (or (int<taskspace>:group:dlv)
      (int<taskspace>:group:current quiet)))
;; (int<taskspace>:group:auto)


(defun int<taskspace>:group (group)
  "Return the `assoc' from `taskspace:groups' for GROUP.

If GROUP is a keyword, get the assoc and then return it.
If GROUP is a list, assume it is already the assoc list and return it.
Else signal an error."
  ;; Keyword? Get the assoc for that group.
  (cond ((keywordp group)
         (assoc group taskspace:groups))

        ;; List? Assume it's already the group assoc from `taskspace:groups',
        ;; and return it.
        ((listp group)
         group)

        (t
         (nub:error
             :taskspace
             "int<taskspace>:group"
           "Cannot understand type '%S'; need a keyword or list! Group: %S"
           (type-of group)
           group))))
;; (int<taskspace>:group :default)
;; (int<taskspace>:group '(:default "Defaults" taskspace:group:default))


(defun int<taskspace>:group:keyword (group)
  "Given GROUP keyword/list, return group's keyword.

GROUP should be return value from `int<taskspace>:group' (assoc from
`taskspace:groups')."
  (nth 0 (int<taskspace>:group group)))
;; (int<taskspace>:group:keyword '(:default "Taskspace of Default" this-dne))


(defun int<taskspace>:group:name/display (group)
  "Given GROUP keyword/list, return GROUP's display name.

GROUP should be either a keyword or the return value from `int<taskspace>:group'
\(assoc from `taskspace:groups')."
  (let ((group-list (int<taskspace>:group group)))
    (if (null (nth 1 group-list)) ;; Does the group even have a display name?
        (symbol-name (int<taskspace>:group:keyword group-list))
      (nth 1 group-list))))
;; (int<taskspace>:group:name/display '(:default "Taskspace of Default" this-dne))
;; (int<taskspace>:group:name/display '(:default nil this-dne))
;; (int<taskspace>:group:name/display :default)


(defun int<taskspace>:group:settings (group)
  "Given GROUP symbol/list, return group's settings.

GROUP should be return value from `int<taskspace>:group' (assoc from
`taskspace:groups')."
  (nth 2 (int<taskspace>:group group)))
;; (int<taskspace>:group:settings '(:default "Taskspace of Default" this-dne))
;; (int<taskspace>:group:settings :default)


;;------------------------------------------------------------------------------
;; Directory Local Variables
;;------------------------------------------------------------------------------

(defvar int<taskspace>:dlv:group nil
  "This should always be nil unless used via Directory Local Variables.

It should only be set via `taskspace:group:dlv'")

;; Mark our DLV variable as safe for DLV use.
(dlv:var:safe/predicate 'int<taskspace>:dlv:group #'int<taskspace>:group:valid?)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :taskspace 'group)
