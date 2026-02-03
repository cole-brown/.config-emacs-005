;;; modules/dev-env/taskspace/taskspace.el --- Extremely Simple Taskspace/Workspace Management -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2019-04-24
;; Timestamp:  2023-09-13
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Extremely Simple Taskspace/Workspace Management
;;
;;; Code:


;;---------------------------------taskspace------------------------------------
;;--                   Simple Taskspace / Task Management                     --
;;------------------------------------------------------------------------------

;; §-TODO-§ [2020-02-25]: cleanup pass?
;; §-TODO-§ [2020-02-25]: find/do the todos here?

(require 'cl-lib)
(require 'org) ; org-element

(imp:require :dlv)
(imp:require :nub)
(imp:require :taskspace 'group)
(imp:require :taskspace 'prompt)


;;------------------------------------------------------------------------------
;; Taskspace Internals
;;------------------------------------------------------------------------------

;;------------------------------
;; Notes
;;------------------------------

(defun int<taskspace>:notes:open (group taskpath &optional no-error no-message)
  "Open the GROUP:TASKPATH's notes file.

If NO-ERROR, returns nil instead of raising an error signal.

If NO-MESSAGE, skips output message."

  ;; Ok - find and open the notes file. Don't want to just assume they are
  ;; where the settings now indicate new ones will be created; people can
  ;; change their settings. Old ones could be :self-contained and new
  ;; :notesless, for example.

  ;; Assume local/:self-contained first.
  (setq notespath (int<taskspace>:path:notes group taskpath :self-contained))

  ;; Does it exist?
  (unless (path:exists? notespath :file)
    ;; Nope; try remote/:noteless.
    (setq notespath (int<taskspace>:path:notes group taskpath :noteless)))

  ;; There is no third try.
  (if (not (path:exists? notespath :file))
      (if no-error
          nil
        (nub:error
            :taskspace
            "int<taskspace>:notes:open"
          "No notes file found! Look for it: %s, %s"
          (int<taskspace>:path:notes group taskpath :self-contained)
          (int<taskspace>:path:notes group taskpath :noteless)))

    ;; Exists; message and visit it.
    (unless no-message
      (message "Opening taskspace notes: %s" notespath))
    (find-file notespath)))


;;------------------------------
;; Org-Mode Helpers
;;------------------------------

(defun int<taskspace>:org:keywords:list (&optional to-lower)
  "Get keyword elements from this org document.

Elements (return value) will be an alist of (key . value).

'Keyword elements' are lines like this in `org-mode' files:
#+PROPERTY: value

If TO-LOWER is not nil, converts all keys to lowercase. DOES NOT CHANGE VALUES!"
  ;; map func to elements...
  (org-element-map
      (org-element-parse-buffer 'element) ;; parse this buffer at 'element level
      'keyword ;; we only care about keywords
    ;; for each keyword element, get it's key and value into the return.
    (lambda (keyword) (cons
                       (if to-lower
                           (downcase (org-element-property :key keyword))
                         (org-element-property :key keyword))
                       (org-element-property :value keyword)))))


(defun int<taskspace>:org:keyword:get (keyword &optional to-lower)
  "Get the specified KEYWORD from this `org-mode' document.

Will be case insensitive if TO-LOWER is non-nil.

If there are more than one, it will return whatever is first.

'Keyword elements' are lines like this in `org-mode' files:
#+PROPERTY: value

So in the non-nil TO-LOWER case, we will return 'value' if asked for:
  'PROPERTY', 'property', 'PrOpeRtY', etc..."
  (alist-get (if to-lower
                 (downcase keyword)
               keyword)
             (int<taskspace>:org:keywords:list to-lower)
             nil nil
             #'string=))


;;------------------------------
;; Kill Ring (Copy/Paste)
;;------------------------------

(defun int<taskspace>:kill-and-return (string &optional msg msg-args)
  "Copy STRING to kill ring and also return STRING.

Optionally output MSG via `message' with MSG-ARGS."
  ;; copy to kill-ring
  (kill-new string)
  ;; say what we did
  (when msg
    (message msg msg-args))
  ;; return it
  string)
;; (int<taskspace>:kill-and-return "hello")
;; (int<taskspace>:kill-and-return "hello" "hey, %s" "there")


;;----------------------------------Taskspace-----------------------------------
;;--                          Interactive Commands                            --
;;------------------------------------------------------------------------------

;;;###autoload
(defun taskspace:dwim:name ()
  "DWIM with the taskspace in order to save task name to kill ring and return it.

Create if none.
Return if just the one.
Prompt to choose from multiple."
  (interactive)

  ;; Get task's full path, reduce to just task directory...
  (let* ((fullpath (call-interactively #'taskspace:dwim:dir))
         (taskname (file:name fullpath)))

    ;; copy to kill-ring
    (kill-new taskname)

    ;; return it
    taskname))
;; (taskspace:dwim:name)
;; M-x taskspace:dwim:name


;;;###autoload
(defun taskspace:dwim:dir (date-input)
  "DWIM with taskspace in order to save task dir to kill ring and return it.

DATE-INPUT is prefix arg:
  - No prefix arg means today.
  - Prefix arg means tomorrow.

If in an `org-mode' doc with a taskspace keyword defined and if the directory it
specifices exists:
  - Return the full path'd version of that `org-mode' keyword's value.

Else:
  - Create if none.
  - Return if just the one.
  - Choose from multiple."
  ;; Numeric arg but don't let lower case "p" auto-magic nothing (no prefix arg)
  ;; into 1. Nothing/0/nil is today. 1 is tomorrow.
  (interactive (list current-prefix-arg))

  ;; Try to get group from context.
  (let ((group (int<taskspace>:group:auto t))
        task-dir-shortcut
        task-msg-shortcut)

    ;; Prompt for group if we couldn't guess it.
    (unless group
      (setq group (int<taskspace>:prompt:group 'auto t)))

    ;; Check for a taskspace keyword if we're in an org-mode buffer. Just use
    ;; that and skip the rest if we find it and it's a directory that exists and
    ;; stuff.
    (when (eq major-mode 'org-mode)
      (let ((task-dir (int<taskspace>:org:keyword:get
                       (int<taskspace>:config group :dir/tasks/org/keyword))))
        (when (and (not (null task-dir))
                   (path:exists? task-dir :dir))
          (setq task-dir-shortcut (path:canonicalize:dir task-dir))
          (setq task-msg-shortcut
                (format "Got Taskspace from org-mode keyword (#+%s). %s"
                        (int<taskspace>:config group :dir/tasks/org/keyword)
                        task-dir-shortcut)))))

    ;; Do we have a shortcut out, or do we go looking for the task-dir?
    (if (not (null task-dir-shortcut))
        (int<taskspace>:kill-and-return task-dir-shortcut task-msg-shortcut)

      ;; No short cut. Start looking at DWIM things in DWIM-y order...

      ;; Default to "today" if date-input isn't parsable string,
      ;; then get date, taskspaces, etc. for that numerical relative day.
      (let* ((date-input (cond
                          ;; no date-input is today is 0
                          ((null date-input) 0)
                          ;; strings should be converted to numbers
                          ((stringp date-input)
                           (string-to-number date-input))
                          ;; just allow numbers through unscathed
                          ((numberp date-input) date-input)
                          ;; default to... today/0 I guess?
                          (t 0)))
             (date (int<taskspace>:naming:get:date group date-input))
             (taskspaces (int<taskspace>:dir:list:date group date))
             (length-ts (length taskspaces)))

        (cond
         ;; error out if we have no idea what date to dwim with...
         ((null date)
          (nub:error
              :taskspace
              "taskspace:dwim:dir"
            "Date string is nil: %s"
            date))

         ;; if none, create one.
         ((null taskspaces)
          ;; call-interactively will give user prompt for description,
          ;; etc. as if they called themselves.
          (funcall #'taskspace:create
                   group
                   (int<taskspace>:prompt:name group)))

         ;; If just one, return it.
         ;; How to create second in that case? Use a non-dwim create func?
         ;;   - I think yes.
         ((= length-ts 1)

          ;; copy & return
          (int<taskspace>:kill-and-return (cl-first taskspaces)
                                          "Existing taskspace: %s"
                                          (cl-first taskspaces)))

         ;; For now, only give existing choices. User can use a non-dwim create
         ;; func if they want new.
         ((> length-ts 1)

          ;; list available choices to user, get the taskspace they chose
          (let ((choice (int<taskspace>:prompt:task/existing group
                                                             taskspaces
                                                             'nondirectory)))
            (int<taskspace>:kill-and-return choice
                                            "Chose taskspace: %s"
                                            choice)))

         ;; Don't need a default case... Fall through with nil.
         ;;(t nil)
         )))))
;; M-x taskspace:dwim:dir
;; (taskspace:dwim:dir)
;; (taskspace:dwim:dir -1 :home)


;;;###autoload
(defun taskspace:create (group description)
  "Create a new taskspace for today with the supplied GROUP & DESCRIPTION."
  ;; Do we need a max len? Leaving out until I feel otherwise.
  (interactive
   (let ((group-prompt (int<taskspace>:prompt:group 'auto t)))
     (list group-prompt
           (int<taskspace>:prompt:name group-prompt))))

  ;; Is DESCRIPTION ok as description part?
  (unless (int<taskspace>:naming:verify group description)
      ;; Error out; we are up to the interactive level now.
      (nub:error
          :taskspace
          "taskspace:create"
        "Invalid description: %s"
        description))

  ;; Create the dir/project for today.
  (let ((taskpath (int<taskspace>:dir:create group description 'today)))
    (when (null taskpath)
      ;; Couldn't create it for some reason...
      ;; TODO: Better reasons if known. "already exists" would be nice for
      ;; that case.
      (nub:error
          :taskspace
          "taskspace:create"
        "Error creating taskspace directory for: %s"
        description))

    ;; Copy files into new taskspace.
    (when (path:exists? (int<taskspace>:config group :file/new/copy) :dir)
      (apply #'int<taskspace>:file:copy
             ;; arg 1: our new taskpath
             taskpath
             ;; arg &rest: all the files to copy with:
             ;;   - full path name
             ;;   - no dot files
             ;;     - no '.', '..'
             ;;     - yes actual dotfiles somehow?
             ;;     - This is what I want, so... ok.
             (path:children (int<taskspace>:config group :file/new/copy)
                            :absolute-paths
                            nil
                            path:rx:dirs:not-parent-or-current-dot)))

    ;; Generate files into new taskspace.
    (when (int<taskspace>:config group :file/new/generate)
      (let ((gen-errors (int<taskspace>:file:generate
                         group
                         taskpath
                         (int<taskspace>:config group :file/new/generate))))
        (when gen-errors
          (nub:error
              :taskspace
              "taskspace:create"
            "Taskspace errors while generating file(s): %s"
            gen-errors))))

    ;; Either of those can put a projectile file into the taskspace.
    ;; Just name it: .projectile
    ;;   See: https://projectile.readthedocs.io/en/latest/projects/#ignoring-files

    ;; Can also put skeleton org file. Or just org file with yasnippet ready
    ;; to go...

    ;; Copy taskpath to kill-ring.
    (kill-new taskpath)
    ;; Say something.
    (message "Created taskspace: %s" (file:name taskpath))
    ;; (message "Created taskspace: %s" taskpath)

    ;; Open the notes file.
    (int<taskspace>:notes:open group taskpath nil t)

    ;; Return the path.
    taskpath))
;; M-x taskspace:create
;; (taskspace:create "testing-create")


;;;###autoload
(defun taskspace:dired:task ()
  "Open the current taskspace's root dir in an Emacs (Dired?) buffer.

If in a :noteless file, go to that note's task dir, if possible.
If in a file or sub-dir of the task dir, go to the task's dir."
  (interactive)

  (let* ((group (or (int<taskspace>:group:auto t)
                    (int<taskspace>:prompt:group 'auto t)))
         (taskpath (int<taskspace>:org:keyword:get
                    (int<taskspace>:config group :dir/tasks/org/keyword))))

    ;; Can short-cut past this if we were in an org file and found the keyword
    ;; link to the task's dir.
    (unless taskpath
      ;; Deduce group or prompt for it.
      (let ((path (int<taskspace>:path:current))
            (root (int<taskspace>:config group :dir/tasks)))

        (setq taskpath
              ;; If we are in a dir/file under a task, get the topmost dir that
              ;; isn't the root.
              (cond ((path:descendant? path root)
                     ;; Go up from current until we get to direct child.
                     (let ((path/curr path)
                           child/found?)
                       (while (and path/curr
                                   (not child/found?))
                         (setq path/curr (path:parent path/curr)
                               child/found? (path:child? path/curr root)))
                       path/curr))

                    ;; If we are in a remote notes file, dunno.
                    ;;   *shrug* Fallthrough to 't.

                    ;; If we are elsewhere, really dunno.
                    (t
                     nil)))))

    (if (not (path:exists? taskpath :dir))
        ;; Not a dir - error out.
        (nub:error
            :taskspace
            "taskspace:dired:task"
          "'%s' taskspace directory doesn't exist?: '%s'"
          group taskpath)

      ;; Ok - message and open (probably in dired but let emacs decide).
      (find-file taskpath)
      (message "Opening '%s' taskspace directory: %s"
               group
               (file:name taskpath))
      ;; Return the task's path?
      taskpath)))
;; (taskspace:dired:task)
;; M-x taskspace:dired:task


;;;###autoload
(defun taskspace:dired:root (group)
  "Open the root directory of GROUP's taskspaces in an Emacs (Dired?) buffer."
  (interactive (list (int<taskspace>:prompt:group 'auto t)))

  (if (not (file:exists? (int<taskspace>:config group :dir/tasks) :dir))
      ;; not a dir - error out
      (nub:error
          :taskspace
          "taskspace:dired:root"
        "Can't find taskspace root directory: '%s'"
        (int<taskspace>:config group :dir/tasks))

    ;; ok - message and open (probably in dired but let emacs decide)
    (find-file (int<taskspace>:config group :dir/tasks))
    ;; say something
    (message "Opening taskspace parent: %s"
             (file:name (int<taskspace>:config group :dir/tasks)))
    ;; return the top dir?
    (int<taskspace>:config group :dir/tasks)))
;; (taskspace:dired:root :home)
;; M-x taskspace:dired:root


;;;###autoload
(defun taskspace:shell (group)
  "Open GROUP's root taskspace directory in an Emacs shell buffer.

Shell opened can be set by modifying:
  (int<taskspace>:config group :function/shell)."
  ;; §-TODO-§ [2020-08-18]: If in a taskspace folder or a remote notes file,
  ;; just let the shell open without prompts.
  (interactive (list (int<taskspace>:prompt:group 'auto t)))

  (let ((shell-fn (int<taskspace>:config group :function/shell)))
    (if (not (functionp shell-fn))
        (nub:error
            :taskspace
            "taskspace:shell"
          "`:function/shell' in taskspace settings for group `%S' is not bound to a fuction: %s"
          shell-fn)

      ;; prompt user for the taskspace with an attempt at DWIM
      (let ((task (call-interactively #'taskspace:dwim:dir)))
        ;; expecting a path from task-dir/dwim
        (if (not (path:exists? task :dir))
            ;; not a dir - error out
            (nub:error
                :taskspace
                "taskspace:shell"
              "Can't find taskspace (not a directory?): '%s'"
              task)

          ;; open with shell-fn
          (funcall shell-fn)
          ;; say something
          (message "Opening taskspace shell: %s" (file:name task))
          ;; return the chosen task's dir?
          task)))))
;; (taskspace:shell :work)
;; M-x taskspace:shell


;;;###autoload
(defun taskspace:notes (date-input group)
  "Open a taskspace's notes file.

DATE-INPUT is prefix arg:
  - No prefix arg means today.
  - Prefix arg means tomorrow.

GROUP should be a group keyword.

DWIM-ish actions:
  - If just one taskspace for DATE-INPUT, open today's notes file.
  - If more than one taskspace, prompt user with auto-complete options for
    DATE-INPUT's notes files.
  - If prefix arg supplied: prompt user with auto-complete options for all notes
    files.
  - If no taskspaces are found for the DATE-INPUT, list all taskspaces for
    GROUP."
  ;; Numeric arg but don't let lower case "p" auto-magic nothing (no prefix arg)
  ;; into 1. Nothing/0/nil is today. 1 is tomorrow.
  (interactive (list current-prefix-arg
                     (int<taskspace>:prompt:group 'auto t)))

  ;; Default to "today" if date-input isn't parsable string,
  ;; then get date, taskspaces, etc. for that numerical relative day.
  (let* ((date-parsed (cond
                       ;; no date-input is today is 0
                       ((null date-input) 0)
                       ;; strings should be converted to numbers
                       ((stringp date-input)
                        (string-to-number date-input))
                       ;; just allow numbers through unscathed
                       ((numberp date-input) date-input)
                       ;; default to... today/0 I guess?
                       (t 0)))
         (date        (int<taskspace>:naming:get:date group date-parsed))
         (taskspaces  (int<taskspace>:dir:list:date group date))
         (taskspaces  (or taskspaces
                          (int<taskspace>:dir:list:all group)))
         (length-ts   (length taskspaces))
         taskpath)

    (message "%S tasks for %S: %S" length-ts date taskspaces)

    (cond
     ;; error out if we have no idea what date to dwim with...
     ((null date)
      (nub:error
          :taskspace
          "taskspace:notes"
        "Date string is nil: %s"
        date))

     ;; If just one, open its notes file.
     ((= length-ts 1)
      (setq taskpath (cl-first taskspaces))
      (message "Only taskspace: %s" taskpath))

     ;; For now, only give existing choices. User can use a non-dwim create func
     ;; if they want new.
     ((> length-ts 1)

      ;; list available choices to user, get the taskspace they chose
      (let ((choice (int<taskspace>:prompt:task/existing group taskspaces 'nondirectory)))
        (setq taskpath choice)
        (message "Chose taskspace: %s" choice)))

     ;; Default case... Fall through with nil.
     (t nil))

    (if (null taskpath)
        (nub:error
            :taskspace
            "taskspace:notes"
          "No taskspace notes found for date: %s"
          date)

      ;; Exists; message and visit it.
      (int<taskspace>:notes:open group taskpath))))
;; M-x taskspace:notes
;; (taskspace:notes)
;; (taskspace:notes -1 (int<taskspace>:prompt:group 'auto t))


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------

;; TODO-PKG:
;;   - Comments/layout like a real package.
;;     e.g. https://github.com/tarsius/moody/blob/master/moody.el


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :taskspace 'taskspace)
