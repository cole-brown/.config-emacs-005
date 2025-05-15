;;; modules/dev-env/taskspace/path.el --- Path Helpers -*- lexical-binding: t; -*-
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
;; Path Helpers
;;
;;; Code:


;;---------------------------------taskspace------------------------------------
;;--                   Simple Taskspace / Task Management                     --
;;------------------------------------------------------------------------------

(require 'cl-lib)

(imp:require :dlv)
(imp:require :nub)
(imp:require :taskspace 'group)
(imp:require :taskspace 'naming)


;;------------------------------------------------------------------------------
;; Paths
;;------------------------------------------------------------------------------

(defun int<taskspace>:path:current ()
  "Return correct 'current filepath' for both Dired mode and not."
  (cond ((equal major-mode 'dired-mode)
         default-directory)

        ((buffer-file-name)
         (path:parent (buffer-file-name)))

        (t
         nil)))


(defun int<taskspace>:path:notes (group taskpath &optional type/notes)
  "Given GROUP and TASKPATH, generate the notes file path.

If TYPE/NOTES is `:self-contained' or `:noteless', this will ignore the config
settings for the group and return based on TYPE/NOTES.

Otherwise, it will check GROUP's config settings for `:type/notes' and
build the notes file path based on that."
  ;; Get filename from config.
  (let ((filename (int<taskspace>:config group :file/notes))
        ;; Use type/notes if a valid value, else get from config.
        (type/notes (if (or (eq type/notes :self-contained)
                            (eq type/notes :noteless))
                        type/notes
                      (int<taskspace>:config group :type/notes))))

    ;; Build output based on what type we figured out.
    (cond ((eq type/notes :self-contained)
           ;; :self-contained notes filepath is just filename tacked
           ;; on to taskpath.
           (path:absolute:file taskpath filename))

          ((eq type/notes :noteless)
           ;; Remote file name is different - you want the task name in it so
           ;; the remote notes folder makes any sense on its own.
           (path:absolute:file
            ;; The remote notes dir from the group's config to get notespath:
            (int<taskspace>:config group :dir/notes)
            ;; And Remote File Name is:
            (concat
             ;; Task Name
             (file:name taskpath)
             ;; Plus a dot...
             "."
             ;; Plus filename, sans 'sort to top' stuff...
             (string-trim filename "_" "_")))))))
;; (int<taskspace>:path:notes :home "c:/2020-08-24_11_jeff/" :self-contained)
;; (int<taskspace>:path:notes :home "c:/2020-08-24_11_jeff/" :noteless)
;; (int<taskspace>:path:notes :home "c:/2020-08-24_11_jeff/" :jeff)
;; (int<taskspace>:path:notes :home "c:/2020-08-24_11_jeff/")


(defun int<taskspace>:path:generate (group taskpath filename)
  "Generate a file path for FILENAME and TASKPATH in GROUP.

This can be outside of the taskspace for e.g. :noteless taskspaces - the note
file will be elsewhere."
  (if (not (string= filename (int<taskspace>:config group :file/notes)))
      ;; Non-note files just go in taskspace...
      (path:absolute:file taskpath filename)

    ;; Notes files may or may not go in taskspace. Find out.
    (if (eq (int<taskspace>:config group :type/notes)
            :self-contained)
        ;; Local file name is just provided name.
        (path:absolute:file taskpath filename)

      ;; Remote file name could be different - may want task name in it.
      (path:absolute:file (int<taskspace>:config group :dir/notes)
                          (concat ;; remote file name:
                           ;; Task Name
                           (file:name taskpath)
                           ;; Plus a dot...
                           "."
                           ;; Plus filename, sans 'sort to top' stuff...
                           (string-trim filename "_" "_"))))))
;; (int<taskspace>:path:generate :default "c:/2020-20-20_20_jeff" "_notes.org")
;; (int<taskspace>:path:generate :default "c:/2020-20-20_20_jeff" "jeff.data")


;;------------------------------------------------------------------------------
;; Files
;;------------------------------------------------------------------------------

(defun int<taskspace>:file:generate (group taskpath file-alist)
  "Generate each file in FILE-ALIST into the new taskpath.

GROUP should be return value from `int<taskspace>:group' (assoc from
`taskspace:groups').

Expect FILE-ALIST entries to be:
  (filename . string-or-func)

Create 'filename' in TASKPATH and then insert string into it, or use func to
generate contents. Does not currently support directory structures/trees.

Return nil or an alist of errors.
Errors alist is all files not generated, where each assoc in errors alist is:
  (filename . 'reason')"

  ;; let it just do nothing when empty list
  (let (errors-alist ;; empty return value alist
        ;; Get taskname from path to supply to any file content gen funcs.
        (taskname (file:name taskpath)))
    (dolist (entry file-alist errors-alist)
      (let* ((file (file:name (eval (cl-first entry))))
             (filepath (int<taskspace>:path:generate group taskpath file))
             (str-or-func (cl-second entry)))

        (cond
         ;; ERROR: already exists...
         ((path:exists? filepath)
          (push `(,filepath . "file already exist") errors-alist))

         ;; ;; ERROR: generator not bound
         ;; ((not (boundp str-or-func))
         ;;  (push `(,filepath . "string/function not bound") errors-alist))

         ;; ERROR: unknown generator
         ((and (not (stringp str-or-func))
               (not (functionp str-or-func)))
          (push `(,filepath . ,(format "generator is not string or function: %s"
                                       str-or-func))
                errors-alist))

         ;; HAPPY!
         (t
          (with-temp-file filepath
            (if (stringp str-or-func)
                (insert str-or-func)
              ;; Call with group so users can have a function for multiple
              ;; groups if applicable...
              (insert (funcall str-or-func group taskname taskpath filepath))))))

        ;; ;; If made a remote notes file, make a .taskspace config now.
        ;; (when (and (string= file (int<taskspace>:config group :file/notes))
        ;;            (not (path:child? filepath taskpath)))
        ;;   (taskspace/with/config taskpath
        ;;     (setq taskspace/config
        ;;           (taskspace/config/set :notes filepath taskspace/config))
        ;;     (taskspace/config/write taskspace/config taskpath)))

        ;; dolist returns the errors
        ))))


(defun int<taskspace>:file:copy (taskpath &rest filepaths)
  "Copy each of the files in FILEPATHS to TASKPATH.

Expect well-qualified filepaths (absolute, relative, or otherwise). Does not
currently support directory structures/trees.

Return nil or an errors alist.
  - Errors alist, where each assoc in errors alist is:
    (filepath . 'reason')"
  ;; let it just do nothing when empty list
  (let (errors-alist) ;; empty return value alist
    (dolist (path filepaths errors-alist)
      (cond
       ;; ERROR: can't find or...
       ((not (path:exists? path))
        (push `(,path . "file does not exist") errors-alist))
       ;; ERROR: can't read file or...
       ((not (path:readable? path))
        (push `(,path . "file is not readable") errors-alist))
       ;; ERROR: not a file (dir or symlink or something)
       ((not (path:exists? path :file))
        (push `(,path . "path is not a file") errors-alist))

       ;; HAPPY: copy it
       (t
        (copy-file path ;; from "the full path of where it is" to...
                   ;; taskpath + "the filename part of where it is"
                   (path:absolute:file taskpath (file:name path))))

       ;; dolist returns the errors
       ))))


;;------------------------------------------------------------------------------
;; Directories
;;------------------------------------------------------------------------------

(defun int<taskspace>:dir:create (group description date-arg)
  "Create a taskspace directory.

GROUP should be return value from `int<taskspace>:group' (assoc from
`taskspace:groups').

DATE-ARG must be nil, 'today (for today), or an number for a relative day.

Directory name is formatted with DESCRIPTION, date, and (monotonically
increasing) serial number."
  (let ((func/name "int<taskspace>:dir:create")
        (func/tags '(:create)))

    ;; Make sure basic folders exist.
    (unless (path:exists? (int<taskspace>:config group :dir/tasks) :dir)
      (message "Taskspace: Making root directory... %s"
               (int<taskspace>:config group :dir/tasks))
      (make-directory (int<taskspace>:config group :dir/tasks)))
    (unless (path:exists? (int<taskspace>:config group :dir/notes) :dir)
      (message "Taskspace: Making remote notes directory... %s"
               (int<taskspace>:config group :dir/notes))
      (make-directory (int<taskspace>:config group :dir/notes)))

    ;; Get today's date.
    (let* ((date (int<taskspace>:naming:get:date group date-arg))
           ;; Get today's dirs.
           (date-dirs (int<taskspace>:dir:list:date group date))
           ;;   - figure out index of this one
           (number (int<taskspace>:naming:get:number group date-dirs))

           ;; Build dir string from all that.
           (dir-name (int<taskspace>:naming:make group date number description))
           (dir-full-path (path:absolute:dir (int<taskspace>:config group :dir/tasks)
                                             dir-name)))

      ;; TODO: taskspace debugging func.
      (nub:debug:func/start
          :taskspace
          func/name
          func/tags
        (cons 'group           group)
        (cons 'description     description)
        (cons 'date-arg        date-arg)
        (cons '--date          date)
        (cons '--date-dirs     date-dirs)
        (cons '--number        number)
        (cons '--dir-name      dir-name)
        (cons '--dir-full-path dir-full-path))


        ;; Only create if:
        ;;   - valid description input and
        ;;   - no dupes or accidental double creates
        ;;   - it doesn't exist (this is probably redundant if verify-description
        ;;     works right)
        (if (and (int<taskspace>:naming:verify group description)
                 (not (cl-some (lambda (x) (int<taskspace>:dir= group
                                                                description
                                                                x
                                                                'description))
                               date-dirs))
                 (not (path:exists? dir-full-path)))
            ;; Make it.
            (progn

              ;; make-directory helpfully has no data on what it returns or why or when
              ;; or anything. But it returns nil on success so... super useful guys.
              (make-directory dir-full-path)

              ;; How about we report something actually useful maybe?
              ;; Full path of created dir on... success?
              ;; Nil on folder non-existance.
              (nub:debug:func/return
                  :taskspace
                  func/name
                  func/tags
                (if (path:exists? dir-full-path)
                    dir-full-path
                  nil)))

          ;; Failed check; complain and return nil.
          (nub:debug
              :taskspace
              func/name
              func/tags
            '(:line:each
              "Failed checks:"
              "  naming:verify:    %S"
              "  not dupes?:       %S"
              "  not pre-existing: %S")
            (int<taskspace>:naming:verify group description)
            (not (cl-some (lambda (x) (int<taskspace>:dir= group
                                                           description
                                                           x
                                                           'description))
                          date-dirs))
            (not (path:exists? dir-full-path)))
          (nub:debug:func/return
              :taskspace
              func/name
              func/tags
            nil)))))
;; (int<taskspace>:dir:create :work "testcreate" nil)


(defun int<taskspace>:dir= (group name dir part)
  "Is NAME equal to a certain PART of DIR?

GROUP should be return value from `int<taskspace>:group' (assoc from
`taskspace:groups').

PART should be one of: 'date 'number 'description

Return nil/non-nil."
  ;; don't accept nulls
  (unless (or (null name) (null dir) (null part))
    ;; strip dir down to file name and
    ;; strip file name down to part (if non-nil part)
    (let* ((dir-name (file:name dir))
           (dir-part (int<taskspace>:naming:split group dir-name part)))
      (if (null dir-part)
          nil ;; don't accept nulls
        ;; else, usable data
        ;; check against input name
        (string= name dir-part)))))
;; (int<taskspace>:dir= :home "2000" "c:/zort/troz/2000_0_testcase" 'date)


(defun int<taskspace>:dir:list:all (group)
  "List all children directories in a taskspace.

GROUP should be return value from `int<taskspace>:group' (assoc from
`taskspace:groups').

Get children directories of taskspace/dir, ignoring:
  (int<taskspace>:config group :dir/tasks/ignore)."
  (let (task-dirs) ;; empty list for return value
    ;; loop on each file in the directory
    (dolist (file
             (path:children (int<taskspace>:config group :dir/tasks)
                            :absolute-paths
                            :dir) ;; Only want dirs.
             task-dirs)
      ;; ignore things in ignore list
      (when (not (member (file:name file)
                         (int<taskspace>:config group :dir/tasks/ignore)))
        (push file task-dirs)))
  ;; dolist returns our constructed list since we put it as `result'
  ;; so we're done
  ))
;; (message "%s" (int<taskspace>:dir:list:all :home))


;; Get all, pare list down to date-str, return.
(defun int<taskspace>:dir:list:date (group date-str)
  "Get any/all taskspaces for today.

GROUP should be return value from `int<taskspace>:group' (assoc from
`taskspace:groups').

DATE-STR should be a string of the date."
  (unless (null date-str)
    (let ((task-dirs (int<taskspace>:dir:list:all group))
          date-dirs) ;; return val
      (dolist (dir task-dirs date-dirs)
        (when (int<taskspace>:dir= group date-str dir 'date)
          (push dir date-dirs))))))
;; (int<taskspace>:dir:list:date :home "2020-03-13")
;; (int<taskspace>:dir:list:date :work "2020-08-26")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :taskspace 'path)
