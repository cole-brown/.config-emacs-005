;;; modules/dev-env/taskspace/init.el --- Per-task notes & files. -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2019-04-24
;; Timestamp:  2023-09-13
;; Version:    2.3
;;
;; TODO: Requirements: cl-lib, seq, dash, org,
;; TODO: Requirements (imp):  nub, dlv, elisp:util:units,
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; taskspace.el is a KISS taskspace (workspace) generator/manager.
;;
;; FAQ:
;;   1) Can it do X?
;;      - No... I really meant simple.
;;   2) Can it do multiple taskspace roots?
;;      - Yes... I had to make it less simple.
;;
;; It can make a folder based on a simple dating and numbering scheme, with a
;; simple description tacked on for human usability.
;;
;; It can copy files into the new taskspace. It can generate files based on a
;; static string or a function supplied in taskspace vars.
;;
;; It can copy the taskspace's full path or name to the kill ring/clipboard.
;;
;; It can open the taskspace dir itself (or the taskspace parent dir)
;; in a buffer.
;;
;; It can 'deal' with these kind of taskspaces:
;;   - Self-Contained
;;;    - Taskspace dir contains:
;;       - notes,
;;       - data,
;;       - etc.
;;    - No other directories or files.
;;   - Split (taskspace notes are separate; taskspace dir contains data, etc)
;;;    - Taskspace dir contains:
;;       - data,
;;       - etc.
;;    - Notes file exists separately.
;;
;;
;;------------------------------
;; Commands:
;;------------------------------
;;   The Main Command:
;;     `taskspace:create'
;;       - Create a new taskspace. Will prompt for the short description.
;;         - e.g. description of "2019-01-01_2_some-task-name" is
;;           "some-task-name".
;;
;;   DWIM Commands:
;;     - Accepts numeric prefix arg.
;;       - 0 or no prefix: Today's date
;;       - positive: Future date; N days from now.
;;       - negative: Past date; N days back.
;;     - DWIM means:
;;       - If none for date: Create.
;;       - If just one existing: Return it.
;;       - If multiple: Choose from prompt of options.
;;     `taskspace:dwim:dir'
;;       - Returns fully qualified path to taskspace.
;;         - e.g. "c:/home/user/taskspace/2019-01-01_2_some-task-name"
;;     `taskspace:dwim:name'
;;       - Returns taskspace (directory) name only.
;;         - e.g. "2019-01-01_2_some-task-name"
;;
;;   Other Commands:
;;     `taskspace:dired:task'
;;       - Opens the directory of a task in emacs
;;         (uses find-file, so defaults to dired-mode buffer).
;;     `taskspace:dired:root'
;;       - Opens the directory of all tasks in emacs (aka `(int<taskspace>:config group :dir/tasks)')
;;         (uses find-file, so defaults to dired-mode buffer).
;;     `taskspace:shell'
;;       - Opens a shell.
;;       - Use `taskspace:dwim:dir' to determine which taskspace is
;;         intended from the context.
;;
;;
;;------------------------------
;; Settings:
;;------------------------------
;; See 'General Settings' header to find out what all can be customized per
;; group right now.
;;
;;
;;------------------------------
;; Use-Package Config, Simple:
;;------------------------------
;;
;;  (use-package taskspace)
;;
;;
;;------------------------------
;; Use-Package Config, Multi-Group:
;;------------------------------
;;
;; (use-package taskspace
;;   ;;------------------------------
;;   :init
;;   ;;------------------------------
;;
;;   ;;---
;;   ;; General (Non-Per-Domain) Init...
;;   ;;---
;;   (defun my/taskspace/generate (group taskname taskpath)
;;     "NOTE: Could be redefined later for more work-specific details, so check
;; e.g. 'finalize-domain-secret.el' for a redef. Or 'C-h f
;; my/taskspace/generate' and see what file it's defined
;; in.
;; "
;;     ;; Format:
;;     ;; header snippet key
;;     ;;
;;     ;; taskname
;;     ;; taskpath
;;     ;;
;;     ;; 'mkdir cmd'
;;     ;;
;;     ;; fancy box to separate this stuff from start of normal notes
;;     (format (concat "%s\n" ;; header
;;                     "\n"
;;                     "#+TASKSPACE: %s\n" ;; taskpath
;;                     "%s\n" ;; taskname
;;                     "\n"
;;                     "%s\n" ;; mkdir cmd for remote servers
;;                     "\n"
;;                     "%s\n" ;; fancy box top
;;                     "%s\n" ;; fancy box middle
;;                     "%s\n" ;; fancy box bottom
;;                     "\n\n")
;;             "my-header-snippet"
;;             taskpath
;;             taskname
;;             (format "mkdir ~/temp/%s" taskname)
;;             "     ┌┬┬┬──────────────────────────────────────────────────────────────┬┬┬┐"
;;             "     ├┼┼┤                             ...                              ├┼┼┤"
;;             "     └┴┴┴──────────────────────────────────────────────────────────────┴┴┴┘"
;;             ))
;;
;;   ;;---
;;   ;; "Home" Domain
;;   ;;---
;;
;;   ;; I can redef later if I want different ones..
;;   (defalias 'my/taskspace/generate/home 'my/taskspace/generate)
;;
;;   (defvar my/taskspace/group/home
;;     '((:type/notes        :self-contained)
;;       (:format/datetime   my/datetime/format/yyyy-mm-dd)
;;       (:dir/tasks         my/taskspace/path/tasks/home)
;;       (:dir/notes         my/taskspace/path/notes/home)
;;       (:file/new/generate ((".projectile" "") ;; projectile: empty file
;;                            ;; notes.org: setup with org header snippet
;;                            ;; ready to go
;;                            ((int<taskspace>:config :home :file/notes)
;;                             my/taskspace/generate/home))))
;;     "Custom settings for my `:home' taskspace group.")
;;
;;   ;;---
;;   ;; "Work" Domain
;;   ;;---
;;
;;   ;; I can redef later if I want different ones..
;;   (defalias 'my/taskspace/generate/work 'my/taskspace/generate)
;;
;;   (defvar my/taskspace/group/work
;;     '((:type/notes        :noteless)
;;       (:format/datetime   my/datetime/format/yyyy-mm-dd)
;;       (:dir/tasks         my/taskspace/path/tasks/work)
;;       (:dir/notes         my/taskspace/path/notes/work)
;;       (:file/new/generate ((".projectile" "") ;; projectile: empty file
;;                            ;; notes.org: setup with org header snippet
;;                            ;; ready to go
;;                            ((int<taskspace>:config :home :file/notes)
;;                             my/taskspace/generate/home))))
;;     "Custom settings for my `:work' taskspace group.")
;;
;;   ;;------------------------------
;;   :custom
;;   ;;------------------------------
;;
;;   (taskspace:groups
;;    '((:work    "Work Taskspace" my/taskspace/group/work)
;;      (:home    "Home Taskspace" my/taskspace/group/home)
;;      (:default "Defaults"       taskspace:group:default))))
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Set imp Root.
;;------------------------------------------------------------------------------

(imp:path:root/set :taskspace
                   (imp:path:current:dir)
                   "init.el")


;;------------------------------------------------------------------------------
;; Load Files
;;------------------------------------------------------------------------------

(imp:timing
    '(:taskspace)
    (imp:file:current)
    (imp:path:current:dir)

  (imp:load :feature  '(:taskspace nub)
            :filename "nub")
  ;; Initialize nub before going any further for debug/error/etc output.
  (int<taskspace>:nub:init)

  (imp:load :feature  '(:taskspace group)
            :filename "group")

  (imp:load :feature  '(:taskspace naming)
            :filename "naming")

  (imp:load :feature  '(:taskspace path)
            :filename "path")

  (imp:load :feature  '(:taskspace prompt)
            :filename "prompt")

  (imp:load :feature  '(:taskspace taskspace)
            :filename "taskspace")

  (imp:load :feature  '(:taskspace keybinds)
            :filename "keybinds")

  ;; End load timing.
  )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide:with-emacs :taskspace)
