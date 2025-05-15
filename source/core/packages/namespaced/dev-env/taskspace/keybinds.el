;;; modules/dev-env/taskspace/keybinds.el --- Keybinds -*- lexical-binding: t; -*-
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
;; Keybinds
;;
;;; Code:


;;---------------------------------taskspace------------------------------------
;;--                   Simple Taskspace / Task Management                     --
;;------------------------------------------------------------------------------

(imp:require :dlv)
(imp:require :nub)
(imp:require :taskspace 'taskspace)


;;------------------------------------------------------------------------------
;; DOOM!
;;------------------------------------------------------------------------------

(defun taskspace:keybind:doom ()
  "Create keybinds in Doom, or raise an error if not in Doom.

Create keybinds using Doom's `map!' macro.

Creates the taskspace keymap under the doom leader key (default SPC)"
  (if (null (symbolp 'doom!)) ;; Doom's loading function should mean this is doom?
      (nub:error
       :taskspace
       "taskspace:keybind:doom"
       "We are not in a Doom Emacs environment..? Cannot set Doom Emacs keybinds.")

    ;; Map under the Doom leader, probably.
    (map! :leader
          ;; Give it a description...
          :desc "taskspace"

          ;; ...and give it keybinds.
          (:prefix ("T" . "taskspace")

           ;; Top level commands...
           :desc "Create new..."  "T" #'taskspace:create
           :desc "Visit notes..." "v" #'taskspace:notes
           :desc "Shell..."       "s" #'taskspace:shell

           ;; 'Copy to kill ring' functions:
           (:prefix ("k" . "Kill...")

            :desc "dir"  "k" #'taskspace:dwim:dir
            :desc "name" "n" #'taskspace:dwim:name)

           ;; 'Open a dired buffer' functions:
           (:prefix ("d" . "dired")
            :desc "task dired buffer" "d" #'taskspace:dired:task
            :desc "root dired buffer" "r" #'taskspace:dired:root)))))


;;------------------------------------------------------------------------------
;; General
;;------------------------------------------------------------------------------

(defmacro taskspace:keybind:general (&rest args)
  "Create keybinds using the supplied `general' ARGS.

If using `evil', consider placing this in your config:
  ;; https://github.com/emacs-evil/evil-collection#making-spc-work-similarly-to-spacemacs
  ;; NOTE: `evil-collection' binds over SPC in many packages. To use SPC as a
  ;; leader key with `general', first set these override states:
  (setq general-override-states '(insert
                                  emacs
                                  hybrid
                                  normal
                                  visual
                                  motion
                                  operator
                                  replace))

Call this function with the desired keybind settings:
  (taskspace:keybind:general
    :prefix  \"SPC n\"
    :states  '(normal visual motion)
    :keymaps 'override)"
  (unless (featurep 'general)
    (nub:error
     :taskspace
     "taskspace:keybind:general"
     "`General' keybind feature is not loaded/defined! Cannot create keybinds."))

  `(progn
     ;; TODO: Do we expect:
     ;;   1) Just the leader key, and we'll make the "Taskspace..." menu and sub-menus and keybinds?
     ;;   2) The prefix for the "Taskspace..." menu, and we'll only make the keybinds?
     ;; ;;---
     ;; ;; Prefix Title
     ;; ;;---
     ;; (general-define-key ,@args
     ;;                     "" (list nil :which-key "Taskspace..."))

     ;;---
     ;; Top Level Commands...
     ;;---
     (general-define-key ,@args
                         :infix "t"
                         "" (list nil :which-key "Taskspace...") ;; Infix Title

                         "t" (list #'taskspace:create :which-key "New")
                         "v" (list #'taskspace:notes  :which-key "Visit")
                         "s" (list #'taskspace:shell  :which-key "Shell"))

     ;;---
     ;; 'Copy to Kill Ring' Functions:
     ;;---
     (general-define-key ,@args
                         :infix "t k"
                         "" (list nil :which-key "Copy/Kill...") ;; Infix Title

                         "k" (list #'taskspace:dwim:dir  :which-key "dir")
                         "n" (list #'taskspace:dwim:name :which-key "name"))

     ;;---
     ;; 'Open a Dired Buffer' Functions:
     ;;---
     (general-define-key ,@args
                         :infix "t d"
                         "" (list nil :which-key "Dired...") ;; Infix Title
                         "d" (list #'taskspace:dired:task :which-key "task's dired buffer")
                         "r" (list #'taskspace:dired:root :which-key "root's dired buffer"))))
;; (taskspace:keybind:general
;;     :prefix  "SPC n"
;;     :states  '(normal visual motion)
;;     :keymaps 'override)


;;------------------------------------------------------------------------------
;; Transient
;;------------------------------------------------------------------------------


(defmacro taskspace:keybind:transient/def (&rest args)
  "Define a `transient' prefix named: `taskspace:keybind:transient'.

Check for `transient' feature first and error if not provided. Done this way so
that `transient' itself isn't a dependency package.

You will need to bind this transient to whatever keys you want.
Example:
  (use-package taskspace
    ;; ...
    :config
    (taskspace:keybind:transient/def)
    (bind-key \"C-c C-t\" taskspace:keybind:transient))"
  (unless (featurep 'transient)
    (nub:error
     :taskspace
     "taskspace:keybind:transient"
     "`transient' feature is not loaded/defined! Cannot create keybinds."))

  `(transient-define-prefix taskspace:keybind:transient ()
    "Taskspace Menu"
    [["Taskspace..."
     ("t" "New"   taskspace:create)
     ("v" "Visit" taskspace:notes)
     ("s" "Shell" taskspace:shell)]

     ["Copy/Kill..."
      ("k" "dir"   taskspace:dwim:dir)
      ("n" "name"  taskspace:dwim:name)]

     ["Dired..."
      ("d" "task's dired buffer" taskspace:dired:task)
      ("r" "root's dired buffer" taskspace:dired:root)]]))
;; (taskspace:keybind:transient
;;     :prefix  "SPC n"
;;     :states  '(normal visual motion)
;;     :keymaps 'override)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :taskspace 'keybinds)
