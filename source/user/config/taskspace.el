;;; user/config/taskspace.el --- taskspace & notes -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2026-04-14
;; Timestamp:  2026-04-22
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Taskspace & Notes
;;
;;; Code:

(imp-require datetime format)


;;------------------------------------------------------------------------------
;; Taskspace
;;------------------------------------------------------------------------------

(imp user:/packages/taskspace/init)


;;------------------------------------------------------------------------------
;; Taskspace
;;------------------------------------------------------------------------------

(use-package taskspace
  ;; This is my own package, so...
  ;;   1. Don't try to install.
  :ensure nil

  ;; Probably need `taskspace' to be a real package?
  ;;   2. Here's where it is; add this dir to the `load-path'.
  ;; :load-path innit:path:package:mis


  ;;------------------------------
  :config
  ;;------------------------------

  (taskspace-keybind-keymap --/keymap/leader "n")

  ;;---
  ;; Error Checking
  ;;---
  (cond ((not (featurep 'secret.d))
         (error "%S: Secrets are not loaded. Feature `%S' is required."
                ;; TODO(imp): make this a func. `imp-feature-current-file'?
                (imp-feature-normalize 'user (imp-path-relative 'user (imp-path-sans-extension (imp-path-current-file))))
                'secret.d))

        ((not (fboundp #'/secret/get))
         (error "%S: Required function `%S' does not exist."
                (imp-feature-normalize 'user (imp-path-relative 'user (imp-path-sans-extension (imp-path-current-file))))
                #'/secret/get))

        ((not (bound-and-true-p --/secret/taskspace))
         (error "%S: Required settings plist `%S' does not exist."
                (imp-feature-normalize 'user (imp-path-relative 'user (imp-path-sans-extension (imp-path-current-file))))
                '--/secret/taskspace)))

  ;;---
  ;; General (Non-Per-Domain) Init...
  ;;---
  (defun --/taskspace/generate (group taskname taskpath filepath)
    "File template for new taskspace note files."
    (mapconcat
     #'identity
     (list
      ;;---
      ;; Header (just a copy of `yasnippet' "/header")
      ;;---
      (format (mapconcat #'identity
                         '("#+TITLE:     %s"
                           "#+AUTHOR:    %s"
                           "#+EMAIL:     %s"
                           "#+DATE:      %s"
                           "#+TIMESTAMP: 0000-00-00") ; Auto-filled by Emacs' `time-stamp' feature.
                         "\n")
              ;; TITLE:
              (let ((filename/no-ext (file-name-nondirectory (file-name-sans-extension filepath))))
                ;; Is filename "<something>.notes.*"? We already stripped the (actual file
                ;; extension) ".*" so now we can check if the "extension" is ".notes".
                (if (string= (file-name-extension filename/no-ext) "notes")
                    ;; Reverse "something.notes" into "Notes.Something".
                    (concat
                     (capitalize (file-name-extension filename/no-ext))
                     "."
                     (mapconcat #'capitalize
                                (split-string (file-name-sans-extension filename/no-ext) "_")
                                "_"))
                  (capitalize filename/no-ext)))
              ;; AUTHOR:
              (or (/secret/get --/secret/pii group :name)
                  (user-full-name)
                  "TODO: Set name")
              ;; EMAIL:
              (or (/secret/get --/secret/pii group :email)
                  (message-user-mail-address)
                  "TODO: Set email")
              ;; DATE:
              (datetime:format :rfc-3339:date))

      ;;---
      ;; Blank
      ;;---
      ""

      ;;---
      ;; Task Name & Path
      ;;---
      (format (mapconcat #'identity
                         '("#+TASKSPACE: %s" ; taskpath
                           "#+TASKSPACE: %s" ; translated taskpath
                           "%s"              ; taskname
                           ""
                           "%s")             ; mkdir cmd for remote servers
                         "\n")
              taskpath
              (path:translate :auto :auto taskpath)
              taskname
              (format "mkdir %s" taskname))

      ;;---
      ;; Blank
      ;;---
      ""

      ;;---
      ;; Fancy Header / Notes Separator
      ;;---
      "     ┌┬┬┬──────────────────────────────────────────────────────────────┬┬┬┐"
      "     ├┼┼┤                             ...                              ├┼┼┤"
      "     └┴┴┴──────────────────────────────────────────────────────────────┴┴┴┘"

      ;;---
      ;; Blank
      ;;---
      ""
      ""
      (if (eq group :work)
          "* JIRA: TODO-123"
        "* Description")
      ""
      "")
     "\n"))
  ;; (--/taskspace/generate :test "test_task" "~/path/to/taskspace/test_task.notes.ext" "~/notes/test_task.notes.ext")

  ;;---
  ;; "Home" Domain
  ;;---

  (let* ((group     :home)
         (group-str (str:normalize:symbol group))
         (path-tasks (or (/secret/get --/secret/taskspace group    :path :tasks)
                         (/secret/get --/secret/taskspace :default :path :tasks)))
         (path-notes (or (/secret/get --/secret/taskspace group    :path :notes)
                         (/secret/get --/secret/taskspace :default :path :notes)))
         (type-notes :noteless))
    (unless path-tasks
      (error "%S: group:`%S': Required task path setting does not exist."
             group
             (imp-feature-normalize 'user (imp-path-relative 'user (imp-path-sans-extension (imp-path-current-file))))))
    (when (eq type-notes :noteless)
      (unless path-notes
        (error "%S: group:`%S': Required notes path setting does not exist."
               (imp-feature-normalize 'user (imp-path-relative 'user (imp-path-sans-extension (imp-path-current-file)))))))

    ;; And create our group custom settings.
    (add-to-list 'taskspace-groups
                 (list group
                       (format "%S Taskspace" (str:case/string:to:title group-str))
                       `((:format/datetime ,(datetime:format:get :rfc-3339:date))
                         (:type/notes      ,type-notes)
                         (:dir/tasks       ,path-tasks)
                         (:dir/notes       ,path-notes)
                         (:file/new/generate
                          ;; Empty projectile file.
                          ((".projectile" "")
                           ;; notes.org: setup with org header snippet ready to go
                           ((taskspace--config ,group :file/notes)
                            --/taskspace/generate))))))

    ;; TODO(dlv): DLVs should have been set during secrets init via `system:multiplexer:dlv:add'.
    )


  ;;---
  ;; "Work" Domain
  ;;---
  (let* ((group     :work)
         (group-str (str:normalize:symbol group))
         (path-tasks (or (/secret/get --/secret/taskspace group    :path :tasks)
                         (/secret/get --/secret/taskspace :default :path :tasks)))
         (path-notes (or (/secret/get --/secret/taskspace group    :path :notes)
                         (/secret/get --/secret/taskspace :default :path :notes)))
         (type-notes :noteless))
    (unless path-tasks
      (error "%S: group:`%S': Required task path setting does not exist."
             group
             (imp-feature-normalize 'user (imp-path-relative 'user (imp-path-sans-extension (imp-path-current-file))))))
    (when (eq type-notes :noteless)
      (unless path-notes
        (error "%S: group:`%S': Required notes path setting does not exist."
               (imp-feature-normalize 'user (imp-path-relative 'user (imp-path-sans-extension (imp-path-current-file)))))))

    ;; And create our group custom settings.
    (add-to-list 'taskspace-groups
                 (list group
                       (format "%S Taskspace" (str:case/string:to:title group-str))
                       `((:format/datetime ,(datetime:format:get :rfc-3339:date))
                         (:type/notes      ,type-notes)
                         (:dir/tasks       ,path-tasks)
                         (:dir/notes       ,path-notes)
                         (:file/new/generate
                          ;; Empty projectile file.
                          ((".projectile" "")
                           ;; notes.org: setup with org header snippet ready to go
                           ((taskspace--config ,group :file/notes)
                            --/taskspace/generate))))))

    ;; TODO(dlv): DLVs should have been set during secrets init via `system:multiplexer:dlv:add'.
    ))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide user (imp-path-relative 'user (imp-path-sans-extension (imp-path-current-file))))
