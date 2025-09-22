;;; mantle/config/org/journal.el --- Configure Org-Journal -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2023-03-16
;; Timestamp:  2025-09-22
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Configure Org-Journal.
;;
;;; Code:


(imp:require :nub)
(imp:require :datetime)
(imp:require :innit)
(imp:require :jerky)
(imp:require :path)
(imp:require :buffer 'search)
(imp:require :buffer 'name)
(imp:require :mode 'org)


;;------------------------------------------------------------------------------
;; Org-Journal
;;------------------------------------------------------------------------------

(imp:use-package org-journal
  :after org

  ;;------------------------------
  :init
  ;;------------------------------

  ;;---
  ;; "Home" Domain
  ;;---
  (jerky:set 'org 'journal 'file 'format
             :namespace :home
             :value (concat (datetime:format:get :iso-8601:date)
                            ;; TODO: 'notebook' not quickest to
                            ;; auto-complete to. Find better.
                            ".notebook.org")
             :docstr "`org-journal-file-format' for :home")

  ;;---
  ;; "Work" Domain
  ;;---
  (jerky:set 'org 'journal 'file 'format
             :namespace :work
             :value (concat (datetime:format:get :iso-8601:date)
                            ;; TODO: 'logbook' not quickest to
                            ;; auto-complete to. Find better.
                            ".logbook.org")
             :docstr "`org-journal-file-format' for :work")

  ;;---
  ;; Domain Keybinds
  ;;---
  (defun mode:org/journal:namespaced (namespace command &rest args)
    "Run an org-journal COMMAND in NAMESPACE.

Sets (lexical context) all org-journal custom vars related to NAMESPACE.
Then runs COMMAND interactively with ARGS."
    ;; Interactive for maintaining `called-interactively-p', I think? I don't remember exactly...
    (interactive)
    (let ((org-journal-file-format (jerky:get 'org 'journal 'file 'format
                                              :namespace namespace))
          (org-journal-dir (jerky:get 'path 'org 'journal
                                      :namespace namespace)))
      (apply #'funcall-interactively command args)))
  ;; (mode:org/journal:namespaced :home #'message "%s %s" org-journal-file-format org-journal-dir)
  ;; (mode:org/journal:namespaced :work #'message "%s %s" org-journal-file-format org-journal-dir)


  ;;------------------------------
  ;; New File Header
  ;;------------------------------

  (defun mode:org/journal:file-header (time)
    "Custom function to create a new journal file's header.

TIME is... nil?!
TIME is supposed to be \"a time value.\"
TIME was expected to be a current time suitable for `format-time-string'.
But TIME is just nil."
    ;; TIME is supposed to be "a time value" according to
    ;; `org-journal-file-header' docstr, but I only get nil.
    ;; So make sure TIME is some sort of time value.
    (let* ((time (or time
                     (datetime:now)))
           (date/month/1st
            ;; NOTE: Not sure what TIME is passed in... I assume "now"? So backdate to start of the month?
            (datetime:replace time
                              :day    1
                              :hour   0
                              :minute 0
                              :second 0))
           (timestamp/month/1st
            (datetime:format
             'rfc-3339 'date
             :time (datetime:convert date/month/1st :lisp:time))))
      (str:format/newline
       '("#+TITLE:       %s"
         "#+DESCRIPTION: TODO: A quip for today."
         "#+AUTHOR:      %s"
         "#+EMAIL:       %s"
         "#+DATE:        %s"
         "#+TIMESTAMP:   0000-00-00" ; Auto-filled by Emacs' `time-stamp' feature.
         ""
         "- TODOs are here:"
         "  - file:%s"
         ""
         "- ‽☢‽☢ <urgentest of titles> ☢‽☢‽"
         "  - oh no...")
       ;; TITLE:
       (let ((file-name (file-name-nondirectory (file-name-sans-extension (buffer-file-name)))))
         (if (string= (file-name-extension file-name) "notes")
             (concat
              (capitalize (file-name-extension file-name))
              "."
              (mapconcat #'capitalize
                         (split-string (file-name-sans-extension file-name) "_")
                         "_"))
           (capitalize file-name)))
       ;; DESCRIPTION:
       ;;   - No format arg, currently.
       ;; AUTHOR:
       (signature:string 'id 'name :default (user-full-name))
       ;; EMAIL:
       (signature:get 'id 'email :namespace (jerky:namespace:get) :default (message-user-mail-address))
       ;; DATE:
       timestamp/month/1st
       ;; "TODOs are here:"
       (path:abbreviate
        (path:join (jerky:get 'path 'lily)
                   "todos"
                   (str:normalize:any (jerky:namespace:get))
                   (format "%s.todos.org"
                           timestamp/month/1st))))))
  ;; (mode:org/journal:file-header (datetime:now))


  ;;------------------------------
  :custom
  ;;------------------------------

  (org-journal-file-header #'mode:org/journal:file-header)

  (org-journal-dir (jerky:get 'path 'org 'journal
                              :namespace (jerky:get 'namespace 'system)))

  ;; Tack (full) day name onto our format for the org-journal headline.
  (org-journal-date-format (concat (datetime:format:get :iso-8601:date)
                                   ", %A"))
  ;; This can be a function if more is wanted. E.g. inserting new header text
  ;; into empty files.
  ;;  - https://github.com/bastibe/org-journal#journal-file-content
  ;; TODO: Do stuff when file is empty.

  ;; A year per file. Could do monthly if too big. Weekly and daily are also
  ;; options. Daily is the default.
  ;; [2019-12-03] - A month per file. Year is getting too big...
  (org-journal-file-type 'monthly)

  ;; org-journal-file-format: Make it a bit more ISO-ish (yyyy-mm-dd).
  ;;   - default:   yyyymmdd
  ;;   - better:    yyyy-mm-dd.org
  ;; But those both are difficult to switch to when various other buffers open,
  ;; so we'll go to this:
  ;;   - betterer:  yyyy-mm-dd.journal.org
  ;; Or, even better, leave it up to someone else to decide and get it from a jerky key:
  (org-journal-file-format (jerky:get 'org 'journal 'file 'format
                                      :namespace (jerky:get 'namespace 'system)))


  ;;------------------------------
  :config
  ;;------------------------------

  ;;------------------------------
  ;; customization
  ;;------------------------------

  ;; This is only the path to the default namespace, so multi-namespace
  ;; shenanigans have to constantly mess with this, I think?
  (unless (dlv:var:safe/predicate? 'org-journal-dir)
    ;; It's marked as risky - force it to safe?
    (dlv:var:safe/predicate 'org-journal-dir #'file-directory-p :quiet))


  ;;------------------------------
  ;; configuration
  ;;------------------------------

  ;; Insert dir local variable(s) for namespaces.
  (dolist (namespace '(:work :home))
    (dlv:set (jerky:get 'path 'org 'journal :namespace namespace)
             'org-journal-mode
             (list 'org-journal-dir
                   (jerky:get 'path 'org 'journal :namespace namespace)
                   :safe))))


;;------------------------------
;; Keybinds : Meow
;;------------------------------

(imp:use-package org-journal
  :when  (imp:flag? :keybinds +meow)
  :after (:and meow
          org)

  ;;------------------------------
  :init
  ;;------------------------------

  ;;------------------------------
  ;; `General'
  ;;------------------------------

  (defun mode:org/journal:keybind/meow (namespace letter)
    "Create keybinds for NAMESPACE in global leader under LETTER.

NAMESPACE must be a keyword. NAMESPACE must exist as a namespace in Jerky.

LETTER must be a 1-character string."
    (if (not (jerky:namespace:has namespace))
        ;; TODO: Raise `innit:error:???' instead of warn?
        (nub:warning
            :innit
            (imp:path:current:file/relative :mantle)
          "No `%1$S' namespace in Jerky; cannot set up `%2$S' keybinds for `%1$S'."
          namespace
          'org-journal)
      (if (not (jerky:get 'path 'org 'journal :namespace namespace))
          ;; TODO: Raise `innit:error:???' instead of warn?
          (nub:warning
              :innit
              (imp:path:current:file/relative :mantle)
            "No `%1$S' key in `%2$S' namespace in Jerky; cannot set up `%3$S' keybinds for `%2$S'."
            (jerky:key:string 'path 'org 'journal)
            namespace
            'org-journal)

        ;; Ok; make our NAMESPACE keybinds!
        (keybind:leader/global:def
          :infix (keybind:infix "n" "j" letter) ;; notes -> journal -> NAMESPACE journal
          "" (list nil :which-key (format "Journal: `%S'..." namespace)) ;; Infix Title

          letter (list (elisp:cmd (mode:org/journal:namespaced namespace
                                                               #'org-journal-new-entry
                                                               current-prefix-arg))
                       :which-key (format "`%S' - New Entry" namespace))

          "J" (list (elisp:cmd (mode:org/journal:namespaced namespace
                                                            #'org-journal-new-scheduled-entry
                                                            current-prefix-arg))
                    :which-key (format "`%S' - New Scheduled Entry" namespace))

          "v" (list (elisp:cmd (mode:org/journal:namespaced namespace
                                                            #'org-journal-open-current-journal-file))
                    :which-key (format "`%S' - Visit Journal" namespace))

          "s" (list (elisp:cmd (mode:org/journal:namespaced namespace
                                                            #'org-journal-search-forever
                                                            nil))
                    :which-key (format "`%S' - Search Journal" namespace))))))


  (defun mantle:meow/keybind/general:journal ()
    "Create the \"Journal...\" keybinds in `general' for `meow'."
    (keybind:leader/global:def
      :infix (keybind:infix "n" "j")         ; notes -> journal
      "" (list nil :which-key "Journal...")) ; Infix Title

    (mode:org/journal:keybind/meow :work "w")
    (mode:org/journal:keybind/meow :home "h"))


  ;;------------------------------
  ;; `Transient'
  ;;------------------------------

  (defun mantle:meow/keybind/transient:journal ()
    "Create the \"Journal...\" keybinds in `transient' for `meow'."
    ;;---
    ;; Make sure we have requried namespaces and paths.
    ;;---
    ;; TODO-meow: Do I have an easy list of valid domains/namespaces for this
    ;; system/machine/computer? Use that instead of hard-coding.
    (dolist (namespace '(:work :home))
      (unless (jerky:namespace:has namespace)
        (nub:error
            :innit
            macro<imp>:path/file
          "No `%1$S' namespace in Jerky; cannot set up `%2$S' keybinds for `%1$S'."
          namespace
          'org-journal))
      (unless (jerky:get 'path 'org 'journal :namespace namespace)
        (nub:error
            :innit
            macro<imp>:path/file
          "No `%1$S' key in `%2$S' namespace in Jerky; cannot set up `%3$S' keybinds for `%2$S'."
          (jerky:key:string 'path 'org 'journal)
          namespace
          'org-journal)))

    ;;---
    ;; Ok; create the keybinds.
    ;;---
    (transient-define-suffix mantle:meow/transient:notes/org-journal:entry/new:work ()
      "`org-journal' new entry for `:work' domain"
      :key "w"
      :description (format "`%S' - New Entry" :work)
      (interactive)
      (mode:org/journal:namespaced :work
                                   #'org-journal-new-entry
                                   current-prefix-arg))

    (transient-define-suffix mantle:meow/transient:notes/org-journal:entry/new-scheduled:work ()
      "`org-journal' new scheduledentry for `:work' domain"
      :key "W"
      :description (format "`%S' - New Scheduled Entry" :work)
      (interactive)
      (mode:org/journal:namespaced :work
                                   #'org-journal-new-scheduled-entry
                                   current-prefix-arg))

    (transient-define-suffix mantle:meow/transient:notes/org-journal:journal/visit:work ()
      "`org-journal' visit journal for `:work' domain"
      :key "v"
      :description (format "`%S' - Visit Journal" :work)
      (interactive)
      (mode:org/journal:namespaced :work
                                   #'org-journal-open-current-journal-file))

    (transient-define-suffix mantle:meow/transient:notes/org-journal:journal/search:work ()
      "`org-journal' search journal for `:work' domain"
      :key "s"
      :description (format "`%S' - Search Journal" :work)
      (interactive)
      (mode:org/journal:namespaced :work
                                   #'org-journal-search-forever))

    (transient-define-suffix mantle:meow/transient:notes/org-journal:entry/new:home ()
      "`org-journal' new entry for `:home' domain"
      :key "h"
      :description (format "`%S' - New Entry" :home)
      (interactive)
      (mode:org/journal:namespaced :home
                                   #'org-journal-new-entry
                                   current-prefix-arg))

    (transient-define-suffix mantle:meow/transient:notes/org-journal:entry/new-scheduled:home ()
      "`org-journal' new scheduledentry for `:home' domain"
      :key "H"
      :description (format "`%S' - New Scheduled Entry" :home)
      (interactive)
      (mode:org/journal:namespaced :home
                                   #'org-journal-new-scheduled-entry
                                   current-prefix-arg))

    (transient-define-suffix mantle:meow/transient:notes/org-journal:journal/visit:home ()
      "`org-journal' visit journal for `:home' domain"
      :key "v"
      :description (format "`%S' - Visit Journal" :home)
      (interactive)
      (mode:org/journal:namespaced :home
                                   #'org-journal-open-current-journal-file))

    (transient-define-suffix mantle:meow/transient:notes/org-journal:journal/search:home ()
      "`org-journal' search journal for `:home' domain"
      :key "s"
      :description (format "`%S' - Search Journal" :home)
      (interactive)
      (mode:org/journal:namespaced :home
                                   #'org-journal-search-forever))

    (transient-define-prefix mantle:meow/transient:notes/org-journal:work ()
      "`org-journal' `:work' domain keybinds"
      ["`org-journal' :work"
       (mantle:meow/transient:notes/org-journal:entry/new:work)
       (mantle:meow/transient:notes/org-journal:entry/new-scheduled:work)
       (mantle:meow/transient:notes/org-journal:journal/visit:work)
       (mantle:meow/transient:notes/org-journal:journal/search:work)])

    (transient-define-prefix mantle:meow/transient:notes/org-journal:home ()
      "`org-journal' `:home' domain keybinds"
      ["`org-journal' :home"
       (mantle:meow/transient:notes/org-journal:entry/new:home)
       (mantle:meow/transient:notes/org-journal:entry/new-scheduled:home)
       (mantle:meow/transient:notes/org-journal:journal/visit:home)
       (mantle:meow/transient:notes/org-journal:journal/search:home)])

    (transient-append-suffix 'mantle:meow/transient:notes
      '(0 -1) ; Append after last group/suffix in the first group.
      ["Journal"
       ("jw" "`org-journal' :work" mantle:meow/transient:notes/org-journal:work)
       ("jh" "`org-journal' :home" mantle:meow/transient:notes/org-journal:home)])
    ;; (mantle:meow/transient:notes)

    ;; TODO: Do I want this again?
    ;; ;; 'C-c <tab>' to show headings only (no top parent notes, no
    ;; ;; children notes, just headings). Org-Mode had 'C-c <tab>' as
    ;; ;; outline-show-children, which only shows direct children
    ;; ;; headings, not all descendants' headings.
    ;; ;; https://yiufung.net/post/org-mode-hidden-gems-pt1/
    ;; ("C-c <tab>" . #'org-kill-note-or-show-branches)

    ;; TODO: Do I want/need this again?
    ;; ;;------------------------------
    ;; ;; Org-Mode & the Thousand Different "RET" Key Functions
    ;; ;;------------------------------
    ;;
    ;; ;; Auto-indent is a bit odd... Try swapping around RET and S-RET these so the literal newline is on normal enter? Also, `evil-org-return'
    ;; ;; is... fucked up. It's description says 'Pressing return twice cancels the continuation of the itemlist or table.', but it's not used for
    ;; ;; continuing lists. "C-RET" is - `+org/insert-item-below'.
    ;; ;; But you can do "C-RET C-u" to delete the indentation...
    ;; ;; ...But you can keep doing that to delete previous lines. So - fuck that noise.
    ;; ;; And it's attempt to detect that you're on an empty line (eolp) and (bolp) to do a literal `org-return' isn't working for me?
    ;; ;; ...Am I using `evil-org-return' even? I don't think so...
    ;; ;; +org/shift-return
    ;;
    ;; (after! evil-org
    ;;         (map! :map evil-org-mode-map
    ;;               ;; Fuck your "more intuitive RET keybinds", Doom.
    ;;               :i [return]   #'+org/shift-return
    ;;               :i "RET"      #'+org/shift-return
    ;;               :i [S-return] (cmd! (org-return electric-indent-mode))
    ;;               :i "S-RET"    (cmd! (org-return electric-indent-mode))
    ;;               ))
    )

  ;;------------------------------
  ;; Actually Create Keybinds:
  ;;------------------------------

  (if (imp:provided? :keybinds 'general 'meow)
      (mantle:meow/keybind/general:journal)
    (mantle:meow/keybind/transient:journal)))




;;------------------------------
;; Keybinds : Evil
;;------------------------------

(imp:use-package org-journal
  :when  (imp:flag? :keybinds +evil)
  :after (:and (:keybinds user general)
          evil
          evil-collection
          org
          evil-org)

  ;;------------------------------
  :init
  ;;------------------------------

  (defun mode:org/journal:keybind/evil (namespace letter)
    "Create keybinds for NAMESPACE in global leader under LETTER.

NAMESPACE must be a keyword. NAMESPACE must exist as a namespace in Jerky.

LETTER must be a 1-character string."
    (if (not (jerky:namespace:has namespace))
        ;; TODO: Raise `innit:error:???' instead of warn?
        (nub:warning
            :innit
            (imp:path:current:file/relative :mantle)
          "No `%1$S' namespace in Jerky; cannot set up `%2$S' keybinds for `%1$S'."
          namespace
          'org-journal)
      (if (not (jerky:get 'path 'org 'journal :namespace namespace))
          ;; TODO: Raise `innit:error:???' instead of warn?
          (nub:warning
              :innit
            (imp:path:current:file/relative :mantle)
            "No `%1$S' key in `%2$S' namespace in Jerky; cannot set up `%3$S' keybinds for `%2$S'."
            (jerky:key:string 'path 'org 'journal)
            namespace
            'org-journal)

        ;; Ok; make our NAMESPACE keybinds!
        (keybind:leader/global:def
          :infix (keybind:infix "n" letter) ;; notes -> NAMESPACE journal
          "" (list nil :which-key (format "Journal: `%S'..." namespace)) ;; Infix Title

          letter (list (elisp:cmd (mode:org/journal:namespaced namespace
                                                               #'org-journal-new-entry
                                                               current-prefix-arg))
                       :which-key (format "`%S' - New Entry" namespace))
          "j" (list (elisp:cmd (mode:org/journal:namespaced namespace
                                                            #'org-journal-new-entry
                                                            current-prefix-arg))
                    :which-key (format "`%S' - New Entry" namespace))

          "J" (list (elisp:cmd (mode:org/journal:namespaced namespace
                                                            #'org-journal-new-scheduled-entry
                                                            current-prefix-arg))
                    :which-key (format "`%S' - New Scheduled Entry" namespace))

          "v" (list (elisp:cmd (mode:org/journal:namespaced namespace
                                                            #'org-journal-open-current-journal-file))
                    :which-key (format "`%S' - Visit Journal" namespace))

          "s" (list (elisp:cmd (mode:org/journal:namespaced namespace
                                                            #'org-journal-search-forever
                                                            nil))
                    :which-key (format "`%S' - Search Journal" namespace))))))


  ;;------------------------------
  :config
  ;;------------------------------

  ;; Create the keybinds.
  (mode:org/journal:keybind/evil :work "w")
  (mode:org/journal:keybind/evil :home "h"))



;;------------------------------
;; Org-Journal Hacks
;;------------------------------

(imp:use-package org-journal
  :after org

  ;;------------------------------
  :config
  ;;------------------------------

  (advice-add #'org-journal-search-by-string :override #'mantle:advice:org/window:org-pop-to-buffer))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'org 'journal)
