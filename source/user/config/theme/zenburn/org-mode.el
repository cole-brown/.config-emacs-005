;;; source/user/org/theme/zenburn.el --- Zenburn Tweaks for Org Mode -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-05-12
;; Timestamp:  2025-11-18
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Fix some things that annoy me about Zenburn's `org-mode' faces.
;; Things like:
;;   - Make TODO keywords less bright (I think? I forget...).
;;   - Give TODO keywords a background.
;;     - For easier scanning for a headline.
;;   - Reduce number of headline colors used.
;;
;; Also while were at it, fix some things that are technically not related to
;; `zenburn':
;;   - Give all `org-mode' TODO keywords visual separation from the headline.
;;     - For easier scanning for a headline.
;;   - Make all `org-mode' TODO keywords the same width.
;;     - For much nicer headline structure when headlines are collapsed.
;;
;; NOTE: Loader of this file is responsible for delaying until after both theme
;; and org are loaded (eg `eval-after-load', `imp-eval-after').
;;
;;; Code:


;; Loader is responsible for ordering and delaying.
;; So throw a tantrum and die.
(unless (and (featurep 'zenburn-theme)
             (featurep 'org)
             (fboundp 'zenburn-with-color-variables))
  (error "`zenburn-theme' or `org' not loaded! Cannot tweak `org' for `zenburn'. %s %s %s @%s"
         (featurep 'zenburn-theme)
         (featurep 'org)
         (fboundp 'zenburn-with-color-variables)
         (imp-path-current-file)))

;; Oh, hey; you're alive. Good job.


(imp-require theme:/face)


;;------------------------------------------------------------------------------
;; Zenburn Theme Tweaks for Org-Mode
;;------------------------------------------------------------------------------

;; This macro is a giant `let' for all the zenburn colors from:
;;   - `zenburn-default-colors-alist'
;;   - `zenburn-override-colors-alist'
;; example: `zenburn-green-3' will be defined inside here
(zenburn-with-color-variables

  ;;---------------------------------------------------------------------------
  ;; Faces
  ;;---------------------------------------------------------------------------
  ;; I need to customize some `org' faces - they're not the greatest (IMO)
  ;; in Zenburn.

  ;; HACK: Face specs fed directly to `org-todo-keyword-faces' don't respect
  ;;       underlying faces like the `org-todo' face does, so we define our own
  ;;       intermediary faces that extend from org-todo.
  ;; NOTE: If I want to figure out these faces and shit better, org defines
  ;;       its faces in `org-faces.el' @ file:/usr/share/emacs/30.1/lisp/org/org-faces.el

  (defgroup --/zenburn/org-todo nil
    "A place to hide `org-todo' faces."
    :group 'tools
    ;; TODO: What is this repo called?
    ;; :link '(url-link "https://github.com/cole-brown/.config-emacs")
    )

  (defface --/zenburn/org-todo/current
    (list (cons
           ;; display type
           t
           ;; attributes
           '(:inherit (bold font-lock-constant-face org-todo))))
    "Face spec for 'current' todo sequence keyword(s)."
    :group '--/zenburn/org-todo)


  (defface --/zenburn/org-todo/project
    (list (cons
           ;; display type
           t
           ;; attributes
           '(:inherit (bold font-lock-doc-face org-todo))))
    "Face spec for 'project' todo sequence keyword(s)."
    :group '--/zenburn/org-todo)


  (defface --/zenburn/org-todo/holding
    (list (cons
           ;; display type
           t
           ;; attributes
           '(:inherit (bold warning org-todo))))
    "Face spec for 'on-hold' todo sequence keyword(s)."
    :group '--/zenburn/org-todo)


  (defface --/zenburn/org-todo/todo
    (list (cons
           ;; display type
           t
           ;; attributes
           ;; (list
           ;;  :weight 'bold
           ;;  :inherit 'warning)
           (list :foreground zenburn-magenta-01
                 :background zenburn-bg-05
                 :weight 'bold)))
    "Face spec for 'todo' keyword in todo sequence."
    :group '--/zenburn/org-todo)


  (defface --/zenburn/org-todo/done-good
    (list (cons
           ;; display type
           t
           ;; attributes
           ;; (list
           ;;  :inherit 'org-done)
           (list :background zenburn-bg-05
                 :inherit 'org-done)))
    "Face spec for good/successful 'done'/'finished' keyword in todo sequence."
    :group '--/zenburn/org-todo)


  (defface --/zenburn/org-todo/done-bad
    (list (cons
           ;; display type
           t
           ;; attributes
           ;; (list
           ;;  :inherit 'org-done)))
           (list :background zenburn-bg-05
                 :foreground zenburn-red-5)))
    "Face spec for bad/failed 'done'/'finished' keyword in todo sequence."
    :group '--/zenburn/org-todo)


  (defface --/zenburn/org-todo/info
    (list (cons
           ;; display type
           t
           ;; attributes
           ;; (list
           ;;  :inherit 'org-done)
           (list :background zenburn-bg-05
                 :foreground zenburn-bg+3)))
    "Face spec for info keyword in todo sequence."
    :group '--/zenburn/org-todo)


  (defface --/zenburn/org-todo/null
    (list (cons
           ;; display type
           t
           ;; attributes
           ;; (list
           ;;  :inherit 'org-done)
           (list :background zenburn-bg-05
                 :foreground zenburn-bg+3)))
    "Face spec for empty/null/spacer keyword in todo sequence."
    :group '--/zenburn/org-todo))


;;---------------------------------------------------------------------------
;; Configure Org-Mode
;;---------------------------------------------------------------------------

(zenburn-with-color-variables
  (theme:face:set! 'zenburn
    ;;---
    ;; Done states - little less dark.
    ;;---
    `(org-done                     :foreground ,zenburn-green-3)
    `(org-agenda-done              :foreground ,zenburn-bg+3)
    `(org-checkbox-statistics-done :foreground ,zenburn-green-3)

    ;; "Done" Headlines - success/fail/info/etc.
    ;;    - Would be nice to have green, red, and gray... but we only have the one 'done' face.
    ;;    - So a lighter gray?
    `(org-headline-done :foreground ,zenburn-fg-05)
    ;; `(org-headline-done :foreground zenburn-green-3)

    ;;---
    ;; Org Headlines - adjust a few to be less dark.
    ;;---
    ;; Some unique, headliney colors.
    `(outline-1 :foreground ,zenburn-orange)
    `(outline-2 :foreground ,zenburn-green+2)
    `(outline-3 :foreground ,zenburn-blue-1)
    `(outline-4 :foreground ,zenburn-red-1)
    ;; ...and repeat.
    `(outline-5 :foreground ,zenburn-orange)
    `(outline-6 :foreground ,zenburn-green+2)
    `(outline-7 :foreground ,zenburn-blue-1)
    `(outline-8 :foreground ,zenburn-red-1)
    ;; ...and 9+ starts over at `outline-1'.

    ;;---
    ;; Org TODO States
    ;;---
    ;; ├CURRENT┤
    `(--/zenburn/org-todo/current
          :foreground ,zenburn-violet
          :background ,zenburn-bg-05)
    ;; ├WAITING┤, ├HOLDING┤
    `(--/zenburn/org-todo/holding
          :foreground ,zenburn-magenta-03
          :background ,zenburn-bg-05)
    ;; ├PROJECT┤
    `(--/zenburn/org-todo/project
          :foreground ,zenburn-blue-3
          :background ,zenburn-bg-05)

    ;;---
    ;; Org ~inline code~ and =inline verbatim=
    ;;---
    ;; Want these to be different enough from all the outline levels.
    ;;   - `org-code' was exactly the same as `outline-1'.
    ;;   - `org-verbatim' was similar to `outline-2' and to code comments (green).
    ;; Change to inheriting from `org-cite', a teal affair.
    ;; Also make the background color of these slightly different from each other.
    `(org-code     :inherit org-cite :background ,zenburn-bg+05)
    `(org-verbatim :inherit org-cite :background ,zenburn-bg-05)

    ;;---
    ;; "#+DOC_KEYWORD" - needs to be slightly lighter.
    ;;---
    `(org-document-info-keyword :foreground ,zenburn-bg+3)))
;; `C-h o' to see help for these:
;; --/zenburn/org-todo/current
;; --/zenburn/org-todo/holding
;; --/zenburn/org-todo/project
;; org-checkbox
;; org-checkbox-statistics-todo
;; org-headline-todo
;; org-todo


;;------------------------------
;; Org TODO Sequence Keywords
;;------------------------------
;; Nicer TODO sequence for org headlines:
;;   1. More states!
;;   2. Use `mode:org:todo/keyword' to make all states the same length so they look beautiful in the org file.
;;   3. Update the faces to make them stand out from their surroundings better. They should:
;;      a. Have a slightly different background to make them pop out a bit.
;;      b. Have different foregrounds based on type of state (success/failure, todo/in-progress/done, etc).
;;      c. Not have a similar colors to the headline colors.
(let ((wrap "├─┤")
      ;; (wrap "[ ]")
      ;; (wrap "「 」")
      )
  (customize-set-variable 'org-todo-keywords
                          `((sequence
                             ,(--/org/todo/keyword "TODO"    wrap "t")             ; A task that needs doing & is ready to do
                             ,(--/org/todo/keyword "PROJECT" wrap "p")             ; A project, which usually contains other tasks
                             ,(--/org/todo/keyword "CURRENT" wrap "c" :timestamp)  ; A task that is in progress
                             ,(--/org/todo/keyword "WAITING" wrap "w" :notes)      ; Something external is holding up this task
                             ,(--/org/todo/keyword "HOLDING" wrap "h" :notes)      ; This task is paused/on hold because of me
                             "|"
                             ,(--/org/todo/keyword "───────" wrap "n" :timestamp)  ; No one cares.
                             ,(--/org/todo/keyword "INFO"    wrap "i" :timestamp)  ; Info.
                             ,(--/org/todo/keyword "MEETING" wrap "e" :timestamp)  ; Meeting Notes.
                             ,(--/org/todo/keyword "MOVED"   wrap "m" :timestamp)  ; Moved somewhere else; no further action here.
                             ,(--/org/todo/keyword "DONE"    wrap "d" :timestamp)  ; Task completed... whatever.
                             ,(--/org/todo/keyword "SUCCESS" wrap "s" :timestamp)  ; Task completed successfully!!!
                             ,(--/org/todo/keyword "FAILURE" wrap "f" :notes)      ; Task was completed the bad way.
                             ,(--/org/todo/keyword "KILLED"  wrap "k" :notes))))   ; Task was cancelled, aborted, or is no longer applicable.

  ;; And set some faces for these. strings.
  (customize-set-variable 'org-todo-keyword-faces
                          `((,(--/org/todo/keyword "TODO"    wrap) . --/zenburn/org-todo/todo)
                            (,(--/org/todo/keyword "PROJECT" wrap) . --/zenburn/org-todo/project)
                            (,(--/org/todo/keyword "CURRENT" wrap) . --/zenburn/org-todo/current)
                            (,(--/org/todo/keyword "WAITING" wrap) . --/zenburn/org-todo/holding)
                            (,(--/org/todo/keyword "HOLDING" wrap) . --/zenburn/org-todo/holding)
                            (,(--/org/todo/keyword "INFO"    wrap) . --/zenburn/org-todo/info)
                            (,(--/org/todo/keyword "MEETING" wrap) . --/zenburn/org-todo/info)
                            (,(--/org/todo/keyword "MOVED"   wrap) . --/zenburn/org-todo/info)
                            (,(--/org/todo/keyword "DONE"    wrap) . --/zenburn/org-todo/done-good)
                            (,(--/org/todo/keyword "SUCCESS" wrap) . --/zenburn/org-todo/done-good)
                            (,(--/org/todo/keyword "FAILURE" wrap) . --/zenburn/org-todo/done-bad)
                            (,(--/org/todo/keyword "KILLED"  wrap) . --/zenburn/org-todo/done-bad)))

  ;; I guess this guy is covered by `hl-todo' instead of `org'?
  ;; (push `(,(--/org/todo/keyword "TODO" wrap) warning bold) hl-todo-keyword-faces)
  ;; ...but `hl-todo' cannot do things that start/end with non-letters...
  ;; So yay.
  )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide user config org theme zenburn)
