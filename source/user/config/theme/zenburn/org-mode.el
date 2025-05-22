;;; mantle/theme/zenburn/org-mode.el --- Zenburn Tweaks for Org Mode -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-05-12
;; Timestamp:  2023-06-27
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
;;; Code:


(imp:require :innit 'theme)


;;------------------------------------------------------------------------------
;; Zenburn Theme Tweaks for Org-Mode
;;------------------------------------------------------------------------------

;; Run this after both `zenburn-theme' and `org-mode' have loaded.
;; Need elisp from both in order to evaluate this.
(imp:eval:after (:and zenburn-theme org)

  ;; Set up keys as variables from :
  ;;   - `zenburn-default-colors-alist'
  ;;   - `zenburn-override-colors-alist'
  (zenburn-with-color-variables

   ;;---------------------------------------------------------------------------
   ;; Faces
   ;;---------------------------------------------------------------------------
   ;; I need to customize some org-mode faces - they're not the greatest (IMO)
   ;; in Zenburn. Could maybe move these out of Zenburn into general theme stuff
   ;; if ever other themes want to adjust the same faces? In that case, split
   ;; like:
   ;;
   ;; "some-theme-file.el":
   ;;    (defface mantle:theme:face:org.todo.keyword:todo
   ;;      (list (cons
   ;;             ;; display type
   ;;             t
   ;;             ;; default attributes
   ;;             (list
   ;;              :weight 'bold
   ;;              :inherit 'warning)))
   ;;      "Face for todo keyword in todo sequence."
   ;;      :group 'innit:group)
   ;;
   ;; "zenburn/whatever.el" or "theme-too.el":
   ;;   (face-spec-set 'mantle:theme:face:org.todo.keyword:todo
   ;;                  (list (cons
   ;;                         ;; display type
   ;;                         t
   ;;                         ;; attributes
   ;;                         (list [...]))))
   ;;------------------------------

   ;; HACK: Face specs fed directly to `org-todo-keyword-faces' don't respect
   ;;       underlying faces like the `org-todo' face does, so we define our own
   ;;       intermediary faces that extend from org-todo.

   (defface mantle:theme:face:org.todo.keyword:active
     (list (cons
            ;; display type
            t
            ;; attributes
            '(:inherit (bold font-lock-constant-face org-todo))))
     "Face spec for 'active' todo sequence keywords."
     :group 'innit:group:theme)


   (defface mantle:theme:face:org.todo.keyword:project
     (list (cons
            ;; display type
            t
            ;; attributes
            '(:inherit (bold font-lock-doc-face org-todo))))
     "Face spec for 'project' todo sequence keyword(s)."
     :group 'innit:group:theme)


   (defface mantle:theme:face:org.todo.keyword:holding
     (list (cons
            ;; display type
            t
            ;; attributes
            '(:inherit (bold warning org-todo))))
     "Face spec for 'on-hold' todo sequence keyword(s)."
     :group 'innit:group:theme)


   ;; TODO: Delete?
   ;; (defface mantle:theme:face:org.todo.keyword:cancel
   ;;   (list (cons
   ;;          ;; display type
   ;;          t
   ;;          ;; attributes
   ;;          '(:inherit (bold error org-todo))))
   ;;   "Face spec for 'cancelled'/'killed' todo sequence keyword(s)."
   ;;   :group 'innit:group:theme)


   ;; TODO: Delete?
   ;; (defface mantle:theme:face:org.todo.keyword:background
   ;;   (list (cons
   ;;          ;; display type
   ;;          t
   ;;          ;; attributes
   ;;          ;; nil
   ;;          (list :background zenburn-bg-05)))
   ;;   "Face spec for the background of todo sequence keywords."
   ;;   :group 'innit:group:theme)


   (defface mantle:theme:face:org.todo.keyword:todo
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
     :group 'innit:group:theme)


   (defface mantle:theme:face:org.todo.keyword:done/good
     (list (cons
            ;; display type
            t
            ;; attributes
            ;; (list
            ;;  :inherit 'org-done)
            (list :background zenburn-bg-05
                  :inherit 'org-done)))
     "Face spec for good/successful 'done'/'finished' keyword in todo sequence."
     :group 'innit:group:theme)


   (defface mantle:theme:face:org.todo.keyword:done/bad
     (list (cons
            ;; display type
            t
            ;; attributes
            ;; (list
            ;;  :inherit 'org-done)))
            (list :background zenburn-bg-05
                  :foreground zenburn-red-5)))
     "Face spec for bad/failed 'done'/'finished' keyword in todo sequence."
     :group 'innit:group:theme)


   (defface mantle:theme:face:org.todo.keyword:info
     (list (cons
            ;; display type
            t
            ;; attributes
            ;; (list
            ;;  :inherit 'org-done)
            (list :background zenburn-bg-05
                  :foreground zenburn-bg+3)))
     "Face spec for info keyword in todo sequence."
     :group 'innit:group:theme)


   (defface mantle:theme:face:org.todo.keyword:null
     (list (cons
            ;; display type
            t
            ;; attributes
            ;; (list
            ;;  :inherit 'org-done)
            (list :background zenburn-bg-05
                  :foreground zenburn-bg+3)))
     "Face spec for empty/null/spacer keyword in todo sequence."
     :group 'innit:group:theme)


   ;;---------------------------------------------------------------------------
   ;; Configure Org-Mode
   ;;---------------------------------------------------------------------------

   (innit:theme:face:set 'zenburn
     ;;---
     ;; Done states - little less dark.
     ;;---
     (list 'org-done                     :foreground zenburn-green-3)
     (list 'org-agenda-done              :foreground zenburn-bg+3)
     (list 'org-checkbox-statistics-done :foreground zenburn-green-3)
     ;; "Done" Headlines - success/fail/info/etc.
     ;;    - Would be nice to have green, red, and gray... but we only have the one 'done' face.
     ;;    - So a lighter gray?
     (list 'org-headline-done :foreground zenburn-fg-05)
     ;; (list 'org-headline-done :foreground zenburn-green-3)

     ;;---
     ;; Org Headlines - adjust a few to be less dark.
     ;;---
     ;; Some unique, headliney colors.
     (list 'outline-1 :foreground zenburn-orange)
     (list 'outline-2 :foreground zenburn-green+2)
     (list 'outline-3 :foreground zenburn-blue-1)
     (list 'outline-4 :foreground zenburn-red-1)
     ;; ...and repeat.
     (list 'outline-5 :foreground zenburn-orange)
     (list 'outline-6 :foreground zenburn-green+2)
     (list 'outline-7 :foreground zenburn-blue-1)
     (list 'outline-8 :foreground zenburn-red-1)
     ;; ...and 9+ starts over at `outline-1'.

     ;;---
     ;; Org TODO States
     ;;---
     ;; ├CURRENT┤
     (list 'mantle:theme:face:org.todo.keyword:active
           :foreground zenburn-violet
           :background zenburn-bg-05)
     ;; ├WAITING┤, ├HOLDING┤
     (list 'mantle:theme:face:org.todo.keyword:holding
           :foreground zenburn-magenta-03
           :background zenburn-bg-05)
     ;; ├PROJECT┤
     (list 'mantle:theme:face:org.todo.keyword:project
           :foreground zenburn-blue-3
           :background zenburn-bg-05)

     ;;---
     ;; Org ~inline code~ and =inline verbatim=
     ;;---
     ;; Want these to be different enough from all the outline levels.
     ;;   - `org-code' was exactly the same as `outline-1'.
     ;;   - `org-verbatim' was similar to `outline-2' and to code comments (green).
     ;; Change to inheriting from `org-cite', a teal affair, with some background color, maybe.
     ;; TODO: Make the foreground color of these slightly different from each other.
     (list 'org-code     :inherit 'org-cite :background zenburn-bg+05)
     (list 'org-verbatim :inherit 'org-cite :background zenburn-bg-05)

     ;;---
     ;; "#+DOC_KEYWORD" - needs to be slightly lighter.
     ;;---
     (list 'org-document-info-keyword :foreground zenburn-bg+3))
  ;; mantle:theme:face:org.todo.keyword:active  - bold green
  ;; mantle:theme:face:org.todo.keyword:holding  - orangey
  ;; mantle:theme:face:org.todo.keyword:project - darker green than bold green
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
    (setq org-todo-keywords
          `((sequence
             ,(mode:org:todo/keyword "TODO"    wrap "t")             ; A task that needs doing & is ready to do
             ,(mode:org:todo/keyword "PROJECT" wrap "p")             ; A project, which usually contains other tasks
             ,(mode:org:todo/keyword "CURRENT" wrap "c" :timestamp)  ; A task that is in progress
             ,(mode:org:todo/keyword "WAITING" wrap "w" :notes)      ; Something external is holding up this task
             ,(mode:org:todo/keyword "HOLDING" wrap "h" :notes)      ; This task is paused/on hold because of me
             "|"
             ,(mode:org:todo/keyword "───────" wrap "n" :timestamp)  ; No one cares.
             ,(mode:org:todo/keyword "INFO"    wrap "i" :timestamp)  ; Info.
             ,(mode:org:todo/keyword "MEETING" wrap "e" :timestamp)  ; Meeting Notes.
             ,(mode:org:todo/keyword "MOVED"   wrap "m" :timestamp)  ; Moved somewhere else; no further action here.
             ,(mode:org:todo/keyword "DONE"    wrap "d" :timestamp)  ; Task completed... whatever.
             ,(mode:org:todo/keyword "SUCCESS" wrap "s" :timestamp)  ; Task completed successfully!!!
             ,(mode:org:todo/keyword "FAILURE" wrap "f" :notes)      ; Task was completed the bad way.
             ,(mode:org:todo/keyword "KILLED"  wrap "k" :notes)))    ; Task was cancelled, aborted, or is no longer applicable.

          ;; And set some faces for these. strings.
          org-todo-keyword-faces
          (list (list (mode:org:todo/keyword "TODO" wrap)    'mantle:theme:face:org.todo.keyword:todo)
                (list (mode:org:todo/keyword "PROJECT" wrap) 'mantle:theme:face:org.todo.keyword:project)

                (list (mode:org:todo/keyword "CURRENT" wrap) 'mantle:theme:face:org.todo.keyword:active)

                (list (mode:org:todo/keyword "WAITING" wrap) 'mantle:theme:face:org.todo.keyword:holding)
                (list (mode:org:todo/keyword "HOLDING" wrap) 'mantle:theme:face:org.todo.keyword:holding)
                (list (mode:org:todo/keyword "INFO" wrap)    'mantle:theme:face:org.todo.keyword:info)
                (list (mode:org:todo/keyword "MEETING" wrap) 'mantle:theme:face:org.todo.keyword:info)

                (list (mode:org:todo/keyword "MOVED" wrap)   'mantle:theme:face:org.todo.keyword:info)
                (list (mode:org:todo/keyword "DONE" wrap)    'mantle:theme:face:org.todo.keyword:done/good)
                (list (mode:org:todo/keyword "SUCCESS" wrap) 'mantle:theme:face:org.todo.keyword:done/good)
                (list (mode:org:todo/keyword "FAILURE" wrap) 'mantle:theme:face:org.todo.keyword:done/bad)
                (list (mode:org:todo/keyword "KILLED" wrap)  'mantle:theme:face:org.todo.keyword:done/bad)))

    ;; I guess this guy is covered by `hl-todo' instead of `org'?
    ;; (push `(,(mode:org:todo/keyword "TODO" wrap) warning bold) hl-todo-keyword-faces)
    ;; ...but `hl-todo' cannot do things that start/end with non-letters...
    ;; So yay.
    )))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'theme 'zenburn 'org-mode)
