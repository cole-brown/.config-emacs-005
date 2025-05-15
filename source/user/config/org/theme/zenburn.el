;;; source/user/org/theme/zenburn.el --- Zenburn Tweaks for Org Mode -*- lexical-binding: t; -*-
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


;;------------------------------------------------------------------------------
;; Zenburn Theme Tweaks for Org-Mode
;;------------------------------------------------------------------------------

;; This macro is a giant `let' for all the zenburn colors from:
;;   - `zenburn-default-colors-alist'
;;   - `zenburn-override-colors-alist'
;; example: `zenburn-green-3'
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

 (defgroup --/theme/zenburn/face/org-todo nil
  "A place to hide `org-todo' faces."
  :group 'tools
  ;; TODO: What is this repo called?
  ;; :link '(url-link "https://github.com/cole-brown/.config-emacs")
  )

 (defface --/theme/zenburn/face/org-todo/keyword=current
   (list (cons
          ;; display type
          t
          ;; attributes
          '(:inherit (bold font-lock-constant-face org-todo))))
   "Face spec for 'current' todo sequence keyword(s)."
   :group '--/theme/zenburn/face/org-todo)


 (defface --/theme/zenburn/face/org-todo/keyword=project
   (list (cons
          ;; display type
          t
          ;; attributes
          '(:inherit (bold font-lock-doc-face org-todo))))
   "Face spec for 'project' todo sequence keyword(s)."
   :group '--/theme/zenburn/face/org-todo)


 (defface --/theme/zenburn/face/org-todo/keyword=holding
   (list (cons
          ;; display type
          t
          ;; attributes
          '(:inherit (bold warning org-todo))))
   "Face spec for 'on-hold' todo sequence keyword(s)."
   :group '--/theme/zenburn/face/org-todo)
 

 (defface --/theme/zenburn/face/org-todo/keyword=todo
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
   :group '--/theme/zenburn/face/org-todo)


 (defface --/theme/zenburn/face/org-todo/keyword=done-good
   (list (cons
          ;; display type
          t
          ;; attributes
          ;; (list
          ;;  :inherit 'org-done)
          (list :background zenburn-bg-05
                :inherit 'org-done)))
   "Face spec for good/successful 'done'/'finished' keyword in todo sequence."
   :group '--/theme/zenburn/face/org-todo)


 (defface --/theme/zenburn/face/org-todo/keyword=done-bad
   (list (cons
          ;; display type
          t
          ;; attributes
          ;; (list
          ;;  :inherit 'org-done)))
          (list :background zenburn-bg-05
                :foreground zenburn-red-5)))
   "Face spec for bad/failed 'done'/'finished' keyword in todo sequence."
   :group '--/theme/zenburn/face/org-todo)


 (defface --/theme/zenburn/face/org-todo/keyword=info
   (list (cons
          ;; display type
          t
          ;; attributes
          ;; (list
          ;;  :inherit 'org-done)
          (list :background zenburn-bg-05
                :foreground zenburn-bg+3)))
   "Face spec for info keyword in todo sequence."
   :group '--/theme/zenburn/face/org-todo)


 (defface --/theme/zenburn/face/org-todo/keyword=null
   (list (cons
          ;; display type
          t
          ;; attributes
          ;; (list
          ;;  :inherit 'org-done)
          (list :background zenburn-bg-05
                :foreground zenburn-bg+3)))
   "Face spec for empty/null/spacer keyword in todo sequence."
   :group '--/theme/zenburn/face/org-todo)


 ;;---------------------------------------------------------------------------
 ;; Configure Org-Mode
 ;;---------------------------------------------------------------------------

 ;; TODO: Now I need some innit funcs?

 
 
 (innit:theme:face:set 'zenburn
                       
                       (hc-zenburn-with-color-variables
                       (innit--face-doom-to-emacs---hello 
                       ;;---
                       ;; Done states - little less dark.
                       ;;---
                       (list 'org-done                     :foreground hc-zenburn-green-3)
                       (list 'org-agenda-done              :foreground hc-zenburn-bg+3)
                       (list 'org-checkbox-statistics-done :foreground hc-zenburn-green-3)
                       ;; "Done" Headlines - success/fail/info/etc.
                       ;;    - Would be nice to have green, red, and gray... but we only have the one 'done' face.
                       ;;    - So a lighter gray?
                       (list 'org-headline-done :foreground hc-zenburn-fg-05)
                       ;; (list 'org-headline-done :foreground hc-zenburn-green-3)

                       ;;---
                       ;; Org Headlines - adjust a few to be less dark.
                       ;;---
                       ;; Some unique, headliney colors.
                       (list 'outline-1 :foreground hc-zenburn-orange)
                       (list 'outline-2 :foreground hc-zenburn-green+2)
                       (list 'outline-3 :foreground hc-zenburn-blue-1)
                       (list 'outline-4 :foreground hc-zenburn-red-1)
                       ;; ...and repeat.
                       (list 'outline-5 :foreground hc-zenburn-orange)
                       (list 'outline-6 :foreground hc-zenburn-green+2)
                       (list 'outline-7 :foreground hc-zenburn-blue-1)
                       (list 'outline-8 :foreground hc-zenburn-red-1)
                       ;; ...and 9+ starts over at `outline-1'.

                       ;;---
                       ;; Org TODO States
                       ;;---
                       ;; ├CURRENT┤
                       (list '--/theme/hc-zenburn/face/org-todo/keyword=current
                             :foreground hc-zenburn-violet
                             :background hc-zenburn-bg-05)
                       ;; ├WAITING┤, ├HOLDING┤
                       (list '--/theme/hc-zenburn/face/org-todo/keyword=holding
                             :foreground hc-zenburn-magenta-03
                             :background hc-zenburn-bg-05)
                       ;; ├PROJECT┤
                       (list '--/theme/hc-zenburn/face/org-todo/keyword=project
                             :foreground hc-zenburn-blue-3
                             :background hc-zenburn-bg-05)

                       ;;---
                       ;; Org ~inline code~ and =inline verbatim=
                       ;;---
                       ;; Want these to be different enough from all the outline levels.
                       ;;   - `org-code' was exactly the same as `outline-1'.
                       ;;   - `org-verbatim' was similar to `outline-2' and to code comments (green).
                       ;; Change to inheriting from `org-cite', a teal affair, with some background color, maybe.
                       ;; Also make the foreground color of these slightly different from each other.
                       (list 'org-code     :inherit 'org-cite :background hc-zenburn-bg+05)
                       (list 'org-verbatim :inherit 'org-cite :background hc-zenburn-bg-05)

                       ;;---
                       ;; "#+DOC_KEYWORD" - needs to be slightly lighter.
                       ;;---
                       (list 'org-document-info-keyword :foreground hc-zenburn-bg+3)))
 ;; --/theme/zenburn/face/org-todo/keyword=current  - bold green
 ;; --/theme/zenburn/face/org-todo/keyword=holding  - orangey
 ;; --/theme/zenburn/face/org-todo/keyword=project - darker green than bold green
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
         (list (list (mode:org:todo/keyword "TODO" wrap)    '--/theme/zenburn/face/org-todo/keyword=todo)
               (list (mode:org:todo/keyword "PROJECT" wrap) '--/theme/zenburn/face/org-todo/keyword=project)

               (list (mode:org:todo/keyword "CURRENT" wrap) '--/theme/zenburn/face/org-todo/keyword=current)

               (list (mode:org:todo/keyword "WAITING" wrap) '--/theme/zenburn/face/org-todo/keyword=holding)
               (list (mode:org:todo/keyword "HOLDING" wrap) '--/theme/zenburn/face/org-todo/keyword=holding)
               (list (mode:org:todo/keyword "INFO" wrap)    '--/theme/zenburn/face/org-todo/keyword=info)
               (list (mode:org:todo/keyword "MEETING" wrap) '--/theme/zenburn/face/org-todo/keyword=info)

               (list (mode:org:todo/keyword "MOVED" wrap)   '--/theme/zenburn/face/org-todo/keyword=info)
               (list (mode:org:todo/keyword "DONE" wrap)    '--/theme/zenburn/face/org-todo/keyword=done-good)
               (list (mode:org:todo/keyword "SUCCESS" wrap) '--/theme/zenburn/face/org-todo/keyword=done-good)
               (list (mode:org:todo/keyword "FAILURE" wrap) '--/theme/zenburn/face/org-todo/keyword=done-bad)
               (list (mode:org:todo/keyword "KILLED" wrap)  '--/theme/zenburn/face/org-todo/keyword=done-bad)))

   ;; I guess this guy is covered by `hl-todo' instead of `org'?
   ;; (push `(,(mode:org:todo/keyword "TODO" wrap) warning bold) hl-todo-keyword-faces)
   ;; ...but `hl-todo' cannot do things that start/end with non-letters...
   ;; So yay.
   )))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :user config org theme zenburn)
