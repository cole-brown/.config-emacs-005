;;; modules/mode/org/keyword.el --- Org-Mode Keyword Stuff -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2023-06-29
;; Timestamp:  2025-10-28
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Org-Mode Keyword Stuff
;;   - Keyword getter & setter functions.
;;   - TODO keyword creator & converter functions.
;;
;;; Code:


(imp-require buffer delete)


;;------------------------------------------------------------------------------
;; Org Keywords
;;------------------------------------------------------------------------------

;; http://kitchingroup.cheme.cmu.edu/blog/2013/05/05/Getting-keyword-options-in-org-files/
(defun --/org/keywords/get ()
  "Parse the buffer and return a cons list of (property . value)
from lines like:
#+PROPERTY: value"
  (org-element-map (org-element-parse-buffer 'element) 'keyword
    (lambda (keyword) (cons (org-element-property :key keyword)
                            (org-element-property :value keyword)))))
;; (int<mode>:org:keywords/get)


;; http://kitchingroup.cheme.cmu.edu/blog/2013/05/05/Getting-keyword-options-in-org-files/
(defun --/org/keyword/get (keyword)
  "Get the value of a KEYWORD from the current (org-mode) buffer.

KEYWORD is an org keyword in the form of:
#+KEYWORD: value"
  (cdr (assoc keyword (--/org/keywords/get))))
;; (--/org/keyword/get "TICKET-ID")


(defun --/org/keyword/set (keyword value)
  "Set the VALUE of a KEYWORD from the current (org-mode) buffer.

KEYWORD is an org keyword in the form of:
#+KEYWORD: VALUE"
  ;; Expand to full buffer contents.
  (org-with-wide-buffer
   ;; Start at the beginning, search for keyword elements.
   (goto-char (point-min))
   (re-search-forward (rx-to-string `(sequence "#+"
                                               ,keyword
                                               ":"))
                      1024
                      t)
   (let ((thing (org-thing-at-point)))
     ;; (message "%s: %s" thing
     ;;          (or (not thing)
     ;;              ;; car of thing should be "file-option"
     ;;              (not (car thing))
     ;;              (not (stringp (car thing)))
     ;;              (not (string= (car thing) "file-option"))
     ;;              ;; cdr of thing should be the keyword
     ;;              (not (cdr thing))
     ;;              (not (stringp (cdr thing)))
     ;;              (not (string= (cdr thing) keyword))
     ;;              ))
     (if (or (not thing)
             ;; car of thing should be "file-option"
             (not (car thing))
             (not (stringp (car thing)))
             (not (string= (car thing) "file-option"))
             ;; cdr of thing should be the keyword
             (not (cdr thing))
             (not (stringp (cdr thing)))
             (not (string= (cdr thing) keyword)))
         (error "Keyword not found; cannot set '%s' to '%s'" keyword value)
       ;; Move past keyword to its value.
       (forward-to-word 1)
       ;; Delete old id and replace with new id.
       (buffer:delete:word 1)
       (insert value)))))


;;------------------------------------------------------------------------------
;; Org TODO Sequences
;;------------------------------------------------------------------------------

(defun --/org/todo/keyword (word wrap &optional key on-enter on-exit-if)
  "Create an org-todo-keyword with WORD.

WRAP should be a string / sequence of chars of length 3:
  - Wrap start/end of WORD in (elt WRAP 0) and (elt WRAP 2).
  - Pad WORD inside of wrapping with (elt WRAP 1).

If KEY is a string, create a keyword with keybind (for `org-todo-keywords'
use).

If ON-ENTER or ON-EXIT-IF are non-nil, they must be:
  - `:timestamp' - Add timestamp to state change on enter/exit.
  - `:notes'     - Add notes w/ timestamp to state change on enter/exit.

ON-EXIT-IF: The 'if' referes to the next state. So an ON-EXIT-IF
of `:notes' will only trigger if the next state doesn't have notes."
  ;;------------------------------
  ;; Validate Inputs
  ;;------------------------------
  (cond ((not (stringp word))
         ;; Failed validation.
         (error "Invalid parameter: WORD ('%S') must be a string"
                word))

        ((or (not (sequencep wrap))
             (not (integerp (elt wrap 0)))
             (not (integerp (elt wrap 1)))
             (not (integerp (elt wrap 2))))
         ;; Failed validation.
         (error "Invalid parameter: WRAP ('%S') must be a sequence of characters"
                wrap))

        ((and (not (null key))
              (not (stringp key)))
         ;; Failed validation.
         (error "Invalid parameter: KEY ('%S') must be a string or nil"
                key))

        ((and (not (null on-enter))
              (keywordp on-enter)
              (not (memq on-enter '(:timestamp :notes))))
         ;; Failed validation.
         (error "Invalid parameter: ON-ENTER ('%S') must be one of: nil, `:timestamp', `:notes'"
                on-enter))

        ((and (not (null on-exit-if))
              (keywordp on-exit-if)
              (not (memq on-exit-if '(:timestamp :notes))))
         ;; Failed validation.
         (error "Invalid parameter: ON-EXIT-IF ('%S') must be one of: nil, `:timestamp', `:notes'"
                on-exit-if))

        ;;------------------------------
        ;; Create org todo keyword string!
        ;;------------------------------
        (t
         (let ((enter (cond ((eq on-enter :timestamp)
                             "!")
                            ((eq on-enter :notes)
                             "@")
                            (t
                             "")))
               (exit (cond ((eq on-exit-if :timestamp)
                            "/!")
                           ((eq on-exit-if :notes)
                            "/@")
                           (t
                            "")))
               keyword-string)

           ;;---
           ;; Basic wrapped keyword.
           ;;---
           ;; <wrap><word></wrap>
           (setq keyword-string
                 (format "%s%-7s%s"
                         (char-to-string (elt wrap 0))
                         word
                         (char-to-string (elt wrap 2))))
           ;; Had to format it first, now I want to replace the normal spaces with
           ;; non-breaking spaces or something so that whitespace-mode won't
           ;; override the face sytle.
           (setq keyword-string (string-replace " "
                                                (char-to-string (elt wrap 1))
                                                keyword-string))

           ;;---
           ;; Add in key/enter/exit codes?
           ;;---
           (unless (null key)
             ;; <formatted-keyword>(<key><time?><note?>)
             (setq keyword-string
                   (format "%s(%s%s%s)"
                           keyword-string
                           ;; These won't go into non-key'd format
                           key
                           ;; on-enter: notes, timestamp, or nothing.
                           enter
                           ;; on-exit-if: notes, timestamp, or nothing.
                           exit)))
           ;;---
           ;; Return.
           ;;---
           keyword-string))))
;; (--/org/todo/keyword "bob" "├─┤")
;; (--/org/todo/keyword "bob" "[-]")
;; (--/org/todo/keyword "bob" "[-]" "b")
;; (--/org/todo/keyword "bob" "[-]" "b" :timestamp)
;; (--/org/todo/keyword "bob" "[-]" "b" :notes)
;; (--/org/todo/keyword "bob" "[-]" "b" nil :notes)
;; (--/org/todo/keyword "bob" "[-]" "b" nil :timestamp)
;; (--/org/todo/keyword "bob" "[-]" "b" :timestamp :notes)


;; (defun --/org/cmd/todo-keywords-convert-old (skip-bare)
;;   "Convert old TODO sequence to new."
;;   (interactive
;;    (list (y-or-n-p "Skip bare->wrap? ")))
;;   (org-with-wide-buffer
;;    (save-excursion
;;      ;; Bare->Wrapped ("TODO" -> "[TODO   ]"), for width consistency.
;;      (unless skip-bare
;;        (message "'TODO' -> '[TODO   ]'")
;;        (let* ((wrap "[ ]")
;;               (replacements
;;                `(("TODO"      . ,(--/org/todo/keyword "TODO" wrap))
;;                  ("STARTED"   . ,(--/org/todo/keyword "CURRENT" wrap))
;;                  ("WAITING"   . ,(--/org/todo/keyword "WAITING" wrap))
;;                  ("DONE"      . ,(--/org/todo/keyword "DONE" wrap))
;;                  ("SUCCESS"   . ,(--/org/todo/keyword "SUCCESS" wrap))
;;                  ("FAILURE"   . ,(--/org/todo/keyword "FAILURE" wrap))
;;                  ("CANCELLED" . ,(--/org/todo/keyword "KILLED" wrap)))))
;;          (dolist (replacement replacements)
;;            (funcall-interactively #'query-replace
;;                                   (car replacement) (cdr replacement)
;;                                   nil
;;                                   (point-min) (point-max)))))
;; 
;;      ;; "[TODO   ]" -> "├TODO───┤"
;;      (let* ((wrap "[ ]")
;;             (replacements/old (list
;;                                (--/org/todo/keyword "TODO" wrap)
;;                                (--/org/todo/keyword "PROJECT" wrap)
;;                                (--/org/todo/keyword "CURRENT" wrap)
;;                                (--/org/todo/keyword "WAITING" wrap)
;;                                (--/org/todo/keyword "HOLDING" wrap)
;;                                (--/org/todo/keyword "DONE" wrap)
;;                                (--/org/todo/keyword "SUCCESS" wrap)
;;                                (--/org/todo/keyword "FAILURE" wrap)
;;                                (--/org/todo/keyword "KILLED" wrap)
;;                                (--/org/todo/keyword " " wrap)
;;                                (--/org/todo/keyword "▶" wrap)
;;                                (--/org/todo/keyword "-" wrap)
;;                                (--/org/todo/keyword "?" wrap)
;;                                (--/org/todo/keyword "…" wrap)
;;                                (--/org/todo/keyword "⁈" wrap)
;;                                (--/org/todo/keyword "X" wrap)
;;                                (--/org/todo/keyword "X" wrap)
;;                                (--/org/todo/keyword "✘" wrap)
;;                                (--/org/todo/keyword "÷" wrap))))
;; 
;;        (let* ((wrap "├─┤")
;;               (replacements/new (list
;;                                  (--/org/todo/keyword "TODO" wrap)
;;                                  (--/org/todo/keyword "PROJECT" wrap)
;;                                  (--/org/todo/keyword "CURRENT" wrap)
;;                                  (--/org/todo/keyword "WAITING" wrap)
;;                                  (--/org/todo/keyword "HOLDING" wrap)
;;                                  (--/org/todo/keyword "DONE" wrap)
;;                                  (--/org/todo/keyword "SUCCESS" wrap)
;;                                  (--/org/todo/keyword "FAILURE" wrap)
;;                                  (--/org/todo/keyword "KILLED" wrap)
;;                                  (--/org/todo/keyword "_" wrap)
;;                                  (--/org/todo/keyword "▶" wrap)
;;                                  (--/org/todo/keyword "-" wrap)
;;                                  (--/org/todo/keyword "?" wrap)
;;                                  (--/org/todo/keyword "…" wrap)
;;                                  (--/org/todo/keyword "⁈" wrap)
;;                                  (--/org/todo/keyword "X" wrap)
;;                                  (--/org/todo/keyword "X" wrap)
;;                                  (--/org/todo/keyword "✘" wrap)
;;                                  (--/org/todo/keyword "÷" wrap))))
;; 
;;          ;; "[TODO   ]" -> "├TODO───┤"
;;          (message "'[TODO   ]' -> '├TODO───┤'")
;; 
;;          ;; And... find/replace 'em.
;;          (dotimes (index (length replacements/new))
;;            (let ((old (nth index replacements/old))
;;                  (new (nth index replacements/new)))
;;              (unless (string= old new)
;;                (funcall-interactively #'query-replace
;;                                       old new
;;                                       nil
;;                                       (point-min) (point-max))))))))))


;;------------------------------------------------------------------------------
;; Per File Org-Keywords
;;------------------------------------------------------------------------------

;; TODO: Need a face param; replace `mode:theme.face/org.todo.keyword/todo'.
(defun --/org/cmd/todo-keywords-in-file-header (todo-sequence)
  "Create 'per-file' keyword lines using TODO-SEQUENCE (or the prompt input).

See here for acceptable inputs:
  - https://orgmode.org:manual/Per_002dfile-keywords.html
  - e.g. TODO FEEDBACK VERIFY | DONE CANCELED

Replaces a \"#+TODO\" keyword if found.

Outputs 2 lines:
  - \"#+TODO: [...]\"
  - \"-*- org-todo-keyword-faces: ([...]) -*-\"

The `org-todo-keyword-faces' line must be at the top of the file to work."
  (interactive "sTODO Sequence: ")

  ;;------------------------------
  ;; TODO Sequence Keyword
  ;;------------------------------
  (let* ((keyword/todo "TODO")
         (keyword/face-var "org-todo-keyword-faces")
         (todo/wrap "├─┤")
         (todo/existing (--/org/keyword/get keyword/todo))
         todo/new.list
         todo/new.str
         keywords-set)

    ;; Need to wrap the keywords!
    (dolist (keyword (nreverse (split-string todo-sequence)))
      (if (string= keyword "|")
          ;; Leave as-is - it's the todo/done separator.
          (push keyword todo/new.list)
        ;; Wrap it.
        (push (--/org/todo/keyword keyword todo/wrap) todo/new.list)))

    ;; Convert the wrapped list into a wrapped string.
    (setq todo/new.str (mapconcat #'identity
                                  todo/new.list
                                  " "))

    ;; Insert into the buffer.
    (if todo/existing
        ;; Find `TODO' keyword in file, change it.
        (if (not (yes-or-no-p (format "Found existing 'TODO' keyword (existing: \"%s\", new: \"%s\")! Replace?"
                                      todo/existing todo/new.str)))
            (message "Ok; changed nothing.")

          (--/org/keyword/set keyword/todo todo/new.str)
          (message "Updated keywords to: \"%S\"" todo/new.str)
          (setq keywords-set t))

      ;; No existing; create new one.
      ;; I'm not going to bother with placing this somewhere smart, for now.
      (org-with-wide-buffer
       (beginning-of-line)
       (insert (format "\n#+%s: %s\n" keyword/todo todo/new.str))
       (message "Created keywords: \"%S\"" todo/new.str))
      (setq keywords-set t))

    ;;------------------------------
    ;; Faces Local Variable
    ;;------------------------------
    (when (and keywords-set
               (yes-or-no-p "Create alist for applying faces to the keywords?"))
      (org-with-wide-buffer
       (let ((faces/alist))
         (dolist (keyword todo/new.list)
           (unless (string= keyword "|")
             (push (cons keyword 'mode:theme.face/org.todo.keyword/todo) faces/alist)))
         (setq faces/alist (nreverse faces/alist))

         ;; Put it before the keywords?
         (beginning-of-line)
         (insert "\n")

         ;; Just make the line for user to fiddle with.
         (insert (format "\n# -*- %s: %S -*-"
                         keyword/face-var
                         faces/alist))
         (insert (mapconcat #'identity
                            '(""
                              "# The above line must be at the top of the file, possibly combining with/replacing whatever already exists there."
                              "# For faces to use, see:"
                              "# - command: `M-x list-faces-display'"
                              "# - variables: `mode:theme.face/org.todo.[...]'"
                              "# - variables: `+org-todo-[...]'"
                              "# - file: ~/.doom.d/config/org-mode.el")
                            "\n"))
         (insert "\n"))))

    ;;------------------------------
    ;; Done.
    ;;------------------------------
    (org-with-wide-buffer
     (insert (mapconcat #'identity
                        '("Done; you may need to move the inserted lines up to/near the top of the file."
                          "To finalize:"
                          "  1. Complete faces alist (if generated)."
                          "  2. Move faces line to top of file."
                          "  3. Move keyword line to near top of file."
                          "  4. Save the file."
                          "  5. Revisit the file.")
                        "\n")))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide user mode org keyword)
