;;; modules/dev-env/taskspace/prompt.el --- Helpers for Prompting Users -*- lexical-binding: t; -*-
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
;; Helpers for Prompting Users
;;
;;; Code:


;;---------------------------------taskspace------------------------------------
;;--                   Simple Taskspace / Task Management                     --
;;------------------------------------------------------------------------------

(require 'seq)
(require 'dash)

(imp:require :dlv)
(imp:require :nub)
(imp:require :taskspace 'group)


;;------------------------------------------------------------------------------
;; Prompt Helpers
;;------------------------------------------------------------------------------

;;------------------------------
;; Prompt: Group
;;------------------------------

(defun int<taskspace>:prompt:group:name (group-assoc)
  "Get displayable groups for prompting user.

GROUP-ASSOC shoud be an entry from `taskspace:groups'.

Returns a cons of:
  - A string containing both the display name and the keyword.
  - The keyword."
  (cons
   ;; Display string first, since that's what `completing-read' shows to user.
   (concat (format "%-10s" (int<taskspace>:group:keyword group-assoc))
           " - "
           (int<taskspace>:group:name/display group-assoc))
   ;; Group keyword second so we can get back to it from user's choice of
   ;; display strings.
   (int<taskspace>:group:keyword group-assoc)))
;; (int<taskspace>:prompt:group:name '(:default "Jeff!" taskspace:group:default))


(defun int<taskspace>:prompt:group:get (choices)
  "Interactive prompt for user to input/choose group name.
Provides both the keyword name and the display string for user to
complete against. Returns keyword name chosen.

CHOICES should be filtered down keyword names from `taskspace:groups'."
  (let ((display-choices (-map #'int<taskspace>:prompt:group:name choices)))
    (alist-get
     (completing-read "Taskspace Group: "
                      ;; Build the names for the options list...
                      display-choices
                      nil
                      ;; Make them confirm if not on the list...
                      'confirm)
     display-choices
     nil nil
     #'string=)))
;; (int<taskspace>:prompt:group:get '((:a "aaa" nil) (:b "b" nil)))


(defun int<taskspace>:prompt:group (&optional auto quiet)
  "Filter groups down to options available to user.

If only one, uses that. If only `:default' available, uses that. If multiple
user groups, prompts user via `int<taskspace>:prompt:group' for which to use.

AUTO can be a few things:
  - nil: No auto-guessing the group unless there is only one non-default.
  - dlv: `int<taskspace>:group:dlv'
  - current: `int<taskspace>:group:current'
  - auto: `int<taskspace>:group:auto'
    - Combines `dlv' and `current', preferring `dlv'.

If QUIET is non-nil, return nil on error instead of raising error signal.

Return group keyword (aka 0th element of entry in `taskspace:groups')."
  ;; `or' will give us either:
  ;;   1) the auto group,
  ;;   2) or the prompted group.
  (or
   ;; 1) Check for an auto-group func to try...
   (cond ((eq nil auto)
          nil)
         ((eq 'auto auto)
          (int<taskspace>:group:auto quiet))
         ((eq 'dlv auto)
          (int<taskspace>:group:dlv))
         ((eq 'current auto)
          (int<taskspace>:group:current quiet))
         ((not (null auto))
          (if quiet
              nil
            (nub:error
                :taskspace
                "int<taskspace>:prompt:group"
              "Unknown AUTO option `%S'"
              auto))))
   ;; 2) No luck on the auto-group... Check the groups and prompt as needed.
   (let ((groups-sans-default
          ;; Filter down to just non-defaults...
          (-filter (lambda (group) (not (eq (int<taskspace>:group:keyword group) :default)))
                   taskspace:groups)))
     ;; Just one group? Return it.
     (cond ((= (length groups-sans-default) 1)
            ;; Get the only group there...
            (nth 0 groups-sans-default))

           ;; Multiple groups? That's what we're actually here for - prompt user!
           ((> (length groups-sans-default) 1)
            (int<taskspace>:prompt:group:get groups-sans-default))

           ;; 0 or less groups.
           (t
            ;; Try to use the default, I guess...
            (int<taskspace>:group :default))))))
;; (int<taskspace>:prompt:group)


;;------------------------------
;; Prompt: Task Name (Description)
;;------------------------------
(defun int<taskspace>:prompt:name (group)
  "Convert minibuffer input into taskspace name.

GROUP should be return value from `int<taskspace>:group' (assoc from
`taskspace:groups').

Prompt in minibuffer for input, read and format to string, then return."
  ;; Replace all whitespace with hyphens.
  (s-replace-regexp (rx (one-or-more whitespace))
                    "-"
                    (read-from-minibuffer
                     (format "New `%s` Task Desc.: "
                             (int<taskspace>:group:name/display group)))))


;;------------------------------
;; Prompt: Choose Existing Task
;;------------------------------

;; Thank you to this thread:
;;   https://emacs.stackexchange.com/questions/32248/how-to-write-a-function-with-an-interactive-choice-of-the-value-of-the-argument
;; I was not finding any usable help/tutorials/documentation
;; for my knowledge/skill level until I found that.
(defun int<taskspace>:prompt:task/existing (group taskspaces &optional display)
  "Prompt user for existing taskspace.

Given a list of taskspaces from e.g. `int<taskspace>:dir:list:date', prompt user
with list of choices, take the user's input, and match back up with an entry in
the list of taskspaces.

GROUP should be return value from `int<taskspace>:group' (assoc from
`taskspace:groups').

DISPLAY can be:
- nil: Pass taskspaces as-is to completion. AKA display as-is.
- nondirectory: Strip each element to `file:name'

Choice is matched back to taskspaces via dumb string matching. First match in
TASKSPACES that substring matches user's choice from `completing-read' is
returned as choice.

Return nil or a string in TASKSPACES."
  ;; Figure out how to display to user first.
  (let (display-names)
    (cond
     ;; nil -> as-is
     ((null display)
      (setq display-names taskspaces))

     ;; nondirectory -> strip each to remove all parent dirs
     ((equal display 'nondirectory)
      (setq display-names (mapcar #'file:name taskspaces)))

     ;; unexpected -> error?
     (t
      (nub:error
          :taskspace
          "int<taskspace>:prompt:task/existing"
        "Unknown display option `%s'"
        display)))

    ;; Give user their choices...
    ;;
    ;; With helm at the wheel, this goes to helm--completing-read-default.
    ;; `confirm' to force completion to one complete choice.
    (let ((choice (completing-read (format "Choose %s: "
                                           (int<taskspace>:group:name/display group))
                                   display-names nil 'confirm)))

      ;; ...and match their choice back up with a taskname.
      (seq-contains taskspaces
                    choice
                    (lambda (input taskname)
                      "Check substring match of user's input against taskname."
                      (string-match-p (regexp-quote input) taskname))))))
;; (int<taskspace>:prompt:task/existing :home (int<taskspace>:dir:list:all :home) 'nondirectory)




;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :taskspace 'prompt)
