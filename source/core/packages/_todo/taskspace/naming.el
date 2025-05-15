;;; modules/dev-env/taskspace/naming.el --- Naming Helpers -*- lexical-binding: t; -*-
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
;; Naming Helpers
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
(imp:require :elisp 'utils 'units)


;;------------------------------------------------------------------------------
;; Taskspace Naming
;;------------------------------------------------------------------------------

(defun int<taskspace>:naming:get:number (group dir-list)
  "Check dirs in DIR-LIST, return highest number part + 1.

GROUP should be return value from `int<taskspace>:group' (assoc from
`taskspace:groups')."
  ;; Next number is one more than...
  (1+
   ;; The max of map/reduce shenanigans (or just -1 if given no dirs).
   (-max
    (or
     (->>
      ;; First, need to change from paths to just the name of the dirs.
      (-map #'file:name dir-list)
      ;; Now pare down to just numbers.
      (-map (lambda (dir) (int<taskspace>:naming:split group dir 'number)))
      ;; Filter out any nils; don't matter to us.
      (-remove #'null)
      ;; string -> int
      (-map #' string-to-number))

     ;; fallback list: negative 1 so we return zero.
     '(-1)))))
;; (int<taskspace>:naming:get:number :work (int<taskspace>:dir:list:date :work "2020-08-26"))
;; (int<taskspace>:naming:get:number :work (int<taskspace>:dir:list:date :work "2020-08-26"))
;; (int<taskspace>:naming:get:number :default '("zort/troz/2000_0_baz"))
;; (int<taskspace>:naming:get:number :default '())
;; (int<taskspace>:naming:get:number :default
;;                       '("zort/troz/2000_0_baz" "zort/troz/2000_qux"
;;                         "zort/troz/2000_qux_jeff" "zort/troz/2000_8_quux"))


(defun int<taskspace>:naming:get:date (group arg)
  "Return a date in the correct string format.

ARG must be nil or 'today (for today), or numberp.

GROUP should be return value from `int<taskspace>:group' (assoc from
`taskspace:groups').

Return date requested by ARG, or nil."
  (let ((day nil))
    ;; check/convert input arg
    (cond ((null arg)
           ;; nil -> today -> 0
           (setq day 0))

          ((numberp arg)
           ;; if arg numberp: 0 today, negative before, positive after
           (setq day arg))

          ((string= arg 'today)
           ;; 'today -> 0
           (setq day 0))

          ;; error case(s): nil
          (t
           (setq day nil)))

    (unless (eq day nil)
      (let* ((now (current-time)) ;; right now
             (now-adjust-secs (floor (unit:second day 'days))) ;; day arg to seconds
             (target (time-add now now-adjust-secs))) ;; actually when we want
        ;; format to spec and return
        (format-time-string (int<taskspace>:config group :format/datetime)
                            target)))))
;; Examples/Tests:
;;                 Today: (int<taskspace>:naming:get:date :default nil)
;;            Also Today: (int<taskspace>:naming:get:date :default 'today)
;; Today Too... I guess?: (int<taskspace>:naming:get:date :default "today")
;;             Not Today: (int<taskspace>:naming:get:date :default -1)
;;             Not Today: (int<taskspace>:naming:get:date :default 1.9)
;;                 Error: (int<taskspace>:naming:get:date :default "jeff")
;;                 Error: (int<taskspace>:naming:get:date :default 'jeff)


(defun int<taskspace>:naming:verify (group name)
  "Verify that NAME is an allowable part of the directory name.

GROUP should be return value from `int<taskspace>:group' (assoc from
`taskspace:groups').

Return nil/non-nil."

  ;; Sanity check 1: `name' must be a valid filename, for a very loose
  ;;                 definition of valid.
  ;; Sanity check 2: Not a path sep in there?
  ;; Valid check:    Verify name obeys my regexp.
  (let ((matched-invalid (string-match file-name-invalid-regexp name))
        (dir-sep-check (file:name name))
        (valid-name (string-match (int<taskspace>:config group :naming/description/rx/valid) name)))

    ;; Check for bad input, fail if so... Bad if:
    ;;   - DOES match /invalid/ filename regexp
    (if (or matched-invalid
            ;; - or non-dir name DOES NOT match input name
            (not (string= name dir-sep-check))
            ;; - or DOES NOT match /valid/ name regexp
            (null valid-name))
        ;; Just return nil for fail.
        nil

      ;; else... Ok name. Do something?

      ;; Verify they didn't try to give us the whole thing? (check for date?)
      ;; (Eh... Not gonna bother right now.)

      ;; return input when valid
      name)))
;; weird name: (int<taskspace>:naming:verify :default "\0")
;; too short:  (int<taskspace>:naming:verify :default "0")
;; good!:      (int<taskspace>:naming:verify :default "hello-there")
;; dir sep:    (int<taskspace>:naming:verify :default "hello-there/here")
;; (int<taskspace>:naming:verify :home "testing-testing")


(defun int<taskspace>:naming:make (group date number description)
  "Create a full name from imputs.

Name created obeys first formatting order found in parts-alists.

GROUP should be return value from `int<taskspace>:group' (assoc from
`taskspace:groups').

DATE should be a date stirng.

NUMBER should be the day's monotonically increasing serial number.

DESCRIPTION should be a string."
  ;; How long is the parts-alist we're looking for?
  ;;   - Stringify each (don't want nulls here...)
  (let* ((func/name "int<taskspace>:naming:make")
         (func/tags '(:create))
         (name-parts (seq-map (lambda (x) (format "%s" x))
                              ;; But take out nulls?
                              (seq-remove #'null
                                          ;; turn inputs into list
                                          (list date number description))))
         (name-len (length name-parts))
         split-alist)
    (nub:debug:func/start
        :taskspace
        func/name
        func/tags
      (cons 'group        group)
      (cons 'date         date)
      (cons 'number       number)
      (cons 'description  description)
      (cons '--name-parts name-parts)
      (cons '--name-len   name-len))

    ;; find the right alist for building the dir string
    (dolist (alist (int<taskspace>:config group :naming/parts-alists) split-alist)
      (when (= name-len (length alist))
        (setq split-alist alist)))

    (nub:debug
        :taskspace
        func/name
        func/tags
      '(:line:each
        "split-alist: %S"
        "  --> null? %S")
      split-alist
      (null split-alist))

    (nub:debug:func/return
        :taskspace
        func/name
        func/tags
      (unless (null split-alist)
        (mapconcat #'identity (seq-remove #'null name-parts)
                   (int<taskspace>:config group :naming/separator))))))
;; (int<taskspace>:naming:make :default "2000" "1" "hi")
;; (int<taskspace>:naming:make :default "2000" nil "hi")
;; (int<taskspace>:naming:make :default "hi" nil nil)
;; (int<taskspace>:naming:make :default "2019-05-14" 0 "testcreate")


;; util to split up dir name and then give desired bit back
;;  - should work for manually made ones that don't have the middle <#> part
(defun int<taskspace>:naming:split (group name part)
  "Split name based on taskspace naming/separator rules.

GROUP should be return value from `int<taskspace>:group' (assoc from
`taskspace:groups').

NAME should just be directory name; do not use path.

Return the requested PART. PART can be one of: 'date 'number 'description"
  (unless (or (null name) (null part))
    ;; unless or if/error?
    (let* ((split-name (split-string name
                                     (int<taskspace>:config group
                                                            :naming/separator)))
           (len-split (length split-name))
           split-alist)

      ;; find the right alist for parsing the split dir string
      (dolist (alist (int<taskspace>:config group :naming/parts-alists) split-alist)
        (when (= len-split (length alist))
          (setq split-alist alist)))

      ;; now try to pull out part requested
      (if (not (assoc part split-alist))
          nil ;; they requested something invalid for this `name'

        ;; figure out what index is desired,
        ;; then pull out the desired string (and return it)
        (nth (cdr (assoc part split-alist)) split-name)))))
;; (int<taskspace>:naming:split :home "2020-03-13_0_container-couchbase" 'date)
;; (int<taskspace>:naming:split :default "2000_0_zort" 'date)
;; (int<taskspace>:naming:split :default "2000_0_zort" nil)
;; (int<taskspace>:naming:split :default "2000_0_zort" 'number)
;; (int<taskspace>:naming:split :default "2000_zort" 'number)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :taskspace 'naming)
