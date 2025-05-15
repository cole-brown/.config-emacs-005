;;; core/modules/emacs/buffer/name.el --- Buffer Name Functions -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <code@brown.dev>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2020-12-04
;; Timestamp:  2023-06-21
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Buffer Name Functions
;;
;;; Code:


(require 'cl-lib)


;;----------------------------------Buffers------------------------------------
;;--                    And Things to Make Them Better.                      --
;;-----------------------------------------------------------------------------

(defcustom buffer:format:bookend/normal
  '("§-" "-§")
  "Start/end strings for special-name formats."
  :group 'buffer:group
  :type '(list string string))


(defcustom buffer:format:bookend/high
  '("§!" "!§")
  "Start/end strings for special-name formats."
  :group 'buffer:group
  :type '(list string string))


(defcustom buffer:format:bookend/info
  '("ⓘ-" "-ⓘ")
  "Start/end strings for special-name formats."
  :group 'buffer:group
  :type '(list string string))


(defcustom buffer:format:priorities
  '((:low    . buffer:format:bookend/normal) ;; no actual low right now
    (:medium . buffer:format:bookend/normal)
    (:high   . buffer:format:bookend/high)

    ;; un-normal priority levels
    (:info   . buffer:format:bookend/info))
  "Priority (for `buffer:special-name') to bookend consts."
  :group 'buffer:group
  :type '(alist :key-type symbol :value-type symbol))


(defcustom buffer:regex:bookend
  (list 'group
        '(optional " ") ; Allow optional make-less-visible leading space.

        ;; Start Bookend
        (list 'or
              (nth 0 buffer:format:bookend/normal)
              (nth 0 buffer:format:bookend/high)
              (nth 0 buffer:format:bookend/info))

        '(optional " ") ; Optional padding space before name.

        ;; Actual Buffer Name
        '(one-or-more printing)

        '(optional " ") ; Optional padding space after name.

        ;; End Bookend
        (list 'or
              (nth 1 buffer:format:bookend/normal)
              (nth 1 buffer:format:bookend/high)
              (nth 1 buffer:format:bookend/info)))
  "Regex for matching a bookended buffer name string.
Will need to update (or make smarter) if get more actual priority levels."
  :group 'buffer:group
  :type  'sexp)


(defcustom buffer:regex:specials
  (list
   'or
   ;;---
   ;; Emacs
   ;;---
   ;; Special buffers start with "*", optionally with leading space.
   '(group
     (optional " ")
     "*" ;; literal asterisk
     (one-or-more printing)
     "*")

   ;;---
   ;; :emacs/buffer
   ;;---
   ;; Bookended Buffer Names are special
   buffer:regex:bookend)
  "`rx' Regular Expression Sexpr for matching a special buffer name string.

Special buffers are:
  - Emacs' special, visible \"*<name>*\" buffers.
  - Emacs' special, less-visible \" *<name>*\" buffers (leading space).
  - :emacs/buffer's special bookended buffers: \"§- <name> -§\" (and other bookends)."
  :group 'buffer:group
  :type  'sexp)
;; buffer:regex:specials
;; (rx-to-string buffer:regex:specials :no-group)


(defun buffer:regex:specials ()
  "Compile variable `buffer:regex:specials' to a regex string."
  (rx-to-string (list 'and
                      'line-start
                      buffer:regex:specials
                      'line-end)
                :no-group))
;; (buffer:regex:specials)


;;-----------------------------------------------------------------------------
;; Naming Functions
;;-----------------------------------------------------------------------------

(defun buffer:name:special (title &optional desc priority)
  "Format TITLE and DESC strings for `buffer:name:special' with PRIORITY.

PRIORITIES can be: :low, :medium, or :high.

TITLE and DESC are formatted by bookending with
`buffer:format:priorities' bookends based on PRIORITY
setting, with nil being medium priority."

  ;; PRIORITY is either known or forced to medium
  (let ((priority (if (assoc priority buffer:format:priorities)
                      priority
                    :medium))
        ;; look for bookends in list, default if fail/nil
        (bookends (or (symbol-value
                       (cdr (assoc priority buffer:format:priorities)))
                      buffer:format:bookend/normal))
        ;; "title" or "title: desc"
        (inner-fmt (if (null desc) "%s" "%s: %s")))

    ;; inner format: "title" or "title: desc", as decided above by `inner-fmt'
    (format
     ;; outer format: "<bookend> <inner-format-%s> <bookend>"
     (format "%s %s %s"
             (nth 0 bookends)
             inner-fmt
             (nth 1 bookends))
     title desc)))
;; (buffer:name:special "jeff")
;; (buffer:name:special "jeff" "is here")
;; (buffer:name:special "jeff" nil :high)
;; (buffer:name:special "jeff" "is here" :high)


(defun buffer:special? (buffer-or-name)
  "Returns non-nil if BUFFER-OR-NAME is a specially named buffer.

Special buffers are:
  - Emacs' special, visible \"*<name>*\" buffers.
  - Emacs' special, less-visible \" *<name>*\" buffers (leading space).
  - :emacs/buffer's special bookended buffers: \"§- <name> -§\" (and other bookends)."
  (string-match-p
   (buffer:regex:specials)
   (if (bufferp buffer-or-name)
       (buffer-name buffer-or-name)
     buffer-or-name)))
;; (buffer:special? (buffer:name:special "jeff"))
;; (buffer:special? "*Messages*")
;; (buffer:special? "file.txt")


;;------------------------------------------------------------------------------
;; Copy Buffer File/Dir Name Functions
;;------------------------------------------------------------------------------
;; See `:path' module functions:
;;   - `path:buffer:copy'
;;   - `path:cmd:buffer:copy:absolute'
;;   - `path:cmd:buffer:copy:project'


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :buffer 'name)
