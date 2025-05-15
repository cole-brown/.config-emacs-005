;;; core/modules/elisp/datetime/format.el --- Datetime Formatting & Formats -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2020-11-16
;; Timestamp:  2023-07-21
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Datetime Formatting & Formats
;;
;;; Code:


;;------------------------------Formatting Time---------------------------------
;;--                 Dates, Times, Datetimes, Timedates...                    --
;;------------(Format, not reformat... Don't, like, delete time.)---------------

(imp:require :elisp 'utils)
(imp:require :jerky)

(imp:require :datetime 'datetime)


;;------------------------------------------------------------------------------
;; Format: Getters, Setters.
;;------------------------------------------------------------------------------

(defun datetime:format/get (&rest name)
  "Return a datetime format string by NAME.

NAME can be strings, symbols, or list(s) of such.

Prepends '(datetime format) to the NAME before asking `jerky' so that all values
are stored under that \"namespace\" or tree branch."
  (apply #'jerky:get 'datetime 'format name))
;; (datetime:format/get 'rfc-3339 'datetime)
;; (datetime:format 'iso-8601 'datetime)


(defun datetime:format/set (&rest args)
  "Set a datetime format string.

Splits ARGS out into a 'name' and 'keyword-args'.

The 'name' is everything that comes before any of our keywords. Prepends
'(datetime format) to the 'name' before asking `jerky' so that all values are
stored under that \"namespace\" or tree branch.

'keywords-args' are:
  `:value'  - datetime format string
  `:docstr' - documentation string"
  (apply #'jerky:set 'datetime 'format args))


;;--------------------------------------------------------------------------------
;; Formatters
;;--------------------------------------------------------------------------------

(defun datetime:format (&rest args)
  "Return a formatted datetime string based on ARGS.

Use `datetime:format/get' to get a datetime format and then calls
`format-time-string' with format and options to get a time string.

Splits ARGS out into a 'name' and a 'keyword-args' plist.
The 'name' is everything that comes before any of our keywords.

Optional Keywords:
  `:time' - A time to use instead of now.
  `:zone' - A timezone to use instead of the local one.

TIME and ZONE should both be compatible with `format-time-string', with a few
additions:
  TIME should be one of:
    - nil, `:now', `now'
      - Use current time.
    - a \"Lisp Timestamp\" list
      - See `current-time' for details on return value.
    - a \"Lisp calendrical information\" list
      - See `decode-time' for details on return value.

  ZONE should be one of:
    - nil, `local' - Local Time
    - `wall'       - Wall Time
    - `utc'        - Universal Time
    - string       - a string as in the `TZ' environment variable
    - list         - as from `current-time-zone'
    - integer      - as from `decode-time'

Return string from `format-time-string'."
  (let* ((name-and-kwargs (elisp:parse:args+kwargs args :time :zone))
         (name   (car name-and-kwargs))
         (kwargs (cdr name-and-kwargs))
         (time   (plist-get kwargs :time))
         (zone   (plist-get kwargs :zone))
         ;; Translate TIME to something `format-time-string' understands.
         (time (if (memq time '(nil :now now))
                   (current-time)
                 (datetime:convert time :lisp:time)))
         (time/len (length time))
         ;; Translate ZONE to something `format-time-string' understands.
         (zone (pcase zone
                 ((or 'nil :local 'local)
                  nil)
                 ((or :wall 'wall)
                  ;; Seems to be the same as `local' time, but the docstr describes it
                  ;; different so... maybe it's different for some people. Keep
                  ;; separated from `local' time.
                  'wall)
                 ('utc
                  t)
                 (_
                  ;; Use ZONE as-is; no error checking. Cuz how the heck would I
                  ;; error check all of the timezone string possibilities, and
                  ;; there's not exactly functions for asserting validity of
                  ;; lists/integers from `current-time-zone'/`decode-time'.
                  zone))))

    ;; Should have a "Lisp Timestamp" now.
    (if (or (= time/len 2)
            (= time/len 4))
        (format-time-string (apply #'datetime:format/get name) time)
      (error "datetime:format: Unknown TIME type/form: (type-of %s) %S"
             (type-of time)
             time))))
;; (datetime:format 'rfc-3339 'datetime)
;; (datetime:format 'rfc-3339 'datetime :zone 'wall)
;; (datetime:format 'rfc-3339 'datetime :zone 'utc)
;; (datetime:format 'rfc-3339 'datetime :time (current-time))
;; (datetime:format 'rfc-3339 'datetime :time (decode-time (current-time)))
;; (datetime:format 'rfc-3339 'datetime :time (datetime:replace (current-time) :day 1 :hour 0 :minute 0 :second 0))


;;------------------------------------------------------------------------------
;; Some useful formats.
;;------------------------------------------------------------------------------

(defun datetime:init ()
  "Set the predefined datetime format strings into jerky."
  ;;--------------------
  ;; ISO-8601 / RFC-3339 Formats
  ;;--------------------

  ;;---
  ;; Date Only
  ;;---
  ;; ISO-8601: short (date only; no timestamp)
  (datetime:format/set 'iso-8601 'date ;; was: 'iso-8601 'short
                       :value  "%Y-%m-%d"
                       :docstr "ISO-8601/RFC-3339; date-only.")
  ;; aka: 'rfc-3339 'date
  (datetime:format/set 'rfc-3339 'date
                       :value  "%Y-%m-%d"
                       :docstr "ISO-8601/RFC-3339; date-only.")

  ;; ISO-8601: But meh. No separators makes it much less useful to the brain.
  (datetime:format/set 'yyyymmdd
                       :value  "%Y%m%d"
                       :docstr "Why would you use this?! Give my eyes a break.")

  ;;---
  ;; Date & Time
  ;;---
  ;; RFC-3339: full (space separator)
  (datetime:format/set 'rfc-3339 'datetime
                       :value  "%Y-%m-%d %H:%M:%S"
                       :docstr "RFC-3339; datetime with space separator for in text.")

  ;; ISO-8601: full ('T' separator; for logs, etc)
  (datetime:format/set 'iso-8601 'datetime
                       :value  "%Y-%m-%dT%T%z"
                       :docstr "ISO-8601; full datetime with 'T' separator for in logs, etc.")

  ;;--------------------
  ;; Org-Mode Formats
  ;;--------------------

  ;; org-mode inactive: useful even when not in org-mode.
  (datetime:format/set 'org 'inactive 'date ;; was: 'org-inactive
                       :value  "[%Y-%m-%d]"
                       :docstr "org-time-stamp-inactive sans day name e.g.: [2022-06-01]")

  ;; org-mode inactive: useful even when not in org-mode.
  (datetime:format/set 'org 'inactive 'date-day
                       :value  "[%Y-%m-%d %a]"
                       :docstr "org-time-stamp-inactive e.g.: [2022-06-01 Wed]")

  ;; org-mode inactive: useful even when not in org-mode.
  (datetime:format/set 'org 'inactive 'rfc-3339
                       :value  "[%Y-%m-%d %H:%M:%S]"
                       :docstr "org-time-stamp-inactive but RFC-3339 e.g.: [2022-06-01 22:05:11]")

  ;; org-mode inactive: useful even when not in org-mode.
  (datetime:format/set 'org 'inactive 'full
                       :value  "[%Y-%m-%d %a %H:%M:%S]"
                       :docstr "org-time-stamp-inactive with time e.g.: [2022-06-01 Wed 22:05:11]")

  ;; Could do the same for active, but I use them less outside org files...

  ;;--------------------
  ;; Misc Formats
  ;;--------------------

  ;; Filename: full ('-T-' date/time separator & '-' time field separator; for filenames)
  (datetime:format/set 'file 'datetime
                       :value  "%Y-%m-%d-T-%H-%M-%S"
                       :docstr (concat "ISO-8601/RFC-3339...ish format, but works on Windows. "
                                       "Can strip hypens to get to ISO-8601 basic "
                                       "format (YYYYMMDDThhmmss)."))

  ;; USA Date: Pretty much the worst format ever invented.
  (datetime:format/set 'usa 'date
                       :value  "%m-%d-%Y"
                       :docstr (concat "Bad, US American format. "
                                       "Just terrible - why do we use this? "
                                       "I am ashamed."))

  ;; USA Date w/ 24 hr time: Pretty much the worst format ever invented, plus
  ;; extra confusion because no one understands 24 hour time over here...
  (datetime:format/set 'usa 'datetime
                       :value  "%m-%d-%Y %H:%M:%S"
                       :docstr (concat "Bad, US American format. "
                                       "Just terrible - why do we use this? "
                                       "I am ashamed.")))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :datetime 'format)
