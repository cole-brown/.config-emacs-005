;;; namespaced/datetime/format.el --- Datetime Formatting & Formats -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2020-11-16
;; Timestamp:  2025-10-28
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Datetime Formatting & Formats
;;
;; Fomat: Dates, Times, Datetimes, Timedates...
;;
;; (Format, not reformat... Don't, like, delete time.)
;;
;;; Code:

(require 'cl-lib)

(imp-require :datetime 'datetime)


;;------------------------------------------------------------------------------
;; Format: Getters, Setters.
;;------------------------------------------------------------------------------

(defvar _:datetime:formats
  `(
    ;;--------------------
    ;; ISO-8601 / RFC-3339 Formats
    ;;--------------------

    ;;---
    ;; Date Only
    ;;---
    ;; ISO-8601: short (date only; no timestamp)
    (:iso-8601:date "%Y-%m-%d" "ISO-8601/RFC-3339; date-only.")
    ;; RFC-3339: short (date only; no timestamp)
    (:rfc-3339:date "%Y-%m-%d" "ISO-8601/RFC-3339; date-only.")

    ;; ISO-8601: But meh. No separators makes it much less useful to the brain.
    (:yyyymmdd "%Y%m%d" "ISO-8601 no separators")

    ;;---
    ;; Date & Time
    ;;---
    ;; RFC-3339: full (space separator)
    (:rfc-3339:datetime "%Y-%m-%d %H:%M:%S" "RFC-3339; datetime with space separator.")

    ;; ISO-8601: full ('T' separator; for logs, etc)
    (:iso-8601:datetime "%Y-%m-%dT%T%z" "ISO-8601; full datetime with 'T' separator for in logs, etc.")

    ;;--------------------
    ;; Org-Mode Formats
    ;;--------------------

    ;;---
    ;; Org-Mode Inactive
    ;;---
    (:org:inactive:date     "[%Y-%m-%d]"
     "org-time-stamp-inactive sans day name e.g.: [2022-06-01]")
    (:org:inactive:date-day "[%Y-%m-%d %a]"
     "org-time-stamp-inactive e.g.: [2022-06-01 Wed]")
    (:org:inactive:rfc-3339 "[%Y-%m-%d %H:%M:%S]"
     "org-time-stamp-inactive but RFC-3339 e.g.: [2022-06-01 22:05:11]")
    (:org:inactive:full     "[%Y-%m-%d %a %H:%M:%S]"
     "org-time-stamp-inactive with time e.g.: [2022-06-01 Wed 22:05:11]")

    ;; Could do the same for active, but I use them less outside org files...

    ;;--------------------
    ;; Misc Formats
    ;;--------------------

    ;; Filename: full ('-T-' date/time separator & '-' time field separator; for filenames)
    (:file:datetime "%Y-%m-%d-T-%H-%M-%S"
     ,(concat "ISO-8601/RFC-3339...ish format, but works on Windows. "
              "Can strip hypens to get to ISO-8601 basic "
              "format (YYYYMMDDThhmmss)."))

    ;; USA Date: Pretty much the worst format ever invented.
    (:usa:date "%m-%d-%Y")

    ;; USA Date w/ 24 hr time: Pretty much the worst format ever invented, plus
    ;; extra confusion because no one understands 24 hour time over here...
    (:usa:datetime "%m-%d-%Y %H:%M:%S"))
  "Saved/named datetime formats.")


(defun datetime:format:get (key)
  "Look up KEY in `_:datetime:formats' and return the format string."
  (nth 0 (alist-get key _:datetime:formats)))
;; (datetime:format:get :rfc-3339:datetime)
;; (datetime:format:get :iso-8601:datetime)


(defun datetime:format:set (key format docstr)
  "Add a datetime format entry to `_:datetime:formats' alist."
  (push (list key format docstr) _:datetime:formats))


;;--------------------------------------------------------------------------------
;; Formatters
;;--------------------------------------------------------------------------------

(cl-defun datetime:format (key &key time zone)
  "Return a formatted datetime string based on KEY.

Use `datetime:format:get' to get a datetime format and then calls
`format-time-string' with format and options to get a time string.

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
  (let* ((format (datetime:format:get key))
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

    (when (not format)
      (error "datetime:format: Unknown format KEY '%s'" key))

    ;; Should have a "Lisp Timestamp" now.
    (if (or (= time/len 2)
            (= time/len 4))
        (format-time-string format time zone)
      (error "datetime:format: Unknown TIME type/form: (type-of %s) %S"
             (type-of time)
             time))))
;; (datetime:format :rfc-3339:datetime)
;; (datetime:format :rfc-3339:datetime :zone 'wall)
;; (datetime:format :rfc-3339:datetime :zone 'utc)
;; (datetime:format :rfc-3339:datetime :time (current-time))
;; (datetime:format :rfc-3339:datetime :time (decode-time (current-time)))
;; (datetime:format :rfc-3339:datetime :time (datetime:replace (current-time) :day 1 :hour 0 :minute 0 :second 0))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide datetime format)
