;;; core/modules/elisp/datetime/datetime.el --- Datetime Conversions -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2023-07-11
;; Timestamp:  2023-09-08
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Datetime Conversions
;;
;; NOTE: For unit conversions, see "core/modules/elisp/utils/units.el" for e.g.
;; `unit:second`. Example:
;;   (unit:second 3.2 'hours)
;;
;;; Code:


(require 'cl-lib)


;;------------------------------------------------------------------------------
;; Dates, Times, and Manipulations
;;------------------------------------------------------------------------------

;; TODO:datetime: a `datetime:type' for getting what type an input is?
;;    - e.g. `:unix', `:lisp:time', etc. See `datetime:convert'.

(defun datetime:convert (time type)
  "Convert TIME to a different TYPE of time.

TYPE should be:
  - `:unix', `:seconds' - integer of number of seconds since Unix epoch
  - `:lisp:time' - a \"Lisp Timestamp\" list
                 - list of 2 or 4 integers:
                   - (HIGH LOW)
                   - (HIGH LOW USEC PSEC)
                 - See `current-time'.
  - `:lisp:cal'  - a \"Lisp Calendrical Information\" list
                 - list of 9 integers:
                   - (SECOND MINUTE HOUR DAY MONTH YEAR DOW DST UTCOFF)
                 - See `decode-time' for details on return value.
  - `:ticks'     - Return a cons: (TICKS . FREQ)
                 - See `time-convert' (FORM == t) for details on return value.
TODO: a plist return type or something more user friendly?"
  (let* ((time/converted
          (time-convert
           ;; `time-convert' doesn't deal with the "Lisp Calendrical Information" lists correctly.
           (if (and (listp time)
                    (= (length time) 9))
               (encode-time time)
             time)
           ;; Translate TYPE into the `form' param that `time-convert' expects.
           (pcase type
             ((or :unix :seconds)
              'integer)
             ((or :lisp:time
                  :lisp:cal)
              'list)
             (:ticks
              t)
             (_
              (error "datetime:convert: Unknown TYPE: (type-of %s) %S"
                     (type-of type)
                     type))))))

    ;; Even more converting?
    (if (eq type :lisp:cal)
        ;; Yeah, decode into list of calendar bits.
        (decode-time time/converted)
      ;; Nope; conversion is done already.
      time/converted)))
;; (datetime:convert (decode-time) :unix)
;; (datetime:convert (current-time) :unix)
;; (datetime:convert (current-time) :lisp:time)
;; (datetime:convert (current-time) :lisp:cal)
;; (datetime:convert (current-time) :ticks)


(defun datetime:now (&optional type)
  "Get a Lisp timestamp of current system time.

Return value depends on TYPE:
  - nil, `:lisp:time' - a \"Lisp Timestamp\" list
                      - See `current-time' for details on return value.
  - `:lisp:cal'       - a \"Lisp Calendrical Information\" list
                      - See `decode-time' for details on return value.
TODO: a plist return type or something more user friendly?"
  (pcase type
    ((or :lisp:time 'nil)
     ;; Get now's timestamp, and return.
     (current-time))
     (:lisp:cal
      ;; Get now's timestamp, decode it, and return.
      (decode-time))
     (_
      (error "datetime:now: Unknown TYPE: (type-of %s) %S"
             (type-of type)
             type))))
;; (datetime:now)
;; (datetime:now :lisp:time)
;; (datetime:now :lisp:cal)


(defun datetime:get (field &optional time)
  "Get a FIELD from TIME.

FIELD should be a keyword and indicates the return type/value:
  -   FIELD   - Return Type, Value, Range
  - `:second' - The number of seconds past the minute (integer [0, 59))
  - `:minute' - The number of minutes past the hour (integer [0, 59))
  - `:hour'   - The hour of the day (integer [0, 23])
  - `:day'    - The day of the month (integer [1, 31])
  - `:month'  - The month of the year (integer [1, 12])
  - `:year'   - The year, (integer usually > 1900)
  - `:dow'    - The day of week (integer [0, 6] where 0 stands for Sunday)
  - `:dst'    - t if daylight saving time is effect
              - nil if it is not in effect
              - -1 if this information is not available.
  - `:utcoff' - Universal Time offset in seconds (integer)

TIME should be:
  - nil, `:now', `now': now
  - Lisp timestamp: `(datetime:now :lisp:time)', `current-time'
  - Lisp calendrical information: `(datetime:now :lisp:cal)'"
  (pcase time
    ;; Must check for nil before `listp'.
    ((or 'nil :now 'now)
     (setq time (current-time)))
    ((pred listp)
     ;; ok; no-op; assume the list is a correct time.
     )
    (_
     (error "datetime:now: Unknown TIME: (type-of %s) %S"
           (type-of time)
           time)))

  (pcase (length time)
    ;;"Lisp timestamp"
    ;; aka `current-time'
    ;; i.e. (current-time) -> (25773 44748 901223 454000)
    (4
     ;; Recurse and ask ourselves to figure this out.
     (datetime:get field (decode-time time)))

    ;; "Lisp calendrical information"
    ;; aka `decode-time'
    ;; i.e. (decode-time) -> (20 35 12 11 7 2023 2 t -25200)
    (9
     ;; TIME is: '(seconds minutes hour day month year dow dst utcoff)
     (pcase field
       ((or :second :seconds)
        (nth 0 time))
       ((or :minute :minutes)
        (nth 1 time))
       ((or :hour :hours)
        (nth 2 time))
       ((or :day :days)
        (nth 3 time))
       ((or :month :months)
        (nth 4 time))
       ((or :year :years)
        (nth 5 time))
       ((or :dow :day-of-week)
        (nth 6 time))
       (:dst
        (nth 7 time))
       ((or :utcoff :zone :tz :time-zone)
        (nth 8 time))))

    (_
     (error "datetime:get: Unknown TIME list format: (type-of %s) %S"
            (type-of time)
            time))))
;; (datetime:get :day :now)
;; (datetime:get :hour :now)


(cl-defun datetime:replace (time &key second minute hour day month year)
  "Return TIME with specified fields replaced.

TIME should be:
  - Lisp timestamp: `(datetime:now :lisp:time)', `current-time'
  - Lisp calendrical information: `(datetime:now :lisp:cal)'

Fields replacable are
  - SECOND - The number of seconds past the minute (integer [0, 59))
  - MINUTE - The number of minutes past the hour (integer [0, 59))
  - HOUR   - The hour of the day (integer [0, 23])
  - DAY    - The day of the month (integer [1, 31])
  - MONTH  - The month of the year (integer [1, 12])
  - YEAR   - The year, (integer usually > 1900)

Return a \"Lisp Calendrical Information\" list, which see `decode-time'."
  (unless (listp time)
    (error "datetime:replace: Unknown TIME: (type-of %s) %S"
           (type-of time)
           time))

  (pcase (length time)
    ;;"Lisp timestamp"
    ;; aka `current-time'
    ;; i.e. (current-time) -> (25773 44748 901223 454000)
    (4
     (setq time (decode-time time)))

    ;; "Lisp calendrical information"
    ;; aka `decode-time'
    ;; i.e. (decode-time) -> (20 35 12 11 7 2023 2 t -25200)
    (9
     ;; TIME is: '(seconds minutes hour day month year dow dst utcoff)
     ;; Ok; no-op
     nil)

    (_
     (error "datetime:replace: Unknown TIME list format: (type-of %s) %S"
            (type-of time)
            time)))

  (make-decoded-time
   :second (or second (datetime:get :second time))
   :minute (or minute (datetime:get :minute time))
   :hour   (or hour   (datetime:get :hour   time))
   :day    (or day    (datetime:get :day    time))
   :month  (or month  (datetime:get :month  time))
   :year   (or year   (datetime:get :year   time))))
;; (datetime:replace (datetime:now) :day 1 :second 0 :minute 0 :hour 0)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :datetime 'datetime)
