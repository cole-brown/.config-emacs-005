;;; core/modules/elisp/datetime/timestamp.el --- Stamp Things with Times -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2020-11-16
;; Timestamp:  2023-07-19
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Stamp Things with Times
;;
;; Named datetime snamping.
;;
;;; Code:

(imp:require :datetime 'format)


;;-------------------------------Stamping Time----------------------------------
;;--                 Dates, Times, Datetimes, Timedates...                    --
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Timestamp Functions (Also Datestamp)
;;------------------------------------------------------------------------------

(defun datetime:timestamp:insert (&rest name)
  "Insert a timestamp NAME into current buffer at point.

Timestamp is of current time and is inserted into current buffer at point."
  (insert (apply #'datetime:format name)))


;;--------------------------------------------------------------------------------
;; Interactive: Prompt for Timestamp
;;--------------------------------------------------------------------------------

(defvar int<datetime>:timestamp:insert/prompt:history nil
  "History variable for `datetime:cmd:timestamp:insert/prompt'.

Just a bucket to hold history for datetime commands to keep segregated from general
history.")


(defun datetime:cmd:timestamp:insert/prompt ()
  "Insert a timestamp into current buffer at point.

Timestamp format will be prompted with auto-complete based on stored `datetime'
formats."
  (interactive)
  ;; And insert whatever the user chose.
  (datetime:timestamp:insert
   ;; Prompt user for what format to use.
   (completing-read "Datetime Format: "
                    ;; List of Format Names
                    (seq-map
                     ;; Trim the prefix string off for display purposes.
                     (lambda (format)
                       (let ((name (car format)))
                         (string-trim-left name
                                           (string-trim-right (jerky:key:string 'datetime 'format 'xTRIMx)
                                                              "xTRIMx"))))
                     ;; Get all datetime formats.
                     (jerky:has (jerky:key:string 'datetime 'format)))
                    nil
                    t ; Must get a match.
                    nil
                    int<datetime>:timestamp:insert/prompt:history
                    nil) ; No default value?
   ))


;;------------------------------------------------------------------------------
;; Interactive: Insert Specific Timestamp
;;------------------------------------------------------------------------------

(defun datetime:cmd:timestamp:insert/rfc-3339 ()
  "Insert a full (date & time) rfc-3339 formatted timestamp.

Timestamp is of current time and is inserted into current buffer at point."
  (interactive)
  (datetime:timestamp:insert 'rfc-3339 'datetime))


(defun datetime:cmd:timestamp:insert/iso-8601 ()
  "Insert a full (date & time) ISO-8601 formatted timestamp.

Timestamp is of current time and is inserted into current buffer at point."
  (interactive)
  (datetime:timestamp:insert 'iso-8601 'datetime))
;; (datetime:cmd:timestamp:insert/iso-8601)


(defun datetime:cmd:timestamp:insert/org ()
  "Insert a timestamp formatted \"[yyyy-mm-dd]\".

Timestamp is of current time and is inserted into current buffer at point."
  (interactive)
  (datetime:timestamp:insert 'org 'inactive 'date))


;;------------------------------------------------------------------------------
;; Misc
;;------------------------------------------------------------------------------

;; Was used in an old weekly-status template; may be useful again some day?
(defun datetime:timestamp:next-friday (format)
  "Return next Friday's date as FORMAT `format-time-string' string."
  (let ((today (nth 6 (decode-time (current-time)))))
    (format-time-string
     format
     (time-add
      (current-time)
      (days-to-time
       (if (eq 5 today) ; saturday is only day bigger than friday
           6
         (- 5 today)))))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :datetime 'timestamp)
