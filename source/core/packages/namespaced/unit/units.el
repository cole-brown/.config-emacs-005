;;; namespaced/unit/units.el --- Nicer units? -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2023-02-24
;; Timestamp:  2025-09-22
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Kilobytes, hogsheads, nautical miles...
;;
;;; Code:


;;--------------------------------------------------------------------------------
;; Units in Symbol Properties
;;--------------------------------------------------------------------------------

(defconst int<unit>:symbol:property 'unit
  "Symbol Property name for units.")


(defun unit:set (symbol unit)
  "Store UNIT in SYMBOL's properties."
  (put symbol int<unit>:symbol:property unit))


(defun unit:get (symbol)
  "Get UNIT from SYMBOL's properties."
  (get symbol int<unit>:symbol:property))


(defmacro int<unit>:convert (caller value unit dictionary)
  "Convert VALUE of UNIT into an amount of base units given DICTIONARY.

VALUE should be a `numberp' (float or int number).

UNIT should be a symbol (and a key from DICTIONARY).

DICTIONARY should be an alist of cons: (unit-symbol . amount-of-base-units)
e.g.: See variables `int<units>:bytes' and `int<units>:seconds'.

CALLER should be a string of calling function's name.

Example:
  (int<unit>:convert \"example\" 1   'h  int<units>:seconds)
    -> 3600.0
  (int<unit>:convert \"example\" 0.5 'kb int<units>:bytes)
    -> 500.0"
  (declare (pure t) (side-effect-free t))
  (let ((macro:dictionary/name (format "%s" dictionary))) ; for more helpful error message.
    `(let* ((macro:func/name       ,caller)
            (macro:value           ,value)
            (macro:unit            ,unit)
            (macro:dictionary      ,dictionary)
            (macro:unit/multiplier (alist-get macro:unit macro:dictionary)))
       ;;------------------------------
       ;; Error Checks
       ;;------------------------------
       (unless macro:unit/multiplier
         (nub:error
             :innit
             macro:func/name
           "Unknown unit name '%S'! See `%s' for allowed units."
           macro:unit
           ,macro:dictionary/name))

       ;; Allow floats as well as ints for e.g.: (unit:second 0.5 'gb)
       (unless (numberp macro:value)
         (nub:error
             :innit
             macro:func/name
           "VALUE should be a number (int or float). Got %S: %S"
           (type-of macro:value)
           macro:value))

       ;;------------------------------
       ;; Unit Conversion
       ;;------------------------------
       ;; Calculate & return as float value; caller can cast to int w/ `floor' if needed.
       (* (float macro:value)
          macro:unit/multiplier))))
;; (int<unit>:convert "example" 0.5 'kb int<units>:bytes)
;; Errors w/ "example" caller name?
;;   (int<unit>:convert "example" 0.5 'kb nil)


;;------------------------------------------------------------------------------
;; Bytes
;;------------------------------------------------------------------------------

(defconst int<units>:bytes:multiplier/decimal (expt 10 3)
  "Decimal / IEC / SI units are multiplied by 1000.")


(defconst int<units>:bytes:multiplier/binary  (expt 2 10)
  "Binary units are multiplied by the closest power of 2 to 1000.")


(defconst int<units>:bytes
  (list
   (cons 'b           1)
   (cons 'B           1)
   (cons 'byte        1)
   (cons 'bytes       1)

   (cons 'kb          (expt int<units>:bytes:multiplier/decimal 1))
   (cons 'kB          (expt int<units>:bytes:multiplier/decimal 1))
   (cons 'kilobyte    (expt int<units>:bytes:multiplier/decimal 1))
   (cons 'kilobytes   (expt int<units>:bytes:multiplier/decimal 1))
   (cons 'kib         (expt int<units>:bytes:multiplier/binary  1))
   (cons 'kiB         (expt int<units>:bytes:multiplier/binary  1))
   (cons 'kibibyte    (expt int<units>:bytes:multiplier/binary  1))
   (cons 'kibibytes   (expt int<units>:bytes:multiplier/binary  1))

   (cons 'mb          (expt int<units>:bytes:multiplier/decimal 2))
   (cons 'MB          (expt int<units>:bytes:multiplier/decimal 2))
   (cons 'megabyte    (expt int<units>:bytes:multiplier/decimal 2))
   (cons 'megabytes   (expt int<units>:bytes:multiplier/decimal 2))
   (cons 'mib         (expt int<units>:bytes:multiplier/binary  2))
   (cons 'MiB         (expt int<units>:bytes:multiplier/binary  2))
   (cons 'mebibyte    (expt int<units>:bytes:multiplier/binary  2))
   (cons 'mebibytes   (expt int<units>:bytes:multiplier/binary  2))

   (cons 'gb          (expt int<units>:bytes:multiplier/decimal 3))
   (cons 'GB          (expt int<units>:bytes:multiplier/decimal 3))
   (cons 'gigabyte    (expt int<units>:bytes:multiplier/decimal 3))
   (cons 'gigabytes   (expt int<units>:bytes:multiplier/decimal 3))
   (cons 'gib         (expt int<units>:bytes:multiplier/binary  3))
   (cons 'GiB         (expt int<units>:bytes:multiplier/binary  3))
   (cons 'gibibyte    (expt int<units>:bytes:multiplier/binary  3))
   (cons 'gibibytes   (expt int<units>:bytes:multiplier/binary  3))

   (cons 'tb          (expt int<units>:bytes:multiplier/decimal 4))
   (cons 'TB          (expt int<units>:bytes:multiplier/decimal 4))
   (cons 'terabyte    (expt int<units>:bytes:multiplier/decimal 4))
   (cons 'terabytes   (expt int<units>:bytes:multiplier/decimal 4))
   (cons 'tib         (expt int<units>:bytes:multiplier/binary  4))
   (cons 'TiB         (expt int<units>:bytes:multiplier/binary  4))
   (cons 'tebibyte    (expt int<units>:bytes:multiplier/binary  4))
   (cons 'tebibytes   (expt int<units>:bytes:multiplier/binary  4))

   (cons 'pb          (expt int<units>:bytes:multiplier/decimal 5))
   (cons 'PB          (expt int<units>:bytes:multiplier/decimal 5))
   (cons 'petabyte    (expt int<units>:bytes:multiplier/decimal 5))
   (cons 'petabytes   (expt int<units>:bytes:multiplier/decimal 5))
   (cons 'pib         (expt int<units>:bytes:multiplier/binary  5))
   (cons 'PiB         (expt int<units>:bytes:multiplier/binary  5))
   (cons 'pebibyte    (expt int<units>:bytes:multiplier/binary  5))
   (cons 'pebibytes   (expt int<units>:bytes:multiplier/binary  5))

   (cons 'eb          (expt int<units>:bytes:multiplier/decimal 6))
   (cons 'EB          (expt int<units>:bytes:multiplier/decimal 6))
   (cons 'exabyte     (expt int<units>:bytes:multiplier/decimal 6))
   (cons 'exabytes    (expt int<units>:bytes:multiplier/decimal 6))
   (cons 'eib         (expt int<units>:bytes:multiplier/binary  6))
   (cons 'EiB         (expt int<units>:bytes:multiplier/binary  6))
   (cons 'exbibyte    (expt int<units>:bytes:multiplier/binary  6))
   (cons 'exbibytes   (expt int<units>:bytes:multiplier/binary  6))

   (cons 'zb          (expt int<units>:bytes:multiplier/decimal 7))
   (cons 'ZB          (expt int<units>:bytes:multiplier/decimal 7))
   (cons 'zettabyte   (expt int<units>:bytes:multiplier/decimal 7))
   (cons 'zettabytes  (expt int<units>:bytes:multiplier/decimal 7))
   (cons 'zib         (expt int<units>:bytes:multiplier/binary  7))
   (cons 'ZiB         (expt int<units>:bytes:multiplier/binary  7))
   (cons 'zebibyte    (expt int<units>:bytes:multiplier/binary  7))
   (cons 'zebibytes   (expt int<units>:bytes:multiplier/binary  7))

   (cons 'yb          (expt int<units>:bytes:multiplier/decimal 8))
   (cons 'YB          (expt int<units>:bytes:multiplier/decimal 8))
   (cons 'yottabyte   (expt int<units>:bytes:multiplier/decimal 8))
   (cons 'yottabytes  (expt int<units>:bytes:multiplier/decimal 8))
   (cons 'yib         (expt int<units>:bytes:multiplier/binary  8))
   (cons 'YiB         (expt int<units>:bytes:multiplier/binary  8))
   (cons 'yobibyte    (expt int<units>:bytes:multiplier/binary  8))
   (cons 'yobibytes   (expt int<units>:bytes:multiplier/binary  8)))
  "Alist of byte units in terms of how many bytes they are.")


(defun unit:byte (value unit)
  "Return integer of bytes for VALUE of UNITS bytes.

VALUE should be a float or int number.

UNIT should be a symbol (and a key from alist `int<units>:bytes').

Example:
  (unit:byte 2 'kb)
    -> 2000
  (unit:byte 2 'kib)
    -> 2048"
  (declare (pure t) (side-effect-free t))
  (floor ; No fractional bytes; cast to int.
   (int<unit>:convert "unit:byte"
                      value
                      unit
                      int<units>:bytes)))
;; (unit:byte 100 'kb)
;; (unit:byte 0.5 'kb)


;; TODO:units: Convert unit to human-readable value:
;; (defun unit:byte:human (value unit)
;;   "Convert VALUE of UNIT bytes to a pretty, human-readable value/unit.
;;
;; Return cons: (float . symbol)
;;   - float will be new value
;;   - symbol will be new unit
;; Example:
;;   (unit:byte:human 10822759 'b)
;;     -> (10.822759 . 'megabytes)"
;;
;;   )


;;--------------------------------------------------------------------------------
;; Time
;;--------------------------------------------------------------------------------

(defconst int<units>:seconds
  (list
   (cons 's       1)
   (cons 'sec     1)
   (cons 'secs    1)
   (cons 'second  1)
   (cons 'seconds 1)

   (cons 'm       60)
   (cons 'min     60)
   (cons 'mins    60)
   (cons 'minute  60)
   (cons 'minutes 60)

   (cons 'h     (* 60 60))
   (cons 'hr    (* 60 60))
   (cons 'hrs   (* 60 60))
   (cons 'hour  (* 60 60))
   (cons 'hours (* 60 60))

   (cons 'day  (* 60 60 24))
   (cons 'days (* 60 60 24))

   (cons 'week  (* 60 60 24 7))
   (cons 'weeks (* 60 60 24 7))

   (cons 'month  (* 60 60 24 (/ 365.0 12))) ; ~30.42 days in a month, on average.
   (cons 'months (* 60 60 24 (/ 365.0 12)))

   (cons 'year  (* 60 60 24 365)) ; ...ignoring leap days, seconds.
   (cons 'years (* 60 60 24 365)))

  "Alist of time units in terms of how many seconds they are.")


(defun unit:second (value unit)
  "Return float number of seconds for VALUE of UNITS bytes.

VALUE should be a `numberp' (float or int number).

UNIT should be a symbol (and a key from alist `int<units>:seconds').

Example:
  (unit:second 1 'h)
    -> 3600.0
  (unit:second 0.5 'm)
    -> 30.0"
  (declare (pure t) (side-effect-free t))
  (int<unit>:convert "unit:second"
                     value
                     unit
                     int<units>:seconds))
;; (unit:second 1 'h)
;; (unit:second 0.5 'm)


;; TODO:units: Convert unit to human-readable value:
;; (defun unit:second:human (value unit)
;;   "Convert VALUE of UNIT seconds to a pretty, human-readable value/unit.
;;
;; Return cons: (float . symbol)
;;   - float will be new value
;;   - symbol will be new unit
;; Example:
;;   (unit:second:human 365 'days)
;;     -> (1 . 'year)"
;;
;;   )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :elisp 'utils 'units)
