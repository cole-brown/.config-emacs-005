;;; namespaced/unit/units.el --- Nicer units? -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2023-02-24
;; Timestamp:  2025-10-28
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

(defconst _:unit:symbol:property 'unit
  "Symbol Property name for units.")


(defun unit:set (symbol unit)
  "Store UNIT in SYMBOL's properties."
  (put symbol _:unit:symbol:property unit))


(defun unit:get (symbol)
  "Get UNIT from SYMBOL's properties."
  (get symbol _:unit:symbol:property))


(defmacro _:unit:convert (caller value unit dictionary)
  "Convert VALUE of UNIT into an amount of base units given DICTIONARY.

VALUE should be a `numberp' (float or int number).

UNIT should be a symbol (and a key from DICTIONARY).

DICTIONARY should be an alist of cons: (unit-symbol . amount-of-base-units)
e.g.: See variables `_:unit:bytes' and `_:unit:seconds'.

CALLER should be a string of calling function's name.

Example:
  (_:unit:convert \"example\" 1   'h  _:unit:seconds)
    -> 3600.0
  (_:unit:convert \"example\" 0.5 'kb _:unit:bytes)
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
;; (_:unit:convert "example" 0.5 'kb _:unit:bytes)
;; Errors w/ "example" caller name?
;;   (_:unit:convert "example" 0.5 'kb nil)


;;------------------------------------------------------------------------------
;; Bytes
;;------------------------------------------------------------------------------

(defconst _:unit:base:decimal (expt 10 3)
  "Decimal / IEC / SI units are multiplied by 1000.")


(defconst _:unit:base:binary  (expt 2 10)
  "Binary units are multiplied by the closest power of 2 to 1000.")


(defconst _:unit:bytes
  (list
   (cons 'b           1)
   (cons 'B           1)
   (cons 'byte        1)
   (cons 'bytes       1)

   (cons 'kb          (expt _:unit:base:decimal 1))
   (cons 'kB          (expt _:unit:base:decimal 1))
   (cons 'kilobyte    (expt _:unit:base:decimal 1))
   (cons 'kilobytes   (expt _:unit:base:decimal 1))
   (cons 'kib         (expt _:unit:base:binary  1))
   (cons 'kiB         (expt _:unit:base:binary  1))
   (cons 'kibibyte    (expt _:unit:base:binary  1))
   (cons 'kibibytes   (expt _:unit:base:binary  1))

   (cons 'mb          (expt _:unit:base:decimal 2))
   (cons 'MB          (expt _:unit:base:decimal 2))
   (cons 'megabyte    (expt _:unit:base:decimal 2))
   (cons 'megabytes   (expt _:unit:base:decimal 2))
   (cons 'mib         (expt _:unit:base:binary  2))
   (cons 'MiB         (expt _:unit:base:binary  2))
   (cons 'mebibyte    (expt _:unit:base:binary  2))
   (cons 'mebibytes   (expt _:unit:base:binary  2))

   (cons 'gb          (expt _:unit:base:decimal 3))
   (cons 'GB          (expt _:unit:base:decimal 3))
   (cons 'gigabyte    (expt _:unit:base:decimal 3))
   (cons 'gigabytes   (expt _:unit:base:decimal 3))
   (cons 'gib         (expt _:unit:base:binary  3))
   (cons 'GiB         (expt _:unit:base:binary  3))
   (cons 'gibibyte    (expt _:unit:base:binary  3))
   (cons 'gibibytes   (expt _:unit:base:binary  3))

   (cons 'tb          (expt _:unit:base:decimal 4))
   (cons 'TB          (expt _:unit:base:decimal 4))
   (cons 'terabyte    (expt _:unit:base:decimal 4))
   (cons 'terabytes   (expt _:unit:base:decimal 4))
   (cons 'tib         (expt _:unit:base:binary  4))
   (cons 'TiB         (expt _:unit:base:binary  4))
   (cons 'tebibyte    (expt _:unit:base:binary  4))
   (cons 'tebibytes   (expt _:unit:base:binary  4))

   (cons 'pb          (expt _:unit:base:decimal 5))
   (cons 'PB          (expt _:unit:base:decimal 5))
   (cons 'petabyte    (expt _:unit:base:decimal 5))
   (cons 'petabytes   (expt _:unit:base:decimal 5))
   (cons 'pib         (expt _:unit:base:binary  5))
   (cons 'PiB         (expt _:unit:base:binary  5))
   (cons 'pebibyte    (expt _:unit:base:binary  5))
   (cons 'pebibytes   (expt _:unit:base:binary  5))

   (cons 'eb          (expt _:unit:base:decimal 6))
   (cons 'EB          (expt _:unit:base:decimal 6))
   (cons 'exabyte     (expt _:unit:base:decimal 6))
   (cons 'exabytes    (expt _:unit:base:decimal 6))
   (cons 'eib         (expt _:unit:base:binary  6))
   (cons 'EiB         (expt _:unit:base:binary  6))
   (cons 'exbibyte    (expt _:unit:base:binary  6))
   (cons 'exbibytes   (expt _:unit:base:binary  6))

   (cons 'zb          (expt _:unit:base:decimal 7))
   (cons 'ZB          (expt _:unit:base:decimal 7))
   (cons 'zettabyte   (expt _:unit:base:decimal 7))
   (cons 'zettabytes  (expt _:unit:base:decimal 7))
   (cons 'zib         (expt _:unit:base:binary  7))
   (cons 'ZiB         (expt _:unit:base:binary  7))
   (cons 'zebibyte    (expt _:unit:base:binary  7))
   (cons 'zebibytes   (expt _:unit:base:binary  7))

   (cons 'yb          (expt _:unit:base:decimal 8))
   (cons 'YB          (expt _:unit:base:decimal 8))
   (cons 'yottabyte   (expt _:unit:base:decimal 8))
   (cons 'yottabytes  (expt _:unit:base:decimal 8))
   (cons 'yib         (expt _:unit:base:binary  8))
   (cons 'YiB         (expt _:unit:base:binary  8))
   (cons 'yobibyte    (expt _:unit:base:binary  8))
   (cons 'yobibytes   (expt _:unit:base:binary  8)))
  "Alist of byte units in terms of how many bytes they are.")


(defun unit:byte (value unit)
  "Return integer of bytes for VALUE of UNITS bytes.

VALUE should be a float or int number.

UNIT should be a symbol (and a key from alist `_:unit:bytes').

Example:
  (unit:byte 2 'kb)
    -> 2000
  (unit:byte 2 'kib)
    -> 2048"
  (declare (pure t) (side-effect-free t))
  (floor ; No fractional bytes; cast to int.
   (_:unit:convert "unit:byte"
                   value
                   unit
                   _:unit:bytes)))
;; (unit:byte 100 'kb)
;; (unit:byte 0.5 'kb)


;; TODO:unit: Convert unit to human-readable value:
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

(defconst _:unit:seconds
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

UNIT should be a symbol (and a key from alist `_:unit:seconds').

Example:
  (unit:second 1 'h)
    -> 3600.0
  (unit:second 0.5 'm)
    -> 30.0"
  (declare (pure t) (side-effect-free t))
  (_:unit:convert "unit:second"
                  value
                  unit
                  _:unit:seconds))
;; (unit:second 1 'h)
;; (unit:second 0.5 'm)


;; TODO:unit: Convert unit to human-readable value:
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
(imp-provide unit units)
