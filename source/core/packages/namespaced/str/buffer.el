;;; namespaced/str/buffer.el --- Buffer String Functions -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2023-04-17
;; Timestamp:  2025-11-05
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; String Functions for Buffers
;; ... or vice versa?
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Functional Programming Shenanigans
;;------------------------------------------------------------------------------

(defun str:buffer:region/get (&optional buffer start end)
  "Get BUFFER's region (from START to END) as a string (no properties).

BUFFER should be a buffer object, buffer name string, or nil.
  - If nil, use `current-buffer`.

START and END should each be an int, a point, or nil.
  - If nil, use `point-min' or `point-max'.

Return buffer's region as as string, or nil."
  (let* ((input/buffer buffer)
         (buffer (if buffer
                     (get-buffer buffer)
                   (current-buffer)))
         (start  (or start
                     (point-min)))
         (end    (or end
                     (point-max))))
    (unless buffer
      (error "No buffer found for '%S'!" input/buffer))

    (buffer-substring-no-properties (min start end)
                                    (max start end))))
;; (str:buffer:region/get (current-buffer) (point-max) (point-min))
;; (str:buffer:region/get)


(defun str:buffer->string:curry (func args)
  "Return a no-args function version of FUNC that operates on entire buffer.

FUNC should:
  - Have parms of: (string-or-list &optional separator)
  - Return a string.
Basically, FUNC should be a 'str:case/string:to:...' function.

ARGS should be a list of args (or nil), which is fed into FUNC after the
`string-or-list` argument.

Return the new no-args function."
  ;; `apply-partially' evaluates arguments now, and we want to delay getting the
  ;; buffer's string until it's the correct (narrowed) buffer...
  ;; So create a lambda instead.
  (lambda ()
    "No-args version of FUNC via `str:buffer->string:curry'."
    (apply func (str:buffer:region/get) args)))
;; (str:buffer->string:curry #'str:case/string:to:title)


;;------------------------------------------------------------------------------
;; Regions
;;------------------------------------------------------------------------------

(defun str:region->region (start end func &rest args)
  "Convert region from START to END by appling FUNC to that substring and then
replacing the region with the function's results.

FUNC should be a function that returns a string and should have parameters:
  (defun FUNC (string [optional-args]) ...)"
  (save-excursion
    (replace-region-contents start end
                             (str:buffer->string:curry func args))))


;; TODO: (str:thing-at-point->region thing-type ...)


(defun str:word-at-point->region (func &rest cases)
  "Get word at point, then use `str:region->region' to apply the FUNC and ARGS."
  (save-excursion
    ;; We might be in the middle of the word, so go to the end, save that point, and then go back to the start of the word.
    (forward-word)
     (let (start
           (end (point)))
       (backward-word)
       (setq start (point))

       ;; Have a region now, so just call `str:case/region:to'.
       (apply #'str:region->region start end func cases))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide str buffer)
