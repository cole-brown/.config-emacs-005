;;; namespaced/color/init.el --- Color Helpers -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-05-06
;; Timestamp:  2025-11-03
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; All the colors of the TTY and more!
;;
;; "Shamelessly *borrowed* from solarized."
;;   - Doom
;; "Proudly nicked from Doom."
;;   - namespaced
;; <this space reserved for next thief>
;;   - <reserved>
;;
;;; Code:

(require 'cl-macs)         ; `cl-loop'
(require 'color)           ; `color-clamp'
(require 'term/tty-colors) ; `tty-color-standard-values'


;;------------------------------------------------------------------------------
;; Translate Colors to Various Color Languages
;;------------------------------------------------------------------------------

(defun color:name->rgb (name)
  "Get a 3-tuple repesenting the color NAME.

NAME should be either:
  - a string (e.g. \"red\", \"#ff0000\")
  - a quoted symbol (e.g. `'red')

Search for NAME via `tty-color-standard-values'

Return a 3-tuple of normalized floats [0.0..1.0] that is useful to Emacs."
  (if-let ((name (cond ((stringp name)
                        name)
                       ((symbolp name)
                        (symbol-name name))
                       (t nil))))
      ;; Get value for name.
      (cl-loop with div = (float (car (tty-color-standard-values "#ffffff")))
               for x in (tty-color-standard-values (downcase name))
               collect (/ x div))
    ;; Invalid input?
    (error "color:name->rgb: Invalid color NAME. Should be a string or quoted symbol, got: %S"
           name)))
;; (color:name->rgb 'red)
;; (color:name->rgb "red")
;; (color:name->rgb "#ff0000")


(defun color:rgb->hex (red green blue)
  "Convert RED, GREEN, and BLUE numbers into a hex color string.

RED, GREEN, and BLUE /should/ be between 0.0 and 1.0 (inclusive).
They will be clamped to that range."
  (if (or (not (numberp red))
          (not (numberp green))
          (not (numberp blue)))
      ;; Invalid input?
      (error "color:rgb->hex: Invalid input(s); colors must be floats between 0 and 1! Got: RED: %S, GREEN: %S, BLUE: %S"
             red
             green
             blue)
    ;; Clamp inputs and convert to hex string.
    (format "#%02x%02x%02x"
            (* (color-clamp red)   255)
            (* (color-clamp green) 255)
            (* (color-clamp blue)  255))))
;; (color:rgb->hex 1 0 0)


(defun color:name->hex (name)
  "Get the hexidecimal string repesenting the color NAME.

NAME should be either:
  - a string (e.g. \"red\", \"#ff0000\")
  - a quoted symbol (e.g. `'red')

This is merely a convenience for:
  (color:rgb->hex (color:name->rgb name))"
  (apply #'color:rgb->hex (color:name->rgb name)))
;; (color:name->hex 'red)


;; We don't want to reimplement every theme to define color names, so this
;; isn't as useful as we won't have a guaranteed alist of color names to values.
(defun color:get (name &optional colors-alist type)
  "Get a specific color NAME's value from COLORS-ALIST.

NAME should be a symbol or a hexadecimal color string.
  - If NAME is a string, the value of `color:name->hex' is returned.
  - If NAME is a symbol, the value is searched for in the COLORS-ALIST.

COLORS-ALIST should be nil or an alist of NAME symbols to color values.
Color values should be either:
  1) hexadecimal color strings
  2) list of hexadecimal color strings
     - This is for TYPE. Index colors as follows:
       0) default/nil type
       1) 256
       2) 16
       3) 8
  3) nil

TYPE should be nil, 256, 16, or 8.

Return a hexadecimal string from `color:name->rgb' or COLORS-ALIST.
Return nil if nothing found."
  ;; NAME is a string, not a symbol, so just convert it to a hex value.
  (cond ((stringp name)
         (color:name->hex name))

        ;; Find NAME in the COLORS-ALIST.
        ((and colors-alist
              (symbolp name))
         (if-let ((values (assq name colors-alist)))
             ;; Found something in the list; get color from that.
             (let ((colors (cdr-safe values)))
               (when (and colors
                          (listp colors))
                 (let ((i (or (plist-get '(256 1 16 2 8 3) type) 0)))
                   (if (> i (1- (length colors)))
                       (car (last colors))
                     (nth i colors)))))
           ;; Not found in list; try a string instead?
           (color:get (symbol-name name) colors-alist type)))

        ;; Symbol but no COLORS-ALIST; try a string?
        ((symbolp name)
         (color:get (symbol-name name) colors-alist type))

        ;; Invalid name?
        (t
         (error "color:get: Invalid input NAME. Should be a string or quoted symbol, got: %S"
                name))))
;; (color:get 'magenta)


;;------------------------------------------------------------------------------
;; Convert Colors into Other Colors
;;------------------------------------------------------------------------------

(defun color:blend (color0 color1 alpha &optional colors-alist)
  "Blend two colors together by an alpha coefficient.

COLOR0 and COLOR1 should be:
  - quoted symbols
  - hexidecimal strings
  - list(s) of the above

ALPHA should be a float between 0.0 and 1.0.

COLORS-ALIST should be an alist of NAME symbols to color values.
Color values should be either:
  1) hexadecimal color strings
  2) list of hexadecimal color strings
     - This is for color type. Index colors as follows:
       0) default/`nil' type
       1) 256
       2) 16
       3) 8
  3) `nil'"
  (if (and color0 color1)
      ;; Both COLORs are symobls.
      (cond ((and (symbolp color0)
                  (symbolp color1))
             ;; Convert to hex strings and recurse.
             (color:blend (color:get color0 colors-alist)
                          (color:get color1 colors-alist)
                          alpha))

            ;; One or both is a list, so recursively blend.
            ((or (listp color0)
                 (listp color1))
             (cl-loop for x in color0
                      when (if (listp color1) (pop color1) color1)
                      collect (color:blend x it alpha)))

            ;; Both colors are hex color strings.
            ((and (string-prefix-p "#" color0)
                  (string-prefix-p "#" color1))
             (apply #'color:rgb->hex
                    (cl-loop for it    in (color:name->rgb color0)
                             for other in (color:name->rgb color1)
                             collect (+ (* alpha it) (* other (- 1 alpha))))))

            ;; Invalid input?
            (t
             (error "color:blend: Invalid color(s): COLOR0: %S, COLOR1: %S"
                    color0 color1)))

    ;; One nil color.
    (error "color:blend: Colors cannot be `nil': COLOR0: %S, COLOR1: %S"
           color0 color1)))


(defun color:darken (color alpha &optional colors-alist)
  "Darken a COLOR by a coefficient ALPHA.

COLOR should be one of:
  - quoted symbol
  - hexidecimal string
  - list of the above

ALPHA should be a float between 0.0 and 1.0.

COLORS-ALIST should be an alist of NAME symbols to color values.
Color values should be either:
  1) hexadecimal color strings
  2) list of hexadecimal color strings
     - This is for color type. Index colors as follows:
       0) default/`nil' type
       1) 256
       2) 16
       3) 8
  3) `nil'"
  ;; Convert color symbol to hex string.
  (cond ((and color (symbolp color))
         (color:darken (color:get color colors-alist) alpha colors-alist))

        ;; Recursively darken list of colors.
        ((listp color)
         (cl-loop for c in color collect (color:darken c alpha colors-alist)))

        ;; Darken color hex string by blending with black.
        ((color:blend color "#000000" (- 1 alpha) colors-alist))))


(defun color:lighten (color alpha &optional colors-alist)
  "Brighten a COLOR by a coefficient ALPHA.

COLOR should be one of:
  - quoted symbol
  - hexidecimal string
  - list of the above

ALPHA should be a float between 0.0 and 1.0.

COLORS-ALIST should be an alist of NAME symbols to color values.
Color values should be either:
  1) hexadecimal color strings
  2) list of hexadecimal color strings
     - This is for color type. Index colors as follows:
       0) default/`nil' type
       1) 256
       2) 16
       3) 8
  3) `nil'"
  ;; Convert color symbol to hex string.
  (cond ((and color (symbolp color))
         (color:lighten (color:get color colors-alist) alpha colors-alist))

        ;; Recursively lighten list of colors.
        ((listp color)
         (cl-loop for c in color collect (color:lighten c alpha collect)))

        ;; Lighten color hex string by blending with white.
        ((color:blend color "#FFFFFF" (- 1 alpha) colors-alist))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide color)
