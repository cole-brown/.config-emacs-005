;;; core/modules/emacs/innit/theme.el --- Theme Helpers -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-05-06
;; Timestamp:  2023-06-23
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Theme Helpers
;;
;; Some things in here originally borrowed from Doom files:
;;   - "core/autoload/themes.el"
;;   - "core/core-ui.el"
;;
;;; Code:


(require 'color) ; `color-clamp'

(imp:require :innit 'vars)
(imp:require :str)
(imp:require :elisp)


;;------------------------------------------------------------------------------
;; Customs
;;------------------------------------------------------------------------------

(defcustom innit:theme:path (path:join innit:path:mantle "theme")
  "Absolute path string to theme's directory.

Used by `imp:load'.

`innit:theme:path' and `innit:theme:file' must be set in order to load
your theme."
  :group 'innit:group
  :type  'string)


(defcustom innit:theme:file nil
  "String of either filename or absolute filepath to theme.

Used by `imp:load'.

`innit:theme:path' and `innit:theme:file' must be set in order to load
your theme."
  :group 'innit:group
  :type  'string)


(defcustom innit:theme:feature nil
  "`imp' feature name for theme.

Used by `imp:load'.

Set to `nil' if you don't want `mantle' to load a theme for you."
  :group 'innit:group
  :type  '(restricted-sexp :match-alternatives (keywordp symbolp stringp)))


;;------------------------------------------------------------------------------
;; Theme Loading
;;------------------------------------------------------------------------------

;;----------------------------
;; Loading Hooks
;;----------------------------

(defvar innit:theme:customize:hook nil
  "Hook to run for theme customization.

This hook is run by `innit:theme:load:hook'.")


(defvar innit:theme:load:hook nil
  "Hook run after the theme is loaded.

Run when loaded with `load-theme' or reloaded with `innit:theme:reload'.")


(defun innit:theme:customize:apply ()
  "Run `innit:theme:customize:hook' functions."
  (run-hooks 'innit:theme:customize:hook))


;; Always run `innit:theme:customize:hook' when running `innit:theme:load:hook'.
(add-hook 'innit:theme:load:hook  #'innit:theme:customize:apply)


;;----------------------------
;; Load Theme
;;----------------------------

(defvar innit:theme:loaded nil
  "Theme that user has loaded, or nil for \"not loaded (yet)\".")


(defun innit:theme:load (theme)
  "Load THEME and mark as loaded.

THEME must be the theme's quoted symbol name (e.g. `'zenburn'); it will be
passed to `load-theme'."
  ;; Load the theme please and yes, I do want to load a theme thank you.
  (load-theme theme :no-confirm)
  ;; Save that theme was loaded.
  (setq innit:theme:loaded theme)
  ;; Apply the hook funcs now that the theme is loaded.
  (innit:hook:run 'innit:theme:load:hook))


;; TODO: Currently doesn't interact with `innit:theme:loaded'. Should it?
(defun innit:theme:reload ()
  "Reload the current Emacs theme(s)."
  (interactive)
  (unless innit:theme:feature
    ;; TODO: nub error func?
    (user-error (concat "innit:theme:reload: "
                        "No theme is active. "
                        "Make sure `innit:theme:feature', `innit:theme:path', "
                        "and `innit:theme:file' are set properly.")))
  (let ((themes (copy-sequence custom-enabled-themes)))
    ;; Turn themes off and back on again...
    (mapc #'disable-theme custom-enabled-themes)
    (let (innit:theme:load:hook) ; Disable load hook while we re-enable the themes.
      (mapc #'enable-theme (reverse themes)))
    ;; Apply the hook funcs once now that the themes are all reloaded.
    (innit:hook:run 'innit:theme:load:hook)
    ;; TODO: Steal Doom fonts init/functions/etc too?
    ;; (doom/reload-font)
    (message "%s %s"
             (propertize
              (format "Reloaded %d theme%s:"
                      (length themes)
                      (if (cdr themes) "s" ""))
              'face 'bold)
             (mapconcat #'prin1-to-string themes ", "))))


;;------------------------------------------------------------------------------
;; Theme Init
;;------------------------------------------------------------------------------

(defun innit:theme:init ()
  "Initialize themes."
  ;; User themes should live in "<user-emacs-directory>/mantle/theme/",
  ;; not "<user-emacs-directory>/".
  (setq custom-theme-directory innit:theme:path)

  ;; Always prioritize the user's themes above the built-in/packaged ones.
  (setq custom-theme-load-path
        ;; Delete `custom-theme-directory' and re-add at start of list.
        (cons 'custom-theme-directory
              (delq 'custom-theme-directory custom-theme-load-path))))


;;------------------------------------------------------------------------------
;; Faces
;;------------------------------------------------------------------------------

(defun int<innit>:theme:face:set (spec)
  "Convert Doom's more user-friendly face SPEC into an Emacs' face spec.

Initially from Doom's `doom--custom-theme-set-face'."
  ;; Multiple faces, same spec:
  (cond ((listp (car spec))
         (cl-loop for face in (car spec)
                  collect
                  (car (int<innit>:theme:face:set (cons face (cdr spec))))))
        ;; FACE :keyword value [...]
        ((keywordp (cadr spec))
         `((,(car spec) ((t ,(cdr spec))))))
        ;; Fallback: Assume it's in Emacs' face spec already.
        (`((,(car spec) ,(cdr spec))))))
;; (int<innit>:theme:face:set (list 'org-done :foreground "#506b50"))
;; (int<innit>:theme:face:set (list '+org-todo-active :foreground "#a9a1e1" :background "#383838"))


(defmacro innit:theme:face:set (theme &rest specs)
  "Apply a list of face SPECS as user customizations for THEME(s).

THEME can be a single symbol or list thereof. If nil, apply these settings to
`user' theme (all themes). It will apply to all themes once they are loaded.

Initially from Doom's `custom-theme-set-faces!'."
  (declare (indent 1))
  ;; Make a function name for the hook based on THEME.
  (let* ((macro<innit/theme>:func (gensym (concat "innit:theme:face:hook:"
                                                  (str:normalize:join theme "/")
                                                  ":"
                                                  ;; `gensym' will suffix the name with `gensym-counter' for a unique name.
                                                  ))))
    ;; Only eval inputs once.
    `(let ((macro<innit/theme>:themes (elisp:list:listify (or ,theme 'user)))
           (macro<innit/theme>:specs  (list ,@specs)))
       (progn
         ;; Create a function for applying the faces.
         (defun ,macro<innit/theme>:func ()
           (let (custom--inhibit-theme-enable)
             (dolist (theme/each macro<innit/theme>:themes)
               (when (or (eq theme/each 'user)
                         (custom-theme-enabled-p theme/each))
                 (apply #'custom-theme-set-faces theme/each
                        (mapcan #'int<innit>:theme:face:set
                                macro<innit/theme>:specs))))))
         ;; Apply the changes immediately if the user is not using `innit' theme
         ;; variables or the theme has already loaded. This allows you to evaluate
         ;; these macros on the fly and customize your faces interactively.
         (when innit:theme:feature
           (funcall #',macro<innit/theme>:func))
         ;; Always add to the customize hook.
         (add-hook 'innit:theme:customize:hook #',macro<innit/theme>:func 100)))))


(defmacro innit:face:set (&rest specs)
  "Apply a list of face SPECS as user customizations.

This is a convenience macro alternative to `custom-set-face' which allows for a
simplified face format, and takes care of load order issues. See
`innit:theme:face:set' for details - this will set SPECS in the `user' theme.

Initially from Doom's `custom-set-faces!'."
  (declare (indent defun))
  `(innit:theme:face:set 'user ,@specs))


;;------------------------------------------------------------------------------
;; Colors
;;------------------------------------------------------------------------------
;; "Proudly nicked from Doom."
;;   - innit
;; "Shamelessly *borrowed* from solarized."
;;   - Doom


;; We don't want to reimplement every theme to define color names, so this
;; isn't as useful as we won't have a guaranteed alist of color names to values.
(defun innit:color (name &optional colors-alist type)
  "Get a specific color NAME's value from COLORS-ALIST.

NAME should be a symbol or a hexadecimal color string.
  - If NAME is a string, the value of `innit:color:name->hex' is returned.
  - If NAME is a symbol, the value is searched for in the COLORS-ALIST.

COLORS-ALIST should be an alist of NAME symbols to color values.
Color values should be either:
  1) hexadecimal color strings
  2) list of hexadecimal color strings
     - This is for TYPE. Index colors as follows:
       0) default/`nil' type
       1) 256
       2) 16
       3) 8
  3) `nil'

TYPE should be `nil', 256, 16, or 8.

Return a hexadecimal string from COLORS-ALIST or nil."
  ;; NAME is a string, not a symbol, so just convert it to a hex value.
  (cond ((stringp name)
         (innit:color:name->hex name))

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
           (innit:color (symbol-name name) colors-alist type)))

        ;; Symbol but no COLORS-ALIST; try a string?
        ((symbolp name)
         (innit:color (symbol-name name) colors-alist type))

        ;; Invalid name?
        (t
         (error "innit:color: Invalid input NAME. Should be a string or quoted symbol, got: %S"
                name))))
;; (innit:color 'magenta)


(defun innit:color:name->rgb (name)
  "Get the hexidecimal string repesenting the color NAME.

NAME should be either:
  - a string (e.g. \"red\", \"#ff0000\")
  - a quoted symbol (e.g. `'red')"
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
    (error "innit:color:name->rgb: Invalid color NAME. Should be a string or quoted symbol, got: %S"
           name)))
;; (innit:color:name->rgb 'red)
;; (innit:color:name->rgb "red")
;; (innit:color:name->rgb "#ff0000")


(defun innit:color:rgb->hex (red green blue)
  "Convert RED, GREEN, and BLUE numbers into a hex color string.

RED, GREEN, and BLUE /should/ be between 0.0 and 1.0 (inclusive).
They will be clamped to that range."
  (if (or (not (numberp red))
          (not (numberp green))
          (not (numberp blue)))
      ;; Invalid input?
      (error "innit:color:rgb->hex: Invalid input(s); colors must be floats between 0 and 1! Got: RED: %S, GREEN: %S, BLUE: %S"
             red
             green
             blue)
    ;; Clamp inputs and convert to hex string.
    (format "#%02x%02x%02x"
            (* (color-clamp red)   255)
            (* (color-clamp green) 255)
            (* (color-clamp blue)  255))))
;; (innit:color:rgb->hex 1 0 0)


(defun innit:color:name->hex (name)
  "Get the hexidecimal string repesenting the color NAME.

NAME should be either:
  - a string (e.g. \"red\", \"#ff0000\")
  - a quoted symbol (e.g. `'red')

This is merely a convenience for:
  (innit:color:rgb->hex (innit:color:name->rgb name))"
  (apply #'innit:color:rgb->hex (innit:color:name->rgb name)))
;; (innit:color:name->hex 'red)


(defun innit:color:blend (color0 color1 alpha &optional colors-alist)
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
           (innit:color:blend (innit:color color0 colors-alist)
                              (innit:color color1 colors-alist)
                              alpha))

          ;; One or both is a list, so recursively blend.
          ((or (listp color0)
               (listp color1))
           (cl-loop for x in color0
                    when (if (listp color1) (pop color1) color1)
                    collect (innit:color:blend x it alpha)))

          ;; Both colors are hex color strings.
          ((and (string-prefix-p "#" color0)
                (string-prefix-p "#" color1))
           (apply #'innit:color:rgb->hex
                  (cl-loop for it    in (innit:color:name->rgb color0)
                           for other in (innit:color:name->rgb color1)
                           collect (+ (* alpha it) (* other (- 1 alpha))))))

          ;; Invalid input?
          (t
           (error "innit:color:name->rgb: Invalid color(s): COLOR0: %S, COLOR1: %S"
                  color0 color1)))

    ;; One nil color.
    (error "innit:color:name->rgb: Colors cannot be `nil': COLOR0: %S, COLOR1: %S"
                  color0 color1)))


(defun innit:color:darken (color alpha &optional colors-alist)
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
         (innit:color:darken (innit:color color colors-alist) alpha colors-alist))

        ;; Recursively darken list of colors.
        ((listp color)
         (cl-loop for c in color collect (innit:color:darken c alpha colors-alist)))

        ;; Darken color hex string by blending with black.
        ((innit:color:blend color "#000000" (- 1 alpha) colors-alist))))


(defun innit:color:lighten (color alpha &optional colors-alist)
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
         (innit:color:lighten (innit:color color colors-alist) alpha colors-alist))

        ;; Recursively lighten list of colors.
        ((listp color)
         (cl-loop for c in color collect (innit:color:lighten c alpha collect)))

        ;; Lighten color hex string by blending with white.
        ((innit:color:blend color "#FFFFFF" (- 1 alpha) colors-alist))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :innit 'theme)
