;;; namespaced/theme/theme.el --- Theme Helpers -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-05-06
;; Timestamp:  2025-11-05
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Face Helpers
;; Mainly cuz Doom has less scary faces than Emacs.
;; And I'm scared.
;; So instead of attempting to grok faces,
;; I'mma just steal from Doom.
;;
;; Some things in here originally borrowed from Doom files:
;;   - "core/autoload/themes.el"
;;   - "core/core-ui.el"
;;
;;; Code:

(imp-require str:/normalize)
(imp-require elisp:/functions)


;;------------------------------------------------------------------------------
;; Faces
;;------------------------------------------------------------------------------

(defun theme:face:doom->emacs (spec)
  "Convert Doom's more user-friendly face SPEC into an Emacs' face spec.

Initially from Doom's `doom--custom-theme-set-face'."
  ;; Multiple faces, same spec:
  (cond ((listp (car spec))
         (cl-loop for face in (car spec)
                  collect
                  (car (theme:face:doom->emacs (cons face (cdr spec))))))
        ;; Doom: FACE :keyword value [...]
        ((keywordp (cadr spec))
         `((,(car spec) ((t ,(cdr spec))))))
        ;; Fallback: Assume it's in Emacs' face spec already.
        (`((,(car spec) ,(cdr spec))))))
;; (theme:face:doom->emacs (list 'org-done :foreground "#506b50"))
;; (theme:face:doom->emacs (list '+org-todo-active :foreground "#a9a1e1" :background "#383838"))


(defmacro theme:face:set! (theme &rest specs)
  "Apply a list of face SPECS as user customizations for THEME(s).

THEME can be a single symbol or list thereof. If nil, apply these settings to
`user' theme (all themes). It will apply to all themes once they are loaded.

Initially from Doom's `custom-theme-set-faces!'."
  (declare (indent 1))
  ;; `_m/tfs/' prefix for macroexpanded vars.
  ;; Make a function name for the hook based on THEME.
  (let* ((_m/tfs/func (gensym (concat "theme:face:set!:/"
                                            (str:normalize:join theme "/")
                                            ":"
                                            ;; `gensym' will suffix the name with
                                            ;; `gensym-counter' for a unique name.
                                            ))))
    ;; Only eval inputs once.
    `(let ((_m/tfs/themes (elisp:list:listify (or ,theme 'user)))
           (_m/tfs/specs  (list ,@specs)))
       (progn
         ;; Create a function for applying the faces.
         (defun ,_m/tfs/func ()
           (let (custom--inhibit-theme-enable)
             (dolist (theme/each _m/tfs/themes)
               (when (or (eq theme/each 'user)
                         (custom-theme-enabled-p theme/each))
                 (apply #'custom-theme-set-faces theme/each
                        (mapcan #'theme:face:doom->emacs
                                _m/tfs/specs))))))

         ;; ;; Apply the changes immediately if the user is not using `innit' theme
         ;; ;; variables or the theme has already loaded. This allows you to evaluate
         ;; ;; these macros on the fly and customize your faces interactively.
         ;; (when theme:feature
         ;;   (funcall #',_m/tfs/func))
         ;; ;; Always add to the customize hook.
         ;; (add-hook 'theme:customize:hook #',_m/tfs/func 100)

         ;; Apply immediately and hope themes don't go floppin' around.
         (funcall #',_m/tfs/func)))))


(defmacro face:set! (&rest specs)
  "Apply a list of face SPECS as `user' theme (global theme) customizations.

This is a convenience macro alternative to `custom-set-face' which allows for a
simplified face format, and takes care of load order issues. See
`theme:face:set' for details - this will set SPECS in the `user' theme.

Initially from Doom's `custom-set-faces!'."
  (declare (indent defun))
  `(theme:face:set 'user ,@specs))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide theme face)
