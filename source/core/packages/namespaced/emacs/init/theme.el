;;; namespaced/innit/theme.el --- Theme Helpers -*- lexical-binding: t; -*-
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

(imp-require :str)
(imp-require :elisp)


;;------------------------------------------------------------------------------
;; Theme Loading
;;------------------------------------------------------------------------------

;; ;;----------------------------
;; ;; Loading Hooks
;; ;;----------------------------

;; (defvar hook://theme:customize nil
;;   "Hook to run for theme customization.

;; This hook is run by `hook://theme:load'.")


;; (defvar hook://theme:load nil
;;   "Hook run after the theme is loaded.

;; Run when loaded with `load-theme' or reloaded with `theme:reload'.")


;; (defun hook-runner://theme:customize ()
;;   "Run `hook://theme:customize' functions."
;;   (run-hooks 'hook://theme:customize))


;; ;; Always run `theme:customize:hook' when running ``hook://theme:load'.
;; (add-hook 'hook://theme:load  #'hook-runner://theme:customize)


;; ;;----------------------------
;; ;; Load Theme
;; ;;----------------------------

;; (defvar theme:loaded nil
;;   "Theme that user has loaded, or nil for \"not loaded (yet)\".")


;; (defun theme:load (theme)
;;   "Load THEME and mark as loaded.

;; THEME must be the theme's quoted symbol name (e.g. `'zenburn'); it will be
;; passed to `load-theme'."
;;   ;; Load the theme please and yes, I do want to load a theme thank you.
;;   (load-theme theme :no-confirm)
;;   ;; Save that theme was loaded.
;;   (setq theme:loaded theme)
;;   ;; Apply the hook funcs now that the theme is loaded.
;;   (innit:hook:run '`hook://theme:load))


;; ;; TODO: Currently doesn't interact with `theme:loaded'. Should it?
;; (defun theme:reload ()
;;   "Reload the current Emacs theme(s)."
;;   (interactive)
;;   (unless theme:feature
;;     ;; TODO: nub error func?
;;     (user-error (concat "theme:reload: "
;;                         "No theme is active. "
;;                         "Make sure `theme:feature', `theme:path', "
;;                         "and `theme:file' are set properly.")))
;;   (let ((themes (copy-sequence custom-enabled-themes)))
;;     ;; Turn themes off and back on again...
;;     (mapc #'disable-theme custom-enabled-themes)
;;     (let (`hook://theme:load) ; Disable load hook while we re-enable the themes.
;;       (mapc #'enable-theme (reverse themes)))
;;     ;; Apply the hook funcs once now that the themes are all reloaded.
;;     (innit:hook:run '`hook://theme:load)
;;     ;; TODO: Steal Doom fonts init/functions/etc too?
;;     ;; (doom/reload-font)
;;     (message "%s %s"
;;              (propertize
;;               (format "Reloaded %d theme%s:"
;;                       (length themes)
;;                       (if (cdr themes) "s" ""))
;;               'face 'bold)
;;              (mapconcat #'prin1-to-string themes ", "))))


;;------------------------------------------------------------------------------
;; Faces
;;------------------------------------------------------------------------------

(defun --theme:face:set (spec)
  "Convert Doom's more user-friendly face SPEC into an Emacs' face spec.

Initially from Doom's `doom--custom-theme-set-face'."
  ;; Multiple faces, same spec:
  (cond ((listp (car spec))
         (cl-loop for face in (car spec)
                  collect
                  (car (--theme:face:set (cons face (cdr spec))))))
        ;; FACE :keyword value [...]
        ((keywordp (cadr spec))
         `((,(car spec) ((t ,(cdr spec))))))
        ;; Fallback: Assume it's in Emacs' face spec already.
        (`((,(car spec) ,(cdr spec))))))
;; (--theme:face:set (list 'org-done :foreground "#506b50"))
;; (--theme:face:set (list '+org-todo-active :foreground "#a9a1e1" :background "#383838"))


(defmacro theme:face:set! (theme &rest specs)
  "Apply a list of face SPECS as user customizations for THEME(s).

THEME can be a single symbol or list thereof. If nil, apply these settings to
`user' theme (all themes). It will apply to all themes once they are loaded.

Initially from Doom's `custom-theme-set-faces!'."
  (declare (indent 1))
  ;; `_m_theme-fs_' prefix for macroexpanded vars.
  ;; Make a function name for the hook based on THEME.
  (let* ((_m_theme-fs_func (gensym (concat "theme:face:set!:/"
                                            (str:normalize:join theme "/")
                                            ":"
                                            ;; `gensym' will suffix the name with
                                            ;; `gensym-counter' for a unique name.
                                            ))))
    ;; Only eval inputs once.
    `(let ((_m_theme-fs_themes (elisp:list:listify (or ,theme 'user)))
           (_m_theme-fs_specs  (list ,@specs)))
       (progn
         ;; Create a function for applying the faces.
         (defun ,_m_theme-fs_func ()
           (let (custom--inhibit-theme-enable)
             (dolist (theme/each _m_theme-fs_themes)
               (when (or (eq theme/each 'user)
                         (custom-theme-enabled-p theme/each))
                 (apply #'custom-theme-set-faces theme/each
                        (mapcan #'--theme:face:set
                                _m_theme-fs_specs))))))
         ;; Apply the changes immediately if the user is not using `innit' theme
         ;; variables or the theme has already loaded. This allows you to evaluate
         ;; these macros on the fly and customize your faces interactively.
         (when theme:feature
           (funcall #',_m_theme-fs_func))
         ;; Always add to the customize hook.
         (add-hook 'theme:customize:hook #',_m_theme-fs_func 100)))))


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
(imp-provide :ns 'theme)

