;;; user/utilities/icons.el --- nerd-icon utility funcs -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2026-06-24
;; Timestamp:  2026-06-25
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  So you can just ask for a Nerd Font icon by name without having to know
;;  where to ask for it or be interactive about it.
;;
;;; Code:

(require 'nerd-icons)


;;------------------------------------------------------------------------------
;; `nerd-icons' private helpers
;;------------------------------------------------------------------------------

(defun --/icon/properties/icon (plist allow-generic)
  "To be passed directly to `nerd-icons-*icon' funcs."
  (let (filtered)
    (when-let ((face (cond ((plist-get plist :icon:face))
                           ((plist-member plist :icon:color)
                            (list :foreground (plist-get plist :icon:color)))
                           ((and allow-generic
                                 (plist-get plist :face)))
                           ((and allow-generic
                                 (plist-member plist :color))
                            (list :foreground (plist-get plist :color)))
                           (t
                            nil))))
      (push face filtered)
      (push :face filtered))
    (when-let ((v-adjust (plist-get plist :v-adjust)))
      (push v-adjust filtered)
      (push :v-adjust filtered))
    (when-let ((height (plist-get plist :height)))
      (push height filtered)
      (push :height filtered))

    filtered))
;; (--/icon/properties/icon '(:color:icon purple :height 11))
;; (--/icon/properties/icon '(:icon:color purple :height 11))


(defun --/icon/properties/text (plist allow-generic)
  (let (filtered)
    (when-let ((face (cond ((plist-get plist :text:face))
                           ((plist-member plist :text:color)
                            (list :foreground (plist-get plist :text:color)))
                           ((and allow-generic
                                 (plist-get plist :face)))
                           ((and allow-generic
                                 (plist-member plist :color))
                            (list :foreground (plist-get plist :color)))
                           (t
                            nil))))
      (push face filtered)
      (push :face filtered))

    filtered))


(defun --/icon/properties/all (plist)
  (let (properties)
    (when-let ((face (plist-get plist :face)))
      (push face properties)
      (push :face properties))
    (when-let ((help (or (plist-get plist :help:echo)
                         (plist-get plist :help-echo))))
      (push help properties)
      (push :help-echo properties))

    properties))


(defun --/icon/func-for (icon)
  "Return nerd-icons function to use for ICON name string.

For ICON strings see: https://www.nerdfonts.com/cheat-sheet"
  (if-let* ((fields (and (stringp icon)
                         (string-split icon "-")))
            (nf-prefix-valid (string= "nf" (nth 0 fields)))
            (nf-type (nth 1 fields))
            (nf-name-valid (stringp (nth 2 fields))))
      (pcase nf-type
        ("cod" #'nerd-icons-codicon) ; Codicons
        ("dev" #'nerd-icons-devicon) ; Devicons
        ((or "fa" "fae") #'nerd-icons-faicon) ; Font Awesome + Font Awesome Extension
        ("iec" #'nerd-icons-ipsicon) ; IEC Power Symbols
        ("linux" #'nerd-icons-flicon) ; Font Logos (Formerly Font Linux)
        ("md" #'nerd-icons-mdicon) ; Material Design Icons
        ("oct" #'nerd-icons-octicon) ; Octicons
        ("pom" #'nerd-icons-pomicon) ; Pomicons
        ("pl" #'nerd-icons-plicon) ; Powerline Extra Symbols
        ((or "seti" "custom") #'nerd-icons-sucicon) ; Seti-UI + Custom
        ("weather" #'nerd-icons-wicon) ; Weather Icons

        ;; TODO: what func for: "indent", "extra", ...?

        ;; unknown type in valid ICON string
        (_
         (warn "%S: Unknown nerd-icons type '%s' in ICON string: '%s'"
               '--/icon/func-for
               nf-type
               icon)))

    ;; unknowable ICON string
    (warn "%S: Cannot parse ICON string: '%s'"
          '--/icon/func-for
          icon)))
;; (--/icon/func-for "nf-md-source_branch_check")
;; (--/icon/func-for "nf-jeff-geoff")
;; (--/icon/func-for "jeff")


;;------------------------------------------------------------------------------
;; `nerd-icons' Utility Functions
;;------------------------------------------------------------------------------

(defun /icon/solo (icon &rest plist)
  "Return `nerd-icons' function to use for ICON name string.

For ICON strings see: https://www.nerdfonts.com/cheat-sheet

Optional PLIST key/value pairs (kvp):
  - `:height'     - number
  - `:v-adjust'   - number (display (raise number))
  - `:icon:face'  - face
  - `:icon:color' - face foreground color
  - `:face'       - face
  - `:color'      - face foreground color

HEIGHT must be `numberp'.
V-ADJUST must be `numberp'.

The face-related kvps are mutually exclusive, in this priority order:
  1. ICON:FACE
  2. ICON:COLOR
  3. FACE
  4. COLOR

ICON:FACE & FACE must be valid values for `face' attribute.

ICON:COLOR & COLOR must be valid color values for `face (:foreground COLOR)'
ex: `red', `#xff0000'

See function `propertize'.

See Info node `(elisp) Face Attributes' for complete information.
URL `https://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Attributes.html'"
  (when-let ((func (--/icon/func-for icon)))
    (apply func icon (--/icon/properties/icon plist t))))
;; (/icon/solo "nf-md-source_branch_check")
;; (/icon/solo "nf-fa-triangle_exclamation" :icon:color 'yellow)
;; (/icon/solo "nf-fa-triangle_exclamation" :height 2 :icon:color 'yellow)
;; (/icon/solo "nf-fa-ra" :icon:color 'red)


(defun /icon/with-text (icon text &rest plist)
  "Return `nerd-icons' function to use for ICON name string.

For ICON strings see: https://www.nerdfonts.com/cheat-sheet

Optional PLIST key/value pairs (kvp):
  - `:help-echo'  - help text (eg for mouse hover)
  - `:height'     - number
  - `:v-adjust'   - number (display (raise number))
  - `:icon:face'  - face
  - `:icon:color' - face foreground color
  - `:text:face'  - face
  - `:text:color' - face foreground color
  - `:face'       - face
  - `:color'      - face foreground color

HELP-ECHO must be `stringp'.

HEIGHT & V-ADJUST are only for the ICON.
  - HEIGHT must be `numberp'.
  - V-ADJUST must be `numberp'.

The FACEs override the COLORs.
  - ICON:* are used to style the ICON character.
  - TEXT:* are used to style the TEXT string.
  - FACE/COLOR are used to style everything.

The FACEs must be valid values for `face' attribute.

The COLORs must be valid color values for `face (:foreground COLOR)'
ex: `red', `#xff0000'

See function `propertize'.

See Info node `(elisp) Face Attributes' for complete information on `face'.
URL `https://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Attributes.html'"
  (apply #'propertize
         (concat
          ;; icon
          (when-let ((func (--/icon/func-for icon)))
            (apply func icon (--/icon/properties/icon plist nil)))
          ;; text
          (when (stringp text)
            (apply #'propertize
                   text
                   (--/icon/properties/text plist nil))))
         (--/icon/properties/all plist)))
;; (/icon/with-text "nf-md-source_branch_check" " branch check" :text:color 'pink)
;; (/icon/with-text "nf-fa-triangle_exclamation" nil :icon:color 'yellow)
;; (/icon/with-text "nf-fa-triangle_exclamation" "hi" :height 2 :color:icon 'yellow)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide user (imp-path-relative 'user (imp-path-sans-extension (imp-path-current-file))))
