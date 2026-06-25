;;; user/config/TODO-FILENAME.el --- TODO-SHORT-DESCRIPTION -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    TODO:(datetime:timestamp:insert :rfc-3339:date)
;; Timestamp:  2026-06-24
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  TODO-COMMENTARY
;;
;;; Code:

;; TODO-INCLUDES
;; (imp-require unit)
;; (require 'cl-lib)


;;------------------------------------------------------------------------------
;; `nerd-icons' Utility Functions
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; Helper Functions, Actual
;;------------------------------------------------------------------------------
;; TODO(icons): Move helpers to separate file? namespaced/font/icons.el?
;; TODO(icons): Rename funcs to `/icon/...' style? namespaced style?

;; TODO(icons): more requirements?
(require 'subr)
;; (require 'nerd-icons)

(defun icon--face-icon (plist)
  "Get face to use for icon from PLIST.

Check `:face', `:face:icon', and `:color:icon'.
Return nil if no face found."
  ;; Options specific to the icon first.
  (cond ((plist-get plist :face:icon))
        ((plist-member plist :color:icon)
         (list :foreground (plist-get plist :color:icon)))
        ((plist-get plist :face))
        (t
         nil)))


(defun icon--face-text (plist)
  "Get face to use for text from PLIST.

Check `:face', `:face:text', and `:color:text'.
Return nil if no face found."
  ;; Options specific to the text first.
  (cond ((plist-get plist :face:text))
        ((plist-member plist :color:text)
         (list :foreground (plist-get plist :color:text)))
        ((plist-get plist :face))
        (t
         nil)))


(defun --/icon/face/icon (plist)
  "Get face to use for icon from PLIST.

Check `:face:icon' and `:color:icon'.
Return nil if no face found."
  (cond ((plist-get plist :icon:face))
        ((plist-member plist :icon:color)
         (list :foreground (plist-get plist :icon:color)))
        (t
         nil)))


(defun --/icon/face/text (plist)
  "Get face to use for text from PLIST.

Check `:face:text' and `:color:text'.
Return nil if no face found."
  (cond ((plist-get plist :text:face))
        ((plist-member plist :text:color)
         (list :foreground (plist-get plist :text:color)))
        (t
         nil)))


(defun --/icon/properties/icon (plist)
  (let (filtered)
    (when-let ((face (--/icon/face/icon plist)))
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


(defun --/icon/properties/text (plist)
  (let (filtered)
    (when-let ((face (--/icon/face/text plist)))
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


(defun icon--func-for (icon)
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

      ;; unknown
      (_ ; TODO: what func for: "indent", "extra", ...?
       (warn "%S: Unknown nerd-icons type '%s' in ICON string: '%s'"
             'icon--func-for
             nf-type
             icon)))

    (warn "%S: Cannot parse ICON string: '%s'"
             'icon--func-for
             icon)))
;; (icon--func-for "nf-md-source_branch_check")
;; (icon--func-for "nf-jeff-geoff")
;; (icon--func-for "jeff")


(defun /icon (icon &rest plist)
  "Return nerd-icons function to use for ICON name string.

For ICON strings see: https://www.nerdfonts.com/cheat-sheet

TODO(icons): finish the rest of the docstr
Optional PLIST:
  - `:height'     - number
  - `:v-adjust'   - number (display (raise number))
  - `:color:icon' - face foreground color
  - `:face:icon'  - face
  - `:face'       - face
  - `:help:echo'  - string
"
  (when-let ((func (icon--func-for icon)))
    (apply func icon (--/icon/properties/icon plist))))
;; (/icon "nf-md-source_branch_check")
;; (/icon "nf-fa-triangle_exclamation" :icon:color 'yellow)
;; (/icon "nf-fa-triangle_exclamation" :height 2 :icon:color 'yellow)


(defun /icon/with-text (icon text &rest plist)
  "Return nerd-icons function to use for ICON name string.

For ICON strings see: https://www.nerdfonts.com/cheat-sheet

TODO(icons): finish the rest of the docstr
Optional PLIST:
  - `:height'     - number
  - `:v-adjust'   - number (display (raise number))
  - `:icon:color' - face foreground color
  - `:icon:face'  - face
  - `:text:color' - face foreground color
  - `:text:face'  - face
  - `:face'       - face
  - `:help:echo'  - string
"
  ;; ;; Only bother propertizing if we need to.
  ;; (let ((face-icon (icon--face-icon plist))
  ;;       (face-text (icon--face-text plist)))
  ;;   (concat
  ;;    (funcall nerd-icons-func
  ;;             icon
  ;;             ;; TODO: build this plist from what exists in PLIST?
  ;;             :face face-icon
  ;;             :v-adjust (or (plist-get plist :v-adjust) 0)
  ;;             :height (or (plist-get plist :height) 1))
  ;;    (if (stringp str)
  ;;        " "
  ;;      "")
  ;;    (when (stringp str)
  ;;      (if face-text
  ;;          ;; TODO: use `:help:echo' here?
  ;;          (propertize str 'face face-text)
  ;;        str)))))

  (let ((text-face (icon--face-text plist)))
    (apply #'propertize
           (concat (apply #'/icon icon plist)
                   (when (stringp text)
                     (if text-face
                         (propertize text 'face text-face)
                       text)))
           (--/icon/properties/all plist))))
;; (/icon/with-text "nf-md-source_branch_check" "branch check" :text:color 'pink)
;; (/icon/with-text "nf-fa-triangle_exclamation" nil :icon:color 'yellow)
;; (/icon/with-text "nf-fa-triangle_exclamation" "hi" :height 2 :color:icon 'yellow)


;; TODO: rewrite again?
(defun icon--get (nerd-icons-func icon str &rest plist)
  "Return string of ICON and STR.

For ICON strings see: https://www.nerdfonts.com/cheat-sheet

Optional PLIST's optional keys:
  - `:height'     - HEIGHT
  - `:v-adjust'   - V-ADJUST
  - `:color:icon' - ICON-COLOR
  - `:face'       - FACE
  - `:help:echo'  - HELP-ECHO

HEIGHT and V-ADJUST are sent to `nerd-icons-mdicon'.

ICON-COLOR is used to color only the icon character.

FACE is used for the icon and label.

HELP-ECHO should be a string and will be put in the `help-echo' property.

[2022-02-04] https://gist.github.com/mbuczko/e15d61363d31cf78ff17427072e0c325"
  ;; Only bother propertizing if we need to.
  (let ((face-icon (icon--face-icon plist))
        (face-text (icon--face-text plist)))
    (concat
     (funcall nerd-icons-func
              icon
              ;; TODO: build this plist from what exists in PLIST?
              :face face-icon
              :v-adjust (or (plist-get plist :v-adjust) 0)
              :height (or (plist-get plist :height) 1))
     (if (stringp str)
         " "
       "")
     (when (stringp str)
       (if face-text
           ;; TODO: use `:help:echo' here?
           (propertize str 'face face-text)
         str)))))


(defun icon-seti (icon str &rest plist)
  "Return string of File ICON and STR.

For ICON strings see: https://www.nerdfonts.com/cheat-sheet

Optional PLIST's optional keys:
  - `:height'     - HEIGHT
  - `:v-adjust'   - V-ADJUST
  - `:color:icon' - ICON-COLOR
  - `:face'       - FACE
  - `:help:echo'  - HELP-ECHO

HEIGHT and V-ADJUST are sent to `nerd-icons-flicon'.

ICON-COLOR is used to color only the icon character.

FACE is used for the icon and label.

HELP-ECHO should be a string and will be put in the `help-echo' property.

[2022-02-04] https://gist.github.com/mbuczko/e15d61363d31cf78ff17427072e0c325"
  (apply #'icon--get #'nerd-icons-sucicon icon str plist))


(defun icon-font-awesome (icon str &rest plist)
  "Return string of Font Awesome ICON and STR.

For ICON strings see: https://www.nerdfonts.com/cheat-sheet

Optional PLIST's optional keys:
  - `:height'     - HEIGHT
  - `:v-adjust'   - V-ADJUST
  - `:color:icon' - ICON-COLOR
  - `:face'       - FACE
  - `:help:echo'  - HELP-ECHO

HEIGHT and V-ADJUST are sent to `nerd-icons-faicon'.

ICON-COLOR is used to color only the icon character.

FACE is used for the icon and label.

HELP-ECHO should be a string and will be put in the `help-echo' property.

[2022-02-04] https://gist.github.com/mbuczko/e15d61363d31cf78ff17427072e0c325"
  (apply #'icon--get #'nerd-icons-faicon icon str plist))
;; (nerd-icons-insert)
;; (nerd-icons-faicon "nf-fa-spotify")
;; (icon-font-awesome "nf-fa-spotify" "Spotify" :color:icon "limegreen" :height 1 :v-adjust -0.05)
;; (insert (icon-font-awesome "nf-fa-spotify" "Spotify" :color:icon "limegreen" :height 1 :v-adjust -0.05))


(defun icon-file-icon (icon str &rest plist)
  "Return string of File ICON and STR.

For ICON strings see: https://www.nerdfonts.com/cheat-sheet

Optional PLIST's optional keys:
  - `:height'     - HEIGHT
  - `:v-adjust'   - V-ADJUST
  - `:color:icon' - ICON-COLOR
  - `:face'       - FACE
  - `:help:echo'  - HELP-ECHO

HEIGHT and V-ADJUST are sent to `nerd-icons-flicon'.

ICON-COLOR is used to color only the icon character.

FACE is used for the icon and label.

HELP-ECHO should be a string and will be put in the `help-echo' property.

[2022-02-04] https://gist.github.com/mbuczko/e15d61363d31cf78ff17427072e0c325"
  (apply #'icon--get #'nerd-icons-flicon icon str plist))


(defun icon-octicon (icon str &rest plist)
  "Return string of Octicon ICON and STR.

For ICON strings see: https://www.nerdfonts.com/cheat-sheet

Optional PLIST's optional keys:
  - `:height'     - HEIGHT
  - `:v-adjust'   - V-ADJUST
  - `:color:icon' - ICON-COLOR
  - `:face'       - FACE
  - `:help:echo'  - HELP-ECHO

HEIGHT and V-ADJUST are sent to `nerd-icons-octicon'.

ICON-COLOR is used to color only the icon character.

FACE is used for the icon and label.

HELP-ECHO should be a string and will be put in the `help-echo' property.

[2022-02-04] https://gist.github.com/mbuczko/e15d61363d31cf78ff17427072e0c325"
  (apply #'icon--get 'nerd-icons-octicon icon str plist))


(defun icon-material (icon str &rest plist)
  "Return string of Material ICON and STR.

For ICON strings see: https://www.nerdfonts.com/cheat-sheet

Optional PLIST's optional keys:
  - `:height'     - HEIGHT
  - `:v-adjust'   - V-ADJUST
  - `:color:icon' - ICON-COLOR
  - `:face'       - FACE
  - `:help:echo'  - HELP-ECHO

HEIGHT and V-ADJUST are sent to `nerd-icons-mdicon'.

ICON-COLOR is used to color only the icon character.

FACE is used for the icon and label.

HELP-ECHO should be a string and will be put in the `help-echo' property.

[2022-02-04] https://gist.github.com/mbuczko/e15d61363d31cf78ff17427072e0c325"
  (apply #'icon--get 'nerd-icons-mdicon icon str plist))


(defun icon-for-mode (mode str &rest plist)
  "Return string of MODE's icon and STR.

Optional PLIST's optional keys:
  - `:separator'  - SEPARATOR
  - `:height'     - HEIGHT
  - `:v-adjust'   - V-ADJUST
  - `:color:icon' - ICON-COLOR
  - `:face'       - FACE
  - `:help:echo'  - HELP-ECHO

SEPARATOR, a string to use to separate the icon and STR. Defaults to \" \".
If no separator is desired, supply something that is not a string, like `:none'
or (an explicit) nil.
  Space Separator (default):
    (icon-for-mode 'emacs-lisp \"Emacs Lisp\")
    (icon-for-mode 'emacs-lisp \"Emacs Lisp\" :separator \" \")
  No Separator:
    (icon-for-mode 'emacs-lisp \"Emacs Lisp\" :separator :none)
    (icon-for-mode 'emacs-lisp \"Emacs Lisp\" :separator nil)

Currently, defaults to setting V-ADJUST based on... `major-mode' variable???
  - `emacs-lisp-mode' gets V-ADJUST of 0.0
  - Everyone else gets V-ADJUST of 0.05
If this is not desired, supply the correct V-ADJUST.

HEIGHT and V-ADJUST are sent to `nerd-icons-icon-for-mode'.

ICON-COLOR is used to color only the icon character.

FACE is used for the icon and label.

HELP-ECHO should be a string and will be put in the `help-echo' property.

[2022-02-04] https://gist.github.com/mbuczko/e15d61363d31cf78ff17427072e0c325"
  ;; Only bother propertizing if we need to.
  (let* ((height    (plist-get plist :height))
         (v-adjust  (or (plist-get plist :v-adjust)
                        (if (eq major-mode 'emacs-lisp-mode)
                            0.0
                          0.05)))
         (face-icon (icon--face-icon plist))
         (face-text (icon--face-text plist))
         (separator (if (stringp (plist-get plist :separator))
                        (plist-get plist :separator)
                      " "))
         (icon      (nerd-icons-icon-for-mode mode
                                              :face     face-icon
                                              :height   height
                                              :v-adjust v-adjust))
         (icon      (if (symbolp icon)
                        ;; Failed to find a specific icon for the mode; fallback:
                        (nerd-icons-octicon "file-text"
                                            :face     face-icon
                                            :height   height
                                            :v-adjust v-adjust)
                      icon)))

    (concat icon
            separator
            (if face-text
                (propertize str 'face face-text)
              str))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide user (imp-path-relative 'user (imp-path-sans-extension (imp-path-current-file))))
