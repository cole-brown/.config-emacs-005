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

      ;; unknown
      (_ ; TODO: what func for: "indent", "extra", ...?
       (warn "%S: Unknown nerd-icons type '%s' in ICON string: '%s'"
             '--/icon/func-for
             nf-type
             icon)))

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
  "Return nerd-icons function to use for ICON name string.

For ICON strings see: https://www.nerdfonts.com/cheat-sheet

TODO(icons): finish the rest of the docstr
Optional PLIST:
  - `:height'     - number
  - `:v-adjust'   - number (display (raise number))
  - `:icon:face'  - face
  - `:icon:color' - face foreground color
  - `:face'  - face
  - `:color' - face foreground color
"
  (when-let ((func (--/icon/func-for icon)))
    (apply func icon (--/icon/properties/icon plist t))))
;; (/icon/solo "nf-md-source_branch_check")
;; (/icon/solo "nf-fa-triangle_exclamation" :icon:color 'yellow)
;; (/icon/solo "nf-fa-triangle_exclamation" :height 2 :icon:color 'yellow)
;; (/icon/solo "nf-fa-ra" :icon:color 'red)


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
  - `:color'      - face foreground color
  - `:help:echo'  - string
"
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
