;;; user/config/emacs/font.el --- fonts & icons -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2025-11-17
;; Timestamp:  2026-03-23
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Nerd Font
;;
;;; Code:

;; ┌──────────────────────────────────═══───────────────────────────────────┐
;; │                               Nerd Icons                               │
;; └──────────────────────────────────═══───────────────────────────────────┘

;; Emacs Package: https://github.com/rainstormstudio/nerd-icons.el
;; Nerd Fonts:    https://www.nerdfonts.com
(use-package nerd-icons

  ;; My Nerd Font: Caskaydia Cove
  ;;-----------------------------
  ;; Downloaded & installed manually:
  ;; https://www.nerdfonts.com/font-downloads

  ;; What Font Am I Even?
  ;;---------------------
  ;; To determine font that Emacs is using in here:
  ;;  `M-x describe-char`
  ;; Something should say the name of the font, like this:
  ;;  > ftcrhb:-SAJA-CaskaydiaCove Nerd Font Mono-regular-normal-normal-*-29-*-*-*-m-0-iso10646-1 (#x38)
  ;; So in this case I have "CaskaydiaCove Nerd Font Mono".

  ;;----------------------------
  :custom
  ;;----------------------------

  ;; `nerd-icons-font-family'
  ;;-------------------------
  ;; The Nerd Font you want to use in GUI.
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want.
  (nerd-icons-font-family "CaskaydiaCove Nerd Font Mono"))


;;------------------------------------------------------------------------------
;; Nerd Icons Addons
;;------------------------------------------------------------------------------

;;----------------------------
;; `nerd-icons-completion'
;;----------------------------
;; https://github.com/rainstormstudio/nerd-icons-completion/

(use-package nerd-icons-completion
  :after nerd-icons
  :config

  ;; setup for marginalia
  (eval-after-load 'marginalia
    (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

  (nerd-icons-completion-mode))

;;----------------------------
;; `nerd-icons-corfu'
;;----------------------------
;; https://github.com/LuigiPiucco/nerd-icons-corfu#usage

(use-package nerd-icons-corfu
  :after (nerd-icons corfu)
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))


;;----------------------------
;; `nerd-icons-dired'
;;----------------------------
;; https://github.com/rainstormstudio/nerd-icons-dired

(use-package nerd-icons-dired
  :after (nerd-icons dired)
  :hook
  (dired-mode-hook . nerd-icons-dired-mode))


;;----------------------------
;; `treemacs-nerd-icons'
;;----------------------------
;; https://github.com/rainstormstudio/treemacs-nerd-icons

(use-package treemacs-nerd-icons
  :after (nerd-icons treemacs)
  :config
  (treemacs-nerd-icons-config))


;;------------------------------------------------------------------------------
;; Helper Functions, Actual
;;------------------------------------------------------------------------------
;; TODO(icons): move helpers to separate file?

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


(defun icon-font-awesome (icon str &rest plist)
  "Return string of Font Awesome ICON and STR.

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
  ;; Only bother propertizing if we need to.
  (let ((face-icon (font--face-icon plist))
        (face-text (font--face-text plist)))
    (concat
     (nerd-icons-faicon icon
                        :face face-icon
                        :v-adjust (or (plist-get plist :v-adjust) 0)
                        :height (or (plist-get plist :height) 1))
     " "
     (if face-text
         (propertize str 'face face-text)
       str))))
;; (nerd-icons-insert)
;; (nerd-icons-faicon "nf-fa-spotify")
;; (font-icon-font-awesome "nf-fa-spotify" "Spotify" :color:icon "limegreen" :height 1 :v-adjust -0.05)
;; (insert (font-icon-font-awesome "spotify" "Spotify" :color:icon "limegreen" :height 1 :v-adjust -0.05))

(defun icon-file-icon (icon str &rest plist)
  "Return string of File ICON and STR.

Optional PLIST's optional keys:
  - `:height'     - HEIGHT
  - `:v-adjust'   - V-ADJUST
  - `:color:icon' - ICON-COLOR
  - `:face'       - FACE
  - `:help:echo'  - HELP-ECHO

HEIGHT and V-ADJUST are sent to `nerd-icons-fileicon'.

ICON-COLOR is used to color only the icon character.

FACE is used for the icon and label.

HELP-ECHO should be a string and will be put in the `help-echo' property.

[2022-02-04] https://gist.github.com/mbuczko/e15d61363d31cf78ff17427072e0c325"
  ;; Only bother propertizing if we need to.
  (let ((face-icon (font--face-icon plist))
        (face-text (font--face-text plist)))
    (concat
     (nerd-icons-fileicon icon
                          :face face-icon
                          :v-adjust (or (plist-get plist :v-adjust) 0)
                          :height (or (plist-get plist :height) 1))
     " "
     (if face-text
         (propertize str 'face face-text)
       str))))


(defun icon-octicon (icon str &rest plist)
  "Return string of Octicon ICON and STR.

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
  ;; Only bother propertizing if we need to.
  (let ((face-icon (font--face-icon plist))
        (face-text (font--face-text plist)))
    (concat
     (nerd-icons-octicon icon
                         :face face-icon
                         :v-adjust (or (plist-get plist :v-adjust) 0)
                         :height (or (plist-get plist :height) 1))
     " "
     (if face-text
         (propertize str 'face face-text)
       str))))


(defun icon-material (icon str &rest plist)
  "Return string of Material ICON and STR.

Optional PLIST's optional keys:
  - `:height'     - HEIGHT
  - `:v-adjust'   - V-ADJUST
  - `:color:icon' - ICON-COLOR
  - `:face'       - FACE
  - `:help:echo'  - HELP-ECHO

HEIGHT and V-ADJUST are sent to `nerd-icons-material'.

ICON-COLOR is used to color only the icon character.

FACE is used for the icon and label.

HELP-ECHO should be a string and will be put in the `help-echo' property.

[2022-02-04] https://gist.github.com/mbuczko/e15d61363d31cf78ff17427072e0c325"
  ;; Only bother propertizing if we need to.
  (let ((face-icon (font--face-icon plist))
        (face-text (font--face-text plist)))
    (concat
     (nerd-icons-material icon
                          :face face-icon
                          :v-adjust (or (plist-get plist :v-adjust) 0)
                          :height (or (plist-get plist :height) 1))
     " "
     (if face-text
         (propertize str 'face face-text)
       str))))


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
    (font-icon-for-mode 'emacs-lisp \"Emacs Lisp\")
    (font-icon-for-mode 'emacs-lisp \"Emacs Lisp\" :separator \" \")
  No Separator:
    (font-icon-for-mode 'emacs-lisp \"Emacs Lisp\" :separator :none)
    (font-icon-for-mode 'emacs-lisp \"Emacs Lisp\" :separator nil)

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
         (face-icon (font--face-icon plist))
         (face-text (font--face-text plist))
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
