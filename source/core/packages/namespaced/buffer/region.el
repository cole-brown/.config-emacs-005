;;; namespaced/buffer/region.el --- Buffer Region Functions -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-09-22
;; Timestamp:  2023-06-21
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Buffer Region Functions
;;
;;; Code:


(require 'cl-lib)


;;------------------------------------------------------------------------------
;; Predicates
;;------------------------------------------------------------------------------

(defun buffer:region:active? ()
  "Return non-nil if selection is active.
Will detect 'evil'/'meow' modal keybind packages and return appropriately."
  (declare (side-effect-free t))
  (cond
   ;;------------------------------
   ;; `meow'?
   ;;------------------------------
   ((bound-and-true-p meow-mode)
    ;; Need to also be in the right meow mode then.
    (and (bound-and-true-p meow-normal-mode)
         (use-region-p)))

   ;;------------------------------
   ;; `evil'?
   ;;------------------------------
   ((bound-and-true-p evil-mode)
    (and (bound-and-true-p evil-local-mode)
         (evil-visual-state-p)))

   ;;------------------------------
   ;; Vanilla Emacs?
   ;;------------------------------
   (t
    (use-region-p))))


;;------------------------------------------------------------------------------
;; Region Helpers
;;------------------------------------------------------------------------------

(defun buffer:region:min (start end)
  "Return the smaller of START and END."
  (min start end))


(defun buffer:region:max (start end)
  "Return the larger of START and END."
  (max start end))


(defun buffer:region:start ()
  "Return beginning position of selection.
Use `evil-visual-beginning' if available.
TODO-meow: Anything special for meow?"
  (declare (side-effect-free t))
  (cond
   ;;------------------------------
   ;; `meow'?
   ;;------------------------------
   ((bound-and-true-p meow-mode)
    ;; https://github.com/meow-edit/meow/blob/master/TUTORIAL.org#states
    ;; Need to also be in the right meow mode then... but which are "right"?
    ;;   - INSERT - Wrong; writing things.
    ;;   - NORMAL - Right; keybinds love being used here.
    ;;   - MOTION - Right; default ("normal") state for modes that don't insert, like `dired', `proced'.
    ;;   - KEYPAD - Right? Leader key's temporary state while figuring out its keybind.
    ;;   - BEACON - Wrong? For applying macros / kinda like multiple-cursors.
    (and (or (meow-normal-mode-p)
             (meow-motion-mode-p)
             (meow-keypad-mode-p))
        (region-beginning)))

   ;;------------------------------
   ;; `evil'?
   ;;------------------------------
   ((bound-and-true-p evil-local-mode)
    (and (markerp evil-visual-beginning)
         (marker-position evil-visual-beginning)))

   ;;------------------------------
   ;; Vanilla Emacs?
   ;;------------------------------
   (t
    (region-beginning))))


(defalias 'buffer:region:beginning 'buffer:region:start)


(defun buffer:region:end ()
  "Return end position of selection.
Use `evil-visual-end' if available.
TODO-meow: Anything special for meow?"
  (declare (side-effect-free t))
  (cond
   ;;------------------------------
   ;; `meow'?
   ;;------------------------------
   ((bound-and-true-p meow-mode)
    ;; https://github.com/meow-edit/meow/blob/master/TUTORIAL.org#states
    ;; Need to also be in the right meow mode then... but which are "right"?
    ;;   - INSERT - Wrong; writing things.
    ;;   - NORMAL - Right; keybinds love being used here.
    ;;   - MOTION - Right; default ("normal") state for modes that don't insert, like `dired', `proced'.
    ;;   - KEYPAD - Right? Leader key's temporary state while figuring out its keybind.
    ;;   - BEACON - Wrong? For applying macros / kinda like multiple-cursors.
    (and (or (meow-normal-mode-p)
             (meow-motion-mode-p)
             (meow-keypad-mode-p))
        (region-end)))

   ;;------------------------------
   ;; `evil'?
   ;;------------------------------
   ((bound-and-true-p evil-local-mode)
    evil-visual-end)

   ;;------------------------------
   ;; Vanilla Emacs?
   ;;------------------------------
   (t
    (region-end))))


(cl-defun buffer:region:get (&key start end properties?)
  "Return the active region as a string or nil if no active region."
  ;; Checks & stuff. `nil' for these means don't do something.
  (when-let* ((active? (buffer:region:active?))
              (start   (or start (buffer:region:start)))
              (end     (or end   (buffer:region:end))))
    (funcall (if properties?
                 #'buffer-substring
               #'buffer-substring-no-properties)
             start end)))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide :buffer 'region)
