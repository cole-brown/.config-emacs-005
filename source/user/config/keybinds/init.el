;;; user/config/keybinds/init.el --- init my keymaps -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2025-11-17
;; Timestamp:  2026-06-02
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Keymaps for My Stuff
;;
;;; Code:

(require 'keymap)

;;------------------------------------------------------------------------------
;; Personal Keymap
;;------------------------------------------------------------------------------

(defvar --/keymap/leader/prefix "C-'"
  "Where to put keybinds?
   - `C-c' is supposed to be the user's space for keybinds, but...
     It has the 'home dir' / 'My Documents' problem: It's the user's place to
     put things, therefore everything puts their shit in there, therefore
     there's hardly any room to put your own shit.
  - `C-'' is unbound (as of 2026-01-12 / Emacs 30.2).
    It may get bound to something eventually, but for now we can use it.")

(defvar-keymap --/keymap/leader
  :doc "Personal Keymap")

(keymap-global-set --/keymap/leader/prefix --/keymap/leader)


;;----------------------------
;; To create keybindings:
;;----------------------------
;; For a single keybind:
;;   (keymap-set --/keymap/leader/text
;;               "b"
;;               '("Unicode Box..." . /art/cmd/box/draw))
;;
;; Or for a whole child keymap:
;;   (defvar-keymap --/keymap/leader/files
;;     :doc "File commands."
;;     "f" '("Find file" . find-file)
;;     "s" '("Save buffer" . save-buffer)
;;     "r" '("Recent files" . recentf-open-files))
;;
;;   (keymap-set --/keymap/leader
;;                "f" (cons "Files" --/keymap/leader/files))


;;------------------------------------------------------------------------------
;; 'insert'
;;------------------------------------------------------------------------------

(defvar-keymap --/keymap/leader/insert
  :doc "Insert Keymap")

(keymap-set --/keymap/leader "i"
            (cons "Insert..." --/keymap/leader/insert))


;;------------------------------------------------------------------------------
;; 'text'
;;------------------------------------------------------------------------------

(defvar-keymap --/keymap/leader/text
  :doc "Text Keymap")

(keymap-set --/keymap/leader "t"
            (cons "Text..." --/keymap/leader/text))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide user (imp-path-relative 'user (imp-path-sans-extension (imp-path-current-file))))
