;;; user/config/keybinds/config.el --- my keybinds -*- lexical-binding: t; -*-
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
;;  Keybinds for My Stuff
;;
;;; Code:

(imp-require elisp)
(imp-require str)
(imp-require datetime)
(imp-require user:/config/emacs/keybinds)

(require 'hydra)
(require 'keymap)

;; NOTE: `user:/config/keybinds/init.el' defines
;;   - keymap: `--/keymap/leader'
;;   - string: `--/keymap/leader/prefix'
;;   - keymap: `--/keymap/leader/insert'
;;     - and other `--/keymap/leader' direct children keymaps

;;------------------------------------------------------------------------------
;; 'insert'
;;------------------------------------------------------------------------------

;;----------------------------
;; 'insert' -> 'datetime'
;;----------------------------

(defvar-keymap --/keymap/leader/insert/datetime
  :doc "Insert Datetime Keymap"

  "d" '("RFC-3339 Datetime" . datetime:cmd:timestamp:insert:rfc-3339)
  "D" `("RFC-3339 Date"     . ,(elisp:cmd (datetime:timestamp:insert 'rfc-3339 'date)))

  "i" '("ISO-8601 Datetime" . datetime:cmd:timestamp:insert:iso-8601)
  "I" `("ISO-8601 Date"     . ,(elisp:cmd (datetime:timestamp:insert 'iso-8601 'date)))

  "o" '("Org Inactive Date" . datetime:cmd:timestamp:insert:org)
  "O" `("Org Inactive RFC-3339" . ,(elisp:cmd (datetime:timestamp:insert 'org 'inactive 'rfc-3339)))
  ;; "a" `("Org Inactive Date & Day" . ,(elisp:cmd (datetime:timestamp:insert 'org 'inactive 'date-day)))
  ;; "A" `("Org Inactive Date, Day, Time" . ,(elisp:cmd (datetime:timestamp:insert 'org 'inactive 'full)))

  "p" '("Datetime Format..." . datetime:cmd:timestamp:insert:prompt))

(keymap-set --/keymap/leader/insert
            "d"
            `("Datetime..." . ,--/keymap/leader/insert/datetime))


;;------------------------------------------------------------------------------
;; 'text'
;;------------------------------------------------------------------------------

;; ASCII/Unicode Lines Box Art Hydra
(keymap-set --/keymap/leader/text
            "b"
            '("Unicode Box..." . /art/cmd/box/draw))

  ;; Join Lines Hydra
(keymap-set --/keymap/leader/text
            "j"
            `("Join Lines..." . ,(elisp:cmd (hydra:call 'buffer:hydra:join-lines))))

;; Case Conversion Hydra
(keymap-set --/keymap/leader/text
            "'"
            `("Case Conversion..." . ,(elisp:cmd (hydra:call 'str:hydra:case))))


;;----------------------------
;; 'text' -> 'alignment'
;;----------------------------

(defvar-keymap --/keymap/leader/text/alignment
  :doc "Text Alignment Keymap"

  "a" '("Align Before" . buffer:cmd:align/before)
  "o" '("Align After" . buffer:cmd:align/after)
  ";" '("Align Regex" . align-regexp)
  "q" `("C-u Align Regex" . ,(elisp:cmd
                             (setq current-prefix-arg '(4))
                             (call-interactively #'align-regexp)))
  "'" '("Align" . align)
  "," '("Align Current" . align-current))

(keymap-set --/keymap/leader/text
            "a"
            `("Alignment..." . ,--/keymap/leader/text/alignment))

;;----------------------------
;; 'text' -> 'center'
;;----------------------------

(defvar-keymap --/keymap/leader/text/center
  :doc "Text Centering Keymap"

  "c" `("Center at 40 (80 width)" . ,(elisp:cmd (buffer:cmd:center/width 80)))
  "t" '("Center to Column..." . #'buffer:cmd:center/to)
  "w" '("Center at Width..." . #'buffer:cmd:center/width))

(keymap-set --/keymap/leader/text
            "c"
            `("Center..." . ,--/keymap/leader/text/center))

;;------------------------------
;; 'text' -> 'fill'
;;------------------------------

(defvar-keymap --/keymap/leader/text/fill
  :doc "Text Fill Keymap"

  ;; Regions
  "r" '("Region" . fill-region)
  "a" '("Region as Paragraph" . fill-region-as-paragraph)
  "l" '("Line" . buffer:cmd:fill/region/single-line)

  ;; Paragraphs
  "p" `(,(if (eq (_:buffer:fill/paragraph/fn-for-mode) #'fill-paragraph)
           "Default Fill ¶"
         "Mode-Aware Fill ¶")
       . buffer:cmd:fill/paragraph/per-mode)
  "i" '("Individual ¶"  . fill-individual-paragraphs)
  "n" '("Non-Uniform ¶" . fill-nonuniform-paragraphs)
  "d" '("Default ¶"     . fill-paragraph)

  ;; DWIM
  "8" `("Fill to  80 (line/region)" . ,(elisp:cmd (buffer:cmd:fill/dwim/to-column 80)))
  "0" `("Fill to 100 (line/region)" . ,(elisp:cmd (buffer:cmd:fill/dwim/to-column 100)))
  "?" '("Fill to...  (line/region)" . buffer:cmd:fill/dwim/to-column)

  ;; Unfill
  "u" '("Unfill ¶" . buffer:cmd:fill/paragraph/unfill))

(keymap-set --/keymap/leader/text
            "f"
            `("Fill..." . ,--/keymap/leader/text/fill))

;;------------------------------
;; 'text' -> 'transpose'
;;------------------------------

(defvar-keymap --/keymap/leader/text/transpose
  :doc "Text Transpose Keymap"

  ;; Emacs
  "c" '("Characters"    . transpose-chars)
  "w" '("Words"         . transpose-words)
  "l" '("Lines"         . transpose-lines)
  "s" '("Sentences"     . transpose-sentences)
  "p" '("Paragraphs"    . transpose-paragraphs)
  "x" '("S-Expressions" . transpose-sexps)

  ;; Org-Mode
  "o" '("Org-Mode Words"    . org-transpose-words)
  "e" '("Org-Mode Elements" . org-transpose-element)
  "t" '("Org-Mode Table"    . org-table-transpose-table-at-point))

(keymap-set --/keymap/leader/text
            "t"
            `("Transpose..." . ,--/keymap/leader/text/transpose))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide user (imp-path-relative 'user (imp-path-sans-extension (imp-path-current-file))))
