;;; mantle/config/org/pretty.el --- Make `org-mode' all pretty? -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2023-03-16
;; Timestamp:  2023-06-29
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Make `org-mode' all pretty?
;;
;;; Code:


;; ╔═══════════════════════════════════════════════════════════════════════════╗
;; ║                    Org-Mode Beautification Project                        ║
;; ╚═══════════════════════════════════════════════════════════════════════════╝
;; WANT!, but... slow? :(
;; TODO-org: Try these out one at a time and see if the slowdown is noticible?
;;           I have plenty of 10k line files to check with...


;;------------------------------
;; Org-Mode Headline Bullets: (Making Org-Mode Pretty)
;;------------------------------
;;
;; ;; Display the titles with nice unicode bullets instead of the text ones.
;; (imp:use-package org-bullets
;;   :disabled ;; is this making things slow?
;;   :after org
;;   :demand t
;;   :hook (org-mode . org-bullets-mode)
;;
;;   :custom
;;   (org-bullets-bullet-list
;;    ;; default: '("◉" "○" "✸" "✿")
;;    '("◆" "♦" "●" "○" "►" "▸")
;;     ;; ♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ☢ ❀ ◆ ◖ ▶
;;     ;;; Small
;;     ;; ► • ★ ▸
;;     )
;;   )


;;------------------------------
;; Pretty Checkboxes in Unicode
;;------------------------------
;; ;; Check box visual upgrade.
;; ;;   empty, indeterminate, marked: [ ], [-], [X] -> ☐, ▣, ☒
;; ;;     aka 'unchecked', 'mixed checked/unchecked children', 'checked'
;; ;; Trial: [2019-07-30 Tue]
;; ;;   - Con: This doesn't update until point has moved off the line... Possibly
;; ;;     interacting with my highlight row thing/mode?
;; ;; Nice lil search for symbols: http://www.unicode.org/charts/
;; (innit:hook:defun
;;     (:name   "org/pretty-checkboxes"
;;      :docstr "Beautify Org's Checkbox Symbols")
;;   (setq prettify-symbols-alist
;;         '(("[ ]" . "☐")
;;           ;; other options:
;;           ;;   - ☐ - 2610 ballot box
;;           ;;     https://www.unicode.org/charts/PDF/U2600.pdf
;;           ("[X]" . "☒")
;;           ;; other options:
;;           ;;   - ☒ - 2612 ballot box with X
;;           ;;     https://www.unicode.org/charts/PDF/U2600.pdf
;;           ;;   - ☑ - 2611 ballot box with check
;;           ("[-]" . "▣")
;;           ;; other options:
;;           ;;   - ▣ - 25A3 white square containing black small square
;;           ;;     https://www.unicode.org/charts/PDF/U25A0.pdf
;;           ;;   - ❍ - ...idk, what other people used at reddit thread.
;;           ;;   - ▽ - 25BD white down-pointing triangle
;;           ;;   - ◎ - 25CE BULLSEYE
;;           ;;   - ☯ - 262F yin-yang
;;           ))
;;   (prettify-symbols-mode 1))


;; ;;------------------------------
;; ;; Pretty List Bullet in Unicode
;; ;;------------------------------
;; ;;  ;; Show list markers with a middle dot instead of the
;; ;;  ;; original character.
;; (innit:hook:defun-and-add
;;     org-mode-hook
;;     (:name   'org:pretty-checkboxes
;;      :file   (imp:path:current:file)
;;      :docstr "Beautify Org's Checkbox Symbols")
;;   (font-lock-add-keywords
;;    nil ;; 'org-mode - some org-mode stuff (e.g. org-journal) is a derived
;;    ;; major mode and thus needed either more than just `org-mode', or to
;;    ;; be `nil' and put in the org-mode hook.
;;    '(("^ *\\([-]\\) "
;;       (0 (prog1 () (compose-region (match-beginning 1)
;;                                    (match-end 1) "•")))))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'org 'pretty)
