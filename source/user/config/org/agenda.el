;;; mantle/config/org/agenda.el --- Configure Org-Mode's Agenda -*- lexical-binding: t; -*-
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
;;  Configure `org-mode' Agenda
;;
;;; Code:


;;------------------------------------------------------------------------------
;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;                              ORG-MODE AGENDA
;;                Oh look; you're way behind on your chores...
;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;------------------------------------------------------------------------------

(imp:require :nub)
(imp:require :datetime)
(imp:require :innit)
(imp:require :jerky)
(imp:require :path)
(imp:require :buffer 'search)
(imp:require :buffer 'name)
(imp:require :mode 'org)


;;------------------------------------------------------------------------------
;; Org-Agenda (Technically Org-Mode's Agenda)
;;------------------------------------------------------------------------------

(imp:eval:after org-mode
  ;;------------------------------
  ;; Org-Agenda & the Billion Orphaned Org Buffers
  ;;------------------------------
  ;; Thanks to: https://emacs.stackexchange.com/a/20438
  ;;---

  ;; `org-agenda-quit' is evil - it leaves a billion org mode buffers open.
  ;; And it's innocently sat there at "q" that apparently nothing in evil overrides...
  (bind-key [remap org-agenda-quit]
            #'org-agenda-exit
            org-agenda-keymap)
  ;; And this is at "Q".
  (bind-key [remap org-agenda-Quit]
            #'org-agenda-exit
            org-agenda-keymap)
  ;; Fuck y'all both.
  )


;; TODO: A trillion things to make `org-agenda' useful to me, probably?


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'org 'agenda)
