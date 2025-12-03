;;; mux/init.el --- One .emacs to Rule Them All -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2020-08-28
;; Timestamp:  2025-12-01
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  One System for the Human in their hovel,
;;  A few for the Programmer in dim-lit halls,
;;  N+1 for IT to manage at work,
;;  One for the Emacs Overengineer on their Dark Chair
;;  In the Tubes of the Internets where Repos lie.
;;     One Dotfile to rule them all. One Dotfile to init them.
;;     One Dotfile to bring them all, and during start-up bind them.
;;  In the Tubes of the Internets where Repos lie.
;;
;; ...yeah; needs a little work probably?
;; Anyway.
;;
;; ┌──────────────┬ Init & Config Help for Multiple Systems ┬──────────────────┐
;; ├──────────────┴────┬ What computer is this anyways? ┬───┴──────────────────┤
;; └───────────────────┴─── (probably the wrong one) ───┴──────────────────────┘
;;
;; > In electronics, a multiplexer (or mux; spelled sometimes as multiplexor),
;; > also known as a data selector, is a device that selects between several
;; > analog or digital input signals and forwards the selected input to a single
;; > output line.
;;   - https://en.wikipedia.org/wiki/Multiplexer
;;
;; Allow multiple systems (computers) to use the same init with small
;; differences.
;;
;; Mainly intended to be paired with another repo so that all these differences,
;; most of which reveal things about your system (directory layouts, usernames,
;; etc), are not included in this public repo.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Setup: groups, vars, customs, etc.
;;------------------------------------------------------------------------------

(imp-path-root-set 'mux (imp-path-current-dir))

(defgroup mux nil
  "Group namespace for `mux' defcustoms."
  :group 'environment)


;;------------------------------------------------------------------------------
;; Load our files.
;;------------------------------------------------------------------------------

(imp-timing
    'mux
    (imp-path-current-file)

  (imp ./debug)
  (imp ./multiplex))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide mux)
