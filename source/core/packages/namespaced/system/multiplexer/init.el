;;; core/modules/system/multiplexer/init.el --- One .emacs to Rule Them All -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2020-08-28
;; Timestamp:  2023-06-26
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
;; Allow multiple systems (computers) to use the same init with small
;; differences.
;;
;; Mainly intended to be paired with another repo so that all these differences,
;; most of which reveal things about your system (directory layouts, usernames,
;; etc), are not included in this public repo.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Set up custom vars.
;;------------------------------------------------------------------------------

(defgroup system:multiplexer:group nil
  "Group namespace for the `:system/multiplexer' defcustoms."
  :prefix "system:multiplexer:"
  :group 'environment)


;;------------------------------------------------------------------------------
;; Load our files.
;;------------------------------------------------------------------------------

(imp:timing
    '(:system multiplexer)
    (imp:file:current)
    (imp:path:current:dir)

  ;; Debugging!
  (imp:load :feature  '(:system multiplexer debug)
            :path     (imp:path:current:dir/relative :system)
            :filename "debug")
  ;; Initialize debugging before going any further.
  (int<system/multiplexer>:nub:init)

  ;; Set-up Jerky namespaces for systems.
  (imp:load :feature  '(:system multiplexer namespaces)
            :path     (imp:path:current:dir/relative :system)
            :filename "namespaces")

  ;; Multiple systems (computers) able to use this same init.
  (imp:load :feature  '(:system multiplexer multiplex)
            :path     (imp:path:current:dir/relative :system)
            :filename "multiplex")

  ;; Directory Local Variables
  (imp:load :feature  '(:system multiplexer dlv)
            :path     (imp:path:current:dir/relative :system)
            :filename "dlv"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :system 'multiplexer)
