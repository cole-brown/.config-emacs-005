;;; core/modules/system/multiplexer/dlv.el --- Dir-Local Variables -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-10-05
;; Timestamp:  2023-06-26
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Per-System Dir-Local Variables
;;
;;; Code:


(imp:require :dlv)
(imp:require :path)


;;------------------------------------------------------------------------------
;; System DLV Hook
;;------------------------------------------------------------------------------

(defvar system:multiplexer:dlv/functions nil
  "Hook variable for 'abnormal hooks' dealing with Directory Local Variables.

Hook functions will set Dir Local Variables for DOMAIN-KEYWORD in DIR-PATH.

Hook functions should be registered with `add-hook'.
  (add-hook 'system:multiplexer:dlv/functions #'func-name)

Function signature must be:
  (defun func-name (DIR-PATH DOMAIN-KEYWORD)
    ...)

  DIR-PATH will be an absolute path string.
    - example: \"/path/to/directory/\"
  DOMAIN-KEYWORD will be a keyword of the system's domain.
    - examples: `:work', `:home'")


(defun system:multiplexer:dlv:domain (dir-path domain-keyword)
  "Run multiplexer DLV hooks to set DIR-PATH to a DOMAIN-KEYWORD.

Hook functions should be registered to `system:multiplexer:dlv/functions' with
`add-hook'.

DIR-PATH will be an absolute path string.
  - example: \"/path/to/directory/\"

DOMAIN-KEYWORD will be a keyword of the system's domain.
  - examples: `:work', `:home'

NOTE: This must be run:
  1) By user, and/or
  2) after user's config
so that user has a chance to add functions to the hook variable."
  (run-hook-with-args 'system:multiplexer:dlv/functions
                      dir-path
                      domain-keyword))


;;------------------------------------------------------------------------------
;; Registered System DLV Paths
;;------------------------------------------------------------------------------

(defcustom system:multiplexer:dlv/args nil
  "Hook arguments to use for `system:multiplexer:dlv:domain/all'.

This should be a list of list of args to `system:multiplexer:dlv:domain'.

Each list of args should be a 2-tuple list of:
  1. Absolute path string
  2. Domain keyword
Example:
  '(\"/path/to/directory/\" :domain-namespace-keyword)"
  :group 'system:multiplexer:group
  ;; List of list tuples so we can use `apply' on the list tuples.
  :type  '(repeat (list string
                        (restricted-sexp :match-alternatives (keywordp)))))


(defun system:multiplexer:dlv:add (dir-path domain-keyword)
  "Add DIR-PATH as a DOMAIN-KEYWORD directory.

DIR-PATH should be a path string; it will get normalized by `path:abs:dir'.

DOMAIN-KEYWORD should be a keyword of the system's domain.
Examples: `:work', `:home', `:etc'

Appends to the `system:multiplexer:dlv/args' variable, which is used by the
`system:multiplexer:dlv:domain/all' function between user's init and config."
  (push (list (path:abs:dir dir-path)
              domain-keyword)
        system:multiplexer:dlv/args))


(defun system:multiplexer:dlv:domain/all ()
  "Set DLV system domain for all registered functions & args lists.

Run `system:multiplexer:dlv/functions' for each list of args in
`system:multiplexer:dlv/args'.

NOTE: This should be run between user's init and config so that user has a
chance to add functions to the hook variable & args to the arg variable in their
init, and then do things after it's all run later in their config."
  (dolist (args system:multiplexer:dlv/args)
    (apply #'system:multiplexer:dlv:domain args)))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :system 'multiplexer 'dlv)
