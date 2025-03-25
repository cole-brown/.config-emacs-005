;;; core/modules/emacs/imp/+flag.el --- Imp Feature Flags -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-04-14
;; Timestamp:  2023-06-22
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;                                 ──────────
;; ╔════════════════════════════════════════════════════════════════════════╗
;; ║                               Imp Flags                                ║
;; ╚════════════════════════════════════════════════════════════════════════╝
;;                                   ──────
;;                           Daemonic Vexillology?
;;                                 ──────────
;;
;;; Code:


;; TODO: Unit tests for this file's functions!


;;------------------------------------------------------------------------------
;; Variables
;;------------------------------------------------------------------------------

(defvar int<imp>:feature:flags nil
  "Alist of imp feature to flags.

The imp feature should be a normalized list (via `int<imp>:flag:normalize'):
  (int<imp>:flag:normalize :foo \"bar\" 'baz)
    -> '(:foo bar baz)
  '(:str)
  etc.

The flags should be a list of symbols starting with \"+\" or \"-\" signs:
  '(+qux -quux)
  '(+random)
  etc.

These are not (currently?) used or enforced by imp. They are only intended for
the use of imp features to enable/disable bits of themselves during their
initialization.")


;;------------------------------------------------------------------------------
;; Flags
;;------------------------------------------------------------------------------

(defun int<imp>:flag:split (flag)
  "Splits FLAG into +/- sign and flag name.

Errors if no +/- sign present.

Returns keyword cons: (sign-keyword . name-keyword)"
  (let ((name (symbol-name flag))
        (regex (rx string-start
                   (group
                    (or "+" "-"))
                   (group
                    (one-or-more printing))
                   string-end)))

    (cond ((not (string-match regex name))
           (error "%s: FLAG doesn't conform to requirements! Regex '%s' didn't match FLAG '%s'"
                  "int<imp>:flag:split"
                  regex
                  name))

          ((string-match-p (rx string-start (or "+" "-")) (match-string 2 name))
           (error "%s: FLAG '%s' doesn't conform to requirements! Name must not start with '+' or '-'! Got: '%s'"
                  "int<imp>:flag:split"
                  name
                  (match-string 2 name)))

          ;; Ok; return the split symbol and flag name as keywords:
          (t
           (cons (intern (concat ":" (match-string 1 name)))
                 (intern (concat ":" (match-string 2 name))))))))
;; (int<imp>:flag:split '+foo)
;; (int<imp>:flag:split '-foo)
;; (int<imp>:flag:split '-+foo)


(defun int<imp>:flag:compare (flag-a flag-b)
  "Compare FLAG-A against FLAG-B.

Returns nil if they are unrelated.
Returns non-nil if they are related.
  - Returns 0 if equal:  +foo +foo
  - Returns +1 if A > B: +foo -foo
  - Returns -1 if A < B: -foo +foo

Errors if either flag match naming requirements."
  (let* ((split-a (int<imp>:flag:split flag-a))
         (sign-a (car split-a))
         (name-a (cdr split-a))
         (split-b (int<imp>:flag:split flag-b))
         (sign-b (car split-b))
         (name-b (cdr split-b)))
    ;; Unrelated?
    (cond ((not (eq name-a name-b))
           nil)
          ;; Exactly the same?
          ((eq sign-a sign-b)
           0)
          ;; Same name, different signs.
          ((eq sign-a :+)
           1)
          (t
           -1))))
;; (int<imp>:flag:compare '+foo '+bar)
;; (int<imp>:flag:compare '+foo '+foo)
;; (int<imp>:flag:compare '+foo '-foo)
;; (int<imp>:flag:compare '-foo '+foo)


;;------------------------------------------------------------------------------
;; Check for Flag Flags
;;------------------------------------------------------------------------------

(defun int<imp>:flag:exists? (feature flag)
  "Check if FEATURE has FLAG flagged either way already.
FEATURE should be a keyword.

FLAG should be a symbol name that starts with a \"+\" or \"-\" sign.

Examples:
  (imp:flag? :numbers +random)
  (imp:flag? :numbers -negative)

Returns non-nil if FEATURE has +/- FLAG flag already, nil if not.
Specifically, returns result of:
  (int<imp>:flag:compare FLAG existing-flag-matched)"
  (let ((feature-flags (int<imp>:alist:get/value feature int<imp>:feature:flags))
        found?)
    ;; Search for the flag flag.
    (while (and feature-flags
                (not found?))
      (setq found? (int<imp>:flag:compare flag
                                          (pop feature-flags))))
    ;; Return result of search.
    found?))
;; (setq int<imp>:feature:flags '((:foo . (+bar))))
;; (int<imp>:flag:exists? :foo '+bar)


(defmacro imp:flag? (feature flag)
  "Check if FEATURE has FLAG flagged.
FEATURE should be a keyword.

FLAG should be a symbol name that starts with a \"+\" or \"-\" sign.

Examples:
  (imp:flag? :numbers +random)
  (imp:flag? :numbers -negative)

Checks:
  - imp feature flags
  - Doom Emacs module flags, if applicable.
    - For Doom, FEATURE is ignored and this just becomes `(featurep! FLAG)'.

Returns non-nil if FEATURE has FLAG flag, nil if not."
  ;; And with true to avoid "void function" error.
  (or (and (memq flag (int<imp>:alist:get/value feature int<imp>:feature:flags))
           t)
      ;; Doom macro `featurep!' exists and Doom feature flag exists?
      (and (fboundp #'featurep!)
           (featurep! flag))))
;; (setq int<imp>:feature:flags '((:foo . (+bar))))
;; (imp:flag? :foo +bar)


;;------------------------------------------------------------------------------
;; Set Flag Flags
;;------------------------------------------------------------------------------

(defmacro imp:flag (feature &rest flag)
  "Set FLAG flag(s) for FEATURE.

FEATURE should be a keyword.

FLAG should be one or more symbol names that start with a \"+\" or \"-\"
sign.

Example:
  (imp:flag :numbers +random -negative)
    -> This sets flag flags for the `:numbers' feature/package/whatever to:
       - Include optional `random' numbers flag.
       - Exclude optional `negative' numbers flag."
  ;;------------------------------
  ;; Error checks...
  ;;------------------------------
  (unless (keywordp feature)
    (error "imp:flag: FEATURE must be a keyword, got: %S"
           feature))

  (unless flag
    (error "imp:flag: `%S' must have one or more flags to add/remove, got: %S"
           feature
           flag))

  ;;------------------------------
  ;; Process flags (w/ error checks)...
  ;;------------------------------
  `(let* ((macro<imp>:feature        ,feature)
          (macro<imp>:flags:add      ',flag)
          (macro<imp>:flags:existing (int<imp>:alist:get/value macro<imp>:feature int<imp>:feature:flags))
          (macro<imp>:flags:update   macro<imp>:flags:existing))
     ;; First check all input flags against existing and error if any cannot be added.
     ;; Then we can do the actual updated as all-or-nothing.
     (dolist (macro<imp>:flag macro<imp>:flags:add)
       (if (int<imp>:flag:exists? macro<imp>:feature
                                  macro<imp>:flag)
           ;; Flag is invalid; error out now.
           (error "imp:flag: `%S' is already flagged for flag matching `%S'. Existing flags: %S"
                  macro<imp>:feature
                  macro<imp>:flag
                  (int<imp>:alist:get/value macro<imp>:feature int<imp>:feature:flags))

         ;; Flag is valid; add to the update list.
         (push macro<imp>:flag macro<imp>:flags:update)))

     ;;------------------------------
     ;; Add flags.
     ;;------------------------------
     ;; Replace existing flag list with the new, updated list.
     (int<imp>:alist:update macro<imp>:feature
                            macro<imp>:flags:update
                            int<imp>:feature:flags)

     ;;------------------------------
     ;; Return full flag list for feature.
     ;;------------------------------
     macro<imp>:flags:update))
;; int<imp>:feature:flags
;; ;; OK:
;; (imp:flag :foo +bar)
;; ;; Fail - already has +bar can't add -bar:
;; (imp:flag :foo -bar)
;; ;; OK: multiple flags
;; (imp:flag :foo -baz +qux +quux)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------

(defun int<imp>:flag:init ()
  "Provide the imp:flag feature."
  (imp:provide:with-emacs :imp 'flag))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
