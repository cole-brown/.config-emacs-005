;;; modules/tools/signature/signature.el --- Signatures, Emails, and Such -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2020-11-16
;; Timestamp:  2023-07-18
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Signatures, Emails, and Such
;;
;;; Code:


;;-------------------------Insert John Hancock Here.----------------------------
;;--                          (Illegible Squiggle)                            --
;;------------------------------------------------------------------------------

;; TODO: Should this be its own package like jerky?
;; TODO: ...is jerky it's own package yet?


(require 'subr-x)
(require 'dash)

(imp:require :jerky)
(imp:require :buffer 'point)
(imp:require :datetime 'format)

(imp:require :mis)


;;------------------------------------------------------------------------------
;; Helpers.
;;------------------------------------------------------------------------------

(defun signature:get (&rest args)
  "Get a signature.

ARGS should be sig's type, optionally followed by `:namespace' and a namespace
symbol if desired."
  (apply #'jerky:get 'signature args))
;; (signature:get 'id 'sigil :namespace :work)
;; (jerky:get '(signature id sigil) :work)


(defun signature:set (&rest args)
  "Set a signature.

ARGS should be sig's type, followed by keyword pairs:
  - required: :value <value>
  - optional: :namespace <namespace>
  - optional: :docstr <docstr>"
 (apply #'jerky:set 'signature args))
;; (signature:set 'id 'hail :value "hi")
;; (signature:get 'id 'hail)
;; (signature:get 'id 'email :namespace :home)


(defun signature:exists? (&rest args)
  "Return non-nil if one or more signatures defined by ARGS exist.

ARGS should be sig's type, followed by keyword pairs:
  - optional: :namespace <namespace>"
  (not (null (apply #'jerky:has 'signature args))))


;;------------------------------------------------------------------------------
;; Create Signatures
;;------------------------------------------------------------------------------

;; https://unicode-table.com/en/blocks/miscellaneous-symbols/
;; Single unicode options:
;;   ʬ ʭ ҈ ҉ † ‡ ‣ ↀ ↂ ∀ ∎ ∮ ≈ ≜ ≡ ≣ ≷ ⊫ ⋈
;;   § ▀ ▄
;;  Can't see now, but maybe in future? Watch the line height though...
;;   ⛤ ⇶ ⌦ ⏩ ⏻ ␑ ␦ ⑄ ⚶
;;
;; Too tall, but nice: ⛧ ⚶
;; Right height, but eh... ok: § ▀
;; ▌
;; ┣━
;; ╠═╣
;; With Colons?
;; ╠:
;; ♫:
;; ♯:
;; §:
;; ▀:
(defun signature:create (sigil name)
  "Create a signature and save it into jerky under 'signature' root key.

SIGIL should be a 1 character string.
  Example: \"♫\"

NAME should be a full name string.
  Example: \"Jeff Jefferson\""
  ;;--------------------------
  ;; Identities To Use When Signing
  ;;--------------------------

  (signature:set 'id 'sigil
                 :namespace :default
                 :value sigil
                 :docstr "Short 1-char signature for notes, comments...")

  (signature:set 'id 'name
                 :namespace :default
                 :value name
                 :docstr "Name for use in signatures.")

  ;;--------------------------
  ;; Full Signatures
  ;;--------------------------

  (signature:set 'sigil 'note
                 :namespace :default
                 :value (concat sigil ":")
                 :docstr "Short signature for prefixing note lines.")

  (signature:set 'name 'sign
                 :namespace :default
                 :value (concat "-" name)
                 :docstr "Name for use in signatures.")

  ;;--------------------------
  ;; Email Addresses?
  ;;--------------------------

  ;; TODO: Email?
  ;; For emails, see: `jerky' paths "signature.email.<blank>"

  ;;--------------------------
  ;; TODOs, Fancy Bookmarks, and Such
  ;;--------------------------

  (signature:set 'sigil 'todo
                 :namespace :default
                 :value (concat sigil "-TODO-" sigil)
                 :docstr "Long signature for postfixing note lines or as last line of note block."))
;; (signature:create)


;;------------------------------------------------------------------------------
;; Signatures - General
;;------------------------------------------------------------------------------

(defun signature:string (&rest args)
  "Get signature string based on ARGS list.

Signature type is expected before keyword args, if any keywords are used.
For example:
  (signature 'id 'sigil :namespace :work)

Signature Types:
  - Created by `signature:create':
    - 'id 'sigil
    - 'id 'name
    - 'sigil 'note
    - 'name 'sign
    - 'sigil 'todo
  - Or whatever was created via `signature:set'.

\(Optional) Keywords are:
  - :namespace <keyword> - Namespace signature is stored under.
  - :timestamp <non-nil> - Append RFC-3339 datetime.
  - :comment   <non-nil> - Wrap signature in comment characters if appropriate.
  - :default   <string>  - Fallback string if signature doesn't exist."
  ;; Break `args' up into type list and keyword args, then check for any of the
  ;; optional keywords.
  ;; NOTE: This fucks up any values that are keywords, which we have lots, so do not do:
  ;;   (-let* (((type keys) (elisp:parse:args+kwargs args :namespace :timestamp :comment :default))
  (-let* ((parsed (elisp:parse:args+kwargs args :namespace :timestamp :comment :default))
          (type (car parsed))
          (keys (cdr parsed))
          ((&plist :namespace :timestamp :comment :default) keys))

    ;; First, do we even have this signature?
    (if-let ((sig (signature:get type :namespace namespace)))
        (progn
          ;; Add timestamp if desired.
          ;; Add it first - commenting could append to end.
          (when timestamp
            (setq sig (concat sig
                              " "
                              ;; Alternative datetimes:
                              ;;   - 'org 'inactive 'date
                              ;;   - 'org 'inactive 'rfc-3339
                              ;;   - 'org 'inactive 'full
                              ;;   - etc.
                              ;;     - See `datetime:init' in ":/core/modules/elisp/datetime/format.el"
                              (datetime:format 'rfc-3339 'datetime))))

          (when comment
            ;; Append ':' and wrap sig with comment characters if necessary.
            (mis :buffer 'current
                 :output 'string
                 (mis:comment "%s%s" sig ":")))

          ;; Return it.
          sig)

      ;; Didn't find the signature... Is there a default to return?
      default)))
;; (signature:string 'id 'sigil)
;; (signature:string 'sigil 'todo :namespace :work)
;; (signature:string 'sigil 'todo :timestamp t :comment t)
;; (signature:string 'id 'email :namespace :work)


(defun signature:insert (&rest args)
  "Get signature based on ARGS list & insert into buffer at current point.

If using `evil', set it to insert mode after signature.
TODO-meow: If using `meow', same?

Signature type is expected before keyword args, if any keywords are used.
For example:
  (signature 'id 'sigil :namespace :work)

Signature Types:
  - Created by `signature:create':
    - 'id 'sigil
    - 'id 'name
    - 'sigil 'note
    - 'name 'sign
    - 'sigil 'todo
  - Or whatever was created via `signature:set'.

(Optional) Keywords are:
  - :namespace <keyword> - Namespace signature is stored under.
  - :timestamp <non-nil> - Append \" [YYYY-MM-DD]\" (org inactive) timestamp.
  - :comment   <non-nil> - Wrap signature in comment characters if appropriate.
  - :default   <string>  - Fallback string if signature doesn't exist."
   (insert (apply #'signature:string args))
   (when (bound-and-true-p evil-mode)
     (evil-insert-state)))
;; (signature:insert 'sigil 'todo)


;; ;;------------------------------------------------------------------------------
;; ;; Signatures - Search...
;; ;;------------------------------------------------------------------------------
;;
;;
;; (defvar int<signature>:search:history nil
;;   "History variable for `signature:cmd:search'.
;;
;; Just a bucket to hold history for sig commands to keep segregated from general
;; history.")
;;
;;
;; ;; TODO: finish signature searching
;; (defun signature:cmd:search (signature)
;;   "Choose a SIGNATURE string and then search for it in the buffer."
;;   (interactive (list
;;     ;; Arg 0: signature type
;;     (completing-read
;;      ;; Prompt:
;;      "Search for Signature: "
;;
;;      ;; Shown list.
;;      ;; TODO: Need to write this function to get the list of options...
;;      (int<signature>:get/all)
;;
;;      ;; No predicate to limit above (shown list).
;;      nil
;;
;;      ;; Set to 'confirm if want confirmation of non-list entry.
;;      ;; But right now I think deny all not on list via `true'.
;;      t
;;
;;      ;; Deprecated.
;;      nil
;;
;;      ;; Get our own separate history for this command.
;;      'int<signature>:search:history
;;
;;      ;; default user input value
;;      nil)))
;;
;;   (if (null signature)
;;       (message "Cannot search for nothing.")
;;
;;     ;; Thank you, StackOverflow.
;;     ;; https://emacs.stackexchange.com/questions/2754/preset-search-isearch-string-from-command-line
;;     (isearch-forward nil 1)
;;    (isearch-yank-string (string-trim signature))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :tools 'signature 'signature)
