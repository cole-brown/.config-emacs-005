;;; core/modules/emacs/innit/vars.el --- It's full of variables, innit? -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-04-13
;; Timestamp:  2023-08-22
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; It's full of variables, innit?
;;
;;; Code:


(imp:require :path)


;;------------------------------------------------------------------------------
;; Constants
;;------------------------------------------------------------------------------

;; We already called the core "core", so... The next layer is "mantle", I guess?
;; And a third layer would be called "crust"?

(defconst innit:path:core/boot (path:join user-emacs-directory "core/boot/")
  "Absolute path to the \"core/boot\" subdirectory.")


(defconst innit:path:core/module (path:join user-emacs-directory "core/modules/")
  "Absolute path to the \"core/module\" subdirectory.")


(defconst innit:path:mantle (path:join user-emacs-directory "mantle/")
  "Absolute path to the \"mantle\" subdirectory.")


(defconst innit:path:module (path:join user-emacs-directory "modules/")
  "Absolute path to the \"modules\" subdirectory.")


(defconst innit:path:snippet (path:join user-emacs-directory "snippets/")
  "Absolute path to the \"snippets\" subdirectory.")


(defconst innit:interactive? (not noninteractive)
  "If non-nil, Emacs is in interactive mode.")


;; TODO: If none of these used, get rid of (or move to somewhere later/more appropriate).
;; (defconst innit:emacs:28+  (> emacs-major-version 27))
;; (defconst innit:emacs:29+  (> emacs-major-version 28))


;;------------------------------------------------------------------------------
;; Versioning & Early-Init Flag
;;------------------------------------------------------------------------------

;; "init.el" will check that version exists as a check that early-init happened.
(defconst int<innit>:version
  '(:major 4
    :minor 2
    ;; Can be ISO-8601 or RFC-3339 date or datetime.
    ;; Will get smashed down to just digits.
    :revision "2023-07-18"
    ;; (datetime:format 'iso-8601 'short)
    ;; (datetime:format 'yyyymmdd)
    )
  "Plist version data for this Emacs config.")


(defun int<innit>:version ()
  "Create SemVer string from version plist."
  (format "%d.%d.%s"
          (plist-get int<innit>:version :major)
          (plist-get int<innit>:version :minor)
          (replace-regexp-in-string (rx (not digit))
                                    ""
                                    (plist-get int<innit>:version :revision))))


(defconst innit:version (int<innit>:version)
  "Semantic Version string of this Emacs config.")


;;------------------------------------------------------------------------------
;; Non-`innit' Settings
;;------------------------------------------------------------------------------

(defmacro innit:settings:optional (settings &rest body)
  "Safely handle running BODY on SETTINGS which may or may not exist.

SETTINGS should be a symbol or list of symbols. Will check that they all are
bound before running BODY."
  (cond ((symbolp settings)
         (when (boundp settings)
           `(progn
             ,@body)))

        ((listp settings)
         (let ((valid :init))
           (dolist (setting settings)
             (setq valid (and valid
                              (symbolp setting)
                              (boundp setting))))
           (when valid
             `(progn
                ,@body))))

        (t
         ;; Just ignore anything else?
         nil)))
;; (innit:settings:optional inhibit-redisplay (message "print me"))
;; (innit:settings:optional innit:test:dne (message "don't print me"))
;; (innit:settings:optional (inhibit-redisplay track-mouse) (message "print me"))
;; (innit:settings:optional (innit:test:dne) (message "don't print me"))
;; (innit:settings:optional "ignored?" (message "don't print me"))


(defmacro innit:customize-set-variable (variable value &optional docstring)
  "Set VARIABLE to VALUE (with optional DOCSTRING) using `customize-set-variable'.

If DOCSTRING is a string, use as-is.

If DOCSTRING is a list of strings, all strings will be joined together with
newline separators."
  (declare (doc-string 3))
  (let ((macro<innit>:variable  (make-symbol "macro<innit>:uninterned:variable"))
        (macro<innit>:value     (make-symbol "macro<innit>:uninterned:value"))
        (macro<innit>:docstring (make-symbol "macro<innit>:uninterned:docstring")))
    `(let ((,macro<innit>:variable   ',variable)
           (,macro<innit>:value     ,value)
           (,macro<innit>:docstring ,docstring))
       (customize-set-variable ,macro<innit>:variable ,macro<innit>:value
                               (if (and ,macro<innit>:docstring
                                        (listp ,macro<innit>:docstring))
                                   ;; Convert a list of strings into a block of text.
                                   (mapconcat #'identity
                                              ,macro<innit>:docstring
                                              "\n")
                                 ;; Use string/nil/whatever as-is.
                                 ,macro<innit>:docstring)))))


(defmacro innit:csetq (variable value)
  "Set VARIABLE to VALUE via property `custom-set' or function `set-default'.

Like `setq', but uses the `custom-set' property if present. Also basically a
stripped down `customize-set-variable', so use that or
`innit:customize-set-variable'?

For packages, try to use the `use-package' `:custom' section before resorting to
those or this?
  - https://github.com/jwiegley/use-package#customizing-variables

Originally from: https://opensource.com/article/20/3/variables-emacs"
  ;; Use the VARIABLE's `custom-set' property if it's defined (via `defcustom'
  ;; `:set' keyword, most likely). Otherwise just call function `set-default'.
  `(funcall (or (get ',variable 'custom-set)
                #'set-default)
            ',variable ,value))


;;------------------------------------------------------------------------------
;; Customs
;;------------------------------------------------------------------------------

(defgroup innit:group nil
  "An Emacs framework for running similar inits on one or more systems."
  :prefix "innit:"
  :group 'tools
  :link '(url-link "https://github.com/cole-brown/.config-emacs"))


(defgroup innit:group:theme nil
  "Theme variables."
  :prefix "mantle:theme"
  :group 'innit:group
  :link '(url-link "https://github.com/cole-brown/.config-emacs"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :innit 'vars)
