;;; namespaced/list/alist/type/types.el --- Alist Type: Generic Types? -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-12-17
;; Timestamp:  2025-11-03
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;                                 ──────────
;; ╔════════════════════════════════════════════════════════════════════════╗
;; ║                         Better Alist Functions                         ║
;; ╚════════════════════════════════════════════════════════════════════════╝
;;                                   ──────
;;               At least these all have 'alist' in the name...
;;                                 ──────────
;;
;; Helper functions for alists that follow a convention for naming and stuff.
;; And will assist you in keeping you alist up-to-date, like it's a variable
;; or something.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Alist Types
;;------------------------------------------------------------------------------

(defconst _:alist:types:preset
  ;;------------------------------
  ;; Alist of: Type Keyword -> Functions Plist
  ;;------------------------------
  '(;;---
    ;; Default/Generic Alists
    ;;---
    ;; Emacs' alist functions default to `eq' when an equality test func is not
    ;; supplied or is `nil', so allow that defaulting to happen.
    (nil           . (:equal nil :valid nil))
    ;; And I like to be explicit, so that is called `:type/default'.
    (:type/default . (:equal nil :valid nil))

    ;;---
    ;; Special Types
    ;;---
    (:type/string  . (:equal string= :valid stringp))
    (:type/keyword . (:equal eq      :valid keywordp))

    ;;---
    ;; Operators
    ;;---
    (:op/eq      . (:equal eq      :valid nil))
    (:op/equal   . (:equal equal   :valid nil))
    (:op/eql     . (:equal eql     :valid nil))
    (:op/=       . (:equal =       :valid numberp))
    (:op/string= . (:equal string= :valid stringp)))
  "Alist of our preset alist type keywords to a functions plist.

For customizing, register with `alist:type:register', which will use
`_:alist:types:registered'.

NOTE: Use a default value when getting values so you know if you found the
value of `:type/default' vs didn't find anything!")


(defvar _:alist:types:registered '()
  "Alist of user registered type keywords to a functions plist.

Register with `alist:type:register', which will not allow any of the keys
in `_:alist:types:preset'.")


(defun _:alist:type:func (type/alist type/func)
  "Get TYPE/ALIST's function for the TYPE/FUNC keyword.

TYPE/ALIST should be (in order of priority):
  - A function - returns that function as-is.
  - A key in the `_:alist:types:preset' alist.
  - A key in the `_:alist:types:registered' alist.

Signals an error if TYPE/ALIST is not one of those.

TYPE/FUNC should be:
  - :equal - returns the equality operator function
  - :valid - returns the key validity checker function

Signals an error if TYPE/FUNC doesn't exist in the functions plist."
  (let ((func/name "_:alist:type:func"))

    ;;------------------------------
    ;; Did we get a function passed in? Use that function.
    ;;------------------------------
    (if (functionp type/alist)
        type/alist

      ;;------------------------------
      ;; Get function from settings.
      ;;------------------------------
      (if-let ((plist/preset (alist-get type/alist _:alist:types:preset)))
          ;; Found a preset; return the function?
          (if (plist-member plist/preset type/func)
              (plist-get plist/preset type/func)
            ;; Error if preset has no function available.
            (_:alist:error func/name
                              '(:line:each
                                "No known function type `%S' for alist preset type `%S'!"
                                "Function types:"
                                "%s")
                              type/func
                              type/alist
                              (pp-to-string plist/preset)))

        ;; Not in presets; look in customs.
        (if-let* ((plist/registered (alist-get type/alist _:alist:types:registered)))
            (if (plist-member plist/registered type/func)
                ;; Found a custom/user-registered; return the function.
                (plist-get plist/registered type/func)

              ;; Error if custom/user-registered doesn't have the function.
              (_:alist:error func/name
                                '(:line:each
                                  "No known function type `%S' for alist registered type `%S'!"
                                  "Function types:"
                                  "%s")
                                type/func
                                type/alist
                                (pp-to-string plist/registered)))

          ;; Also not in presets; error on unknown alist type.
          (_:alist:error func/name
                            '(:line:each
                              "Alist type '%S' is unknown! Register it with `(alist:type:register %S %s)'?"
                              "Known types:"
                              "  Presets:"
                              "%s"
                              "  Registered:"
                              "%s")
                            type/alist
                            type/alist
                            "equality-function validity-function"
                            (pp-to-string _:alist:types:preset)
                            (if _:alist:types:registered
                                (pp-to-string _:alist:types:registered)
                              "(none)")))))))
;; Good:
;;   (_:alist:type:func :type/default :equal)
;;   (_:alist:type:func :type/default :valid)
;;   (_:alist:type:func :type/string :equal)
;;   (_:alist:type:func :type/string :valid)
;; Error:
;;   alist: known preset, func: unknown
;;   (_:alist:type:func :type/string :jeff)
;;   alist: unknown, func: good
;;   (_:alist:type:func :jeff :equal)


;;------------------------------------------------------------------------------
;; Equality
;;------------------------------------------------------------------------------

(defun _:alist:type:func/equal? (type)
  "Returns the equality operator function for alist TYPE."
  (_:alist:type:func type :equal))


;;------------------------------------------------------------------------------
;; Validity
;;------------------------------------------------------------------------------

(defun _:alist:type:func/always-valid (_)
  "Ignores input; returns t."
  t)


(defun _:alist:type:func/valid? (type)
  "Returns a predicate for alist TYPE to use to check if a key is valid."
  (if-let ((func (_:alist:type:func type :valid)))
      func
    ;; Else the validity function is `nil', so translate that to a
    ;; "this is always valid" function.
    #'_:alist:type:func/always-valid))


(defun _:alist:type:valid? (type key)
  "Returns non-nil if KEY is valid for alist TYPE.

Raises an error if KEY is invalid.

Raises an error if alist TYPE has no validity function."
  (if-let ((func (_:alist:type:func/valid? type)))
      (if (funcall func key)
          ;; Valid KEY; return non-nil.
          t
        ;; Invalid; signal error.
        (_:alist:error "_:alist:type:valid?"
                          "Key `%S' is invalid for alist type `%S'."
                          key
                          type))
    ;; Unknown TYPE:
    ;; I think we should always have errored in `_:alist:type:func/valid?',
    ;; but just to be safe:
    (_:alist:error "_:alist:type:valid?"
                      "Alist type '%S' has no validity predicate?"
                      type)))
;; (_:alist:type:valid? :type/string "jeff")
;; (_:alist:type:valid? nil "jeff")
;; (_:alist:type:valid? nil nil)
;; (_:alist:type:valid? :dne nil)


;;------------------------------------------------------------------------------
;; Custom / User-Defined Types
;;------------------------------------------------------------------------------

(defun alist:type:register (type equality validity)
  "Register/update alist TYPE keyword to use functions EQUALITY and VALIDITY.

EQUALITY function is for finding alist entries (comparing keys).

VALIDITY function is for validating alist keys."
  ;;------------------------------
  ;; Error checks.
  ;;------------------------------
  (cond ((not (keywordp type))
         (_:alist:error "alist:type:register"
                           '("TYPE must be a keyword! "
                             "Got: %S")
                           type))

        ((assoc type _:alist:types:preset)
         (_:alist:error "alist:type:register"
                           '("Alist type '%S' is pre-defined! "
                             "Register with a different keyword. "
                             "Preset types: %s")
                           type
                           (pp-to-string _:alist:types:preset)))

        ((not (functionp equality))
         (_:alist:error "alist:type:register"
                           '("EQUALITY must be a function! "
                             "Got: %S")
                           equality))

        ((not (functionp validity))
         (_:alist:error "alist:type:register"
                           '("VALIDITY must be a function! "
                             "Got: %S")
                           validity))

        ;;------------------------------
        ;; Register user type.
        ;;------------------------------
        (t
         (setf (alist-get type _:alist:types:registered)
               (list :equal equality :valid validity)))))
;; Error:
;;   (alist:type:register #'ignore #'ignore :default)
;;   (alist:type:register :type/default #'ignore #'ignore)
;;   (alist:type:register :jeff :this-is-incorrect #'ignore)
;;   (alist:type:register :jeff #'ignore :this-is-incorrect)
;; Good:
;;   (alist:type:register :jeff/keyword #'eq #'keywordp)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide alist type types)
