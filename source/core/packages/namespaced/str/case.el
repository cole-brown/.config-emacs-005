;;; namespaced/str/case.el --- StRiNg CaSe CoNvErSiOn -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2023-06-23
;; Timestamp:  2025-11-05
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; String Case Conversions
;;
;; Emacs has:
;;   1. `upcase'
;;   2. `downcase'
;;   3. `capitalize'
;;
;; This has:
;;   1. Simple Types
;;      1. `:lower'
;;      2. `:upper'
;;      3. `:title'
;;   2. snake_case_types
;;      1. `:snake'
;;      2. `:snake.lower'
;;      3. `:snake.upper'
;;      4. `:snake.title'
;;   3. CamelCaseTypes
;;      1. `:camel'
;;      2. `:camel.lower'
;;      3. `:camel.upper'
;;   4. AlTeRnAtInG cAsE tYpEs
;;      1. `:alternating'
;;      2. `:alternating.lower'
;;      3. `:alternating.upper'
;;      4. `:alternating.random'
;;
;;; Code:


(require 'rx)
(imp-require str:/normalize)
(imp-require str:/string)
(imp-require str:/regex)



;; TODO: Note or func or something for:
;; Separate CamelCase & snake_case into separate words.
;; (subword-mode t)
;; TODO: Maybe a toggle in the case hydra?


;;------------------------------------------------------------------------------
;; Regexes
;;------------------------------------------------------------------------------

(defconst _:str:case:rx:groups
  '(:separators 1)
  "Explicit group numbers for 'compile' functions.")

;;------------------------------
;; regex string builders
;;------------------------------

(defun _:str:case:rx:build.flags (&rest flags)
  "Build optional keyword/string FLAGS into an `rx' expr."
  (declare (pure t) (side-effect-free t))
  (let (regex
        done
        flag)
    (while (and flags
                (not done))
      (setq flag (car flags)
            flags (cdr flags))
      ;;---
      ;; Overrides first.
      ;;---
      (cond ((eq :none flag)
             ;; Override anything already parsed, set our regex and force ourselves to the end of the loop.
             (setq regex ""
                   flags nil
                   done t))
            ((eq :default flag)
             ;; Override anything already parsed, set our regex and force ourselves to the end of the loop.
             (setq regex str:rx:default:separators.word
                   flags nil
                   done t))
            ;;---
            ;; Normal additive flags.
            ;;---
            ((eq :whitespace flag)
             (push 'whitespace regex))
            ((eq :punctuation flag)
             (push 'punctuation regex))
            ((stringp flag)
             (push flag regex))
            (t
             (error "_:str:case:rx:build.flags: Flag '%S' not implemented."
                    flag))))
    ;; Build and return the regex list/string.
    (cond
     ;; Already finished? Return as-is.
     (done
      regex)
     ;; Still need to finalize.
     ((= (length regex) 1)
      (nth 0 regex))
     ((> (length regex) 1)
      (apply #'list 'any regex))
     (t
      (error "_:str:case:rx:build.flags: No regex items to build? %S -> %S"
             flags regex)))))
;; (_:str:case:rx:build.flags :default)
;; (_:str:case:rx:build.flags :whitespace)
;; (_:str:case:rx:build.flags :punctuation)
;; (_:str:case:rx:build.flags :whitespace :punctuation)
;; (_:str:case:rx:build.flags :whitespace :punctuation "-")


(defun _:str:case:rx:build.separators (separators)
  "Returns a regex string or list of word separators for use in
`rx' or `rx-to-string'.

SEPARATORS should be:
  - a separator (regex) string,
  - a keyword for `_:str:case:rx:build.flags',
  - a list of separator strings,
  - a list for `rx',
  - the keyword `:none' (for 'no separators allowed'),
  - `:default' or nil (to use defaults)."
  (declare (pure t) (side-effect-free t))

  (cond
   ;; String: assume it's a regex already.
   ((stringp separators)
    separators)

   ;; Keyword: use `_:str:case:rx:build.flags'.
   ((keywordp separators)
    (_:str:case:rx:build.flags separators))

   ;; List of all strings: create a regex `any' out of it.
   ((and (listp separators)
         (> (length separators) 0) ;; `-all?' just always returns `t' for empty lists/nil.
         (seq-every-p #'stringp separators))
    (cons 'any separators))

   ;; List of not-all-strings: assume it's a list for `rx'.
   ((and (listp separators)
         (not (null separators)))
    separators)

   ;; Else use the default separators.
   (t
    str:rx:default:separators.word)))
;; (_:str:case:rx:build.separators nil)
;; (_:str:case:rx:build.separators :default)
;; (_:str:case:rx:build.separators :none)


(defun _:str:case:rx:compile.words (regex separators docstr)
  "Build a predicate for matching an entire string where each word must meet REGEX.

REGEX should be a regex string or an rx expression list.

SEPARATORS should be:
  - a separator (regex) string,
  - a keyword for `_:str:case:rx:build.flags',
  - a list of separator strings,
  - a list for `rx',
  - or nil (to use defaults).

Returns a lambda function to use to match a string with the document
string DOCSTR and a return value of `string-match' function's return value."
  (declare (pure t)
           (side-effect-free t)
           (doc-string 3))
  ;; What regex lambda do we want to create?
  ;; With allowed SEPARATORS or without?
  ;; Return a lambda that does the regex checking and returns match/nil.
  (if (and (keywordp separators)
           (eq :none separators))
      ;; Build lambda for regex match /WITHOUT/ SEPARATORS.
      ;; Don't guard the match-data, so that it can be used after calling the lambda.
      (lambda (check/string)
        docstr
        ;; Check the string with our regex.
        (string-match
         ;; Skip the word separators part of the regex.
         (rx-to-string `(sequence
                         string-start
                         (one-or-more word-start
                                      ;; The passed in regex for each word:
                                      ,regex
                                      ;; NO WORD SEPARATORS!
                                      word-end)
                         string-end)
                       :no-group)
         check/string))

    ;; Build lambda for regex match with SEPARATORS.
    ;; Don't guard the match-data, so that it can be used after calling the lambda.
    (lambda (check/string)
      docstr
      ;; Check the string with our regex.
      (string-match
       ;; Use optional word separators in the regex.
       (rx-to-string `(sequence
                       string-start
                       ;; Do first word manually so that separators can get a group for matching.
                       word-start
                       ;; The passed in regex for each word:
                       ,regex
                       ;; A group to capture a word separator. Capture here because once you get into the `...-or-more' you lose it unless
                       ;; the string ends with a word with a separator.
                       ;; e.g.
                       ;;   "x-hello-there"  -> No separator on final word.
                       ;;   "x-hello-there-" -> Separator is "-" on final word.
                       (group-n ,(plist-get _:str:case:rx:groups :separators)
                                (optional ,(_:str:case:rx:build.separators separators)))
                       (zero-or-more word-boundary
                                     ;; The passed in regex for each word:
                                     ,regex
                                     ;; Word separator?
                                     (optional ,(_:str:case:rx:build.separators separators)))
                       word-end
                       string-end)
                     :no-group)
       check/string))))
;; (_:str:case:rx:compile.words '(or "hi" "hello") nil "hi")
;; (_:str:case:rx:compile.words '(or "hi" "hello") :none "hi")
;; (funcall (_:str:case:rx:compile.words (rx "hello") nil "docstr") "hello there")
;; (funcall (_:str:case:rx:compile.words (rx "hello") nil "docstr") "hello")
;; (funcall (_:str:case:rx:compile.words (rx "hello") nil "docstr") "hello hello")
;; (funcall (_:str:case:rx:compile.words (rx "hello") "-" "docstr") "hello-hello")
;; (let ((string "hello hello"))
;;   (funcall (_:str:case:rx:compile.words (rx "hello") nil "docstr") string)
;;   (list (match-string 0 string)
;;         (match-string 1 string)
;;         (match-string 2 string)))


(defun _:str:case:rx:compile.no-separators (regex separators docstr)
  "Build a predicate for matching an alternating case regex to a string of words.
The SEPARATORS will be removed from the string befoer the REGEX is run on the string.

REGEX should be a regex string or an rx expression list.

SEPARATORS should be:
  - a separator (regex) string,
  - a keyword for `_:str:case:rx:build.flags',
  - a list of separator strings,
  - a list for `rx',
  - or nil (to use defaults).

Returns a lambda function to use to match a string with the document
string DOCSTR and a return value of `string-match' function's return value."
  (declare (pure t)
           (side-effect-free t)
           (doc-string 3))
  ;; Don't guard the match-data, so that it can be used after calling the lambda.
  (lambda (check/string)
    docstr
    ;; Check the string with our regex.
    (string-match
     ;; Regex without SEPARATORS.
     (rx-to-string `(sequence
                     string-start
                     (one-or-more word-start
                                  ;; The passed in regex for each word:
                                  ,regex
                                  ;; No word separator. We delete them before checking instead.
                                  ;; (optional ,(_:str:case:rx:build.separators separators))
                                  word-end)
                     string-end)
                   :no-group)

     ;; Remove the word separators before checking so that the alternating regexes work correctly.
     (replace-regexp-in-string (rx-to-string (_:str:case:rx:build.separators separators)
                                             :no-group)
                               ""
                               check/string
                               :fixedcase))))


(defun _:str:case:rx:compile (var.case &rest plist/override)
  "Compile VAR.CASE's full regex string.

PLIST/OVERRIDE is a plist of optional key/values. Allowed keys are:
  - `:rx.id'
  - `:rx.compiler'
  - `:separators'
  - `:docstr'

If any of these are set, they override VAR.CASE's plist values of the same key."
  (declare (pure t)
           (side-effect-free t)
           (doc-string 3))
  ;; Set up values to use based on overrides, VAR.CASE values, or defaults.
  (let* ((rx.compiler (or (plist-get plist/override :rx.compiler)
                          (_:str:case:property.get var.case :rx.compiler)
                          #'_:str:case:rx:compile.words))
         (rx.id       (or (plist-get plist/override :rx.id)
                          (_:str:case:property.get var.case :rx.id)))
         (separators  (or (plist-get plist/override :separators)
                          (_:str:case:property.get var.case :separators)))
         (docstr      (or (plist-get plist/override :docstr)
                          (_:str:case:property.get var.case :docstr))))
    ;; Create the regex.
    (funcall rx.compiler
             rx.id
             separators
             docstr)))


;;------------------------------
;; `rx' plists
;;------------------------------

(defun _:str:case:property.get (rx-plist property)
  "Get PROPERTY from an RX-PLIST.

RX-PLIST should be an `_:str:case:___' const.

PROPERTY should be:
  - `:rx.id'
  - `:separators'
  - `:rx.full'"
  (declare (pure t) (side-effect-free t))
  (plist-get rx-plist property))


(defun _:str:case:property.set (rx-plist property value)
  "Set PROPERTY in an RX-PLIST.

RX-PLIST should be an `_:str:case:___' const.

PROPERTY should be:
  - `:rx.id'
  - `:separators'
  - `:rx.full'"
  (plist-put rx-plist property value))


;;---
;; Case Type: Simple (lower, UPPER, Title)
;;---

(defconst _:str:case:lower
  '(:rx.id (one-or-more (any lower-case digit))
    :rx.compiler _:str:case:rx:compile.words
    :docstr "Match 'lowercase' strings.")
  "An rx sexpr for lowercase words.")

(_:str:case:property.set
 _:str:case:lower
 :rx.full (_:str:case:rx:compile _:str:case:lower))
;; (pp _:str:case:lower)


(defconst _:str:case:upper
  '(:rx.id (one-or-more (any upper-case digit))
    :rx.compiler _:str:case:rx:compile.words
    :docstr "Match 'UPPERCASE' strings.")
  "An rx sexpr for UPPERCASE WORDS.")


(_:str:case:property.set
 _:str:case:upper
 :rx.full (_:str:case:rx:compile _:str:case:upper))



(defconst _:str:case:title
  `(:rx.id (sequence
            (zero-or-more digit)
            ;; Each word must start with one capital.
            upper-case
            ;; ...and the rest are not capital.
            (one-or-more ,(_:str:case:property.get _:str:case:lower :rx.id)))
    :rx.compiler _:str:case:rx:compile.words
    :docstr "Match 'Title Case' strings.")
  "An rx sexpr for Title Cased Words.")


(_:str:case:property.set
 _:str:case:title
 :rx.full (_:str:case:rx:compile _:str:case:title))


;;;---
;; Case Type: CamelCase
;;---

(defconst _:str:case:camel/grouping.upper
  '((zero-or-more digit)
    ;; Exactly one uppercase letter.
    upper-case
    ;; After first uppercase, must have at least one lowercase.
    (zero-or-more digit)
    (one-or-more lower-case))
  "An `rx' list of a CamelCase group of letters.
e.g. 'Camel'Case or Camel'Case'

NOTE: Does not match \"IPAddress / DNSConn\" type of BASTARDIZEDCamelCase.")


(defconst _:str:case:camel.lower
  `(:rx.id (sequence
            ;; lowerCaseCamel must start with a lowercase group before the camel humps.
            (zero-or-more digit)
            (one-or-more lower-case)
            ;; Then it can continue on with the same 'one upper then some lower' pattern.
            (zero-or-more
             ,@_:str:case:camel/grouping.upper))
    :separators :none
    :rx.compiler _:str:case:rx:compile.words
    :docstr "Match 'lowerCamelCase' strings.")
  "An rx sexpr for lowerCaseWords.")


(_:str:case:property.set
 _:str:case:camel.lower
 :rx.full (_:str:case:rx:compile _:str:case:camel.lower))


(defconst _:str:case:camel.upper
  `(:rx.id (sequence
            ;; UpperCaseCamel must start with a camel hump.
            ,@_:str:case:camel/grouping.upper
            ;; Then it can continue on with the same 'one upper then some lower' pattern.
            (zero-or-more
             ,@_:str:case:camel/grouping.upper))
    :separators :none
    :rx.compiler _:str:case:rx:compile.words
    :docstr "Match 'UpperCamelCase' strings.")
  "An rx sexpr for UpperCamelCaseWords.")


(_:str:case:property.set
 _:str:case:camel.upper
 :rx.full (_:str:case:rx:compile _:str:case:camel.upper))


(defconst _:str:case:camel.any
  `(:rx.id (or ,(_:str:case:property.get _:str:case:camel.lower :rx.id)
               ,(_:str:case:property.get _:str:case:camel.upper :rx.id))
    :separators :none
    :rx.compiler _:str:case:rx:compile.words
    :docstr "Match any 'CamelCase' strings.")
  "An rx sexpr for CamelCaseWords, either lowerCamelCase or UpperCamelCase.")


(_:str:case:property.set
 _:str:case:camel.any
 :rx.full (_:str:case:rx:compile _:str:case:camel.any))


;;---
;; Case Type: snake_case
;;---

(defconst _:str:case:snake.lower
  `(:rx.id ,(_:str:case:property.get _:str:case:lower :rx.id)
    :separators ("_" "-")
    :rx.compiler _:str:case:rx:compile.words
    :docstr "Match 'lower_snake_case' strings.")
  "An rx sexpr for lower-cased snake_case_words.")
;; (pp-macroexpand-expression _:str:case:snake.lower)

(_:str:case:property.set
 _:str:case:snake.lower
 :rx.full (_:str:case:rx:compile _:str:case:snake.lower))


(defconst _:str:case:snake.upper
  `(:rx.id ,(_:str:case:property.get _:str:case:upper :rx.id)
    :separators ("_")
    :rx.compiler _:str:case:rx:compile.words
    :docstr "Match 'UPPER_SNAKE_CASE' strings.")
  "An rx sexpr for upper-cased SNAKE_CASE_WORDS.")


(_:str:case:property.set
 _:str:case:snake.upper
 :rx.full (_:str:case:rx:compile _:str:case:snake.upper))


(defconst _:str:case:snake.title
  `(:rx.id ,(_:str:case:property.get _:str:case:title :rx.id)
    :separators ("_")
    :rx.compiler _:str:case:rx:compile.words
    :docstr "Match 'Title_Snake_Case' strings.")
  "An rx sexpr for title-cased Snake_Case_Words.")


(_:str:case:property.set
 _:str:case:snake.title
 :rx.full (_:str:case:rx:compile _:str:case:snake.title))


(defconst _:str:case:snake.any
  `(:rx.id (or ,(_:str:case:property.get _:str:case:snake.lower :rx.id)
               ,(_:str:case:property.get _:str:case:snake.upper :rx.id)
               ,(_:str:case:property.get _:str:case:snake.title :rx.id))
    :separators ("_")
    :rx.compiler _:str:case:rx:compile.words
    :docstr "Match any type of 'snake_case' strings.")
  "An rx sexpr for snake_case_words, either all upper-case or all lower-case.")


(_:str:case:property.set
 _:str:case:snake.any
 :rx.full (_:str:case:rx:compile _:str:case:snake.any))


;;---
;; Case Type: aLtErNaTiNg
;;---

(defconst _:str:case:alternating.lower
  ;; Can start off with not-a-letter, but first letter must be lowercase.
  '(:rx.id (sequence
            (zero-or-more digit)
            lower-case
            ;; After first lowercase, must have at least one upper.
            (zero-or-more digit)
            upper-case
            ;; Then it can continue on with alternating lower, upper.
            ;;   - It must be able to end on either lower or upper.
            (zero-or-more
             (zero-or-more digit)
             (or lower-case word-end)
             (zero-or-more digit)
             (or upper-case word-end)))
    :separators :default
    :rx.compiler _:str:case:rx:compile.no-separators
    :docstr "Match 'aLtErNaTiNg LoWeRcAsE' strings.")
  "An rx sexpr for \"lOwErCaSe AlTeRnAtInG WoRdS\".")


(_:str:case:property.set
 _:str:case:alternating.lower
 :rx.full (_:str:case:rx:compile _:str:case:alternating.lower))


(defconst _:str:case:alternating.upper
  ;; Can start off with not-a-letter, but first letter must be uppercase.
  '(:rx.id (sequence
            (zero-or-more digit)
            upper-case
            ;; After first lowercase, must have at least one upper.
            (zero-or-more digit)
            lower-case
            ;; Then it can continue on with alternating lower, upper.
            ;;   - It must be able to end on either lower or upper.
            (zero-or-more
             (zero-or-more digit)
             (or upper-case word-end)
             (zero-or-more digit)
             (or lower-case word-end)))
    :separators :default
    :rx.compiler _:str:case:rx:compile.no-separators
    :docstr "Match 'AlTeRnAtInG uPpErCaSe' strings.")
  "An rx sexpr for \"UpPeRcAsE aLtErNaTiNg wOrDs\".")


(_:str:case:property.set
 _:str:case:alternating.upper
 :rx.full (_:str:case:rx:compile _:str:case:alternating.upper))


(defconst _:str:case:alternating.any
  `(:rx.id (or ,(_:str:case:property.get _:str:case:alternating.lower :rx.id)
               ,(_:str:case:property.get _:str:case:alternating.upper :rx.id))
    :separators :default
    :rx.compiler _:str:case:rx:compile.no-separators
    :docstr "Match 'UpPeRcAsE aLtErNaTiNg wOrDs' or 'lOwErCaSe AlTeRnAtInG WoRdS' strings.")
  "An rx sexpr for \"UpPeRcAsE aLtErNaTiNg wOrDs\" or \"lOwErCaSe AlTeRnAtInG WoRdS\".")


(_:str:case:property.set
 _:str:case:alternating.any
 :rx.full (_:str:case:rx:compile _:str:case:alternating.any))


;;---
;; Case Types
;;---

(defconst str:cases:rx/types.base
  '(:lower :upper :title :snake :camel :alternating)
  "A list of keywords of our general types of cases.")


(defconst str:cases:rx/types.all
  '(;; simple types
    :lower
    :upper
    :title
    ;; snake_case_types
    :snake
    :snake.lower
    :snake.upper
    :snake.title
    ;; CamelCaseTypes
    :camel
    :camel.lower
    :camel.upper
    ;; AlTeRnAtInG cAsE tYpEs
    :alternating
    :alternating.lower
    :alternating.upper
    :alternating.random)
  "A list of keywords of our general types of cases.")


(defconst str:cases:rx/types.identify
  '(;; simple types
    :lower
    :upper
    :title
    ;; snake_case_types
    :snake
    :snake.lower
    :snake.upper
    :snake.title
    ;; CamelCaseTypes
    :camel
    :camel.lower
    :camel.upper
    ;; AlTeRnAtInG cAsE tYpEs
    :alternating
    :alternating.lower
    :alternating.upper)
  "A list of keywords of our general types of cases.")


(defun _:str:case:validate/case (keyword &optional types.valid)
  "Validate case KEYWORD.

If TYPES.VALID is non-nil, it should be a list of valid keywords
(e.g. `str:cases:rx/types.base'). Defaults to using `str:cases:rx/types.all'."
  (memq keyword (or types.valid
                    str:cases:rx/types.all)))


(defun _:str:case:type.get (type property)
  "Get PROPERTY for case TYPE.

PROPERTY can be: `:rx.id', `:separators', or `:rx.full'"
  (let (var.case)

    ;; Return property from case variable.
    (_:str:case:property.get
     ;; Find TYPE's variable.
     (pcase type
       ;;------------------------------
       ;; Cases: Simple
       ;;------------------------------
       (:lower
        (setq var.case _:str:case:lower))

       (:upper
        (setq var.case _:str:case:upper))

       (:title
        (setq var.case _:str:case:title))

       ;;------------------------------
       ;; Cases: Camel
       ;;------------------------------
       ((or :camel
            :camel.any)
        (setq var.case _:str:case:camel.any))

       (:camel.lower
        (setq var.case _:str:case:camel.lower))

       (:camel.upper
        (setq var.case _:str:case:camel.upper))

       ;;------------------------------
       ;; Cases: Snake
       ;;------------------------------
       ((or :snake
            :snake.any)
        (setq var.case _:str:case:snake.any))

       (:snake.lower
        (setq var.case _:str:case:snake.lower))

       (:snake.upper
        (setq var.case _:str:case:snake.upper))

       (:snake.title
        (setq var.case _:str:case:snake.title))

       ;;------------------------------
       ;; Cases: Alternating
       ;;------------------------------
       ((or :alternating
            :alternating.any)
        (setq var.case _:str:case:alternating.any))

       (:alternating.lower
        (setq var.case _:str:case:alternating.lower))

       (:alternating.upper
        (setq var.case _:str:case:alternating.upper))

       ;;------------------------------
       ;; Fallthrough / Error
       ;;------------------------------
       (_
        (error "_:str:case:get: Unknown case type: %S"
               type)))

     ;; Property keyword we want from the var.
     property)))


;;------------------------------
;; General
;;------------------------------

(defun str:case:identify (string &optional types)
  "Identifies case type(s) of STRING.

If TYPES is non-nil, uses that list of type keywords instead of
`str:cases:rx/types.identify'. See:
  - `str:cases:rx/types.identify' for all identifiable type keywords.
  - `str:cases:rx/types.base' for the general type keywords.

Returns a plist of: '(:types <list> :separators <plist-or-nil>)
  - :types list will be keywords from TYPES or `str:cases:rx/types.all'.
  - :separators plist, if not nil, will be a plist of <type> to string.
    + e.g. '(:snake \"_\") from \"snake_case\".
    + e.g. '(:snake \"-\") from \"snake-case\".

Examples:
  (str:case:identify \"hello_there\")
    -> (:types (:snake.lower :snake :lower) :separators (:snake.lower \"_\" :snake \"_\" :lower \"_\"))
  (str:case:identify \"HelloThere\")
    -> (:types (:camel.upper :camel) :separators nil)
  (str:case:identify \"hello there\")
    -> (:types (:lower) :separators (:lower \" \"))
  (str:case:identify \"Hello\")
    -> (:types (:camel.upper :camel :snake.title :snake :title) :separators nil)
  (str:case:identify \"hElLo\")
    -> (:types (:alternating.lower :alternating) :separators nil)
  (str:case:identify \"HeLlO\")
    -> (:types (:alternating.upper :alternating) :separators nil)"
  (declare (pure t) (side-effect-free t))

  (str:rx:with/case.sensitive
   ;; Force to a string.
   (let ((str (format "%s" string))
         (types (or types
                    str:cases:rx/types.all))
         matches
         separators)
     ;; Check all regexs in `_:str:case:rx'.
     (while types
       (let* ((type (car types))
              (string/matches? (_:str:case:type.get type :rx.full)))
         ;; Jump to next plist kvp.
         (setq types (cdr types))

         (save-match-data
           ;; Does it match this case?
           (when (funcall string/matches? str)
             ;; Is there a separator match to save?
             (when-let* ((separator (match-string (plist-get _:str:case:rx:groups :separators)
                                                  str))
                         (separator (if (and (stringp separator)
                                             (not (string= "" separator)))
                                        separator
                                      nil)))
               (push separator separators)
               (push type separators))
             ;; Save matched type.
             (push type matches)))))

     ;; Return whatever we matched.
     (list :types matches
           :separators separators))))
;; (str:case:identify "hello_there")
;; (str:case:identify "HelloThere")
;; (str:case:identify "hello there")
;; (str:case:identify "Hello")
;; (str:case:identify "hElL")
;; (str:case:identify "hElLo")
;; (str:case:identify "HeLl")
;; (str:case:identify "HeLlO")


;;------------------------------------------------------------------------------
;; Case Conversion - Strings
;;------------------------------------------------------------------------------

(defun _:str:case:normalize->str (caller string-or-list)
  "Takes a string or list of strings, and returns a string

If STRING-OR-LIST is a string, returns STRING-OR-LIST.
If STRING-OR-LIST is a list, joins the list together with spaces.

CALLER is used when signaling an error message."
  (declare (pure t) (side-effect-free t))
  (cond ((listp string-or-list)
         (apply #'str:join " " string-or-list))
        ((stringp string-or-list)
         string-or-list)
        (t
         (error "%s: Expected a string or list of strings, got: %S"
                caller
                string-or-list))))
;; (_:str:case:normalize->str "test" "hello world")
;; (_:str:case:normalize->str "test" '("hello" "there"))
;; (_:str:case:normalize->str "test" :hi)


(defun _:str:case:normalize->list (caller string-or-list)
  "Takes a string or list of strings, and returns a list of strings.

If STRING-OR-LIST is a string, returns STRING-OR-LIST.
If STRING-OR-LIST is a list, joins the list together with spaces.

CALLER is used when signaling an error message."
  (declare (pure t) (side-effect-free t))
  (cond ((listp string-or-list)
         string-or-list)
        ((stringp string-or-list)
         (str:split (rx whitespace) string-or-list))
        (t
         (error "%s: Expected a string or list of strings, got: %S"
                caller
                string-or-list))))
;; (_:str:case:normalize->list "test" "hello world")
;; (_:str:case:normalize->list "test" '("hello" "there"))
;; (_:str:case:normalize->list "test" :hi)


;;------------------------------
;; Simple
;;------------------------------

(defun str:case/string:to:lower (string-or-list &optional _unused:separator)
  "Convert STRING-OR-LIST to lowercase."
  (declare (pure t) (side-effect-free t))
  (downcase (_:str:case:normalize->str "str:case/string:to:lower" string-or-list)))
;; (str:case/string:to:lower "HELLO")


(defun str:case/string:to:upper (string-or-list &optional _unused:separator)
  "Convert STRING-OR-LIST to uppercase."
  (declare (pure t) (side-effect-free t))
  (upcase (_:str:case:normalize->str "str:case/string:to:upper" string-or-list)))
;; (str:case/string:to:upper "hello")
;; (str:case/string:to:upper '("hello" "world"))


;;------------------------------
;; Title Case
;;------------------------------

(defun str:case/string:to:title (string-or-list &optional _separator)
  "Convert STRING-OR-LIST from \"title case\" to \"Title Case\"."
  (declare (pure t) (side-effect-free t))
  (apply #'str:join " "
         (mapcar #'capitalize
                 (_:str:case:normalize->list "str:case/string:to:title"
                                                string-or-list))))
;; (str:case/string:to:title "hello there")
;; (str:case/string:to:title '("hello" "there"))


;;------------------------------
;; CamelCase
;;------------------------------

(defun str:case/string:to:camel.lower (string-or-list &optional _separator)
  "Convert STRING-OR-LIST from \"lower camel case\" to \"lowerCamelCase\"."
  (declare (pure t) (side-effect-free t))
  (let ((words (_:str:case:normalize->list "str:case/string:to:camel.lower" string-or-list)))
    (apply #'str:join ""
           (car words) ;; Leave first lowercased.
           ;; Titlecase the rest for CamelHumps.
           (mapcar #'capitalize (cdr words)))))
;; (str:case/string:to:camel.lower "hello there")
;; (str:case/string:to:camel.lower '("hello" "there"))


(defun str:case/string:to:camel.upper (string-or-list &optional _separator)
  "Convert STRING-OR-LIST from \"upper camel case\" to \"UpperCamelCase\"."
  (declare (pure t) (side-effect-free t))
  (apply #'str:join ""
         (mapcar #'capitalize
                 (_:str:case:normalize->list "str:case/string:to:camel.string"
                                                string-or-list))))
;; (str:case/string:to:camel.upper "hello there")


;;------------------------------
;; snake_case
;;------------------------------

(defun str:case/string:from:snake (string &optional separator)
  "Convert STRING from \"snake_case\" (or \"snake-case\", or whatever
SEPARATOR is) to \"snake case\"."
  (declare (pure t) (side-effect-free t))
  (let ((separator (or separator
                       "_")))
    (save-match-data
      (replace-regexp-in-string (rx-to-string `(one-or-more ,separator))
                                " "
                                string))))
;; (str:case/string:from:snake "hello_there")
;; (str:case/string:from:snake "hello-there" "-")


(defun str:case/string:to:snake.lower (string-or-list &optional separator)
  "Convert STRING-OR-LIST list or string from '(\"snake\" \"case\") or \"snake case\" to \"snake_case\"."
  (declare (pure t) (side-effect-free t))
  (let ((separator (or separator
                       "_")))
    (if (listp string-or-list)
        (string-join string-or-list separator)
      (save-match-data
        (replace-regexp-in-string (rx (one-or-more space))
                                  separator
                                  string-or-list)))))
;; (str:case/string:to:snake.lower "lower snake case")
;; (str:case/string:to:snake.lower "lower snake case" "-")


(defun str:case/string:to:snake.upper (string-or-list &optional separator)
  "Convert STRING-OR-LIST from \"upper snake case\" to \"UPPER_SNAKE_CASE\"."
  (declare (pure t) (side-effect-free t))
  (let ((separator (or separator
                       "_")))
    (if (listp string-or-list)
        (string-join string-or-list separator)
      (save-match-data
        (replace-regexp-in-string (rx (one-or-more space))
                                  separator
                                  (str:case/string:to:upper string-or-list))))))
;; (str:case/string:to:snake.upper "upper snake case")
;; (str:case/string:to:snake.upper "upper snake case" "-")


(defun str:case/string:to:snake.title (string-or-list &optional separator)
  "Convert STRING-OR-LIST from \"title snake case\" to \"Title_Snake_Case\"."
  (declare (pure t) (side-effect-free t))
  (let ((separator (or separator
                       "_")))
    (if (listp string-or-list)
        (string-join string-or-list separator)
      (save-match-data
        (replace-regexp-in-string (rx (one-or-more space))
                                  separator
                                  (str:case/string:to:title string-or-list))))))
;; (str:case/string:to:snake.title "title snake case")
;; (str:case/string:to:snake.title "title snake case" "-")


;;------------------------------
;; AlTeRnAtInG cAsE
;;------------------------------

(defun str:case/string:to:alternating.general (string-or-list first-char-lower)
  "Convert STRING-OR-LIST from \"alternating case\" to \"AlTeRnAtInG cAsE\".

If FIRST-CHAR-LOWER is non-nil, alternating case will start off with a
lower case character."
  (declare (pure t) (side-effect-free t))
  ;; Case conversion toggle on only visible characters.
  (let* ((string (_:str:case:normalize->str "str:case/string:to:alternating.general"
                                               string-or-list))
         (string.length (length string))
         (index 0)
         next
         (upper? (null first-char-lower))
         chars)
    (while (and index
                (< index string.length))
      (message "next: %s, index: %s, char: %s"
               next index (string (elt string index)))
      ;; Should we search for something to change?
      (when (or (null next)
                (< next index))
        ;; Find next character to change.
        (save-match-data
          (setq next (string-match (rx letter)
                                   string
                                   index)))
        (message "find next `next'! index: %S, next: %S" index next))

      ;; Should we uppercase/lowercase this char or just pass it on?
      (if (and index
               next
               (= index next))
          (progn
            (if upper?
                (push (upcase (elt string index)) chars)
              (push (downcase (elt string index)) chars))
            (setq upper? (not upper?)))

        (push (elt string index) chars))

      (message "chars: %S" chars)
      (setq index (1+ index)))

    (apply #'string (nreverse chars))))
;; (str:case/string:to:alternating.general "hello" t)
;; (str:case/string:to:alternating.general "hello" nil)


(defun str:case/string:to:alternating.upper (string-or-list &optional _separator)
  "Convert STRING-OR-LIST from \"alternating case\" to \"AlTeRnAtInG cAsE\"."
  (declare (pure t) (side-effect-free t))
  (str:case/string:to:alternating.general string-or-list nil))


(defun str:case/string:to:alternating.lower (string-or-list &optional _separator)
  "Convert STRING-OR-LIST from \"alternating case\" to \"aLtErNaTiNg CaSe\"."
  (declare (pure t) (side-effect-free t))
  (str:case/string:to:alternating.general string-or-list t))


(defun str:case/string:to:alternating.random (string-or-list &optional _separator)
  "Convert STRING-OR-LIST from \"alternating case\" to either
\"AlTeRnAtInG cAsE\" or \"aLtErNaTiNg CaSe\"."
  (declare (pure t) (side-effect-free t))
  (str:case/string:to:alternating.general string-or-list
                                          (= (% (random) 2) 0)))


;;------------------------------
;; General Conversion
;;------------------------------

(defconst _:str:case:from:converted-by-lowercasing
  '(:lower
    :upper
    :title
    :alternating
    :alternating.lower
    :alternating.upper)
  "Case types that are taken care of in the 'convert from' step by the conversion to lower-case.")


(defun str:case/string:from (string &optional separator)
  "Process string for conversion by converting it from whatever it is into a list of string words."
  (declare (pure t) (side-effect-free t))

  ;; We want to get to lowercase for processing, so start off by just lowercasing the entire string.
  ;; This takes care of: (:lower :upper :title :alternating)
  (let ((str.working (str:case/string:to:lower string)))
    ;; Convert from anything known into list of lowercased words.
    ;; Return `str.working' when we're done.
    (dolist (from (str:case:identify str.working) str.working)
      (cond
       ((memq from _:str:case:from:converted-by-lowercasing)
        ;; This is fine; already taken care of in `let'.
        str.working)

       (:snake
        (setq str.working (str:case/string:from:snake str.working separator)))

       (t
        ;; Haven't coded a conversion for that yet...
        (error "str:case/string:to:from: Encountered a case type (%S) that was unexpected! Working String: '%s'"
               from str.working))))))
;; (str:case/string:from "HELLO_THERE")


(defun str:case/string:to (string &rest cases)
  "Convert STRING according to CASES keywords.

If CASES has keyword `:separator', the item following it must be a string of
the separator to use.
Example:
  (str:case/string:to \"hello there\" :snake :upper :separator \"-\")
    -> \"HELLO-THERE\"

Can print unused CASES keywords if `:print' CASE keyword is used."
  (declare (pure t) (side-effect-free t))

  (let (cases/to
        print?
        skip
        separator)

    ;; Parse CASES out into case types and optional separator.
    (dotimes (index (length cases))
      (if skip
          (setq skip nil)

        (let ((curr (elt cases index))
              (next (elt cases (1+ index))))
          (cond
           ;; Our special keyword.
           ((and (keywordp curr)
                 (eq :print curr))
            (setq print? t))

           ;; A case type; copy to our working list.
           ((memq curr str:cases:rx/types.all)
            (push curr cases/to))

           ;; Save separator to use.
           ((eq :separator curr)
            (if (and next
                     (stringp next))
                (setq separator next
                      skip t)
              (error "str:case/string:to: Separator must be a string, got: %S"
                     next)))

           ;; ERROR!
           (t
            (error "str:case/string:to: unknown case: %S"
                   curr))))))

    ;; Get the case types back to the order supplied.
    (setq cases/to (nreverse cases/to))

    (unless cases/to
      (error (concat "%s: Require CASES keywords to convert string! "
                     "string: '%s', cases: %S")
             "str:case/string:to"
             string
             cases/to))

    (let ((words.lower (str:case/string:from string))
          (cases.remaining cases/to))
      (prog1
          ;; Return the string created from this:
          (cond
           ;;------------------------------
           ;; snake_cases
           ;;------------------------------
           ;; Check non-default for a case type first. e.g. UPPER_SNAKE_CASE.
           ((and (memq :snake cases/to)
                 (memq :upper cases/to))
            (setq cases.remaining (remove :snake cases.remaining))
            (setq cases.remaining (remove :upper cases.remaining))
            (str:case/string:to:snake.upper words.lower separator))

           ((and (memq :snake cases/to)
                 (memq :title cases/to))
            (setq cases.remaining (remove :snake cases.remaining))
            (setq cases.remaining (remove :title cases.remaining))
            (str:case/string:to:snake.title words.lower separator))

           ;; Default snake_case.
           ((memq :snake cases/to)
            (setq cases.remaining (remove :snake cases.remaining))
            (setq cases.remaining (remove :lower cases.remaining))
            (str:case/string:to:snake.lower words.lower separator))

           ;;------------------------------
           ;; CamelCases
           ;;------------------------------
           ((and (memq :camel cases/to)
                 (memq :upper cases/to))
            (setq cases.remaining (remove :camel cases.remaining))
            (setq cases.remaining (remove :upper cases.remaining))
            (str:case/string:to:camel.upper words.lower separator))


           ;; Default camel_case.
           ((memq :camel cases/to)
            (setq cases.remaining (remove :camel cases.remaining))
            (setq cases.remaining (remove :lower cases.remaining))
            (str:case/string:to:camel.lower words.lower separator))

           ;;------------------------------
           ;; AlTeRnAtInG cases
           ;;------------------------------
           ((or (memq :alternating.upper cases/to)
                (and  (memq :alternating cases/to)
                      (memq :upper cases/to)))
            (setq cases.remaining (remove :alternating.upper cases.remaining))
            (setq cases.remaining (remove :alternating cases.remaining))
            (setq cases.remaining (remove :upper cases.remaining))
            (str:case/string:to:alternating.upper words.lower separator))

           ;; Default aLtErNaTiNg CaSe.
           ((or (memq :alternating.lower cases/to)
                (and  (memq :alternating cases/to)
                      (memq :lower cases/to))
                (memq :alternating cases/to))
            (setq cases.remaining (remove :alternating.lower cases.remaining))
            (setq cases.remaining (remove :alternating cases.remaining))
            (setq cases.remaining (remove :lower cases.remaining))
            (str:case/string:to:alternating.lower words.lower separator))

           ((or (memq :alternating.random cases/to)
                (and (memq :alternating cases/to)
                     (memq :random cases/to)))
            (setq cases.remaining (remove :alternating.random cases.remaining))
            (setq cases.remaining (remove :alternating cases.remaining))
            (setq cases.remaining (remove :random cases.remaining))
            (str:case/string:to:alternating.random words.lower separator))

           ;;------------------------------
           ;; UPPER/Title/LOWER cases
           ;;------------------------------
           ;; Do these last so they don't steal from e.g. '(:snake :upper).
           ((memq :upper cases/to)
            (setq cases.remaining (remove :upper cases.remaining))
            (str:case/string:to:upper words.lower separator))

           ((memq :title cases/to)
            (setq cases.remaining (remove :title cases.remaining))
            (str:case/string:to:title words.lower separator))

           ((memq :lower cases/to)
            (setq cases.remaining (remove :lower cases.remaining))
            (str:case/string:to:lower words.lower separator)))

        ;; Print unused cases?
        (when print?
          (setq cases.remaining (remove :print cases.remaining))
          (message "str:case/string:to: Unused cases keywords: %S"
                   cases.remaining))))))
;; (str:case/string:to "Hello_There" :snake :upper)
;; (str:case/string:to "Hello_There" :separator "-" :snake :upper)
;; (str:case/string:to "Hello_There" :snake :upper :print)


;;------------------------------------------------------------------------------
;; Case Conversion - Region
;;------------------------------------------------------------------------------

;;------------------------------
;; Simple
;;------------------------------

(defun str:case/region:to:lower (start end)
  "Convert region in current buffer described by START and END
integers/markers to lowercase."
  (interactive "r")
  (downcase-region start end))


(defun str:case/region:to:upper (start end)
  "Convert region in current buffer described by START and END
integers/markers to uppercase."
  (interactive "r")
  (upcase-region start end))


;;------------------------------
;; Title Case
;;------------------------------

(defun str:case/region:to:title (start end)
  "Convert region in current buffer described by START and END
integers/markers from \"title case\" to \"Title Case\"."
  (interactive "r")
  (str:region->region start end #'str:case/string:to:title))


;;------------------------------
;; CamelCase
;;------------------------------

(defun str:case/region:to:camel.lower (start end)
  "Convert region in current buffer described by START and END
integers/markers from \"lower camel case\" to \"lowerCamelCase\"."
  (interactive "r")
  (str:region->region start end #'str:case/string:to:camel.lower))


(defun str:case/region:to:camel.upper (start end)
  "Convert region in current buffer described by START and END
integers/markers from \"upper camel case\" to \"UpperCamelCase\"."
  (interactive "r")
  (str:region->region start end #'str:case/string:to:camel.upper))


;;------------------------------
;; snake_case
;;------------------------------

(defun str:case/region:from:snake (start end)
  "Convert region in current buffer described by START and END
integers/markers from \"snake_case\" to \"snake case\"."
  (interactive "r")
  (str:region->region start end #'str:case/string:from:snake))


(defun str:case/region:to:snake.lower (start end)
  "Convert region in current buffer described by START and END
integers/markers list or string from '(\"snake\" \"case\") or \"snake case\" to \"snake_case\"."
  (interactive "r")
  (str:region->region start end #'str:case/string:from:snake.lower))
;; (str:case/region:to:snake.lower "lower snake case")


(defun str:case/region:to:snake.upper (start end)
  "Convert region in current buffer described by START and END
integers/markers from \"upper snake case\" to \"UPPER_SNAKE_CASE\"."
  (interactive "r")
  (str:region->region start end #'str:case/string:from:snake.upper))


(defun str:case/region:to:snake.title (start end)
  "Convert region in current buffer described by START and END
integers/markers from \"title snake case\" to \"Title_Snake_Case\"."
  (interactive "r")
  (str:region->region start end #'str:case/string:from:snake.title))


;;------------------------------
;; AlTeRnAtInG cAsE
;;------------------------------

(defun str:case/region:to:alternating.general (start end first-char-lower)
  "Convert region in current buffer described by START and END
integers/markers from \"alternating case\" to \"AlTeRnAtInG cAsE\".

If FIRST-CHAR-LOWER is non-nil, alternating case will start off with a
lower case character."
  (interactive "r")
  (str:region->region start end #'str:case/string:to:alternating.general first-char-lower))


(defun str:case/region:to:alternating.upper (start end)
  "Convert region in current buffer described by START and END
integers/markers from \"alternating case\" to \"AlTeRnAtInG cAsE\"."
  (interactive "r")
  (str:region->region start end #'str:case/string:to:alternating.upper))


(defun str:case/region:to:alternating.lower (start end)
  "Convert region in current buffer described by START and END
integers/markers from \"alternating case\" to \"aLtErNaTiNg CaSe\"."
  (interactive "r")
  (str:region->region start end #'str:case/string:to:alternating.lower))


(defun str:case/region:to:alternating.random (start end)
  "Convert region in current buffer described by START and END
integers/markers from \"alternating case\" to either
\"AlTeRnAtInG cAsE\" or \"aLtErNaTiNg CaSe\"."
  (interactive "r")
  (str:region->region start end #'str:case/string:to:alternating.random))


;;------------------------------
;; General Conversion
;;------------------------------

;; TODO:str: better interactivity - help with completion of CASES
(defun str:case/region:to (start end &rest cases)
  "Convert region in current buffer described by START and END
integers/marker according to CASES keywords."
  (interactive "r\nsCases: ")

  ;; Convert interactive string to keywords?
  (when (and (= (length cases) 1)
             (stringp (car cases)))
    ;; Split and convert each into a keyword.
    (setq cases (mapcar #'keyword:normalize:any
                        (str:split " " cases)))
    ;; Check that they're all valid.
    (unless (seq-every-p #'_:str:case:validate/case cases)
      (error "Not all cases are valid! See `str:cases:rx/types.all' for valids. cases: %S" cases)))

  ;; Run the conversion(s) on the region.
  (apply #'str:region->region start end #'str:case/string:to cases))


;;------------------------------------------------------------------------------
;; Words
;;------------------------------------------------------------------------------

;; TODO: ->word func for each ->region func.

;;------------------------------
;; General Conversion
;;------------------------------

(defun str:case/word:to (&rest cases)
  "Convert word at point according to CASES keywords."
  (interactive "sCases: ")

  (apply #'str:word-at-point->region #'str:case/region:to cases))


;;------------------------------------------------------------------------------
;; Characters
;;------------------------------------------------------------------------------

;; TODO: Alist of chars to toggled case?
;;         - Because I know I've wanted to be able to "toggle" the "case" of
;;           e.g. numbers before...

(defun str:case/char:toggle (char)
  "Toggle CHAR between upper and lower case."
  (if (eq (upcase char) char)
      (downcase char)
    (upcase char)))
;; (str:case/char:toggle ?h)
;; (str:case/char:toggle ?H)
;; (str:case/char:toggle ?1)


(defun str:cmd:case/char:toggle ()
  "Toggle the case of the character at point between upper and lower case."
  (interactive)

  (if (imp:mode? 'evil-mode)
      ;; Evil has its own thing:
      (evil-invert-case)
    ;; DIY:
    (let* ((char/orig (following-char))
           (char/toggle (str:case/char:toggle char/orig)))
      ;; Is it a char that has upper & lower cases?
      (if (eq char/orig char/toggle)
          ;; Just go to next char.
          (forward-char)
        ;; Replace w/ toggled char and end up at next char.
        (delete-char 1 nil)
        (insert-char char/toggle 1)))))


(defun str:case/string:toggle (string)
  "Toggle each character in STRING between upper and lower case."
  (concat (seq-map #'str:case/char:toggle (append string nil))))
;; (str:case/string:toggle "hello")
;; (str:case/string:toggle "HeLlO")
;; (str:case/string:toggle (str:case/string:toggle "hello"))


(defun str:cmd:case/region:toggle (start end)
  "Toggle the case of each character between START and END of region."
  (interactive "r")

  (if (imp:mode? 'evil-mode)
      ;; Evil has its own thing:
      (evil-invert-case)
    ;; DIY:
    (str:region->region start end #'str:case/string:toggle)))


;;------------------------------------------------------------------------------
;; TODO: DWIM interactive funcs.
;;------------------------------------------------------------------------------

;; TODO: ->word or ->region depending on point/mark.
;; TODO: Change hydra to use DWIMs.


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide str case)
