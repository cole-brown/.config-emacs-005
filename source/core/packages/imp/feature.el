;;; core/modules/emacs/imp/feature.el --- imp feature tree, name normalization, etc -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2020-10-28
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
;; ║                              Imp Features                              ║
;; ╚════════════════════════════════════════════════════════════════════════╝
;;                                   ──────
;;           I imagine some horns, a tail... maybe an evil cackle?
;;                                 ──────────
;;
;; - feature tree
;; - name normalization
;; - feature predicates
;; - feature at path
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Loaded Features Tree
;;------------------------------------------------------------------------------

(defvar imp:features nil
  "Features that have been loaded by `imp:provide'.

It is a tree; an alist of alists of ... ad nauseam. Provided features are the
leaves, and their feature names should be built from the path traversed to get
to them.
  - I.e. directory structures w/ files as leaves.

For example:
  '((:imp
     (provide)
     (require))
    (:metasyntactic
     (foo (bar (baz (qux (quux (quuux (quuuux (quuuuux))))))
               (thud (grunt))
               (bletch)
               (fum)
               (bongo)
               (zot)))
     (bazola (ztesch))
     (fred (jim (sheila (barney))))
     (corge (grault (flarp)))
     (zxc (spqr (wombat)))
     (shme)
     (spam (eggs))
     (snork)
     (blarg (wibble))
     (toto (titi (tata (tutu))))
     (pippo (pluto (paperino)))
     (aap (noot (mies)))
     (oogle (foogle (boogle (zork (gork (bork)))))))
    (:pinky (narf (zort (poit (egad (troz (fiddely-posh))))))))
    - is a tree with 3 'roots':
      - :imp
        - provide
        - require
      - :metasyntactic
        - ...
      - :pinky
        - ...")
;; (setq imp:features nil)


(defvar imp:features:locate nil
  "Alist of imp features to paths/filenames.

NOTE: Use `equal' for the `imp<alist>:[...]' EQUAL-FN params!

'(normalized-feature . (file-path-0 ...))

Example:
  (list (list :imp \"init.el\")
        (list (imp:feature :imp path) \"path.el\")
        (list (imp:feature :imp multiple)
              \"multiple/foo.el\"
              \"multiple/subdir/bar.el\"
              \"multiple/subdir/baz.el\"
              \"common/baz.el\")
        ...)")
;; (pp imp:features:locate)
;; (setq imp:features:locate nil)


(defconst int<imp>:features:locate:equal #'equal
  "Equality func to use for alists.

Supply as EQUAL-FN param to all 'imp/alist.el' functions used
by `imp:features:locate'.")


;;------------------------------------------------------------------------------
;; Feature Helpers
;;------------------------------------------------------------------------------

(defun imp:feature:exists? (features)
  "Check for list of FEATURES in the `imp:features' tree."
  ;; When not `imp:features', always return `nil'.
  (when imp:features
    (not (null (int<imp>:tree:contains? (int<imp>:list:flatten features)
                                        imp:features)))))
;; (imp:feature:exists? '(:imp))
;; (imp:feature:exists? '(:imp provide))
;; (imp:feature:exists? '(:imp (provide)))


(defun imp:feature? (&rest feature)
  "Check if FEATURE is provided by 'imp' or Emacs."
  (or (imp:feature:exists? feature)
      (featurep (imp:feature:normalize:imp->emacs feature))))
;; (imp:feature? 'which-key)
;; (imp:feature? :which-key)
;; (imp:feature? :imp)


(defun imp:mode? (mode)
  "Check if MODE exists and is enabled."
  ;; Can't use `bound-and-true-p' due to MODE being passed in,
  ;; so check bound and truthy separately.
  (and (boundp mode)
       (symbol-value mode)))
;; (imp:mode? 'evil-mode)
;; (imp:mode? 'evil-mode-jeff)


(defalias 'imp:featurep 'imp:feature?)


(defun int<imp>:feature:count (&optional tree)
  "Count features in TREE.

TREE should be a list of lists and/or symbols.
TREE should be a branch of `imp:features`.

Return count of leaf nodes in TREE."
  (cond ((null tree) 0)

        ((not (consp tree)) 1)

        (t
         (+ (int<imp>:feature:count (car tree))
            (int<imp>:feature:count (cdr tree))))))


(defun imp:feature:count ()
  "Count features in `imp:features`.

Return count of leaf nodes."
  (int<imp>:feature:count imp:features))
;; (imp:feature:count)



;;------------------------------------------------------------------------------
;; Normalization
;;------------------------------------------------------------------------------

(defconst int<imp>:feature:replace:rx
  (list
   ;;------------------------------
   ;; Not allowed for specific reasons:
   ;;------------------------------
   '(":" "") ; Used by imp as separator between feature symbol list: '(:foo bar baz) -> ":foo:bar:baz"
   '("+" "") ; Used occasionally to denote optional features; remove to be sane. '(:foo +optional) -> ":foo:optional"
   ;;------------------------------
   ;; Not allowed for no real reason, in Qwerty keyboard order:
   ;;------------------------------
   (list (rx-to-string '(or "`" "~" "!" "@" "#" "$" "%" "^" "&" "*" "(" ")"
                            "[" "]" "{" "}" "\\" "|"
                            ";" "'" "\""
                            ",")
                       :no-group)
         ""))
  "Alist of regexs to replace and their replacement strings.

Using lists instead of cons for alist entries because `cons' doesn't like
strings.

Used symbol-by-symbol in `imp:feature:normalize:imp->emacs' when
translating an imp symbol chain into one symbol for Emacs.")
;; (imp:feature:normalize:imp->emacs :imp 'test?-xx:y::z '+!@$%^&*| 'symbols)


(defconst int<imp>:feature:replace:separator
  ":"
  "Used for 'imp' -> Emacs feature normalization.

String to use in between symbols when translating an imp symbol chain to
an Emacs symbol.")


(defun int<imp>:str:empty? (str &optional trim?)
  "Return non-nil if STR is nil or empty.

If TRIM? is non-nil, use `string-trim' before checking if string is empty."
  (or (null str)
      (string= ""
               (if trim?
                   (string-trim str)
                 str))))
;; (int<imp>:str:empty? nil)
;; (int<imp>:str:empty? "")
;; (int<imp>:str:empty? " ")
;; (int<imp>:str:empty? " " :trim)


(defun int<imp>:feature:name:normalize (input)
  "Normalize INPUT to a symbol.

Uses `int<imp>:feature:replace:rx' to replace invalid characters in the INPUT
string or symbol name.

Returns a string."
  (let ((value (if (stringp input)
                   input
                 (symbol-name input))))
    ;; Replace each regex, return normalized string.
    (dolist (pair int<imp>:feature:replace:rx value)
      (setq value
            (replace-regexp-in-string (nth 0 pair)
                                      (nth 1 pair)
                                      value)))))
;; (int<imp>:feature:name:normalize "foo")
;; (int<imp>:feature:name:normalize :foo)
;; (int<imp>:feature:name:normalize 'test?-xx:y::z)
;; (int<imp>:feature:name:normalize "+!@$%^&*|")


(defun int<imp>:feature:normalize:string (&rest input)
  "Normalize INPUT to a list of strings.

Always returns a backwards list.
  Example:
    (int<imp>:feature:normalize :foo)
      -> '(\"foo\")
    (int<imp>:feature:normalize '(:foo))
      -> '(\"foo\")
    (int<imp>:feature:normalize '(:foo bar) 'baz)
      -> '(\"baz\" \"bar\" \"foo\")"
  (let ((func/name "int<imp>:feature:normalize:string")
        output)
    (dolist (item (int<imp>:list:flatten input))
      (let ((normalized (int<imp>:feature:name:normalize item)))
        (if (int<imp>:str:empty? normalized)
            (int<imp>:error func/name
                            "Cannot use INPUT '%S'; it normalizes to nothing: %S"
                            item
                            normalized)
          (push normalized output))))

    ;; Return the list or raise an error.
    (if (null output)
        (int<imp>:error func/name
                        "No normalized strings produced from INPUT: %S"
                        input))
    output))
;; (int<imp>:feature:normalize:string "+spydez" "foo" "bar")
;; (int<imp>:feature:normalize:string '((nil)))
;; (int<imp>:feature:normalize:string 'test?-xx:y::z)
;; (int<imp>:feature:normalize:string '+!@$%^&*|)
;; (int<imp>:feature:normalize:string :imp 'test?-xx:y::z "+!@$%^&*|" 'symbols)


(defun int<imp>:feature:normalize (&rest input)
  "Normalize INPUT to a list of feature keyword/symbols.

Always returns a list.
First symbol in output list will be a keyword; rest will be symbols.
  Example:
    (int<imp>:feature:normalize :foo)
      -> '(:foo)
    (int<imp>:feature:normalize '(:foo))
      -> '(:foo)
    (int<imp>:feature:normalize '(foo :bar) 'baz)
      -> '(:foo bar baz)"
  (let ((func/name "int<imp>:feature:normalize")
        (strings/reversed (int<imp>:feature:normalize:string input))
        output)
    (while strings/reversed
      (let ((item (pop strings/reversed)))
        (push (intern
               ;; If this is the last item we're processing, it should be the
               ;; keyword as it'll be first in the output list.
               (concat (if (null strings/reversed)
                           ":"
                         "")
                       item))
              output)))

    ;; Return the list or raise an error.
    (if (null output)
        (int<imp>:error func/name
                        "No normalized features produced from INPUT: %S"
                        input))
    output))
;; (int<imp>:feature:normalize '+layout/spydez)
;; (int<imp>:feature:normalize :spydez)
;; (int<imp>:feature:normalize "spydez")
;; (int<imp>:feature:normalize "+spydez")
;; (int<imp>:feature:normalize '("+spydez" "foo" "bar"))
;; (int<imp>:feature:normalize '(("+spydez" "foo" "bar")))
;; (int<imp>:feature:normalize '(((:test))) '(("+spydez" "foo" "bar")))
;; (int<imp>:feature:normalize 'something-that-doesnt-exist-in-emacs)
;; (int<imp>:feature:normalize '(something-that-doesnt-exist-in-emacs))
;; (let ((feature 'something-that-doesnt-exist-in-emacs))
;;   (int<imp>:feature:normalize (list feature)))
;; (int<imp>:feature:normalize :imp 'test?-xx:y::z "+!@$%^&*|" 'symbols)



(defun imp:feature:normalize:imp->emacs (&rest feature)
  "Translate the feature to a single symbol appropriate for Emacs' `provide'.

FEATURE should be:
  1) A keyword/symbol,
  2) or a list of keywords/symbols.

FEATURE will be normalized, then converted into a single symbol
\(not a keyword)."
  (intern
   (mapconcat #'identity
              (seq-filter (lambda (x) (not (int<imp>:str:empty? x)))
                          (nreverse (int<imp>:feature:normalize:string feature)))
              int<imp>:feature:replace:separator)))
;; (imp:feature:normalize:imp->emacs :imp 'test 'symbols)
;; (imp:feature:normalize:imp->emacs '(:imp test symbols))
;; (imp:feature:normalize:imp->emacs '(:imp test) 'symbols)
;; (imp:feature:normalize:imp->emacs '(:imp provide))
;; (imp:feature:normalize:imp->emacs :imp 'provide)
;; (imp:feature:normalize:imp->emacs :imp)
;; (imp:feature:normalize:imp->emacs '(((:imp))) '((provide)))
;; (imp:feature:normalize:imp->emacs :imp 'test?-xx:y::z '+!@$%^&*| 'symbols)


(defun imp:feature:normalize (&rest input)
  "Normalize INPUT to feature in one of two ways.

If only one INPUT param, returns a symbol/keyword.
  - This is useful for converting strings to symbols for e.g. `imp:provide'.
If more than one INPUT param, returns a list of symbol/keywords.

If INPUT item is:
  - Keyword: Return as-is.
  - Symbol:  Return as-is.
  - String:  Convert to a keyword.
E.g.
  1) `:module' -> `:module'
  2) `feature' -> `feature'
  3) \"str-4874\" -> `:str-4874'"
  (let* ((normalized (int<imp>:feature:normalize input)))
    ;; Return the list, the one item, or error?
    (cond ((null normalized)
           (int<imp>:error "imp:feature:normalize"
                           "Error normalizing features from INPUT; no features produced: %S"
                           input))

          ((= 1 (length normalized))
           (nth 0 normalized))

          (t
           normalized))))
;; (imp:feature:normalize '+layout/spydez)
;; (imp:feature:normalize :spydez)
;; (imp:feature:normalize "spydez")
;; (imp:feature:normalize "+spydez")
;; (imp:feature:normalize "+spydez" "foo" "bar")
;; (imp:feature:normalize '("+spydez" "foo" "bar"))
;; (imp:feature:normalize :imp 'test?-xx:y::z "+!@$%^&*|" 'symbols)

(defalias 'imp:feature 'imp:feature:normalize)


(defun int<imp>:feature:normalize:display (&rest feature)
  "Normalizes FEATURE down into a single keyword with separators.

Similar output to `imp:feature:normalize:imp->emacs'."
  ;; Prepend ":" and turn into a keyword.
  (intern (concat ":"
                  ;; Compress feature list down into a single string w/ separators.
                  (mapconcat #'identity
                             (nreverse (int<imp>:feature:normalize:string feature))
                             int<imp>:feature:replace:separator))))
;; (int<imp>:feature:normalize:display '(:imp test symbols))
;; (int<imp>:feature:normalize:display '(:imp test) 'symbols)
;; (int<imp>:feature:normalize:display '(:imp provide))
;; (int<imp>:feature:normalize:display :imp 'provide)
;; (int<imp>:feature:normalize:display :imp)
;; (int<imp>:feature:normalize:display '(((:imp))) '((provide)))
;; (int<imp>:feature:normalize:display "foo/bar/baz.el")


;;------------------------------------------------------------------------------
;; Add Feature.
;;------------------------------------------------------------------------------

(defun int<imp>:feature:add (normalized)
  "Add the NORMALIZED features to the `imp:features' tree.

NORMALIZED must already be a normalized (by `int<imp>:feature:normalize')
list of keywords/symbols."
  (int<imp>:debug "int<imp>:feature:add" "Adding to imp:features...")
  (int<imp>:debug "int<imp>:feature:add" "  feature: %S" normalized)
  ;; (int<imp>:debug "int<imp>:feature:add" "imp:features before:\n%s"
  ;;                 (pp-to-string imp:features))

  ;; Add normalized features to `imp:features' tree & set updated tree back
  ;; to `imp:features'.
  (int<imp>:tree:update normalized nil imp:features)

  (int<imp>:debug "int<imp>:feature:add" "imp:features after:\n%s"
                  (pp-to-string imp:features))
  ;; Not sure what to return, but the updated features seems decent enough.
  imp:features)
;; (setq imp:features nil)
;; (int<imp>:feature:add :imp 'test)
;; imp:features
;; (int<imp>:feature:add :imp 'ort 'something 'here)
;; (int<imp>:alist:get/value :imp imp:features)
;; (int<imp>:tree:contains? '(:imp) imp:features)
;; (int<imp>:tree:contains? '(:imp ort something) imp:features)


;;------------------------------------------------------------------------------
;; Demand Features Exist!
;;------------------------------------------------------------------------------

(defun imp:feature:assert (feature:base &rest feature)
  "A \"soft require\"; error if the feature is not already loaded.

Normalize FEATURE:BASE and FEATURE into an imp feature
\(via `imp:feature:normalize'), then checks if it's loaded or not.

Return normalized feature symobl if loaded.
Raise an error signal if not found.
Only check `imp:features' variable; does not check Emacs' `features' list."
  (if (imp:feature:exists? (cons feature:base feature))
      t
    (int<imp>:error "imp:feature:assert"
                    "No `%S' feature exists in imp's features!"
                    (imp:feature:normalize (list feature:base feature)))))


;;------------------------------------------------------------------------------
;; Features & Paths to them
;;------------------------------------------------------------------------------

(defun int<imp>:feature:locations (feature:base)
  "Return FEATURE:BASE's entry in `imp:features:locate' or nil."
  (int<imp>:alist:get/value feature:base
                            imp:features:locate))


(defun int<imp>:feature:paths (feature:base &rest feature)
  "Find (relative) path(s) to files for FEATURE:BASE + FEATURE.

This will only provide the paths for the feature itself, each of which may
`imp:require' more features.

Return list of: '(path:root . (paths:relative))

Errors if:
  - No root path for FEATURE:BASE.
  - No paths found for input parameters."
  (let ((func/name "int<imp>:feature:paths")
        (check (int<imp>:feature:normalize feature:base feature)))

    ;;------------------------------
    ;; Error Check Inputs
    ;;------------------------------
    ;; FEATURE:BASE must:
    ;; 1) Be an imp feature.
    ;; (imp:feature:assert feature:base)
    ;;   ...must it be a feature? Not so sure. All I /think/ we need is the entries in
    ;;   `imp:path:roots' and `imp:features:locate' and the

    ;; 2) Have registered a root path.
    (unless (int<imp>:path:root/contains? feature:base)
      (int<imp>:error func/name
                      "Feature `%S' does not have a root path in imp."
                      feature:base))

    ;;------------------------------
    ;; Get the paths and load them?
    ;;------------------------------
    (let* ((path:root (int<imp>:path:root/dir feature:base))
           (feature:locations (int<imp>:feature:locations feature:base))
           (paths (int<imp>:alist:get/value check
                                            feature:locations
                                            int<imp>:features:locate:equal)))
      (int<imp>:debug func/name
                      '("Get feature paths:\n"
                        "  - feature:base: %S\n"
                        "  - path:root:    %s\n"
                        "  - feature:locations: %S\n"
                        "  - paths: %S")
                      feature:base
                      path:root
                      feature:locations
                      paths)

      ;;---
      ;; Error Checks
      ;;---
      (unless feature:locations
        (int<imp>:error func/name
                        "No feature locations found for: %S"
                        feature:base))

      (unless paths
        (int<imp>:error func/name
                        "No feature paths found for: %S"
                        check))

      ;;---
      ;; Done; return.
      ;;---
      (int<imp>:debug func/name
                      '("Return feature paths for `%S':\n"
                        "  - path:root:    %s\n"
                        "  - paths: %S")
                      feature:base
                      path:root
                      paths)
      (cons path:root paths))))


(defun imp:feature:at (feature:base feature:alist)
  "Provide imp with an alist of imp features to paths/filenames.

This is used when `imp:require' is called for a sub-feature that isn't loaded.
imp will look in the `imp:path:roots' entry for the features file, load that
file, and then use the provided alist to find what files are required for said
sub-feature. If there is no features file, imp will load the root file.

FEATURE:BASE should be your base feature's keyword.
  example: `:imp' is imp's FEATURE:BASE.

FEATURE:ALIST should be an alist with each entry in this format:
  '(feature-keyword-or-list . (file-path-0 ...))
  Which is equal to:
  '(feature-keyword-or-list file-path-0 ...)

Features in the FEATURE:ALIST should:
  1) Not be normalized:
     e.g. '(:imp path) instead of (imp:feature :imp path)
  2) Be either:
     a) The base feature keyword (e.g. `:imp').
     b) A list of the base feature keyword plus other symbols
        (e.g. `(:imp path)').

Paths in the FEATURE:ALIST should be relative to your `imp:path:root'.

The paths will be loaded in the order provided.

For example:
  '((:imp        \"init.el\")
    ((:imp path) \"path.el\")
    ((:imp multiple)
     \"common/foo.el\"
     \"multiple/bar.el\"
     \"multiple/subdir/baz.el\"
     \"multiple/subdir/qux.el\")
    ...)"
  (let ((func/name "imp:feature:at")
        features:at)
    ;;------------------------------
    ;; Verify Inputs.
    ;;------------------------------

    ;;---
    ;; FEATURE:BASE root path needs to exist already.
    ;;---
    (unless (keywordp feature:base)
      (int<imp>:error func/name
                      "FEATURE:BASE must be a keyword! Got: %S"
                      feature:base))
    (if-let ((feature:base:path (int<imp>:path:root/dir feature:base)))
        (unless (stringp feature:base:path)
          (int<imp>:error func/name
                          "Registered root path for FEATURE:BASE must be a string! Got: %S"
                          feature:base:path))
      (int<imp>:error func/name
                      '("FEATURE:BASE must have a registered root path! "
                        "Did not find it in `imp:path:roots'.")))

    ;;---
    ;; FEATURE:ALIST must be valid format.
    ;;---
    ;; Massage into shape for adding to alist while we verify.
    (dolist (entry feature:alist)
      (let ((feature (car entry))
            (paths   (cdr entry)))
        ;; Must have either just a keyword, or a list of symbols (starting with keyword).
        (unless (or (keywordp feature)
                    (and (listp feature)
                         (keywordp (car feature))
                         (seq-each #'symbolp feature)))
          (int<imp>:error func/name
                          '("FEATURE:ALIST entry `%S' has an invalid feature! "
                            "Must be a keyword or list of symbols (starting w/ keyword). "
                            "Got: %S")
                          entry
                          feature))
        ;; Must have one string or list of strings for the paths.
        (unless (or (stringp paths)
                    (and (listp paths)
                         (seq-each #'stringp paths)))
          (int<imp>:error func/name
                          '("FEATURE:ALIST entry `%S' has invalid path(s)! "
                            "Must be a path string or a list of path strings. "
                            "Got: %S")
                          entry
                          paths))

        ;; Valid; finalize and add to alist.
        (push (cons (int<imp>:feature:normalize feature)
                    (seq-map #'int<imp>:path:sans-extension paths))
              features:at)))

    ;; We should have created something. Error if not.
    (unless features:at
      (int<imp>:error func/name
                      '("Nothing created to be added.. No input? FEATURE:ALIST: %S -> `features:at': %S")
                      feature:alist
                      features:at))

    ;;------------------------------
    ;; Add to the features locations alist.
    ;;------------------------------
    ;; Return their created alist if we succeeded. `nil' if failed.
    (if (int<imp>:alist:update feature:base
                               features:at
                               imp:features:locate
                               int<imp>:features:locate:equal)
        features:at
      nil)))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
