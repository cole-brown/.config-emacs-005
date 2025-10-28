;;; imp/feature.el --- imp feature tree, name normalization, etc -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2020-10-28
;; Timestamp:  2025-10-28
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
;; - feature name normalization
;; - feature predicates
;; - feature at path
;;
;;; Code:

(require 'rx)

;;------------------------------------------------------------------------------
;; Loaded Features Tree
;;------------------------------------------------------------------------------

(defvar imp-features nil
  "Features that have been loaded by `imp-provide'.

It is a tree; an alist of alists of ad nauseam. Provided features are the
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
;; (setq imp-features nil)


;;------------------------------------------------------------------------------
;; Feature Helpers
;;------------------------------------------------------------------------------

(defun imp-feature-exists? (&rest feature)
  "Check for list of FEATURES in the `imp-features' tree."
  ;; When not `imp-features', always return `nil'.
  (when imp-features
    (not (null (imp--tree-contains? (imp-feature-split feature :symbols)
                                    imp-features)))))
;; (pp imp-features)
;; (imp-feature-exists? 'imp)
;; (imp-feature-exists? '(imp provide))
;; (imp-feature-exists? '(imp (provide)))


(defun imp-feature? (&rest feature)
  "Check if FEATURE is provided by 'imp' or Emacs."
  (or (imp-feature-exists? feature)
      (featurep (imp-feature-normalize feature))))
;; (imp-feature? 'which-key)
;; (imp-feature? :which-key)
;; (imp-feature? :imp)


(defun imp-feature-assert (&rest feature)
  "A \"soft require\"; error if the feature is not already loaded.

Normalize FEATURE:BASE and FEATURE into an imp feature, then check
if it's loaded or not.

Return normalized feature symbol if loaded.
Raise an error signal if not found."
  (let ((normalized (imp-feature-normalize feature)))
    (if (apply #'imp-feature? feature)
        normalized
      (imp--error "imp-feature-assert"
                  '("Required feature is not loaded: \n"
                    "  feature:    %S\n"
                    "  normalized: %S\n"
                    "Check your order of providing/loading, "
                    "or make sure it initializes its root and "
                    "features with imp first.")
                  feature
                  (imp-feature-normalize feature)))))
;; (imp-feature-assert 'which-key)
;; (imp-feature-assert :which-key)
;; (imp-feature-assert :imp)
;; (imp-feature-assert :error)


(defun imp--feature-count (&optional tree)
  "Count features in TREE.

TREE should be a list of lists and/or symbols.
TREE should be a branch of `imp-features`.

Return count of leaf nodes in TREE."
  (cond ((null tree) 0)

        ((not (consp tree)) 1)

        (t
         (+ (imp--feature-count (car tree))
            (imp--feature-count (cdr tree))))))


(defun imp-feature-count ()
  "Count features in `imp-features`.

Return count of leaf nodes."
  (imp--feature-count imp-features))
;; (imp-feature-count)


;;------------------------------------------------------------------------------
;; Normalization
;;------------------------------------------------------------------------------

(defconst imp--feature-replace-rx
  (list
   ;;------------------------------
   ;; Not allowed for specific reasons:
   ;;------------------------------
   ;; Features have to be symbols sometimes, so no whitespace.
   (list (rx-to-string '(or (any whitespace) "\n") :no-group)
         "")

   ;; Features use colon to denote the feature root (a feature with an entry
   ;; in `imp-path-roots'). eg `imp:/feature' is this file.
   ;; Disallow it everywhere, then carefully insert it in the correct place?
   '(":" "")

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

Used symbol-by-symbol in `imp-feature-normalize' when
translating an imp symbol chain into one symbol for Emacs.")
;; (imp-feature-normalize :imp 'test?-xx:y::z '+!@$%^&*| 'symbols)


(defconst imp--feature-separator-chain
  "/"
  "Separator to use in between chain links when translating an imp feature
chain to a single, normalized symbol.")


(defconst imp--feature-separator-root
  ":"
  "Separator to use in between a feature root and subfeatures.")

(defun imp--feature-string (name)
  (let*((funcname 'imp--feature-string)
        (string (cond ((null name)
                       (imp--error funcname
                                   "Cannot convert nil to string: %S"
                                   (type-of name)
                                   name))
                      ((stringp name)
                       name)
                      ((symbolp name)
                       (symbol-name name))
                      (t
                       (imp--error funcname
                                   "Cannot convert %s to string: %S"
                                   (type-of name)
                                   name)))))
    ;; A bare root (eg "imp:") should trim off root separator.
    (if (string-suffix-p imp--feature-separator-root string)
        (string-remove-suffix imp--feature-separator-root string)
      string)))
;; (imp--feature-string 'imp)
;; (imp--feature-string 'imp:)


(defun imp--feature-symbol (name)
  (when name
    (intern (imp--feature-string name))))
;; (imp--feature-symbol "imp")
;; (imp--feature-symbol "imp:")
;; (imp--feature-symbol 'imp)
;; (imp--feature-symbol 'imp:)


(defun imp--feature-split (name)
  (string-split name imp--feature-separator-chain))
;; (imp--feature-split "imp:/foo/bar/baz")


(defun imp--feature-join (&rest name)
  (mapconcat #'identity
             name
             imp--feature-separator-chain))
;; (apply #'imp--feature-join (imp--feature-split "imp:/foo/bar/baz"))


(defun imp--feature-normalize-link (name)
  "Normalize NAME to a list of strings.

Uses `imp--feature-replace-rx' to replace invalid characters in the NAME
string or symbol name."
  (let ((funcname 'imp--feature-normalize-link))
    (imp--feature-split ; Split on "/".
     ;; Run string through rx replacements.
     (seq-reduce (lambda (value rx-pair)
                   (let ((replaced (replace-regexp-in-string (nth 0 rx-pair)
                                                             (nth 1 rx-pair)
                                                             value)))
                     (if (or (null replaced)
                             (string-empty-p replaced))
                         (imp--error funcname
                                     "Cannot convert to string; ended up with nothing: '%s'"
                                     name))
                     replaced))
                 imp--feature-replace-rx
                 ;; Normalize name to string.
                 (imp--feature-string name)))))
;; (imp--feature-normalize-link 'imp:/foo/bar/baz)
;; (imp--feature-normalize-link "foo:/bar")
;; (imp--feature-normalize-link "	white  space\n")
;; (imp--feature-normalize-link "foo")
;; (imp--feature-normalize-link :foo)
;; (imp--feature-normalize-link 'foo)
;; (imp--feature-normalize-link 'test?-xx:y::z)
;; (imp--feature-normalize-link "+!@$%^&*|")
;; (imp--feature-normalize-link nil)
;; (imp--feature-normalize-link "!@$%^&*|")


(defun imp--feature-normalize-chain (&rest chain)
  "Normalize CHAIN to a list of normalized strings."
  (let ((funcname 'imp--feature-normalize-chain)
        (normalized (imp--list-flatten
                     (seq-map #'imp--feature-normalize-link
                              (imp--list-flatten chain :unqoute))
                     :unqoute)))
    (if normalized
        normalized
      (imp--error funcname
                  "Nothing normalized from CHAIN: %S"
                  chain))))
;; (imp--feature-normalize-chain 'imp:/foo/bar/baz)
;; (imp--feature-normalize-chain "	white  space\n")
;; (imp--feature-normalize-chain "+spydez" "foo" "bar")
;; (imp--feature-normalize-chain "+spydez/foo/bar")
;; (imp--feature-normalize-chain '((nil)))
;; (imp--feature-normalize-chain 'test?-xx:y::z)
;; (imp--feature-normalize-chain '+!@$%^&*|)
;; (imp--feature-normalize-chain :imp 'test?-xx:y::z "+!@$%^&*|" 'symbols)
;; (imp--feature-normalize-chain :imp)


(defun imp--feature-root? (normalized-chain)
  (let ((root-maybe (imp--feature-symbol (car-safe normalized-chain))))
    (when (imp--path-root-dir root-maybe :no-error)
        root-maybe)))
;; (imp--feature-root? (imp--feature-normalize-chain :imp 'foo 'bar "baz"))
;; (imp--feature-root? '("imp"))
;; (imp--feature-root? '(":dne"))


(defun imp-feature-root (&rest feature)
  "Return root of FEATURE, if any. Else nil."
  (imp--feature-root? (apply #'imp--feature-normalize-chain feature)))
;; (imp-feature-root nil)
;; (imp-feature-root 'imp:/foo/bar/baz)
;; (imp-feature-root :imp 'foo 'bar "baz")
;; (imp-feature-root 'imp)
;; (imp-feature-root "imp")
;; (imp-feature-root '+layout/spydez)
;; (imp-feature-root ":spydez")
;; (imp-feature-root ":spydez" "foo" "bar")
;; (imp-feature-root "spydez" "foo" "bar")
;; (imp-feature-root '("+spydez" "foo" "bar"))


(defun imp-feature-unrooted (&rest feature)
  "Return FEATURE sans root, if any. Else nil."
  (let* ((normalized (apply #'imp--feature-normalize-chain feature))
         (root (imp--feature-root? normalized)))
    (seq-map #'imp--feature-symbol
             (if root
                 (cdr normalized)
               normalized))))
;; (imp-feature-unrooted 'imp:/foo/bar/baz)
;; (imp-feature-unrooted :imp 'foo 'bar "baz")
;; (imp-feature-unrooted 'imp)
;; (imp-feature-unrooted 'dne:/foo/bar/baz)


(defun imp-feature-first (&rest feature)
  "Return first symbol in feature, regardless of root."
  (imp--feature-symbol (car-safe (apply #'imp--feature-normalize-chain feature))))
;; (imp-feature-first nil)
;; (imp-feature-first 'imp:/foo/bar/baz)
;; (imp-feature-first 'dne/foo/bar/baz)


(defun imp-feature-join (&rest feature)
  (let* ((normalized (apply #'imp--feature-normalize-chain feature))
         (rooted (apply #'imp-feature-root normalized)))
    (apply #'imp--feature-join
           (if rooted
               (cons (concat (imp--feature-string rooted)
                             imp--feature-separator-root)
                     (cdr normalized))
             normalized))))
;; (imp-feature-join 'imp:/foo/bar/baz)
;; (imp-feature-join :imp 'foo 'bar "baz")
;; (imp-feature-join '(:imp foo bar "baz"))
;; (imp-feature-join "+foo/bar/baz")
;; (imp-feature-join "+foo" "bar" "baz")
;; (imp-feature-join '((nil)))
;; (imp-feature-join 'test?-xx:y::z)
;; (imp-feature-join '+!@$%^&*|)
;; (imp-feature-join :imp 'test?-xx:y::z "+!@$%^&*|" 'symbols)


(defun imp-feature-split (feature &optional symbols?)
  "`foo:/bar/baz' -> '(\"foo\" \"bar\" \"baz\")

Return list of strings.
If SYMBOLS? is non-nil, return list of symbols."
  (let ((split (apply #'imp--feature-normalize-chain
                      (if (proper-list-p feature)
                          feature
                        (list feature)))))
    (seq-map (if symbols? #'imp--feature-symbol #'identity)
             (cons (string-remove-suffix imp--feature-separator-root
                                         (car split))
                   (cdr split)))))
;; (imp-feature-split nil)
;; (imp-feature-split '+layout/spydez)
;; (imp-feature-split '+layout/spydez t)
;; (imp-feature-split 'foo:/bar/baz)
;; (imp-feature-split 'dne)
;; (imp-feature-split 'dne:/thing)
;; (imp-feature-split :imp 'foo 'bar "baz")
;; (imp-feature-split (imp-feature-join :imp 'foo 'bar "baz"))
;; (imp-feature-split (imp-feature-join "+spydez" "foo" "bar") t)


(defun imp-feature-normalize (&rest feature)
  "Translate the feature to a single symbol appropriate for Emacs' `provide'.

FEATURE should be:
  1) A keyword/symbol,
  2) or a list of keywords/symbols.

FEATURE will be normalized, then converted into a single symbol."
  (imp--feature-symbol (apply #'imp-feature-join feature)))
;; (imp-feature-normalize :imp 'test 'symbols)
;; (imp-feature-normalize '(:imp test symbols))
;; (imp-feature-normalize '(':imp 'test 'symbols))
;; (imp-feature-normalize '(:imp test) 'symbols)
;; (imp-feature-normalize '(:imp provide))
;; (imp-feature-normalize :imp 'provide)
;; (imp-feature-normalize :imp)
;; (imp-feature-normalize '(((:imp))) '((provide)))
;; (imp-feature-normalize :imp 'test?-xx:y::z '+!@$%^&*| 'symbols)


;;------------------------------------------------------------------------------
;; Feature Getters & Setters
;;------------------------------------------------------------------------------

(defun imp--feature-add (&rest feature)
  "Add the NORMALIZED features to the `imp-features' tree.

NORMALIZED must already be a normalized (by `imp-feature-normalize') symbol."
  ;; (imp--debug "imp--feature-add" "Adding to imp-features...")
  ;; (imp--debug "imp--feature-add" "  feature: %S" normalized)
  ;; (imp--debug "imp--feature-add" "imp-features before:\n%s"
  ;;                 (pp-to-string imp-features))

  (let ((symbols (imp-feature-split feature :symbols)))
    ;; Add normalized features to `imp-features' tree & set updated tree back
    ;; to `imp-features'.
    (imp--tree-update symbols nil imp-features)

    ;; (imp--debug "imp--feature-add" "imp-features after:\n%s"
    ;;             (pp-to-string imp-features))

    ;; Not sure what to return, but the updated features seems decent enough.
    imp-features))
;; (setq imp-features nil)
;; (imp--feature-add '(foo bar))
;; imp-features
;; (imp--feature-add '(imp or something here))
;; (imp--alist-get-value 'imp imp-features)
;; (imp--tree-contains? '(imp) imp-features)
;; (imp--tree-contains? '(imp or something) imp-features)


(defun imp--feature-get-tree (&rest feature)
  "Get full tree of features that contains FEATURE from `imp-features'."
  (imp--tree-key-exists? (apply #'imp-feature-first feature) imp-features))
;; (imp--feature-get-tree 'imp)


(defun imp--feature-delete (&rest feature)
  "Remove the NORMALIZED feature, and all subfeatures, from `imp-features'."
  (imp--tree-delete (imp-feature-split feature :symbols) imp-features))
;; (pp imp-features)
;; (imp--feature-delete 'foo/bar)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
