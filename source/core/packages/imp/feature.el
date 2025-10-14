;;; imp/feature.el --- imp feature tree, name normalization, etc -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2020-10-28
;; Timestamp:  2025-10-13
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

(defun imp-feature-exists? (features)
  "Check for list of FEATURES in the `imp-features' tree."
  ;; When not `imp-features', always return `nil'.
  (when imp-features
    (not (null (imp--tree-contains? (imp--feature-normalize-chain features)
                                    imp-features)))))
;; (imp-feature-exists? '(:imp))
;; (imp-feature-exists? '(:imp provide))
;; (imp-feature-exists? '(:imp (provide)))


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
   (list (rx-to-string '(any whitespace) :no-group)
         "")

   ;; Features use colon to denote the feature root (a feature with an entry
   ;; in `imp-path-roots'). eg `imp:/feature' is this file.
   '(":" "")

   ;; TODO: disallow or allow `imp--feature-replace-separator'?

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


(defconst imp--feature-replace-separator
  "/"
  "Used for 'imp' -> Emacs feature normalization.

String to use in between symbols when translating an imp symbol chain to
an Emacs symbol.")


(defun imp--feature-normalize-link (name)
  "Normalize NAME to a string.

Uses `imp--feature-replace-rx' to replace invalid characters in the NAME
string or symbol name.

Return a string."

  (let ((funcname 'imp--feature-normalize-link)
        (name (string-trim name))

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

                (cond ((null name)
                       (imp--error funcname
                                   "Cannot convert nil to string: '%s'"
                                   (type-of name)
                                   name))
                      ((stringp name)
                       name)
                      ((symbolp name)
                       (symbol-name name))
                      (t
                       (imp--error funcname
                                   "Cannot convert %s to string: '%s'"
                                   (type-of name)
                                   name))))))
;; (imp--feature-normalize-link nil)
;; (imp--feature-normalize-link "!@$%^&*|")
;; (imp--feature-normalize-link "foo")
;; (imp--feature-normalize-link :foo)
;; (imp--feature-normalize-link 'test?-xx:y::z)
;; (imp--feature-normalize-link "+!@$%^&*|")


(defun imp--feature-normalize-chain (&rest chain)
  "Normalize CHAIN to a list of normalized strings."
  (let ((funcname 'imp--feature-normalize-chain)
        (normalized (seq-map #'imp--feature-normalize-link (imp--list-flatten chain))))
    (cond ((seq-some #'imp--string-empty? normalized)
           (imp--error funcname
                       "No normalized strings produced from CHAIN: %S"
                       normalized))
          ((null normalized)
           (imp--error funcname
                    "Nothing normalized from CHAIN: %S"
                    chain))
          (t
           normalized))))
;; (imp--feature-normalize-chain "+spydez" "foo" "bar")
;; (imp--feature-normalize-chain "+spydez/foo/bar")
;; (imp--feature-normalize-chain '((nil)))
;; (imp--feature-normalize-chain 'test?-xx:y::z)
;; (imp--feature-normalize-chain '+!@$%^&*|)
;; (imp--feature-normalize-chain :imp 'test?-xx:y::z "+!@$%^&*|" 'symbols)


(defun imp-feature-join (&rest feature)
  (mapconcat #'identity
             (seq-filter (lambda (x) (not (imp--string-empty? x)))
                         (imp--feature-normalize-chain feature))
             imp--feature-replace-separator))
;; (imp-feature-join "+spydez/foo/bar")
;; (imp-feature-join "+spydez" "foo" "bar")
;; (imp-feature-join '((nil)))
;; (imp-feature-join 'test?-xx:y::z)
;; (imp-feature-join '+!@$%^&*|)
;; (imp-feature-join :imp 'test?-xx:y::z "+!@$%^&*|" 'symbols)


(defun imp-feature-split (&rest feature)
  "`:foo/bar/baz' -> '(\":foo\" \"bar\" \"baz\")"
  (string-split (apply #'imp-feature-join feature)
                imp--feature-replace-separator))
;; (imp-feature-split nil)
;; (imp-feature-split '+layout/spydez)
;; (imp-feature-split :foo/bar/baz)
;; (imp-feature-split (imp-feature-join "+spydez" "foo" "bar"))


(defun imp-feature-root (&rest feature)
  "Return root of FEATURE, if any. Else nil."
  (when feature
    (let ((root (car-safe (imp-feature-split (apply #'imp--feature-normalize-chain feature)))))
      (when (string-prefix-p ":" root)
        root))))
;; (imp-feature-root '+layout/spydez)
;; (imp-feature-root :spydez)
;; (imp-feature-root "spydez")
;; (imp-feature-root ":spydez")
;; (imp-feature-root ":spydez" "foo" "bar")
;; (imp-feature-root "spydez" "foo" "bar")
;; (imp-feature-root '("+spydez" "foo" "bar"))





;; TODO-HERE: Which is a rooted feature?
;; :root/first/second
;; root:/first/second





(defun imp-feature-normalize (&rest feature)
  "Translate the feature to a single symbol appropriate for Emacs' `provide'.

FEATURE should be:
  1) A keyword/symbol,
  2) or a list of keywords/symbols.

FEATURE will be normalized, then converted into a single symbol."
  (intern
   (mapconcat #'identity
              (seq-filter (lambda (x) (not (imp--string-empty? x)))
                          (imp--feature-normalize-chain feature))
              imp--feature-replace-separator)))
;; (imp-feature-normalize :imp 'test 'symbols)
;; (imp-feature-normalize '(:imp test symbols))
;; (imp-feature-normalize '(:imp test) 'symbols)
;; (imp-feature-normalize '(:imp provide))
;; (imp-feature-normalize :imp 'provide)
;; (imp-feature-normalize :imp)
;; (imp-feature-normalize '(((:imp))) '((provide)))
;; (imp-feature-normalize :imp 'test?-xx:y::z '+!@$%^&*| 'symbols)


;;------------------------------------------------------------------------------
;; Feature Getters & Setters
;;------------------------------------------------------------------------------

(defun imp--feature-add (normalized)
  "Add the NORMALIZED features to the `imp-features' tree.

NORMALIZED must already be a normalized (by `imp--feature-normalize-for-imp')
list of keywords/symbols."
  (imp--debug "imp--feature-add" "Adding to imp-features...")
  (imp--debug "imp--feature-add" "  feature: %S" normalized)
  ;; (imp--debug "imp--feature-add" "imp-features before:\n%s"
  ;;                 (pp-to-string imp-features))

  ;; Add normalized features to `imp-features' tree & set updated tree back
  ;; to `imp-features'.
  (imp--tree-update normalized nil imp-features)

  (imp--debug "imp--feature-add" "imp-features after:\n%s"
              (pp-to-string imp-features))
  ;; Not sure what to return, but the updated features seems decent enough.
  imp-features)
;; (setq imp-features nil)
;; (imp--feature-add '(:imp test))
;; imp-features
;; (imp--feature-add '(:imp or something here))
;; (imp--alist-get-value :imp imp-features)
;; (imp--tree-contains? '(:imp) imp-features)
;; (imp--tree-contains? '(:imp or something) imp-features)


(defun imp--feature-get-tree (normalized)
  "Get tree of NORMALIZED features from `imp-features'."
  (imp--tree-contains? normalized imp-features))


(defun imp--feature-delete (normalized)
  "Remove the NORMALIZED feature, and all subfeatures, from `imp-features'."
  (imp--tree-delete normalized imp-features))


;;------------------------------------------------------------------------------
;; Features & Paths to Them
;;------------------------------------------------------------------------------

;; TODO-LOAD: DELETE THIS WHEN imp-parser becomes imp-load.
(defun imp--feature-paths (&rest feature)
  "Find (relative) path(s) to files for FEATURE-ROOT + FEATURE.

This will only provide the paths for the feature itself, each of which may
`imp-require' more features.

Return list of: '(path-root . (paths-relative))

Error if:
  - No root path for FEATURE root.
  - No paths found for input parameters."
  (let* ((func-name "imp--feature-paths")
         (features-normal (imp--feature-normalize-chain feature)))

    ;;------------------------------
    ;; Error Check Inputs
    ;;------------------------------
    ;; FEATURE-ROOT must:
    ;; 1) Be an imp feature.
    ;; (imp-feature-assert feature-root)
    ;;   ...must it be a feature? Not so sure. All I /think/ we need is the entry in `imp-path-roots'.

    ;; 2) Have registered a root path.
    (unless (imp--path-root-contains? (car features-normal))
      (imp--error func-name
                  "Feature `%S' does not have a root path in imp."
                  (car features-normal)))

    ;;------------------------------
    ;; Get the paths and load them?
    ;;------------------------------
    (let* ((path-root (imp--path-root-dir (car features-normal)))
           (feature-locations (imp--feature-locations (car features-normal)))
           (paths (imp--alist-get-value features-normal
                                        feature-locations
                                        imp--features-locate-equal)))
      (imp--debug func-name
                  '("Get feature paths:\n"
                    "  - feature-root: %S\n"
                    "  - path-root:    %s\n"
                    "  - feature-locations: %S\n"
                    "  - paths: %S")
                  (car features-normal)
                  path-root
                  feature-locations
                  paths)

      ;;---
      ;; Error Checks
      ;;---
      (unless feature-locations
        (imp--error func-name
                    "No feature locations found for: %S"
                    (car features-normal)))

      (unless paths
        (imp--error func-name
                    "No feature paths found for: %S"
                    features-normal))

      ;;---
      ;; Done; return.
      ;;---
      (imp--debug func-name
                  '("Return feature paths for `%S':\n"
                    "  - path-root:    %s\n"
                    "  - paths: %S")
                  (car features-normal)
                  path-root
                  paths)
      (cons path-root paths))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
