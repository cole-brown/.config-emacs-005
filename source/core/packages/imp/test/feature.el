;;; core/modules/emacs/imp/test/feature.el --- Tests for "feature.el" -*- no-byte-compile: t; lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-12-30
;; Timestamp:  2023-06-22
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Tests for "feature.el"
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Test Requirements
;;------------------------------------------------------------------------------

(test<imp>:init:load "base.el")

(test<imp>:init:load "../feature.el")


;;------------------------------------------------------------------------------
;; Test Debugging Helpers
;;------------------------------------------------------------------------------

;;------------------------------
;; `imp/features'
;;------------------------------

(defvar test<imp/feature>:features:backup nil
  "Backup `imp/features' so we can test it and then restore to its actual values.")


(defvar test<imp/feature>:features:test nil
  "Save `imp/features' after a test so we can check it for debugging if needed.")


;;------------------------------
;; `imp/features-at'
;;------------------------------

(defvar test<imp/feature/at>:features:created nil
  "Backup `imp/features' so we can test it and then restore to its actual values.")


(defvar test<imp/feature/at>:features:root nil
  "Path to temp dir for files for testing `imp/feature-at'.")


;;------------------------------
;; Feature Normalization
;;------------------------------

(defun test<imp/feature>:assert:seq-equal (func:equal sequence:test sequence:expected)
  "Assert that SEQUENCE:TEST and SEQUENCE:EXPECTED are equal.

Each element in SEQUENCE:TEST must equal (via FUNC:EQUAL) the element at
the same index in SEQUENCE:EXPECTED."
  ;;------------------------------
  ;; Verify inputs.
  ;;------------------------------
  (should func:equal)
  (should (listp sequence:expected))
  (should (listp sequence:test))

  ;;------------------------------
  ;; Assert nothing?
  ;;------------------------------
  (if (null sequence:expected)
      ;; Expected value is nothing so test result should be nothing.
      (should-not sequence:test)

    ;;------------------------------
    ;; Assert each element is equal.
    ;;------------------------------

    ;; Need the lists to be the same length.
    (should (= (length sequence:expected)
               (length sequence:test)))

    ;; And now each test value should match its expected value.
    (dotimes (i (length sequence:expected))
      (should (funcall func:equal
                       (nth i sequence:expected)
                       (nth i sequence:test)))))

  ;; `nil' return for "everything's good" confused me; return true instead.
  t)


;;------------------------------
;; Set-Up / Tear-Down: General
;;------------------------------

(defun test<imp/feature>:setup (_)
  "Backup `imp/features' and clear it out for 'feature.el' testing."
  (setq test<imp/feature>:features:backup      imp/features
        imp/features                           nil
        test<imp/feature>:features:test nil))


(defun test<imp/feature>:teardown (test-name)
  "Restore `imp/features'."
  ;; Save whatever testing did to `test<imp/feature>:features:test'
  (setq test<imp/feature>:features:test   imp/features
        ;; Restore `imp/features'.
        imp/features                      test<imp/feature>:features:backup
        test<imp/feature>:features:backup nil))


;;------------------------------
;; Test Helpers: `imp/feature-at'
;;------------------------------

(defun test<imp/feature/at>:delete (test-name path:root)
  "Delete PATH:ROOT directory and anything in/under it."
  (cond ((null path:root)
         (error "Expecting `path:root' to be a path string; got null!"))
        ((not (stringp path:root))
         (error "Expecting `path:root' to be a path string; got: %S"
                path:root))
        ((not (file-directory-p path:root))
         ;; Could be because we didn't get to the 'create dir' step?
         ;; (error (concat "Expecting `path:root' to be a "
         ;;                "path to an existing directory, but it is not! "
         ;;                "path: '%s'")
         ;;        path:root)
         :no-directory)
        ;; Directory exists so delete it and its files.
        (t
         ;; `delete-directory' just returns nil?... so, can't check anything more.
         (delete-directory path:root :recursive))))


(defun test<imp/feature/at>:create (test-name root features paths)
  "Create PATHS files for FEATURES at ROOT path."
  (let ((func/name "test<imp/feature/at>:create"))
    (test<imp>:should:marker test-name func/name)
    (imp--debug func/name
                    '("inputs:\n"
                      "  root:     %s\n"
                      "  features: %S\n"
                      "  paths:   %S\n"
                      "    -> path:final: %S")
                    root
                    features
                    paths
                    (car (last paths)))

    (let ((path:final (imp/path-join root (car (last paths)))))
      (test<imp>:should:marker:small (format "path:final: %s" path:final))
      (dolist (path paths)
        (let ((path:full (imp/path-join root path)))
          (test<imp>:should:marker:small (format "Create path '%s'..."
                                                 path:full))
          (imp--debug func/name
                          "Create path '%s'..."
                          path:full)
          ;; Create the file at path.
          (make-empty-file path:full :make-parents)

          (let ((buffer (get-buffer-create path:full)))
            ;; Add a var we can check for.
            (with-current-buffer buffer
              (test<imp>:should:marker:small (format "Write file var: '%s'..."
                                                     path:full))
              (insert (format "(setq test<imp/feature/at>::%s t)\n"
                              (imp/feature-normalize-imp->emacs features))))

            ;; Put our feature definition in the last file in the list?
            (when (string= path:full path:final)
              (with-current-buffer buffer
                (test<imp>:should:marker:small (format "Write `imp/provide': %s: '%s'..."
                                                       features
                                                       path:full))
                (insert (format "\n(imp/provide %s)\n"
                                (mapconcat (lambda (feature-symbol)
                                             ;; Quote symbols, leave keywords alone.
                                             (concat (if (keywordp feature-symbol)
                                                         ""
                                                       "'")
                                                     (symbol-name feature-symbol)))
                                           (imp--feature-normalize features)
                                           " ")))))

            ;; Finish up.
            (with-current-buffer buffer
              (write-file path:full))
            (kill-buffer buffer)))))))


(defun test<imp/feature/at>:init (test-name feature:base path:root &rest features:at)
  "Initialize a feature root and feature list for testing `imp/feature-at'.

FEATURE:BASE should be the base feature name (`:feature').

PATH:ROOT should be a path string.

FEATURES:AT should be an alist of entries:
  '(feature:list . 'feature:path)
   - `feature:list' should be:
     a) A feature keyword: `:feature'
     b) Or a list of the feature: '(:feature), '(:feature path), etc.
   - `feature:path' should be:
     a) A relative path string to a file to load for the feature.
     b) A list of such strings, for multiple files.

Will create test files for these features at the paths, with the last path's
file providing the feature name.

Returns the root path to FEATURE:BASE.

Example:
  (test<imp/feature/at>:init
    \"example-test-name\"
    :feature
    \"/path/to/imp/test/features/\"
    (list (list :feature
                \"init.el\")
          (list '(:feature path)
                \"path.el\")
          (list '(:feature multiple)
                \"common/common.el\"
                \"multiple/base.el\"
                \"multiple/subdir/init.el\"
                \"multiple/subdir/multiple.el\")))
    -> \"/path/to/imp/test/features/feature\""
  (let ((func/name "test<imp/feature/at>:init"))
    ;;------------------------------
    ;; Sanity Check Params.
    ;;------------------------------
    (test<imp>:should:marker test-name func/name)
    (imp--debug func/name
                    '("inputs:\n"
                      "  test-name:    %s\n"
                      "  path:root:    %s\n"
                      "  feature:base: %S\n"
                      "  features:at:\n"
                      "%s")
                    test-name
                    path:root
                    feature:base
                    (pp-to-string features:at))
    (should test-name)
    (should (stringp test-name))

    (should feature:base)
    (should (keywordp feature:base))

    (should path:root)
    (should (stringp path:root))
    ;; Delete previous test's data if test didn't clean up somehow.
    (when (file-directory-p path:root)
      (test<imp>:should:marker:small (format "Delete old temp test dir '%s'..." path:root))
      (test<imp/feature/at>:delete test-name path:root)
      (should-not (file-directory-p path:root)))

    (test<imp>:should:marker:small (format "Create temp test dir '%s'..." path:root))
    (make-directory path:root :parents)
    (should (file-directory-p path:root))

    (test<imp>:should:marker:small (format "Verify `features:at'...\n%s"
                                           (pp-to-string features:at)))
    (should features:at)
    (should (listp features:at))
    (should (seq-each (lambda (entry)
                        "Check each feature/path."
                        (let ((feature (car entry))
                              (paths   (cdr entry)))
                          ;; Feature should be base keyword or a list of keywords/symbols.
                          (should feature)
                          (should (or (keywordp feature)
                                      (and (listp feature)
                                           (seq-each #'symbolp feature))))
                          ;; Paths should be list of strings.
                          (should paths)
                          (should (or (stringp paths)
                                      (and (listp paths)
                                           (seq-each #'stringp paths))))))
                      features:at))

    ;;------------------------------
    ;; Create the files.
    ;;------------------------------
    (test<imp>:should:marker:small "Create tests files...")
    (dolist (entry features:at)
      (let ((features (car entry))
            (paths   (cdr entry)))
        (test<imp/feature/at>:create test-name
                                     path:root
                                     features
                                     paths)))

    ;;------------------------------
    ;; Register root.
    ;;------------------------------
    (test<imp>:should:marker:small (format "`(imp/path-root-set %S %s)'..."
                                           feature:base
                                           path:root))
    (setq test<imp/feature/at>:features:root path:root)
    (message "path:root:                          %s" path:root)
    (message "test<imp/feature/at>:features:root: %s" test<imp/feature/at>:features:root)

    (should (imp/path-root-set feature:base path:root))

    ;;------------------------------
    ;; Register with `imp/feature-at'.
    ;;------------------------------
    (test<imp>:should:marker:small (format "`imp/feature-at'..."
                                           feature:base
                                           path:root))
    (let ((features:at:created (imp/feature-at feature:base
                                               features:at)))
      (should features:at:created)
      ;; (pp features:at:created)

      ;; Save created features.
      (setq test<imp/feature/at>:features:created features:at:created))))


;;------------------------------
;; Set-Up / Tear-Down: `imp/feature-at'
;;------------------------------

(defun test<imp/feature/at>:setup (name feature:base path:root features:at)
  "Run `test<imp/feature>:setup' and make `imp/feature-at' test dir & files."
  ;; First: Normal set-up.
  (test<imp/feature>:setup name)

  ;; Next: `imp/feature-at' set-up.
  ;; 1) Make temp dir & files.
  ;; 2) Initialize `feature:base':
  ;;    a) `imp/path-root'
  ;;    b) `imp/feature-at'
  (apply #'test<imp/feature/at>:init name feature:base path:root features:at))


(defun test<imp/feature/at>:teardown (name)
  "Delete the `imp/feature-at' test dir then run `test<imp/feature>:teardown'."
  ;; Clean up our temp data.
  (when test<imp/feature/at>:features:root
    (test<imp/feature/at>:delete name
                                 test<imp/feature/at>:features:root)
    (setq test<imp/feature/at>:features:root nil))

  ;; Leave `test<imp/feature/at>:features:created' alive so it can be looked
  ;; at if desired.

  ;; And do our suite's teardown as well.
  (test<imp/feature>:teardown name))


;; ╔═════════════════════════════╤═══════════╤═════════════════════════════════╗
;; ╟─────────────────────────────┤ ERT TESTS ├─────────────────────────────────╢
;; ╠════════════════════════╤════╧═══════════╧════╤════════════════════════════╣
;; ╟────────────────────────┤ Imps are people too!├────────────────────────────╢
;; ╚════════════════════════╧═════════════════════╧════════════════════════════╝


;;------------------------------------------------------------------------------
;; Tests: Feature Helpers
;;------------------------------------------------------------------------------

;;------------------------------
;; imp/feature-exists?
;;------------------------------

(ert-deftest test<imp/feature>::imp/feature-exists? ()
  "Test that `imp/feature-exists?' behaves appropriately."
  (test<imp>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<imp/feature>::imp/feature-exists?"
      #'test<imp/feature>:setup
      #'test<imp/feature>:teardown

    ;;===
    ;; Run the test.
    ;;===

    ;;------------------------------
    ;; Need to have some features available to test for.
    ;;------------------------------

    (should (imp--feature-add '(:test exists here)))
    (should (imp--feature-add '(:test exists too)))
    (should (imp--feature-add '(:test foo bar baz)))
    (should (imp--feature-add '(:test foo qux quux)))
    (should (imp--feature-add '(:test foo qux qox qix qex qax)))

    ;;------------------------------
    ;; Exists?
    ;;------------------------------

    (should (imp/feature-exists? '(:test)))
    (should (imp/feature-exists? '(:test exists)))
    (should (imp/feature-exists? '(:test exists here)))
    (should (imp/feature-exists? '(:test exists too)))

    (should (imp/feature-exists? '(:test foo)))
    (should (imp/feature-exists? '(:test foo bar)))
    (should (imp/feature-exists? '(:test foo bar baz)))
    (should (imp/feature-exists? '(:test foo qux)))
    (should (imp/feature-exists? '(:test foo qux quux)))
    (should (imp/feature-exists? '(:test foo qux qox)))
    (should (imp/feature-exists? '(:test foo qux qox qix)))
    (should (imp/feature-exists? '(:test foo qux qox qix qex)))
    (should (imp/feature-exists? '(:test foo qux qox qix qex qax)))

    ;;------------------------------
    ;; Doesn't Exist?
    ;;------------------------------
    (should-not (imp/feature-exists? '(:imp)))
    (should-not (imp/feature-exists? '(:jeff)))
    (should-not (imp/feature-exists? '(:test foo baz)))
    (should-not (imp/feature-exists? '(:test qux)))))


(ert-deftest test<imp/feature>::imp/feature-exists?::regression:no-features-error ()
  "Test that `imp/feature-exists?' behaves appropriately when `imp/features' is nil.

Bug was that it `imp--tree-contains?' would raise an error when trying to
look for the features chain if `imp/features' was nil."
  (test<imp>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<imp/feature>::imp/feature-exists?"
      #'test<imp/feature>:setup
      #'test<imp/feature>:teardown

    ;;===
    ;; Run the test.
    ;;===

    ;; Should just get `nil' when we have no features at all.
    (should-not imp/features)
    (should-not (imp/feature-exists? '(:test)))))



;;------------------------------------------------------------------------------
;; Tests: Normalization
;;------------------------------------------------------------------------------

;;------------------------------
;; imp--feature-name-normalize
;;------------------------------

(ert-deftest test<imp/feature>::imp--feature-name-normalize ()
  "Test that `imp--feature-name-normalize' correctly normalizes to a string."
  (test<imp>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<imp/feature>::imp--feature-name-normalize"
      #'test<imp/feature>:setup
      #'test<imp/feature>:teardown

    ;;===
    ;; Run the test.
    ;;===

    ;; Test stringify.
    (should (string= (imp--feature-name-normalize "foo")
                     "foo"))
    (should (string= (imp--feature-name-normalize :foo)
                     "foo"))
    (should (string= (imp--feature-name-normalize 'foo)
                     (imp--feature-name-normalize :foo)))

    (should (string= (imp--feature-name-normalize "foo-bar")
                     "foo-bar"))
    (should (string= (imp--feature-name-normalize :foo-bar)
                     "foo-bar"))
    (should (string= (imp--feature-name-normalize 'foo-bar)
                     (imp--feature-name-normalize :foo-bar)))

    ;; Also, it should replace some things according to
    ;; `imp--feature-replace-rx'.
    (should (string= (imp--feature-name-normalize "foo:bar")
                     "foobar"))
    (should (string= (imp--feature-name-normalize :foo:bar)
                     "foobar"))
    (should (string= (imp--feature-name-normalize 'foo:bar)
                     (imp--feature-name-normalize :foo:bar)))

    (should (string= (imp--feature-name-normalize "+foo:bar")
                     "foobar"))
    (should (string= (imp--feature-name-normalize :+foo:bar)
                     "foobar"))
    (should (string= (imp--feature-name-normalize '+foo+bar)
                     (imp--feature-name-normalize :+foo:bar)))))


;;------------------------------
;; imp--feature-normalize-string
;;------------------------------

(ert-deftest test<imp/feature>::imp--feature-normalize-string ()
  "Test that `imp--feature-normalize-string' correctly normalizes inputs
to a list of string."
  (test<imp>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<imp/feature>::imp--feature-normalize-string"
      #'test<imp/feature>:setup
      #'test<imp/feature>:teardown

    ;;===
    ;; Run the test.
    ;;===

    ;; NOTE: `imp--feature-normalize-string' returns the list in reverse order.

    ;; Test stringify.
    (test<imp/feature>:assert:seq-equal #'string=
                                        (imp--feature-normalize-string "foo")
                                        '("foo"))
    (test<imp/feature>:assert:seq-equal #'string=
                                        (imp--feature-normalize-string :foo)
                                        '("foo"))
    (test<imp/feature>:assert:seq-equal #'string=
                                        (imp--feature-normalize-string 'foo)
                                        (imp--feature-normalize-string :foo))

    (test<imp/feature>:assert:seq-equal #'string=
                                        (imp--feature-normalize-string "foo" "bar")
                                        ;; Expect reverse order.
                                        '("bar" "foo"))
    (test<imp/feature>:assert:seq-equal #'string=
                                        (imp--feature-normalize-string :foo-bar "baz")
                                        '("baz" "foo-bar"))
    (test<imp/feature>:assert:seq-equal #'string=
                                        ;; Expect it to flatten the inputs into one output list.
                                        (imp--feature-normalize-string :foo-bar "baz" '(qux ((quux))))
                                        '("quux" "qux" "baz" "foo-bar"))

    ;; Also, it should replace some things according to
    ;; `imp--feature-replace-rx'.
    (test<imp/feature>:assert:seq-equal #'string=
                                        (imp--feature-normalize-string "foo" "b:a:r")
                                        ;; Expect reverse order.
                                        '("bar" "foo"))
    (test<imp/feature>:assert:seq-equal #'string=
                                        (imp--feature-normalize-string :foo-bar "+baz")
                                        '("baz" "foo-bar"))
    (test<imp/feature>:assert:seq-equal #'string=
                                        ;; Expect it to flatten the inputs into one output list.
                                        (imp--feature-normalize-string :foo-bar "baz" '(:qux ((qu+ux))))
                                        '("quux" "qux" "baz" "foo-bar"))))


;;------------------------------
;; imp--feature-normalize
;;------------------------------

(ert-deftest test<imp/feature>::imp--feature-normalize ()
  "Test that `imp--feature-normalize' behaves appropriately."
  (test<imp>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<imp/feature>::imp--feature-normalize"
      #'test<imp/feature>:setup
      #'test<imp/feature>:teardown

    ;;===
    ;; Run the test.
    ;;===
    ;; Only difference between `imp--feature-normalize' and `imp/feature-normalize':
    ;; Always get a list back, even for single feature.

    (should (equal '(:imp)
                   (imp--feature-normalize :imp)))

    (should (equal '(:imp test symbols)
                   (imp--feature-normalize :imp 'test 'symbols)))

    (should (equal '(:imp provide)
                   (imp--feature-normalize :imp 'provide)))

    ;; First item in returned list should be a keyword; rest should be symbols.
    (should (equal '(:imp strings are stringy)
                   (imp--feature-normalize "imp" "strings" :are '+stringy)))))


;;------------------------------
;; imp/feature-normalize-imp->emacs
;;------------------------------

(ert-deftest test<imp/feature>::imp/feature-normalize-imp->emacs ()
  "Test that `imp/feature-normalize-imp->emacs' behaves appropriately."
  (test<imp>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<imp/feature>::imp/feature-normalize-imp->emacs"
      #'test<imp/feature>:setup
      #'test<imp/feature>:teardown

    ;;===
    ;; Run the test.
    ;;===
    (should (equal 'imp/test-symbols
                   (imp/feature-normalize-imp->emacs '(:imp test symbols))))

    (should (equal 'imp/test-symbols
                   (imp/feature-normalize-imp->emacs '(:imp test) 'symbols)))

    (should (equal 'imp/provide
                   (imp/feature-normalize-imp->emacs '(:imp provide))))

    (should (equal (imp/feature-normalize-imp->emacs '(:imp provide))
                   (imp/feature-normalize-imp->emacs :imp 'provide)))

    (should (equal (imp/feature-normalize-imp->emacs '(:imp provide))
                   (imp/feature-normalize-imp->emacs '(((:imp))) '((provide)))))

    (should (equal 'imp/strings
                   (imp/feature-normalize-imp->emacs '("imp" "strings"))))))


;;------------------------------
;; imp/feature-normalize
;;------------------------------

(ert-deftest test<imp/feature>::imp/feature-normalize ()
  "Test that `imp/feature-normalize' behaves appropriately."
  (test<imp>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<imp/feature>::imp/feature-normalize"
      #'test<imp/feature>:setup
      #'test<imp/feature>:teardown

    ;;===
    ;; Run the test.
    ;;===
    ;; Main difference between `imp--feature-normalize' and `imp/feature-normalize':
    ;; Get a single symbol back if gave a single feature keyword/symbol.

    (should (equal :imp
                   (imp/feature-normalize :imp)))

    (should (equal '(:imp test symbols)
                   (imp/feature-normalize :imp 'test 'symbols)))

    (should (equal '(:imp provide)
                   (imp/feature-normalize :imp 'provide)))

    ;; First item in returned list should be a keyword; rest should be symbols.
    (should (equal '(:imp strings are stringy)
                   (imp/feature-normalize "imp" "strings" :are '+stringy)))))


;;------------------------------
;; imp--feature-add
;;------------------------------

(ert-deftest test<imp/feature>::imp--feature-add ()
  "Test that `imp--feature-add' behaves appropriately."
  (test<imp>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<imp/feature>::imp--feature-add"
      #'test<imp/feature>:setup
      #'test<imp/feature>:teardown

    ;;===
    ;; Run the test.
    ;;===
    (let (features)
      ;; We should have nothing right now.
      (should-not imp/features)
      (should (equal features imp/features))

      ;; Add a feature.
      (setq features '((:imp (test (symbols)))))
      (should (equal features
                     (imp--feature-add '(:imp test symbols))))
      (should (equal features imp/features))

      ;; Another feature.
      (setq features '((:imp (provide) (test (symbols)))))
      (should (equal features
                     (imp--feature-add '(:imp provide))))
      (should (equal features imp/features))

      ;; And another?
      ;; Errors because features should be normalized before calling `imp--feature-add'.
      (should-error (imp--feature-add '("imp" "strings"))))))


;;------------------------------
;; imp/feature-assert
;;------------------------------

(ert-deftest test<imp/feature>::imp/feature-assert ()
  "Test that `imp/feature-assert' behaves appropriately."
  (test<imp>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<imp/feature>::imp/feature-assert"
      #'test<imp/feature>:setup
      #'test<imp/feature>:teardown

    ;;===
    ;; Run the test.
    ;;===

    ;;------------------------------
    ;; Need to have some features available to test for.
    ;;------------------------------
    (should (imp--feature-add '(:test exists here)))
    (should (imp--feature-add '(:test exists too)))
    (should (imp--feature-add '(:test foo bar baz)))
    (should (imp--feature-add '(:test foo qux quux)))
    (should (imp--feature-add '(:test foo qux qox qix qex qax)))

    ;;------------------------------
    ;; Ok if a feature.
    ;;------------------------------
    (should (imp/feature-assert :test))
    (should (imp/feature-assert :test 'exists))
    (should (imp/feature-assert :test 'exists 'here))
    (should (imp/feature-assert :test 'exists 'too))

    (should (imp/feature-assert :test 'foo))
    (should (imp/feature-assert :test 'foo 'bar))
    (should (imp/feature-assert :test 'foo 'bar 'baz))

    (should (imp/feature-assert :test 'foo 'qux))
    (should (imp/feature-assert :test 'foo 'qux 'quux))
    (should (imp/feature-assert :test 'foo 'qux 'qox))
    (should (imp/feature-assert :test 'foo 'qux 'qox 'qix))
    (should (imp/feature-assert :test 'foo 'qux 'qox 'qix 'qex))
    (should (imp/feature-assert :test 'foo 'qux 'qox 'qix 'qex 'qax))

    ;;------------------------------
    ;; Error if not a feature.
    ;;------------------------------
    (should-error (imp/feature-assert :imp))
    (should-error (imp/feature-assert :jeff))
    (should-error (imp/feature-assert :test 'foo 'baz))
    (should-error (imp/feature-assert :test 'qux))))


;;------------------------------
;; imp/feature-at
;;------------------------------

(ert-deftest test<imp/feature>::imp/feature-at--features-register ()
  "Test that `imp/feature-at' can find a load path for a feature registered with it."
  (let ((path:root (imp/path-join test<imp>:path:root:loading
                                  "features")))
    (test<imp>:fixture
        ;;===
        ;; Test name, setup & teardown func.
        ;;===
        "test<imp/feature>::imp/feature-at--features-register"
        ;; Normal setup (we'll do `imp/features-at' set-up ourself).
        #'test<imp/feature>:setup
        ;; Tear down the `imp/features-at' set-up we did.
        #'test<imp/feature/at>:teardown

      ;;===
      ;; Run the test.
      ;;===

      (let ((feature:base :feature)
            (features:at (list (list :feature
                                     "init.el")
                               (list '(:feature path)
                                     "path.el")
                               (list '(:feature multiple)
                                     "common/common.el"
                                     "multiple/base.el"
                                     "multiple/subdir/init.el"
                                     "multiple/subdir/final.el")))
            (features:expected '(((:feature multiple)
                                  "common/common" "multiple/base" "multiple/subdir/init" "multiple/subdir/final")
                                 ((:feature path)
                                  "path")
                                 ((:feature)
                                  "init")))
            features:created)

        ;;------------------------------
        ;; Initialize `imp/features-at'.
        ;;------------------------------
        ;; Create files for testing load, and add FEATURE:BASE/FEATURES:AT to `imp/features-locate'.
        (setq features:created (apply #'test<imp/feature/at>:init
                                      test-name
                                      feature:base
                                      path:root
                                      features:at))
        (test<imp>:should:marker test-name "features:created")
        (should features:created)

        ;; Verify `imp/feature-at' entry created correctly.
        (dolist (expected features:expected)
          (test<imp>:should:marker:small (format "expected: %S" expected))
          (let* ((feature:expect (car expected))
                 (path:expect (cdr expected))
                 ;; Default `eq' isn't sufficient for comparing lists of keywords/symbol.
                 (path:created (alist-get feature:expect features:created nil nil #'equal)))
            (should feature:expect)
            (should path:expect)
            ;; Getting something for `path:created' means `feature:expect' exists in `features:created'.
            (should path:created)
            ;; Check paths are correct.
            (dolist (path path:expect)
              (should path)
              (should (stringp path))
              (should (seq-contains-p path:created path #'string=)))))))))


;;------------------------------
;; imp--feature-paths
;;------------------------------

(ert-deftest test<imp/feature>::imp--feature-paths ()
  "Test that `imp--feature-paths' creates paths correctly."
  (let ((path:root (imp/path-join test<imp>:path:root:loading
                                  "features"))
        (feature:base :feature)
        (features:at (list (list :feature
                                 "init.el")
                           (list '(:feature path)
                                 "path.el")
                           (list '(:feature multiple)
                                 "common/common.el"
                                 "multiple/base.el"
                                 "multiple/subdir/init.el"
                                 "multiple/subdir/final.el")))
        (features:expected '(((:feature multiple)
                              "common/common" "multiple/base" "multiple/subdir/init" "multiple/subdir/final")
                             ((:feature path)
                              "path")
                             ((:feature)
                              "init")))
        features:created)

    (test<imp>:fixture
        ;;===
        ;; Test name, setup & teardown func.
        ;;===
        "test<imp/feature>::imp--feature-paths"
        ;; Set-up with our variables.
        (lambda (name)
          "Set-up for testing `imp/feature-at' paths."
          (test<imp/feature/at>:setup name
                                      feature:base
                                      path:root
                                      features:at))
        #'test<imp/feature/at>:teardown

      ;;===
      ;; Run the test.
      ;;===

      ;;------------------------------
      ;; Requirements:
      ;;   1. Must be a feature.
      ;;   2. Must have a root in `imp/path-roots'.
      ;;   3. Must have an entry in `imp/features-locate'.
      ;;------------------------------
      ;;
      ;; 1. Features
      (dolist (entry features:expected)
        (let ((feature:expected (car entry)))
          (should (imp--feature-add feature:expected))))

      ;; 2. Roots
      ;;    - Expect it to be provided by set-up.
      ;;
      ;; 3. Locate
      ;;    - Expect it to be provided by set-up.

      ;;------------------------------
      ;; Test!
      ;;------------------------------

      ;; Check our expected features all have paths.
      (dolist (entry features:expected)
        (let* ((feature:expected (car entry))
               (paths:expected (cdr entry))
               (result (imp--feature-paths feature:expected)))
          ;; Should have a list as a result.
          (should result)
          (should (listp result))

          ;; List should be:
          (let ((root (car result))
                (paths (cdr result)))
            ;; A root path string.
            (should root)
            (should (stringp root))
            ;; Each root should be our `path:root'.
            (should (string= path:root root))

            ;; A list of sub-path strings.
            (should paths)
            (should (listp paths))
            ;; Sub-path strings should all be in our expected path strings.
            (dolist (path paths)
              (should path)
              (should (stringp path))
              (should (seq-contains-p paths:expected path #'string=)))))))))
