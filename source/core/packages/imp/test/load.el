;;; core/modules/emacs/imp/test/load.el --- Tests for "load.el" -*- no-byte-compile: t; lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-01-05
;; Timestamp:  2023-06-22
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Tests for "load.el"
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Test Requirements
;;------------------------------------------------------------------------------

(test<imp>:init:load "base.el")

(test<imp>:init:load "../feature.el")
(test<imp>:init:load "../alist.el")
(test<imp>:init:load "../tree.el")
(test<imp>:init:load "../path.el")
(test<imp>:init:load "../+timing.el")
(test<imp>:init:load "../provide")
(test<imp>:init:load "../load.el")


;; ╔═════════════════════════════╤═══════════╤═════════════════════════════════╗
;; ╟─────────────────────────────┤ ERT TESTS ├─────────────────────────────────╢
;; ╠══════════╤══════════════════╧═══════════╧══════════════════╤══════════════╣
;; ╟──────────┤ Because Emacs needs to give Alists some love... ├──────────────╢
;; ╚══════════╧═════════════════════════════════════════════════╧══════════════╝


;;------------------------------------------------------------------------------
;; Tests: Imp Load Functions
;;------------------------------------------------------------------------------

;;------------------------------
;; imp--load-file
;;------------------------------
;; Is just a wrapper around `load'; testing other `:imp/load' functions will test
;; this fine (until we encounter a bug in this function, I guess).


;;------------------------------
;; imp--load-paths
;;------------------------------

(ert-deftest test<imp/load>::imp--load-paths ()
  "Test that `imp--load-paths' behaves appropriately."
  (test<imp>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<imp/load>::imp--load-paths"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    (should-error test<imp>:file:loading?)
    (should-error test<imp>:loading:load:loaded)
    (should-error test<imp>:loading:dont-load:loaded)

    ;;------------------------------
    ;; Load feature & root.
    ;;------------------------------

    ;;---
    ;; +Supply a root:+
    ;;---
    ;; `imp--load-path' doesn't care about features or `imp/path-roots';
    ;; everything is supplied in the params.

    ;;---
    ;; Load a feature:
    ;;---
    ;; Unlike `imp--load-feature', `imp--load-paths' does not care about
    ;; provided features and will load regardless, and we'll test for that.
    (imp/provide test<imp>:feature:loading:dont-load)

    ;;---
    ;; Set up variables:
    ;;---
    ;; First, set these variable to `nil' so they exist.
    (setq test<imp>:loading:load:loaded      nil  ;; If 'test/loading/load.el' is loaded, it will be set to `t'.
          test<imp>:loading:dont-load:loaded nil) ;; If 'test/loading/dont-load.el' is loaded, it will be set to `t'.

    ;;------------------------------
    ;; Load:
    ;;------------------------------

    ;;---
    ;; If feature is alredy provided, I don't care - load it again.
    ;;---
    (should-not test<imp>:loading:dont-load:loaded)
    (should (file-exists-p (imp/path-join test<imp>:path:root:loading
                                          (concat test<imp>:file:loading:dont-load ".el"))))
    ;; Call `imp--load-paths on it's feature; should now be be loaded.
    (should (imp--load-paths test<imp>:feature:loading:dont-load
                                 test<imp>:path:root:loading
                                 (list test<imp>:file:loading:dont-load)))
    (should test<imp>:loading:dont-load:loaded)

    ;;---
    ;; If feature is not provided, also don't care - load it.
    ;;---
    (should-not test<imp>:loading:load:loaded)
    (should (imp--load-paths test<imp>:feature:loading:load
                                 test<imp>:path:root:loading
                                 (list test<imp>:file:loading:load)))
    (should test<imp>:loading:load:loaded)

    ;;------------------------------
    ;; Errors:
    ;;------------------------------

    ;;---
    ;; Can't find the file to load.
    ;;---
    (should-error test<imp>:loading:load:doesnt-exist)
    (should-not (file-exists-p (imp/path-join test<imp>:path:root:loading
                                              test<imp>:file:loading:doesnt-exist)))
    (should-error (imp--load-paths test<imp>:feature:loading:load
                                       test<imp>:path:root:loading
                                       (list test<imp>:file:loading:doesnt-exist)))
    (should-error test<imp>:loading:load:doesnt-exist)))


;;------------------------------
;; imp--load-feature
;;------------------------------

(ert-deftest test<imp/load>::imp--load-feature ()
  "Test that `imp--load-feature' behaves appropriately."
  (test<imp>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<imp/load>::imp--load-feature"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    (should-error test<imp>:file:loading?)
    (should-error test<imp>:loading:load:loaded)
    (should-error test<imp>:loading:dont-load:loaded)

    ;;------------------------------
    ;; Load feature & root.
    ;;------------------------------

    ;;---
    ;; Supply a root:
    ;;---
    ;; For testing that it can load something it knows about but that hasn't been loaded yet.
    (test<imp>:setup/root:loading)

    ;;---
    ;; Supply feature paths:
    ;;---
    (should-not imp/features-locate)
    (should (imp--load-file (imp/path-join test<imp>:path:root:loading
                                               imp/path-filename-features)))
    (should imp/features-locate)

    ;;---
    ;; Load a feature:
    ;;---
    ;; For testing that nothing happens when it's already loaded.
    (should imp/features)
    (should test<imp>:loading:features:loaded)
    ;; Clear out loaded flag so we can check.
    (setq test<imp>:loading:features:loaded nil)
    (should (imp/provided? test<imp>:feature:loading:features))
    ;; Should return `t' but not actually load the file.
    (should (imp--load-feature test<imp>:feature:loading:features))
    (should-not test<imp>:loading:features:loaded)

    ;;---
    ;; Set up variables:
    ;;---
    ;; First, set these variable to `nil' so they exist.
    (setq test<imp>:loading:load:loaded      nil  ;; If 'test/loading/load.el' is loaded, it will be set to `t'.
          test<imp>:loading:dont-load:loaded nil) ;; If 'test/loading/dont-load.el' is loaded, it will be set to `t'.

    ;;------------------------------
    ;; Load:
    ;;------------------------------

    ;;---
    ;; If feature is alredy loaded, nothing should happen.
    ;;---
    ;; Pretend we've loaded it.
    (imp/provide test<imp>:feature:loading:dont-load)
    (should-not test<imp>:loading:dont-load:loaded)
    ;; Call `imp--load-feature on it's feature; shouldn't be loaded since we've "loaded" it already.
    (should (imp--load-feature test<imp>:feature:loading:dont-load))
    (should-not test<imp>:loading:dont-load:loaded)

    ;;---
    ;; If we know the base feature, we should be able to load the file by the feature name.
    ;;---
    (should-not test<imp>:loading:load:loaded)
    (should (imp--load-feature test<imp>:feature:loading:load))
    (should test<imp>:loading:load:loaded)
    (should test<imp>:file:loading?)

    ;;------------------------------
    ;; Errors:
    ;;------------------------------

    ;;---
    ;; Know the base feature, but can't find anything to load.
    ;;---
    (should-error test<imp>:loading:load:doesnt-exist)
    (should-error (imp--load-feature test<imp>:feature:loading:doesnt-exist))
    (should-error test<imp>:loading:load:doesnt-exist)

    ;;---
    ;; Don't know the base feature.
    ;;---
    ;; We fallback to asking Emacs to `load' it, but it doesn't know anything about this either.
    ;; This will error since imp knows absolutely nothing about it.
    (should-error (imp--load-feature 'something-that-doesnt-exist-in-imp-or-emacs))

    ;; imp knows the base feature, at least, but we have no feature paths, so this also errors.
    (should-error (imp--load-feature (list test<imp>:feature:loading :unknown-feature)))))


(ert-deftest test<imp/load>::imp--load-feature--find-features ()
  "Test that `imp--load-feature' behaves appropriately and can
find/load 'imp-features.el'."
  (test<imp>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<imp/load>::imp--load-feature"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    (should-error test<imp>:file:loading?)
    (should-error test<imp>:loading:load:loaded)
    (should-error test<imp>:loading:dont-load:loaded)

    ;;------------------------------
    ;; Cannot load without a root.
    ;;------------------------------

    ;; Before we supply the root, test that we error trying to load a feature.
    (should-error (imp--load-feature test<imp>:feature:loading:features))

    ;;------------------------------
    ;; Load feature & root.
    ;;------------------------------

    ;;---
    ;; Supply a root:
    ;;---
    ;; For testing that it can load something it knows about but that hasn't been loaded yet.
    (test<imp>:setup/root:loading)

    ;;---
    ;; Do _NOT_ supply feature paths:
    ;;---
    (should-not imp/features-locate)
    ;; (should (imp--load-file (imp/path-join test<imp>:path:root:loading
    ;;                                            imp/path-filename-features)))
    ;; (should imp/features-locate)

    ;; But do make sure the expected file is present.
    (should (file-exists-p (imp/path-join test<imp>:path:root:loading
                                          imp/path-filename-features)))

    ;;---
    ;; Load a feature:
    ;;---
    (should-not imp/features)
    ;; These shouldn't be defined since we haven't loaded anything at all yet.
    (should-error test<imp>:loading:features:loaded)
    (should-not (imp/provided? test<imp>:feature:loading:features))

    ;; Don't want to test that it can load the features file directly.
    ;; Test that it will load it while trying to load an actual feature.
    ;; ;; Should return `t' but not actually load the file.
    ;; (should (imp--load-feature test<imp>:feature:loading:features))
    ;; (should-not test<imp>:loading:features:loaded)

    ;;---
    ;; Set up variables:
    ;;---
    ;; First, set these variable to `nil' so they exist.
    (setq test<imp>:loading:load:loaded      nil  ;; If 'test/loading/load.el' is loaded, it will be set to `t'.
          test<imp>:loading:dont-load:loaded nil  ;; If 'test/loading/dont-load.el' is loaded, it will be set to `t'.
          test<imp>:loading:features:loaded  nil) ;; If 'test/loading/imp-features.el' is loaded, it will be set to `t'.

    ;;------------------------------
    ;; Load:
    ;;------------------------------

    ;;---
    ;; Trying to load by feature name should find/load the features file so
    ;; that it can find/load the feature named.
    ;;---
    (should-not test<imp>:loading:load:loaded)
    (should (imp--load-feature test<imp>:feature:loading:load))
    ;; Should have the vars from both 'load.el' and 'imp-features.el' now.
    (should test<imp>:loading:load:loaded)
    (should test<imp>:loading:features:loaded)
    (should test<imp>:file:loading?)))


;;------------------------------
;; imp--load-parse
;;------------------------------

;;---
;; Test Helper:
;;---
(defun test<imp/load>::helper::imp--load-parse (test-name marker-name in expected)
  "Helper for testing `imp--load-parse'.

MARKER-NAME should be a string for marking this sub-test.

IN should be a plist with keys:
  Required:
    - `:feature', `:filename', `:path'
  Optional:
    - `:error'
EXPECTED should be a plist with keys:
  `:feature', `:path'
    - `:error' is figured out based on IN plist's `:error' (or lack thereof)."
  (declare (indent 2))
  (test<imp>:should:marker test-name marker-name)

  (let* (;;---
         ;; Input Values for plist:
         ;;---
         (in:feature  (plist-get in :feature))  ;; symbol or list-of-symbols
         (in:filename (plist-get in :filename)) ;; string or `nil'
         (in:path     (plist-get in :path))     ;; string or `nil'
         (in:error    (plist-get in :error))    ;; `nil', `t', or don't supply in `plist'.
         ;;---
         ;; Inputs:
         ;;---
         (in:plist (list :feature  in:feature
                         :filename in:filename
                         :path     in:path
                         :error    in:error))
         (plist-symbol-name "in:plist")
         (path:current-dir test<imp>:path:root:test) ;; Used if path & filename are relative.
         ;;---
         ;; Expected Outputs:
         ;;---
         (out:expected:feature (plist-get expected :feature)) ;; Always a list-of-symbols.
         (out:supplied:path    (plist-get expected :path))
         (out:expected:path    (if (file-name-absolute-p out:supplied:path) ;; Always should be an absolute path.
                                   out:supplied:path
                                 (imp/path-join path:current-dir out:supplied:path)))
         (out:expected:error   (if (memq :error in:plist) ;; `in:error' if provided, else default is `t'.
                                   in:error
                                 t))
         (out:expected:keys    '(:path :feature :error))  ;; These keys (and no others) should be in `out:plist'.
         ;;---
         ;; Output:
         ;;---
         out:plist
         out:plist:feature
         out:plist:path
         out:plist:error
         out:plist:keys) ;; Found keys in `out:plist' go here to make sure we find all of them.

    ;;---
    ;; Shouldn't error.
    ;;---
    (setq out:plist (imp--load-parse test-name
                                         path:current-dir
                                         plist-symbol-name
                                         in:plist))

    ;;---
    ;; Validate `out:plist'.
    ;;---
    (test<imp>:should:marker:small "Validate `out:plist'")

    ;; Should have valid output.
    (should out:plist)
    (should (listp out:plist))
    ;; A plist should have matching pairs of keys and values.
    (should (= 0
               (% (length out:plist) 2)))
    ;; A plist should have keywords as keys.
    (let ((loop-list in:plist)
          keys
          key
          value
          exists:path-or-filename)
      ;; Check plist keys & values.
      (while loop-list
        (setq key       (car loop-list)
              value     (cadr loop-list)
              loop-list (cddr loop-list))
        (test<imp>:should:marker:small (format "Validate key `%S'" key))

        (should (keywordp key))
        (push key out:plist:keys)
        ;; Check key is valid and key's value exists (if possible).
        (cond
         ;; `:feature' must be provided.
         ((eq key :feature)
          ;;
          (should value))

         ;; `:path' or `:filename' must be provided.
         ((memq key '(:path :filename))
          ;; Just checking that at least one exists right now.
          (setq exists:path-or-filename (or exists:path-or-filename
                                            (not (null key)))))

         ;; `:error' is an optional boolean.
         ((eq key :error)
          ;; Doesn't even have to exist so good for you.
          t)

         (t
          (should-not
           (format (concat "test<imp/load>::helper::imp--load-parse-"
                           "unknown input key: %S")
                   key)))))

      (should exists:path-or-filename)

      ;; Did we find every expected key in the output and no unexpected?
      (should-not (seq-difference out:expected:keys out:plist:keys)))

    ;;---
    ;; Validate output values.
    ;;---
    (setq out:plist:feature (plist-get out:plist :feature)
          out:plist:path    (plist-get out:plist :path)
          out:plist:error   (plist-get out:plist :error))

    ;; `:feature'
    ;;---
    (should out:plist:feature)
    ;; `out:plist:feature' should always be a list of symbols.
    (should (listp out:plist:feature))
    ;; Does `out:plist:feature' match expected?
    ;; Must have correct symbols in correct order.
    (should (equal out:expected:feature
                   out:plist:feature))

    ;; `:path'
    ;;---
    (should out:plist:path)
    (should (stringp out:plist:path))
    (should (string= out:expected:path
                     out:plist:path))

    ;; `:error'
    ;;---
    ;; (should out:plist:error) ;; No; can be `nil'.
    (should (booleanp out:plist:error))
    (should (eq out:expected:error
                out:plist:error)))

  "[OK]")

;;---
;; Tests:
;;---

(ert-deftest test<imp/load>::imp--load-parse ()
  "Test that `imp--load-parse' behaves appropriately."
  (test<imp>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<imp/load>::imp--load-parse"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    ;;------------------------------
    ;; Valid Parses.
    ;;------------------------------
    ;; Supply all.
    ;;   `:filename' nil
    ;;   `:error'    nil
    (test<imp/load>::helper::imp--load-parse
        test-name
        "filename-nil-and-error-nil"
      ;; Inputs:
      (list :feature  :test ;; symbol or list-of-symbols
            :filename nil   ;; string or `nil'
            :path     "path/relative/file.el" ;; string or `nil'
            :error    nil   ;; `nil', `t', or don't supply at all.
            )
      ;; Expected Outputs:
      (list :feature '(:test) ;; Always a list-of-symbols.
            :path    "path/relative/file.el" ;; Helper will make this an absolute path if it is relative.
            ))

    ;; Supply all.
    ;;   `:error'    nil
    (test<imp/load>::helper::imp--load-parse
        test-name
        "error-nil"
      ;; Inputs:
      (list :feature  :test ;; symbol or list-of-symbols
            :filename "file.el"   ;; string or `nil'
            :path     "path/relative" ;; string or `nil'
            :error    nil   ;; `nil', `t', or don't supply at all.
            )
      ;; Expected Outputs:
      (list :feature '(:test) ;; Always a list-of-symbols.
            :path    "path/relative/file.el" ;; Always absolute path.
            ))

    ;; Do not supply `:error'.
    (test<imp/load>::helper::imp--load-parse
        test-name
        "error-dne"
      ;; Inputs:
      (list :feature  :test ;; symbol or list-of-symbols
            :filename "file.el"   ;; string or `nil'
            :path     "path/relative" ;; string or `nil')
            )
      ;; Expected Outputs:
      (list :feature '(:test) ;; Always a list-of-symbols.
            :path    "path/relative/file.el" ;; Always absolute path.
            ))

    ;;------------------------------
    ;; Errors:
    ;;------------------------------
    ;; Invalid input list (not a plist).
    (should-error (imp--load-parse test-name
                                       test<imp>:path:root:test
                                       "in:plist"
                                       nil))
    (should-error (imp--load-parse test-name
                                       test<imp>:path:root:test
                                       "in:plist"
                                       '(42)))
    (should-error (imp--load-parse test-name
                                       test<imp>:path:root:test
                                       "in:plist"
                                       '(:filename "hello" :path)))

    ;; Unknown key in input plist.
    (should-error (imp--load-parse test-name
                                       test<imp>:path:root:test
                                       "in:plist"
                                       '(:feature :greeting
                                         :filename "hello"
                                         :path "path/to"
                                         :jeff t)))


    ;; Duplicate key in input plist.
    (should-error (imp--load-parse test-name
                                       test<imp>:path:root:test
                                       "in:plist"
                                       '(:feature :greeting
                                         :filename "hello"
                                         :path "path/to"
                                         :feature :greeting)))

    ;; No path in plist and no path:current-dir.
    (should-error (imp--load-parse test-name
                                       nil
                                       "in:plist"
                                       '(:feature :greeting
                                         :filename "hello")))))


;;------------------------------
;; imp/load
;;------------------------------

(ert-deftest test<imp/load>::imp/load ()
  "Test that `imp/load' behaves appropriately."
  (test<imp>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<imp/load>::imp/load"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    (should-error test<imp>:file:loading?)
    (should-error test<imp>:loading:load:loaded)
    (should-error test<imp>:loading:dont-load:loaded)

    ;;------------------------------
    ;; Load feature & root.
    ;;------------------------------

    ;;---
    ;; +Supply a root:+
    ;;---
    ;; `imp/load' doesn't care about `imp/path-roots'.

    ;;---
    ;; Load a feature:
    ;;---
    ;; `imp/load' will not load again if already provided.
    (imp/provide test<imp>:feature:loading:dont-load)

    ;;---
    ;; Set up variables:
    ;;---
    ;; First, set these variable to `nil' so they exist.
    (setq test<imp>:loading:load:loaded      nil  ;; If 'test/loading/load.el' is loaded, it will be set to `t'.
          test<imp>:loading:dont-load:loaded nil) ;; If 'test/loading/dont-load.el' is loaded, it will be set to `t'.

    ;;------------------------------
    ;; Load:
    ;;------------------------------

    ;;---
    ;; If feature is alredy provided, do not load.
    ;;---
    (let (result)
      (should-not test<imp>:loading:dont-load:loaded)
      (should (file-exists-p (imp/path-join test<imp>:path:root:loading
                                            (concat test<imp>:file:loading:dont-load ".el"))))
      ;; Call `imp/load on it's feature...
      (setq result (imp/load :feature  test<imp>:feature:loading:dont-load
                             :path     test<imp>:path:root:loading
                             :filename test<imp>:file:loading:dont-load
                             :error    nil))
      ;; ...should not load.
      (should-not result)
      (should-not test<imp>:loading:dont-load:loaded))

    ;;---
    ;; If feature is alredy provided, but `:skip nil' is set, load anyways.
    ;;---
    (let (result)
      (should-not test<imp>:loading:dont-load:loaded)
      (should (file-exists-p (imp/path-join test<imp>:path:root:loading
                                            (concat test<imp>:file:loading:dont-load ".el"))))
      ;; Call `imp/load on it's feature... with `:skip nil' this time.
      (setq result (imp/load :feature  test<imp>:feature:loading:dont-load
                             :path     test<imp>:path:root:loading
                             :filename test<imp>:file:loading:dont-load
                             :error    nil
                             :skip     nil))
      ;; Should load this time.
      (should result)
      (should test<imp>:loading:dont-load:loaded))

    ;;---
    ;; If feature is not provided, load it.
    ;;---
    (let (result)
      (should-not test<imp>:loading:load:loaded)
      ;; Call `imp/load on it's feature...
      (setq result (imp/load :feature  test<imp>:feature:loading:load
                             :path     test<imp>:path:root:loading
                             :filename test<imp>:file:loading:load
                             :error    nil))
      ;; ...should have loaded.
      (should result)
      (should test<imp>:loading:load:loaded))

    ;;------------------------------
    ;; Errors:
    ;;------------------------------
    ;; Expecting wrong feature.
    (should-not (eval (imp/load :feature  test<imp>:feature:loading:doesnt-exist
                                :path     test<imp>:path:root:loading
                                :filename test<imp>:file:loading:load
                                :error    nil)))
    (should-error (eval (imp/load :feature  test<imp>:feature:loading:doesnt-exist
                                  :path     test<imp>:path:root:loading
                                  :filename test<imp>:file:loading:load
                                  :error    t)))
    (should-error (eval (imp/load :feature  test<imp>:feature:loading:doesnt-exist
                                  :path     test<imp>:path:root:loading
                                  :filename test<imp>:file:loading:load)))))
