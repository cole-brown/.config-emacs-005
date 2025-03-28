;;; core/modules/emacs/imp/test/test.el --- Tests for "test.el" -*- no-byte-compile: t; lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-01-31
;; Timestamp:  2023-06-22
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Tests for "test.el"
;; ...but who is testing /these/ tests?
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
(test<imp>:init:load "../test.el")


;; ╔═════════════════════════════╤═══════════╤═════════════════════════════════╗
;; ╟─────────────────────────────┤ ERT TESTS ├─────────────────────────────────╢
;; ╠══════════╤══════════════════╧═══════════╧══════════════════╤══════════════╣
;; ╟──────────┤ Because Emacs needs to give Alists some love... ├──────────────╢
;; ╚══════════╧═════════════════════════════════════════════════╧══════════════╝


;;------------------------------------------------------------------------------
;; Tests: Imp Load Functions
;;------------------------------------------------------------------------------


;;------------------------------
;; imp--test-load-parse
;;------------------------------

;;---
;; Test Helper:
;;---
(defun test<imp/test>::helper::imp--test-load-parse (test-name marker-name in expected)
  "Helper for testing `imp--test-load-parse'.

MARKER-NAME should be a string for marking this sub-test.

IN should be a plist with keys:
  Required:
    - `:filename' and/or `:path'
  Optional:
    - `:feature:pre', `:feature:post', `:error'
EXPECTED should be a plist with keys:
  `:feature:pre', `:feature:post', `:path'
    - `:error' is figured out based on IN plist's `:error' (or lack thereof)."
  (declare (indent 2))
  (test<imp>:should:marker test-name marker-name)

  (let* ((in:keys:valid (list :feature:pre
                              :feature:post
                              :filename
                              :path
                              :error))
         ;;---
         ;; Input Values for plist:
         ;;---
         (in:feature:pre  (plist-get in :feature:pre))  ;; symbol or list-of-symbols
         (in:feature:post (plist-get in :feature:post)) ;; symbol or list-of-symbols
         (in:filename     (plist-get in :filename))     ;; string or `nil'
         (in:path         (plist-get in :path))         ;; string or `nil'
         (in:error        (plist-get in :error))        ;; `nil', `t', or don't supply in `plist'.
         ;;---
         ;; Inputs:
         ;;---
         (in:plist (list :feature:pre  in:feature:pre
                         :feature:post in:feature:post
                         :filename     in:filename
                         :path         in:path
                         :error        in:error))
         (plist-symbol-name "in:plist")
         (path:current-dir test<imp>:path:root:test) ;; Used if path & filename are relative.
         ;;---
         ;; Expected Outputs:
         ;;---
         (out:expected:feature:pre  (plist-get expected :feature:pre))  ;; Always a list-of-symbols.
         (out:expected:feature:post (plist-get expected :feature:post)) ;; Always a list-of-symbols.
         (out:supplied:path         (plist-get expected :path))
         (out:expected:path         (if (file-name-absolute-p out:supplied:path) ;; Always should be an absolute path.
                                        out:supplied:path
                                      (imp/path-join path:current-dir out:supplied:path)))
         (out:expected:error        (if (memq :error in:plist) ;; `in:error' if provided, else default is `t'.
                                        in:error
                                      t))
         (out:expected:keys         '(:path :feature:pre :feature:post :error))  ;; These keys (and no others) should be in `out:plist'.
         ;;---
         ;; Output:
         ;;---
         out:plist
         out:plist:feature:pre
         out:plist:feature:post
         out:plist:path
         out:plist:error
         out:plist:keys) ;; Found keys in `out:plist' go here to make sure we find all of them.

    ;;---
    ;; Validate input.
    ;;---
    ;; IN should be a plist, so it should be an even length
    ;; (should have matching pairs of keys and values).
    (should (= 0
               (% (length in) 2)))
    ;; Only iterate/check keys.
    (dotimes (i (/ (length in) 2))
      (let* ((i:key (* i 2))
             (key (nth i:key in)))
        (should (keywordp key))
        (should (memq key in:keys:valid))))

    ;;---
    ;; Shouldn't error.
    ;;---
    (setq out:plist (imp--test-load-parse test-name
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
         ;; `:feature:pre' & `:feature:post' are optional
         ((memq key '(:feature:pre :feature:post))
          t)

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
           (format (concat "test<imp/test>::helper::imp--test-load-parse-"
                           "unknown input key: %S")
                   key)))))

      (should exists:path-or-filename)

      ;; Did we find every expected key in the output and no unexpected?
      (should-not (seq-difference out:expected:keys out:plist:keys)))

    ;;---
    ;; Validate output values.
    ;;---
    (setq out:plist:feature:pre  (plist-get out:plist :feature:pre)
          out:plist:feature:post (plist-get out:plist :feature:post)
          out:plist:path         (plist-get out:plist :path)
          out:plist:error        (plist-get out:plist :error))

    ;; `:feature:pre' & `:feature:post'
    ;;---
    ;; They're optional, so only validate if they're there.
    (when out:plist:feature:pre
      (should out:plist:feature:pre)
      ;; `out:plist:feature' should always be a list of symbols.
      (should (listp out:plist:feature:pre))
      ;; Does `out:plist:feature' match expected?
      ;; Must have correct symbols in correct order.
      (should (equal out:expected:feature:pre
                     out:plist:feature:pre)))
    (when out:plist:feature:post
      (should out:plist:feature:post)
      ;; `out:plist:feature' should always be a list of symbols.
      (should (listp out:plist:feature:post))
      ;; Does `out:plist:feature' match expected?
      ;; Must have correct symbols in correct order.
      (should (equal out:expected:feature:post
                     out:plist:feature:post)))

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

(ert-deftest test<imp/test>::imp--test-load-parse ()
  "Test that `imp--test-load-parse' behaves appropriately."
  (test<imp>:fixture
   ;;===
   ;; Test name, setup & teardown func.
   ;;===
   "test<imp/test>::imp--test-load-parse"
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
   (test<imp/test>::helper::imp--test-load-parse
    test-name
    "filename-nil-and-error-nil"
    ;; Inputs:
    (list :feature:pre  :test ;; symbol or list-of-symbols
          :feature:post :test ;; symbol or list-of-symbols
          :filename     nil   ;; string or `nil'
          :path         "path/relative/file.el" ;; string or `nil'
          :error        nil   ;; `nil', `t', or don't supply at all.
          )
    ;; Expected Outputs:
    (list :feature:pre  '(:test) ;; Always a list-of-symbols.
          :feature:post '(:test) ;; Always a list-of-symbols.
          :path    "path/relative/file.el" ;; Helper will make this an absolute path if it is relative.
          :error   nil
          ))

   ;; Do not supply `:feature:pre'.
   ;;   `:error'    nil
   (test<imp/test>::helper::imp--test-load-parse
    test-name
    "error-nil"
    ;; Inputs:
    (list :feature:post  :test ;; symbol or list-of-symbols
          :filename      "file.el"   ;; string or `nil'
          :path          "path/relative" ;; string or `nil'
          :error         nil   ;; `nil', `t', or don't supply at all.
          )
    ;; Expected Outputs:
    (list :feature:post '(:test) ;; Always a list-of-symbols.
          :path         "path/relative/file.el" ;; Always absolute path.
          ))

   ;; Do not supply `:error'.
   (test<imp/test>::helper::imp--test-load-parse
    test-name
    "error-dne"
    ;; Inputs:
    (list :feature:pre  :test ;; symbol or list-of-symbols
          :feature:post :test ;; symbol or list-of-symbols
          :filename     "file.el"   ;; string or `nil'
          :path         "path/relative" ;; string or `nil')
          )
    ;; Expected Outputs:
    (list :feature:pre  '(:test) ;; Always a list-of-symbols.
          :feature:post '(:test) ;; Always a list-of-symbols.
          :path         "path/relative/file.el" ;; Always absolute path.
          ))

   ;;------------------------------
   ;; Errors:
   ;;------------------------------
   ;; Invalid input list (not a plist).
   (should-error (imp--test-load-parse test-name
                                           test<imp>:path:root:test
                                           "in:plist"
                                           nil))
   (should-error (imp--test-load-parse test-name
                                           test<imp>:path:root:test
                                           "in:plist"
                                           '(42)))
   (should-error (imp--test-load-parse test-name
                                           test<imp>:path:root:test
                                           "in:plist"
                                           '(:filename "hello" :path)))

   ;; Unknown key in input plist.
   (should-error (imp--test-load-parse test-name
                                           test<imp>:path:root:test
                                           "in:plist"
                                           '(:feature  :greeting ;; `:feature:pre' / `:feature:post' expected
                                             :filename "hello"
                                             :path     "path/to")))

   ;; Duplicate key in input plist.
   (should-error (imp--test-load-parse test-name
                                           test<imp>:path:root:test
                                           "in:plist"
                                           '(:feature:post :greeting
                                             :filename     "hello"
                                             :path         "path/to"
                                             :feature      :greeting)))

   ;; No path in plist and no path:current-dir.
   (should-error (imp--test-load-parse test-name
                                           nil
                                           "in:plist"
                                           '(:feature:pre :greeting
                                             :filename    "hello")))))


;;------------------------------
;; imp/test-load
;;------------------------------

(ert-deftest test<imp/test>::imp/test-load ()
  "Test that `imp/test-load' behaves appropriately."
  (test<imp>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<imp/test>::imp/test-load"
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

    ;; NOTE: `imp/test-load' doesn't care about `imp/path-roots', so don't bother supplying one.

    ;;---
    ;; Load a feature:
    ;;---
    ;; `imp/test-load' should load again even if already provided.
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
    ;; If feature is alredy provided... load anyways.
    ;;---
    (let (result)
      (should-not test<imp>:loading:dont-load:loaded)
      (should (file-exists-p (imp/path-join test<imp>:path:root:loading
                                            (concat test<imp>:file:loading:dont-load ".el"))))
      ;; Call `imp/test-load on it's feature...
      (setq result (imp/test-load :feature:post test<imp>:feature:loading:dont-load
                                  :path         test<imp>:path:root:loading
                                  :filename     test<imp>:file:loading:dont-load
                                  :error        nil))
      ;; ...should now be load for real.
      (should result)
      (should test<imp>:loading:dont-load:loaded))

    ;;---
    ;; If feature is not provided, load it.
    ;;---
    (let (result)
      (should-not test<imp>:loading:load:loaded)
      ;; Call `imp/test-load on it's feature...
      (setq result (imp/test-load :feature:post test<imp>:feature:loading:load
                                  :path         test<imp>:path:root:loading
                                  :filename     test<imp>:file:loading:load
                                  :error        nil))
      ;; ...should have loaded.
      (should result)
      (should test<imp>:loading:load:loaded))

    ;;------------------------------
    ;; Errors:
    ;;------------------------------
    ;; Expecting wrong feature.
    (should-not (imp/test-load :feature:post test<imp>:feature:loading:doesnt-exist
                               :path         test<imp>:path:root:loading
                               :filename     test<imp>:file:loading:load
                               :error        nil))
    (should-error (imp/test-load :feature:post test<imp>:feature:loading:doesnt-exist
                                 :path         test<imp>:path:root:loading
                                 :filename     test<imp>:file:loading:load
                                 :error        t))
    (should-error (imp/test-load :feature:post test<imp>:feature:loading:doesnt-exist
                                 :path         test<imp>:path:root:loading
                                 :filename     test<imp>:file:loading:load))))
