;;; core/modules/emacs/imp/test/require.el --- Tests for "require.el" -*- no-byte-compile: t; lexical-binding: t; -*-
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
;; Tests for "require.el"
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
(test<imp>:init:load "../provide.el")
(test<imp>:init:load "../load.el")
(test<imp>:init:load "../require.el")


;;------------------------------------------------------------------------------
;; Test Debugging Helpers
;;------------------------------------------------------------------------------

;;------------------------------
;; Set-Up / Tear-Down
;;------------------------------

(defun test<imp/require>:setup:vars ()
  "Deletes variable from 'test/loading/*.el' files."
  ;; These are fine to do even if they already don't exist.
  (makunbound 'test<imp>:file:loading?)
  (makunbound 'test<imp>:loading:load:loaded)
  (makunbound 'test<imp>:loading:dont-load:loaded))


;; ╔═════════════════════════════╤═══════════╤═════════════════════════════════╗
;; ╟─────────────────────────────┤ ERT TESTS ├─────────────────────────────────╢
;; ╠══════════╤══════════════════╧═══════════╧══════════════════╤══════════════╣
;; ╟──────────┤ Because Emacs needs to give Alists some love... ├──────────────╢
;; ╚══════════╧═════════════════════════════════════════════════╧══════════════╝


;;------------------------------------------------------------------------------
;; Tests: Imp Require Functions
;;------------------------------------------------------------------------------

;;------------------------------
;; imp/require
;;------------------------------

(ert-deftest test<imp/require>::imp/require ()
  "Test that `imp/require' behaves appropriately."
  (test<imp>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<imp/require>::imp/require"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    (test<imp/require>:setup:vars)
    (should-error test<imp>:file:loading?)
    (should-error test<imp>:loading:load:loaded)
    (should-error test<imp>:loading:dont-load:loaded)

    ;;------------------------------
    ;; Require feature & root.
    ;;------------------------------

    ;;---
    ;; Supply a root:
    ;;---
    ;; For testing that it can load something it knows about but that hasn't been required.
    (imp/path-root-set test<imp>:feature:loading
                       test<imp>:path:root:loading)

    ;;---
    ;; Require a feature:
    ;;---
    ;; For testing that nothing happens when it's already required.
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
    ;; If feature is alredy required, nothing should happen.
    ;;---
    (should-not test<imp>:loading:dont-load:loaded)
    ;; Call `imp/require on it's feature; shouldn't be loaded since we've required it already.
    (should (imp/require test<imp>:feature:loading:dont-load))
    (should-not test<imp>:loading:dont-load:loaded)

    ;;---
    ;; If we know the base feature, we should be able to load the file by the feature name.
    ;;---
    (should-not test<imp>:loading:load:loaded)
    (should (imp/require test<imp>:feature:loading:load))
    (should test<imp>:loading:load:loaded)
    (should test<imp>:file:loading?)
    ;; And it should now be provided.
    (should (imp/provided? test<imp>:feature:loading:load))

    ;;------------------------------
    ;; Errors:
    ;;------------------------------

    ;;---
    ;; Know the base feature, but can't find anything to load.
    ;;---
    (should-error test<imp>:loading:load:doesnt-exist)
    (should-error (imp/require test<imp>:feature:loading:doesnt-exist))
    (should-error test<imp>:loading:load:doesnt-exist)

    ;;---
    ;; Don't know the base feature.
    ;;---
    ;; We fallback to asking Emacs to `require' it, but it doesn't know anything about this either.
    ;; This won't error; it'll just return nil.
    (should-not (imp/require 'something-that-doesnt-exist-in-emacs))))
