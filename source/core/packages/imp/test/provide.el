;;; core/modules/emacs/imp/test/provide.el --- Tests for "provide.el" -*- no-byte-compile: t; lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-01-04
;; Timestamp:  2023-06-22
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Tests for "provide.el"
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


;;------------------------------------------------------------------------------
;; Test Debugging Helpers
;;------------------------------------------------------------------------------

(defun test<imp/provide>:setup:features ()
  "Manually set `imp/features' so we can test getting things out of it."
  ;; Add in a few defaults.
  (setq imp/features '((:test
                        (one
                         (two)))
                       (:foo nil
                        (:bar (baz)))
                       (:hello nil
                        (world)
                        (there)))))


(defun test<imp/provide>:load:exists? ()
  "Returns t if `test<imp>:file:loading?' exists in the symbol table."
  (condition-case err
      (progn
        ;; Just try to access this. If we can't, `condition-case' will catch the `void-variable' signal.
        test<imp>:file:loading?
        ;; If we could access it, return `t' because it exists.
        t)
    ;; Couldn't access it, so it doesn't exist, so return nil.
    (void-variable nil)))
;; Probably exists to start off with:
;;   test<imp>:file:loading?
;;   (test<imp>:load:exists?)
;;   (test<imp>:load:unset)
;; And now it should not exist:
;;   (test<imp>:load:exists?)


;;------------------------------
;; Set-Up / Tear-Down
;;------------------------------

(defun test<imp/provide>:setup:load (&optional load)
  "Deletes `test<imp>:file:loading?' variable if it exists.

If LOAD is non-nil, loads 'test/loading/load.el' to set-up the
`imp/provide-loading?' helpers."
  ;; Make sure `test<imp>:file:loading?' doesn't exist.
  ;; We may or may not have loaded the file so we can't rely on its functions, so:
  (makunbound 'test<imp>:file:loading?)

  (when load
    (test<imp>:init:load "loading/load.el")))


;; ╔═════════════════════════════╤═══════════╤═════════════════════════════════╗
;; ╟─────────────────────────────┤ ERT TESTS ├─────────────────────────────────╢
;; ╠══════════╤══════════════════╧═══════════╧══════════════════╤══════════════╣
;; ╟──────────┤ Because Emacs needs to give Alists some love... ├──────────────╢
;; ╚══════════╧═════════════════════════════════════════════════╧══════════════╝


;;------------------------------------------------------------------------------
;; Tests: Imp Provide Functions
;;------------------------------------------------------------------------------

;;------------------------------
;; imp/provide-loading?
;;------------------------------

(ert-deftest test<imp/provide>::imp/provide-loading? ()
  "Test that `imp/provide-loading?' behaves appropriately."
  (test<imp>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<imp/provide>::imp/provide-loading?"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    ;; Make sure we do not have a valid `test<imp/provide>:file:loading?' yet.
    (test<imp/provide>:setup:load)
    (should-not (test<imp/provide>:load:exists?))

    ;; Now load 'test/loading/load.el' so we can check if `imp/provide-loading?' works
    ;; when loading a file.
    (test<imp/provide>:setup:load :load)
    (should (test<imp/provide>:load:exists?))
    ;; `test<imp>:file:loading?' should have been set to `t' during its load.
    (should (eq t
                test<imp>:file:loading?))))


;;------------------------------
;; imp/provided?
;;------------------------------

(ert-deftest test<imp/provide>::imp/provided? ()
  "Test that `imp/provided?' behaves appropriately."
  (test<imp>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<imp/provide>::imp/provided?"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    ;; We shouldn't have any features yet in `imp/path-roots'...
    (should-not imp/features)
    ;; Add some features.
    (test<imp/provide>:setup:features)
    (should imp/features)
    (should (imp--alist-get-value :test imp/features))
    (should (imp--alist-get-value :hello imp/features))

    ;; Now test!
    (should (imp/provided? :test))
    (should (imp/provided? :test 'one))
    (should-not (imp/provided? :test 'two))
    (should (imp/provided? :test 'one 'two))
    (should-not (imp/provided? :test 'one 'two 'three))

    (should (imp/provided? :foo))
    (should-not (imp/provided? :foo 'bar))
    (should (imp/provided? :foo :bar))
    (should (imp/provided? :foo :bar 'baz))

    (should (imp/provided? :hello))
    (should (imp/provided? :hello 'world))
    (should (imp/provided? :hello 'there))
    (should-not (imp/provided? :hello 'world 'there))
    (should-not (imp/provided? :hello 'there 'world))))


;;------------------------------
;; imp/provide
;;------------------------------

(ert-deftest test<imp/provide>::imp/provide ()
  "Test that `imp/provide' behaves appropriately."
  (test<imp>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<imp/provide>::imp/provide"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    ;; We shouldn't have any features yet in `imp/path-roots'...
    (should-not imp/features)

    (imp/provide :test 'one 'two 'three)
    (should (equal imp/features
                   '((:test (one (two (three)))))))

    (imp/provide :test 'one 'and 'uh 'more)
    (should (equal imp/features
                   '((:test (one
                             (and (uh (more)))
                             (two (three)))))))

    (imp/provide :test 'one 'and 'done)
    (should (equal imp/features
                   '((:test (one
                             (and
                              (done)
                              (uh (more)))
                             (two
                              (three)))))))))


(ert-deftest test<imp/provide>::imp/provide-with-emacs ()
  "Test that `imp/provide-with-emacs' behaves appropriately."
  (test<imp>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<imp/provide>::imp/provide-with-emacs"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    ;; We shouldn't have any features yet in `imp/path-roots'...
    (should-not imp/features)

    (let* ((feature '(:test one two three))
           (feature/emacs (imp/feature-normalize-imp->emacs feature)))
      (imp/provide-with-emacs feature)
      ;; Feature should have been added to `imp'.
      (should (equal imp/features
                     '((:test (one (two (three)))))))
      ;; Feature should have been added to `emacs'.
      (should (memq feature/emacs features))

      ;; And... clean up emacs' `features' list.
      (setq features (remove feature/emacs features)))

    (let* ((feature '(:test one and uh more))
           (feature/emacs (imp/feature-normalize-imp->emacs feature)))
      (imp/provide-with-emacs feature)
      ;; Feature should have been added to `imp'.
      (should (equal imp/features
                   '((:test (one
                             (and (uh (more)))
                             (two (three)))))))
      ;; Feature should have been added to `emacs'.
      (should (memq feature/emacs features))

      ;; And... clean up emacs' `features' list.
      (setq features (remove feature/emacs features)))

    (let* ((feature '(:test one and done))
           (feature/emacs (imp/feature-normalize-imp->emacs feature)))
      (imp/provide-with-emacs feature)
      ;; Feature should have been added to `imp'.
      (should (equal imp/features
                     '((:test (one
                               (and
                                (done)
                                (uh (more)))
                               (two
                                (three)))))))
      ;; Feature should have been added to `emacs'.
      (should (memq feature/emacs features))

      ;; And... clean up emacs' `features' list.
      (setq features (remove feature/emacs features)))))
