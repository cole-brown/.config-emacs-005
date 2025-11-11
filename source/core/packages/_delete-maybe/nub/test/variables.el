;;; core/modules/output/nub/test/variables.el --- Tests for "variables.el" -*- no-byte-compile: t; lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-12-03
;; Timestamp:  2023-06-26
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Tests for "variables.el"
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Test Requirements
;;------------------------------------------------------------------------------

(imp:test:load :filename "base.el")


;; ╔═════════════════════════════╤═══════════╤═════════════════════════════════╗
;; ╟─────────────────────────────┤ ERT TESTS ├─────────────────────────────────╢
;; ╠══════════════╤══════════════╧═══════════╧═══════════════╤═════════════════╣
;; ╟──────────────┤ Test the vacillating, varying variables. ├─────────────────╢
;; ╚══════════════╧══════════════════════════════════════════╧═════════════════╝


;;------------------------------------------------------------------------------
;; Tests: Functions
;;------------------------------------------------------------------------------

;;------------------------------
;; test<nub>:fixture - :user/auto
;;------------------------------

(ert-deftest test<nub/utils>::user/auto ()
  "Test that `:user/auto' works as expected."

  ;; Make sure we start out without any user set.
  (setq test<nub>:user nil)
  (should-not test<nub>:user)

  ;; This macro should set the user. `:user/auto' should mean "use `test<nub>:user:make'".
  (test<nub>:fixture
      ;; Test name, nub user, setup func, teardown func.
      "test<nub/utils>::user/auto"
      :user/auto
      nil
      nil

    ;; `:user/auto' is based off of the test's name, so make sure that's set.
    (should test-name)
    (should (string= "test<nub/utils>::user/auto" test-name))

    ;; `test<nub>:fixture' should have set `test<nub>:user'.
    (should test<nub>:user)
    ;; Want `test<nub>:user' to be `eq', not just `equal'.
    (should (eq test<nub>:user
                (test<nub>:user:make test-name)))))


;;------------------------------
;; test<nub>:fixture - Specified user.
;;------------------------------

(ert-deftest test<nub/utils>::user/manual ()
  "Test that a specified user works as expected."

  ;; Make sure we start out without any user set.
  (setq test<nub>:user nil)
  (should-not test<nub>:user)

  ;; This macro should set the user.
  (test<nub>:fixture
      ;; Test name, nub user, setup func, teardown func.
      "test<nub/utils>::user/manual"
      :jeff
      nil
      nil

    ;; `test<nub>:fixture' should have set `test<nub>:user'.
    (should test<nub>:user)
    ;; `:user/auto' is based off of the test's name, so make sure that's set.
    (should test-name)
    ;; And make sure we are /not/ using `:user/auto' this time.
    (should-not (eq test<nub>:user
                    (test<nub>:user:make test-name)))

    ;; We should instead have the user exactly as we set it.
    (should (eq test<nub>:user :jeff))))


;;------------------------------
;; int<nub>:user:exists?
;;------------------------------

(ert-deftest test<nub/utils>::int<nub>:user:exists? ()
  "Test that `int<nub>:user:exists?' behaves."

  (test<nub>:fixture
      ;; Test name, nub user, setup func, teardown func.
      "test<nub/utils>::int<nub>:user:exists?"
      :user/auto
      nil
      nil

    ;; Assert nub users only has our current testing user.
    (test<nub>:assert:users)))


;;------------------------------
;; int<nub>:init:user
;;------------------------------
;; Already tested by this point.
;; Used in test set-up.


;;------------------------------
;; int<nub>:terminate:user
;;------------------------------
;; Already tested by this point.
;; Used in test set-up.


;;------------------------------
;; int<nub>:var:user-at-level
;;------------------------------

(ert-deftest test<nub/utils>::int<nub>:var:user-at-level ()
  "Test that a specified user works as expected."
  (test<nub>:fixture
      ;; Test name, nub user, setup func, teardown func.
      "test<nub/utils>::int<nub>:var:user-at-level"
      :user/auto
      nil
      nil

    ;;------------------------------
    ;; Test getting something.
    ;;------------------------------
    ;; Make sure we have a few levels.
    (should nub:output:levels)
    (should (listp nub:output:levels))
    (should (> (length nub:output:levels)
               1))

    ;; Create an alist of users -> alist of levels -> something.
    (let (alist/user
          alist/level)
      ;; Alist of levels -> something.
      (dolist (level nub:output:levels)
        (push (cons level level) alist/level))

      ;; Alist of users -> alist.
      (push (cons test<nub>:user alist/level) alist/user)

      ;;---
      ;; Test the function.
      ;;---
      (dolist (level nub:output:levels)
        (should (eq level
                    (int<nub>:var:user-at-level test<nub>:user
                                                level
                                                alist/user
                                                :does-not-exist)))))

    ;;------------------------------
    ;; Test getting nil.
    ;;------------------------------
    (let* ((alist/user '((:error   . nil)
                         (:warning . nil)
                         (:info    . t)
                         (:debug   . t)))
           (alist/default '((:error   . :test:default)
                            (:warning . :test:default)
                            (:info    . :test:default)
                            (:debug   . :test:default)))
           (alist (list (cons test<nub>:user             alist/user)
                        (cons int<nub>:var:user:fallback alist/default))))
      ;; Want `nil' for `:error' & `:warning'.
      (should (eq nil
                  (int<nub>:var:user-at-level test<nub>:user
                                              :error
                                              alist
                                              ;; No default.
                                              )))
      (should (eq nil
                  (int<nub>:var:user-at-level test<nub>:user
                                              :warning
                                              alist
                                              ;; No default.
                                              )))
      ;; Double-check `nil' isn't some fluke or something...
      (should (eq t
                  (int<nub>:var:user-at-level test<nub>:user
                                              :info
                                              alist
                                              ;; No default.
                                              )))
      (should (eq t
                  (int<nub>:var:user-at-level test<nub>:user
                                              :debug
                                              alist
                                              ;; No default.
                                              ))))))


;;------------------------------
;; int<nub>:var:prefix
;;------------------------------

(ert-deftest test<nub/utils>::int<nub>:var:prefix ()
  "Test that `int<nub>:var:prefix' works correctly."

  (test<nub>:fixture
      ;; Test name, nub user, setup func, teardown func.
      "test<nub/utils>::int<nub>:var:prefix"
      :user/auto
      nil
      nil

    (let (prefix/default
          prefix/user)

      ;;------------------------------
      ;; Should not have an entry for the user.
      ;;------------------------------
      (should-not (alist-get test<nub>:user int<nub>:var:prefix))

      ;;------------------------------
      ;; Should get default when we ask for something.
      ;;------------------------------
      (should (alist-get int<nub>:var:user:fallback int<nub>:var:prefix))

      (should (stringp (int<nub>:var:prefix int<nub>:var:user:fallback :error)))

      (should (stringp (int<nub>:var:prefix test<nub>:user :error)))

      ;; Save before we test creating/using new prefixes for this user.
      (setq prefix/user (int<nub>:var:prefix test<nub>:user :error)
            prefix/default (int<nub>:var:prefix int<nub>:var:user:fallback :error))

      (should (string= prefix/default
                       prefix/user))

      ;;------------------------------
      ;; Test having actual prefixes.
      ;;------------------------------

      ;; Create some prefixes.
      (int<nub>:init:prefix test<nub>:user
                            '((:error   . "[TEST: ERROR   ]: ")
                              (:warning . "[TEST: WARNING ]: ")
                              (:info    . "[TEST: INFO    ]: ")
                              (:debug   . "[TEST:    debug]: ")))

      ;; Should still have fallback user's, unchanged.
      (should (stringp (int<nub>:var:prefix int<nub>:var:user:fallback :error)))
      (should (string= prefix/default
                       (int<nub>:var:prefix int<nub>:var:user:fallback :error)))

      ;; Should now have a different prefix for our user.
      (should (stringp (int<nub>:var:prefix test<nub>:user :error)))

      (should-not (string= (int<nub>:var:prefix test<nub>:user :error)
                           (int<nub>:var:prefix int<nub>:var:user:fallback :error)))
      (should-not (string= prefix/user
                           (int<nub>:var:prefix test<nub>:user :error)))
      (should (string= "[TEST: ERROR   ]: "
                       (int<nub>:var:prefix test<nub>:user :error)))

      ;;------------------------------
      ;; Test the other levels too...
      ;;------------------------------
      (should (string="[TEST: WARNING ]: "
                      (int<nub>:var:prefix test<nub>:user :warning)))
      (should (string="[TEST: INFO    ]: "
                      (int<nub>:var:prefix test<nub>:user :info)))
      (should (string="[TEST:    debug]: "
                      (int<nub>:var:prefix test<nub>:user :debug))))))


;;------------------------------------------------------------------------------
;; Test: Variable Getters/Setters
;;------------------------------------------------------------------------------

;;------------------------------
;; int<nub>:var:enabled?
;;------------------------------

(ert-deftest test<nub/utils>::int<nub>:var:enabled? ()
  "Test that `int<nub>:var:enabled?' functions for init, get, and set work correctly."

  (test<nub>:fixture
      ;; Test name, nub user, setup func, teardown func.
      "test<nub/utils>::int<nub>:var:enabled?"
      :user/auto
      nil
      nil

    (let* ((enabled-locally? :local) ;; truthy value to check for
           (enabled-fn (lambda (level default)
                         "Just return `enabled-locally?'."
                         enabled-locally?))
           (enabled? (list '(:error   . :error) ;; truthy value to check for
                           '(:warning . nil)     ;; nil value to check for
                           ;; no :info - use default (should be `t').
                           (cons :debug enabled-fn)))) ;; Use predicate.

           ;;------------------------------
           ;; Init with the `enabled?' alist.
           ;;------------------------------

           (should (int<nub>:init:enabled? test<nub>:user enabled?))

           ;;------------------------------
           ;; Get
           ;;------------------------------
           ;; (pp int<nub>:var:enabled?)

           ;; `:error' Level: Should have enabled? set to `:error'.
           (should (eq :error
                       (int<nub>:var:enabled? test<nub>:user :error :default)))

           ;; `:warning' Level: Should have enabled? set to `nil'.
           (should (eq nil
                       (int<nub>:var:enabled? test<nub>:user :warning :default)))

           ;; `:info' Level: Should get fallback of `t'.
           (should (eq t
                       (int<nub>:var:enabled? test<nub>:user :info :default)))
           ;; `:info' Level: Should get our default vaule.
           (should (eq :does-not-exist
                       (int<nub>:var:enabled? test<nub>:user :info :does-not-exist)))

           ;; `:debug' Level: Should get whatever `enabled-fn' returns, which is whatever `enabled-locally?' is.
           (should (eq :local
                       (int<nub>:var:enabled? test<nub>:user :debug :default)))
           (should (eq enabled-locally?
                       (int<nub>:var:enabled? test<nub>:user :debug :default)))
           ;; Now set to something else and check again.
           (setq enabled-locally? nil)
           (should (eq enabled-locally?
                       (int<nub>:var:enabled? test<nub>:user :debug :default)))

           ;;------------------------------
           ;; Set
           ;;------------------------------
           (int<nub>:var:enabled?:set test<nub>:user :error :error-too)
           (should (eq :error-too
                       (int<nub>:var:enabled? test<nub>:user :error :default)))

           (int<nub>:var:enabled?:set test<nub>:user :info :info-now-set-for-user)
           (should (eq :info-now-set-for-user
                       (int<nub>:var:enabled? test<nub>:user :info :default))))))


;;------------------------------
;; int<nub>:var:sink
;;------------------------------

(ert-deftest test<nub/utils>::int<nub>:var:sink ()
  "Test that `int<nub>:var:sink' functions for init, get, and set work correctly."

  (test<nub>:fixture
      ;; Test name, nub user, setup func, teardown func.
      "test<nub/utils>::int<nub>:var:sink"
      :user/auto
      nil
      nil

    (let* ((sink:info nil)
           (sink:info:fn (lambda (msg &rest args)
                           "Output sink for `:info' level."
                           (push (format msg args) sink:info)))
           (sinks (list (cons :error t) ;; `t'   == Use default.
                        '(:warning . nil)  ;; `nil' == Do nothing.
                        (cons :info (list sink:info:fn t))))) ;; Can also have a list of things.

           ;;------------------------------
           ;; Init with the `sink' alist.
           ;;------------------------------

           (should (int<nub>:init:sink test<nub>:user sinks))

           ;;------------------------------
           ;; Get
           ;;------------------------------
           ;; (pp int<nub>:var:sink)

           (should (eq t
                       (int<nub>:var:sink test<nub>:user :error :default)))

           (should (eq nil
                       (int<nub>:var:sink test<nub>:user :warning :default)))

           (should (equal (list sink:info:fn t)
                          (int<nub>:var:sink test<nub>:user :info :default)))

           ;; Default for `:debug' is the `message' function.
           (should (eq 'message
                       (int<nub>:var:sink test<nub>:user :debug :default)))

           ;;------------------------------
           ;; Set
           ;;------------------------------
           (int<nub>:var:sink:set test<nub>:user :error 'some-new-error-func)
           (should (eq 'some-new-error-func
                       (int<nub>:var:sink test<nub>:user :error :default)))

           (int<nub>:var:sink:set test<nub>:user :debug t)
           (should (eq t
                       (int<nub>:var:sink test<nub>:user :debug :default))))))


;;------------------------------
;; int<nub>:var:debugging
;;------------------------------

(ert-deftest test<nub/utils>::int<nub>:var:debugging ()
  "Test that `int<nub>:var:debugging' functions for init, get, and set work correctly."

  (test<nub>:fixture
      ;; Test name, nub user, setup func, teardown func.
      "test<nub/utils>::int<nub>:var:debugging"
      :user/auto
      nil
      nil

    ;; Shouldn't be debugging yet.
    (should-not (int<nub>:var:debugging test<nub>:user))

    ;; Set to debugging.
    (int<nub>:var:debugging:set test<nub>:user t)
    (should (int<nub>:var:debugging test<nub>:user))

    ;; Toggle off and back on again.
    (int<nub>:var:debugging:set test<nub>:user :toggle)
    (should-not (int<nub>:var:debugging test<nub>:user))
    (int<nub>:var:debugging:set test<nub>:user :toggle)
    (should (int<nub>:var:debugging test<nub>:user))))


;;------------------------------
;; int<nub>:var:debug:tags
;;------------------------------

(ert-deftest test<nub/utils>::int<nub>:var:debug:tags ()
  "Test that `int<nub>:var:debug:tags' functions for init, get, and set work correctly."

  (test<nub>:fixture
      ;; Test name, nub user, setup func, teardown func.
      "test<nub/utils>::int<nub>:var:debug:tags"
      :user/auto
      nil
      nil

    ;;------------------------------
    ;; Shouldn't have any tags to start with.
    ;;------------------------------
    (should-not (int<nub>:var:debug:tags test<nub>:user))

    ;;------------------------------
    ;; Set some tags & check active.
    ;;------------------------------
    (let ((tags '(:foo :bar :baz)))
      (int<nub>:var:debug:tags:set test<nub>:user tags)
      (should (equal tags
                     (int<nub>:var:debug:tags test<nub>:user))))

    ;; Are the set tags active?
    (should (int<nub>:var:debug:tag:active? test<nub>:user :foo))
    (should (int<nub>:var:debug:tag:active? test<nub>:user :bar))
    (should (int<nub>:var:debug:tag:active? test<nub>:user :baz))
    ;; Are other tags not active?
    (should-not (int<nub>:var:debug:tag:active? test<nub>:user :qux))
    (should-not (int<nub>:var:debug:tag:active? test<nub>:user :quux))
    (should-not (int<nub>:var:debug:tag:active? test<nub>:user :quuux))

    ;;------------------------------
    ;; Toggle tags.
    ;;------------------------------

    ;; Disable foo
    (int<nub>:var:debug:tag:set test<nub>:user :foo nil)
    ;; Are the active states correct?
    (should-not (int<nub>:var:debug:tag:active? test<nub>:user :foo))
    (should (int<nub>:var:debug:tag:active? test<nub>:user :bar))
    (should (int<nub>:var:debug:tag:active? test<nub>:user :baz))
    (should-not (int<nub>:var:debug:tag:active? test<nub>:user :qux))

    ;; Toggle foo on.
    (int<nub>:var:debug:tag:set test<nub>:user :foo :toggle)
    ;; Are the active states correct?
    (should (int<nub>:var:debug:tag:active? test<nub>:user :foo))
    (should (int<nub>:var:debug:tag:active? test<nub>:user :bar))
    (should (int<nub>:var:debug:tag:active? test<nub>:user :baz))
    (should-not (int<nub>:var:debug:tag:active? test<nub>:user :qux))

    ;; Toggle foo off.
    (int<nub>:var:debug:tag:set test<nub>:user :foo :toggle)
    ;; Are the active states correct?
    (should-not (int<nub>:var:debug:tag:active? test<nub>:user :foo))
    (should (int<nub>:var:debug:tag:active? test<nub>:user :bar))
    (should (int<nub>:var:debug:tag:active? test<nub>:user :baz))
    (should-not (int<nub>:var:debug:tag:active? test<nub>:user :qux))

    ;; Toggle a new tag on.
    (int<nub>:var:debug:tag:set test<nub>:user :qux :toggle)
    ;; Are the active states correct?
    (should-not (int<nub>:var:debug:tag:active? test<nub>:user :foo))
    (should (int<nub>:var:debug:tag:active? test<nub>:user :bar))
    (should (int<nub>:var:debug:tag:active? test<nub>:user :baz))
    (should (int<nub>:var:debug:tag:active? test<nub>:user :qux))))


;;------------------------------
;; int<nub>:var:debug:tags/common
;;------------------------------

(ert-deftest test<nub/utils>::int<nub>:var:debug:tags/common ()
  "Test that `int<nub>:var:debug:tags/common' functions for init, get, and set work correctly."

  (test<nub>:fixture
      ;; Test name, nub user, setup func, teardown func.
      "test<nub/utils>::int<nub>:var:debug:tags/common"
      :user/auto
      nil
      nil

    ;;------------------------------
    ;; Shouldn't have any common tags to start with.
    ;;------------------------------
    (should-not (int<nub>:var:debug:tags/common test<nub>:user))

    ;;------------------------------
    ;; Set some common tags.
    ;;------------------------------
    (let ((tags '(:foo :bar :baz)))
      (int<nub>:var:debug:tags/common:set test<nub>:user tags)
      (should (equal tags
                     (int<nub>:var:debug:tags/common test<nub>:user))))))
