;;; core/modules/output/nub/test/debug.el --- Tests for "debug.el" -*- no-byte-compile: t; lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-12-10
;; Timestamp:  2023-06-26
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Tests for "debug.el"
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Requirements
;;------------------------------------------------------------------------------

(imp:test:load :filename "base.el")


;;------------------------------------------------------------------------------
;; Test Helpers: Debug
;;------------------------------------------------------------------------------

;;---
;; NOTE: Try not to change any debugging data for existing users, including
;; the fallback user.
;;
;; Someone could be debugging something somewhere.
;;---

(defun test<nub/debug>:disable-debugging ()
  "Turn off debugging, delete tags."
  (when test<nub>:user
    (nub:debug:clear test<nub>:user)))


(defun test<nub/debug>:setup (_)
  "Get ready for debug test."
  ;; Reset our testing vars.
  (setq test<nub/debug>:called? nil)

  ;; Clear any debugging from last test run.
  (test<nub/debug>:disable-debugging))


(defun test<nub/debug>:teardown (_)
  "Any test clean-up you want to happen when test finishes."
  ;; Currently cleaning up in setup so I can examine vars after tests fail.
  )


(defvar test<nub/debug>:called? nil
  "Store how many times the `test<nub/debug>:call' has been called since
the test started.")


(defun test<nub/debug>:call ()
  "A function to call - if it was called, `test<nub/debug>:called?' will be non-nil.

Allows testing whether or not a parameter in a debug call is evaluated."
  ;; Set to 1 or increment by 1.
  (setq test<nub/debug>:called? (1+ (or test<nub/debug>:called? 0))))


;; ╔═════════════════════════════╤═══════════╤═════════════════════════════════╗
;; ╟─────────────────────────────┤ ERT TESTS ├─────────────────────────────────╢
;; ╠══╤══════════════════════════╧═══════════╧═════════════════════════════╤═══╣
;; ╟──┤ How do you debug debugging tests if testing debugging doesn't work?├───╢
;; ╚══╧════════════════════════════════════════════════════════════════════╧═══╝
;; Is the alignment of that fancy box bugged?
;;                         Is anything _not_ bugged?
;;                                                             ...Are you a bug?


;;------------------------------------------------------------------------------
;; Tests: Debug Evaluations
;;------------------------------------------------------------------------------

;; TODO: `int<nub>:var:debugging' is gone; remove from tests & replace.
;; TODO: var getter: (int<nub>:var:enabled? :test :debug :default)
;; TODO: var setter: (int<nub>:var:enabled?:set :test :debug t/nil/:toggle)
;; TODO: debug helper is still: (int<nub>:debug:active? caller user tags)

(ert-deftest test<nub/debug>::debug-evals ()
  "Test that the debug macros do not evaluate inputs unless debugging
(w/ correct tags)."
  ;; Squelch the actual output; still save to the test's output lists.
  (setq test<nub>:redirect/output:type nil)

  (test<nub>:fixture
      ;; Test name, nub user, setup func, teardown func.
      "test<nub/debug>::debug-evals"
      :user/auto
      #'test<nub/debug>:setup
      #'test<nub/debug>:teardown
    (should-not test<nub/debug>:called?)

    ;;------------------------------
    ;; Not debugging -> args not evaluated.
    ;;------------------------------
    ;; First gotta make sure we are not debugging.
    (should-not (int<nub>:var:debugging test<nub>:user))
    (should-not (int<nub>:var:debug:tags test<nub>:user))

    ;; And `test<nub/debug>:call' hasn't been called yet.
    (should-not test<nub/debug>:called?)

    (let ((called?/expected nil)
          ;; Check a for a lot of tags? No `:testing' to start with.
          (debug:tags/checking '(:init
                                 :start-up
                                 :construction
                                 :ignition
                                 :launch
                                 :begin

                                 :run

                                 :uninit
                                 :tear-down
                                 :deconstruction
                                 :rapid-unplanned-deconstruction
                                 :smash-landing
                                 :end))
          (debug:tags/using '(:testing)))

      ;;---
      ;; Debug disabled, so expectd this to not eval.
      ;;---
      (should-not (int<nub>:var:debugging test<nub>:user))
      (should-not (int<nub>:var:debug:tags test<nub>:user))
      (nub:debug
          test<nub>:user
          test-name
          debug:tags/using
        "#00: Hello there!"
        (test<nub/debug>:call))
      (should-not test<nub/debug>:called?)

      ;;---
      ;; Enable debug and now it should be called.
      ;;---
      (int<nub>:var:debugging:set test<nub>:user t)
      (should (int<nub>:var:debugging test<nub>:user))
      (should-not (int<nub>:var:debug:tags test<nub>:user))
      ;; Debug should be enabled globally since we set the flag and haven't set any tags.
      (setq called?/expected 1)
      (nub:debug
          test<nub>:user
          test-name
          debug:tags/using
        "#01: Hello there!"
        (test<nub/debug>:call))
      (should (= called?/expected
                 test<nub/debug>:called?))

      ;;---
      ;; Turn on our tags - it should /not/ be evaluted as the `:testing' tag isn't in the list.
      ;;---
      (int<nub>:var:debug:tags:set test<nub>:user debug:tags/checking)
      (int<nub>:var:debugging:set  test<nub>:user t)
      (should (int<nub>:var:debugging test<nub>:user))
      ;; Should have exactly the long list of tags we just set.
      (should-not (seq-difference (int<nub>:var:debug:tags test<nub>:user)
                                  debug:tags/checking))
      ;; Should not have the `:testing' tag.
      (should-not (seq-intersection debug:tags/using (int<nub>:var:debug:tags test<nub>:user)))
      (should (int<nub>:var:debugging test<nub>:user)) ;; We are debugging something.
      (should-not (nub:debug:debugging? test<nub>:user debug:tags/using)) ;; We are not debugging the `:testing` tag.

      ;; Should not be called, so same expected as before.
      ;; (setq called?/expected 1)
      (nub:debug
          test<nub>:user
          test-name
          debug:tags/using
        "#02: Hello there!"
        (test<nub/debug>:call))
      (should (= called?/expected
                 test<nub/debug>:called?))

      ;;---
      ;; Add `:testing' tag to list and it should be called again.
      ;;---
      (int<nub>:var:debug:tags:set test<nub>:user (seq-concatenate 'list debug:tags/checking debug:tags/using))
      (int<nub>:var:debugging:set  test<nub>:user t)
      (should (seq-intersection debug:tags/using (int<nub>:var:debug:tags test<nub>:user)))
      (should (int<nub>:var:debugging test<nub>:user))
      (should (nub:debug:debugging? test<nub>:user debug:tags/using))

      ;; Should be called, so expected+1.
      (setq called?/expected (1+ called?/expected))
      (nub:debug
          test<nub>:user
          test-name
          debug:tags/using
        "#03: Hello there!"
        (test<nub/debug>:call))
      (should (= called?/expected
                 test<nub/debug>:called?)))))


;;------------------------------------------------------------------------------
;; Tests: Functions
;;------------------------------------------------------------------------------

;;------------------------------
;; int<nub>:debug:tags:verify
;;------------------------------

(ert-deftest test<nub/debug>::int<nub>:debug:tags:verify ()
  "Test that `int<nub>:debug:tags:verify' returns the correct answer."

  (test<nub>:fixture
      ;; Test name, nub user, setup func, teardown func.
      "test<nub/debug>::int<nub>:debug:tags:verify"
      :user/auto
      #'test<nub/debug>:setup
      #'test<nub/debug>:teardown

    ;;------------------------------
    ;; Error Cases: Signal Errror & Return `nil'
    ;;------------------------------
    ;; `nil' is not valid as tags.
    (should-error (int<nub>:debug:tags:verify test-name
                                              test<nub>:user
                                              nil
                                              :error))
    (should-not (int<nub>:debug:tags:verify test-name
                                            test<nub>:user
                                            nil))
    ;; `nil' is not valid as /a/ tag.
    (should-error (int<nub>:debug:tags:verify test-name
                                              test<nub>:user
                                              '(nil)
                                              :error))
    (should-not (int<nub>:debug:tags:verify test-name
                                            test<nub>:user
                                            '(nil)))
    ;; Tags must be a list.
    (should-error (int<nub>:debug:tags:verify test-name
                                              test<nub>:user
                                              :valid-tag
                                              :error))
    (should-not (int<nub>:debug:tags:verify test-name
                                            test<nub>:user
                                            :valid-tag))
    ;; Other invalid tags:
    (should-error (int<nub>:debug:tags:verify test-name
                                              test<nub>:user
                                              '(symbol-name)
                                              :error))
    (should-not (int<nub>:debug:tags:verify test-name
                                            test<nub>:user
                                            '(symbol-name)))
    (should-error (int<nub>:debug:tags:verify test-name
                                              test<nub>:user
                                              '("string")
                                              :error))
    (should-not (int<nub>:debug:tags:verify test-name
                                            test<nub>:user
                                            '("string")))

    ;;------------------------------
    ;; Valid Cases: Return input tags.
    ;;------------------------------
    (let ((tags '(:foo)))
      (should (eq tags
                  (int<nub>:debug:tags:verify test-name
                                              test<nub>:user
                                              tags)))

      (setq tags '(:foo :bar))
      (should (eq tags
                  (int<nub>:debug:tags:verify test-name
                                              test<nub>:user
                                              tags))))))


;;------------------------------
;; int<nub>:debug:active?
;;------------------------------

(ert-deftest test<nub/debug>::int<nub>:debug:active? ()
  "Test that `int<nub>:debug:active?' returns the correct answer."

  (test<nub>:fixture
      ;; Test name, nub user, setup func, teardown func.
     "test<nub/debug>::int<nub>:debug:active?"
      :user/auto
      #'test<nub/debug>:setup
      #'test<nub/debug>:teardown

      (let ((debug:tags/using '(:testing)))

        ;;------------------------------
        ;; Not debugging - always no.
        ;;------------------------------
        (should-not (int<nub>:var:debugging test<nub>:user))
        (should-not (int<nub>:var:debug:tags test<nub>:user))

        (should-not (int<nub>:debug:active? test-name test<nub>:user debug:tags/using))
        (should-not (int<nub>:debug:active? test-name test<nub>:user nil))

        ;; Set the tag filter and still not debugging.
        (int<nub>:var:debug:tags:set test<nub>:user debug:tags/using)
        (should (equal (int<nub>:var:debug:tags test<nub>:user) debug:tags/using))

        (should-not (int<nub>:debug:active? test-name test<nub>:user debug:tags/using))
        (should-not (int<nub>:debug:active? test-name test<nub>:user nil))

        ;;------------------------------
        ;; Debugging - depends on tags.
        ;;------------------------------

        ;; Reset tag filter and enable debugging.
        (int<nub>:var:debug:tags:set test<nub>:user nil)
        (int<nub>:var:debugging:set test<nub>:user t)
        (should (int<nub>:var:debugging test<nub>:user))
        (should-not (int<nub>:var:debug:tags test<nub>:user))

        ;; No tag filter set so should always be true now.
        (should (int<nub>:debug:active? test-name test<nub>:user debug:tags/using))
        (should (int<nub>:debug:active? test-name test<nub>:user '(:unused :another-one)))
        (should (int<nub>:debug:active? test-name test<nub>:user nil))

        ;; Add a filter and now it depends on input tags.
        (int<nub>:var:debug:tags:set test<nub>:user debug:tags/using)
        (should (int<nub>:var:debugging test<nub>:user))
        (should (equal (int<nub>:var:debug:tags test<nub>:user) debug:tags/using))

        ;; Active for this specific tag.
        (should (int<nub>:debug:active? test-name test<nub>:user debug:tags/using))
        ;; Not active for those specific tags.
        (should-not (int<nub>:debug:active? "test-name" test<nub>:user '(:unused :another-one)))
        ;; Active for some tags.
        (should (int<nub>:debug:active? test-name test<nub>:user nil)))))


;;------------------------------
;; nub:debug
;;------------------------------

(ert-deftest test<nub/debug>::nub:debug ()
  "Test that `nub:debug' functions correctly w/ debug toggle & tags."

  (test<nub>:fixture
      ;; Test name, nub user, setup func, teardown func.
      "test<nub/debug>::nub:debug"
      :user/auto
      #'test<nub/debug>:setup
      #'test<nub/debug>:teardown

      ;;===
      ;; Run the test.
      ;;===
      (let* ((debug:tags/using '(:should-trigger-debug-message))
             (debug:tags/not-using '(:NO-DEBUG-MESSAGE))
             (debug:tags/assert (lambda (should-be-debugging?)
                                  ;; May or may not be expecting the 'using' tag.
                                  (if should-be-debugging?
                                      (should (int<nub>:debug:active? test-name test<nub>:user debug:tags/using))
                                    (should-not (int<nub>:debug:active? test-name test<nub>:user debug:tags/using)))
                                  ;; Never expect the 'not-using' tag.
                                  (should-not (int<nub>:debug:active? test-name test<nub>:user debug:tags/not-using))))
             (debug:messages nil)
             (debug:call/number 0)
             (debug:call/number:message nil)
             (debug:call/number:fmt (lambda (expected?)
                                      (let ((msg (format "#%02d: Hello there."
                                                         debug:call/number)))
                                        (setq debug:call/number:message msg)
                                        (when expected?
                                          (push (list msg) debug:messages))))))

        ;;------------------------------
        ;; Not debugging - no output.
        ;;------------------------------
        (should-not (int<nub>:var:debugging test<nub>:user))
        (should-not (int<nub>:var:debug:tags test<nub>:user))
        (should-not test<nub/debug>:called?)
        (test<nub>:assert:output test-name :debug nil)
        (funcall debug:tags/assert nil)

        ;; We should not get any output right now as we're not debugging.
        (funcall debug:call/number:fmt nil)
        (nub:debug
            test<nub>:user
            test-name
            debug:tags/using
            debug:call/number:message)
        (setq debug:call/number (1+ debug:call/number))

        (should-not debug:messages)
        (test<nub>:assert:output test-name :debug debug:messages)

        ;; We should always get an error when calling without any tags.
        (funcall debug:call/number:fmt nil)
        (should-error
         (nub:debug
             test<nub>:user
             test-name
             nil
             debug:call/number:message))
        (setq debug:call/number (1+ debug:call/number))

        (should-not debug:messages)
        (test<nub>:assert:output test-name :debug debug:messages)

        ;;------------------------------
        ;; Debugging - depends on tags.
        ;;------------------------------

        ;; Enable debugging without filtering tags.
        (int<nub>:var:debug:tags:set test<nub>:user nil)
        (int<nub>:var:debugging:set  test<nub>:user t)
        (should (int<nub>:var:debugging test<nub>:user))
        (should-not (int<nub>:var:debug:tags test<nub>:user))
        (test<nub>:assert:output test-name :debug nil)
        ;; Can't check this yet - no tags filter so everything is a 'yes'.
        ;; (funcall debug:tags/assert nil)

        ;; No tag filter set so should always log the debug message.
        (funcall debug:call/number:fmt :expected)
        (nub:debug
            test<nub>:user
            test-name
            debug:tags/using
            debug:call/number:message)

        (setq debug:call/number (1+ debug:call/number))
        (should debug:messages)
        (should (listp debug:messages))
        (should (= 1 (length debug:messages)))
        (test<nub>:assert:output test-name :debug debug:messages)

        ;; Add a filter and now it depends on input tags.
        (int<nub>:var:debug:tags:set test<nub>:user debug:tags/using)
        (should (int<nub>:var:debugging test<nub>:user))
        (should (equal (int<nub>:var:debug:tags test<nub>:user)
                       debug:tags/using))
        (funcall debug:tags/assert :debugging-enabled)

        ;; Use a debug tag not in our filter - no debug message.
        (funcall debug:call/number:fmt nil)
        (nub:debug
            test<nub>:user
            test-name
            debug:tags/not-using
            debug:call/number:message)
        (setq debug:call/number (1+ debug:call/number))
        ;; Same as before - no additional message output.
        (should debug:messages)
        (should (listp debug:messages))
        (should (= 1 (length debug:messages)))
        (test<nub>:assert:output test-name :debug debug:messages)

        ;; Use our input tag - expect debug message.
        (funcall debug:call/number:fmt :expected)
        (nub:debug
            test<nub>:user
            test-name
            debug:tags/using
            debug:call/number:message)
        (setq debug:call/number (1+ debug:call/number))
        ;; Same as before - no additional message output.
        (should debug:messages)
        (should (listp debug:messages))
        (should (= 2 (length debug:messages)))
        (test<nub>:assert:output test-name :debug debug:messages))))


;;------------------------------
;; nub:debug-or-message
;;------------------------------

(ert-deftest test<nub/debug>::nub:debug-or-message ()
  "Test that `nub:debug-or-message' functions correctly w/ debug toggle & tags."

  (test<nub>:fixture
      ;; Test name, nub user, setup func, teardown func.
      "test<nub/debug>::int<nub>:debug"
      :user/auto
      #'test<nub/debug>:setup
      #'test<nub/debug>:teardown

      ;;===
      ;; Run the test.
      ;;===
      (let* ((debug:tags/using '(:should-trigger-debug-message))
             (debug:tags/not-using '(:NO-DEBUG-MESSAGE))
             (debug:tags/assert (lambda (should-be-debugging?)
                                  ;; May or may not be expecting the 'using' tag.
                                  (if should-be-debugging?
                                      (should (int<nub>:debug:active? test-name test<nub>:user debug:tags/using))
                                    (should-not (int<nub>:debug:active? test-name test<nub>:user debug:tags/using)))
                                  ;; Never expect the 'not-using' tag.
                                  (should-not (int<nub>:debug:active? test-name test<nub>:user debug:tags/not-using))))
             (debug:messages nil)
             (debug:messages/expected 0)
             (debug:call/number 0)
             (debug:call/number:message nil)
             (debug:call/number:fmt (lambda (expected?)
                                      (let ((msg (format "#%02d: Hello there."
                                                         debug:call/number)))
                                        (setq debug:call/number:message msg)
                                        (when expected?
                                          (push (list msg) debug:messages)))))
             (debug:allow-messages? nil))

        ;;------------------------------------------------------------------------
        ;; Firstly: Should act exactly the same as `int<nub>:debug' when `message?' is nil.
        ;;------------------------------------------------------------------------

        ;;------------------------------
        ;; Not debugging - no output.
        ;;------------------------------
        (should-not (int<nub>:var:debugging test<nub>:user))
        (should-not (int<nub>:var:debug:tags test<nub>:user))
        (should-not test<nub/debug>:called?)
        (test<nub>:assert:output test-name :debug nil)
        (funcall debug:tags/assert nil)

        ;; We should not get any output right now as we're not debugging.
        (funcall debug:call/number:fmt nil)
        (nub:debug-or-message
            test<nub>:user
            test-name
            debug:tags/using
            debug:allow-messages?
          debug:call/number:message)
        (setq debug:call/number (1+ debug:call/number))

        (should-not debug:messages)
        (test<nub>:assert:output test-name :debug debug:messages)

        ;; We should always get an error when calling without any tags.
        (funcall debug:call/number:fmt nil)
        (should-error
         (nub:debug-or-message
             test<nub>:user
             test-name
             nil
             debug:allow-messages?
             debug:call/number:message))
        (setq debug:call/number (1+ debug:call/number))

        (should-not debug:messages)
        (test<nub>:assert:output test-name :debug debug:messages)

        ;;------------------------------
        ;; Debugging - depends on tags.
        ;;------------------------------

        ;; Enable debugging without filtering tags.
        (int<nub>:var:debug:tags:set test<nub>:user nil)
        (int<nub>:var:debugging:set  test<nub>:user t)
        (should (int<nub>:var:debugging test<nub>:user))
        (should-not (int<nub>:var:debug:tags test<nub>:user))
        (test<nub>:assert:output test-name :debug nil)
        ;; Can't check this yet - no tags filter so everything is a 'yes'.
        ;; (funcall debug:tags/assert nil)

        ;; No tag filter set so should always log the debug message.
        (funcall debug:call/number:fmt :expected)
        (nub:debug-or-message
            test<nub>:user
            test-name
            debug:tags/using
            debug:allow-messages?
            debug:call/number:message)
        (setq debug:call/number (1+ debug:call/number)
              debug:messages/expected (1+ debug:messages/expected))
        (should debug:messages)
        (should (listp debug:messages))
        (should (= debug:messages/expected (length debug:messages)))
        (test<nub>:assert:output test-name :debug debug:messages)

        ;; Add a filter and now it depends on input tags.
        (int<nub>:var:debug:tags:set test<nub>:user debug:tags/using)
        (should (int<nub>:var:debugging test<nub>:user))
        (should (equal (int<nub>:var:debug:tags test<nub>:user)
                       debug:tags/using))
        (funcall debug:tags/assert :debugging-enabled)

        ;; Use a debug tag not in our filter - no debug message.
        (funcall debug:call/number:fmt nil)
        (nub:debug-or-message
            test<nub>:user
            test-name
            debug:tags/not-using
            debug:allow-messages?
            debug:call/number:message)
        (setq debug:call/number (1+ debug:call/number))
        ;; Same as before - no additional message output.
        (should debug:messages)
        (should (listp debug:messages))
        (should (= debug:messages/expected (length debug:messages)))
        (test<nub>:assert:output test-name :debug debug:messages)

        ;; Use our input tag - expect debug message.
        (funcall debug:call/number:fmt :expected)
        (nub:debug-or-message
            test<nub>:user
            test-name
            debug:tags/using
            debug:allow-messages?
            debug:call/number:message)
        (setq debug:call/number (1+ debug:call/number)
              debug:messages/expected (1+ debug:messages/expected))
        (should debug:messages)
        (should (listp debug:messages))
        (should (= debug:messages/expected (length debug:messages)))
        (test<nub>:assert:output test-name :debug debug:messages)

        ;;------------------------------------------------------------------------
        ;; Secondly: Should output debug message regardless of debugging toggle/flags `message?' is non-nil.
        ;;------------------------------------------------------------------------

        ;; Enable messaging output. Actual callers of `nub:debug-or-message' will use a conditional in the macro call,
        ;; but we are just testing so: set to a constant.
        (setq debug:allow-messages? :message)

        ;; Enable debugging without filtering tags.
        (int<nub>:var:debug:tags:set test<nub>:user nil)
        (int<nub>:var:debugging:set  test<nub>:user t)
        (should (int<nub>:var:debugging test<nub>:user))
        (should-not (int<nub>:var:debug:tags test<nub>:user))

        ;; Always outputs.
        (funcall debug:call/number:fmt :expected)
        (nub:debug-or-message
            test<nub>:user
            test-name
            debug:tags/using
            debug:allow-messages?
            debug:call/number:message)
        (setq debug:call/number (1+ debug:call/number)
              debug:messages/expected (1+ debug:messages/expected))
        (should debug:messages)
        (should (listp debug:messages))
        (should (= debug:messages/expected (length debug:messages)))
        (test<nub>:assert:output test-name :debug debug:messages)

        ;; Add a filter and... it always outputs still.
        (int<nub>:var:debug:tags:set test<nub>:user debug:tags/using)
        (should (int<nub>:var:debugging test<nub>:user))
        (should (equal (int<nub>:var:debug:tags test<nub>:user)
                       debug:tags/using))
        (funcall debug:tags/assert :debugging-enabled)

        ;; Always outputs.
        (funcall debug:call/number:fmt :expected)
        (nub:debug-or-message
            test<nub>:user
            test-name
            debug:tags/not-using
            debug:allow-messages?
            debug:call/number:message)
        (setq debug:call/number (1+ debug:call/number)
              debug:messages/expected (1+ debug:messages/expected))
        (should debug:messages)
        (should (listp debug:messages))
        (should (= debug:messages/expected (length debug:messages)))
        (test<nub>:assert:output test-name :debug debug:messages)

        ;; Use our input tag and... it always outputs still.
        (funcall debug:call/number:fmt :expected)
        (nub:debug-or-message
            test<nub>:user
            test-name
            debug:tags/using
            debug:allow-messages?
            debug:call/number:message)
        (setq debug:call/number (1+ debug:call/number)
              debug:messages/expected (1+ debug:messages/expected))
        ;; Same as before - no additional message output.
        (should debug:messages)
        (should (listp debug:messages))
        (should (= debug:messages/expected (length debug:messages)))
        (test<nub>:assert:output test-name :debug debug:messages))))
