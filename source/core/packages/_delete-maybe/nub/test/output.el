;;; core/modules/output/nub/test/output.el --- Tests for "output.el" -*- no-byte-compile: t; lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-12-07
;; Timestamp:  2023-06-26
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Tests for "output.el"
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Requirements
;;------------------------------------------------------------------------------

(imp:test:load :filename "base.el")


;; ╔═════════════════════════════╤═══════════╤═════════════════════════════════╗
;; ╟─────────────────────────────┤ ERT TESTS ├─────────────────────────────────╢
;; ╠══════════════════════╤══════╧═══════════╧═══════╤═════════════════════════╣
;; ╟──────────────────────┤ Output: Errors et cetera ├─────────────────────────╢
;; ╚══════════════════════╧══════════════════════════╧═════════════════════════╝


;;------------------------------------------------------------------------------
;; Tests: Basics
;;------------------------------------------------------------------------------

;;---
;; NOTE: Test that the output redirection works first, so that we can use that in the rest of the output tests.
;;---

;;------------------------------
;; Output Redirection
;;------------------------------

(ert-deftest test<nub/output>::output-redirection ()
  "Test that our output redirection for testing works."

  ;; Squelch the actual output; still save to the test's output lists.
  (setq test<nub>:redirect/output:type nil)

  (test<nub>:fixture
      ;; Test name, setup & teardown func.
      "test<nub/output>::output-redirection"
      :user/auto
      nil
      nil

    ;; Make sure that they at least simply work.
    (let ((test-data '((:error   . "error output")
                       (:warning . "Warning Output")
                       (:debug   . "DEBUG OUTPUT"))))
      (dolist (data test-data)
        (nub:output test<nub>:user
                    (car data)
                    test-name
                    (cdr data))
        (test<nub>:assert:output test-name
                                 (car data)
                                 (list (list (cdr data))))))))


;;------------------------------------------------------------------------------
;; Tests: Functions
;;------------------------------------------------------------------------------

;;------------------------------
;; int<nub>:output:message
;;------------------------------

(ert-deftest test<nub/output>::int<nub>:output:message ()
  "Test that `int<nub>:output:message' sends output to the correct place based on verbosity."

  ;; Squelch the actual output; still save to the test's output lists.
  (setq test<nub>:redirect/output:type nil)

  ;;------------------------------
  ;; Test Actual Error signals error
  ;;------------------------------
  ;; Not using a registerd user should signal an error.
  (should-error (int<nub>:output:message :test-unregistered-user
                                         :error
                                         "hello there"
                                         nil))

  ;; Valid user at error output level should signal an error right now since
  ;; we haven't set up for unit testing yet.
  (should-error (int<nub>:output:message int<nub>:var:user:fallback
                                         :error
                                         "hello there"
                                         nil))

  ;;------------------------------
  ;; Test w/ Unit-Testing hooks in-place.
  ;;------------------------------
  (test<nub>:fixture
      ;; Test name, nub user, setup func, teardown func.
      "test<nub/output>::int<nub>:output:message"
      :user/auto
      nil
      nil

    ;; Each verbosity level should go to its separate list of intercepted messages.
    (let ((msg.error   "[ERROR   ]: Hello there.")
          (msg.warning "[WARNING ]: Hello there.")
          (msg.debug   "[   debug]: Hello there."))

      ;;------------------------------
      ;; Test Error
      ;;------------------------------
      ;; This should /not/ error this time.
      ;; We've squelched the normal output functions.
      (should (int<nub>:output:message test<nub>:user :error msg.error nil))

      ;; Testing error messages sink should have its message.
      (test<nub>:assert:output test-name :error msg.error)

      ;; Warning and debug should have nothing so far.
      (test<nub>:assert:output test-name :warning nil)
      (test<nub>:assert:output test-name :debug   nil)

      ;;------------------------------
      ;; Test Warning
      ;;------------------------------
      (should (int<nub>:output:message test<nub>:user :warning msg.warning nil))

      ;; Error & Warning should have their messages now.
      (test<nub>:assert:output test-name :error   msg.error)
      (test<nub>:assert:output test-name :warning msg.warning)
      ;; Debug should still have nothing.
      (test<nub>:assert:output test-name :debug nil)

      ;;------------------------------
      ;; Test Debug
      ;;------------------------------
      (should (int<nub>:output:message test<nub>:user :debug msg.debug nil))

      ;; All three should have their messages.
      (test<nub>:assert:output test-name :error   msg.error)
      (test<nub>:assert:output test-name :warning msg.warning)
      (test<nub>:assert:output test-name :debug   msg.debug))))


;;------------------------------
;; int<nub>:output:format
;;------------------------------

(ert-deftest test<nub/output>::int<nub>:output:format ()
  "Test that `int<nub>:output:format' creates a formatting message for error/warning/debug outputs."

  ;; Squelch the actual output; still save to the test's output lists.
  (setq test<nub>:redirect/output:type nil)

  (test<nub>:fixture
      ;; Test name, nub user, setup func, teardown func.
      "test<nub/output>::int<nub>:output:format"
      :user/auto
      nil
      nil

    ;;------------------------------
    ;; Test that it applies formatting to args.
    ;;------------------------------
    (let* ((fmt.args '("Hello, " "%S.")) ;; Some args for creating a formatting string.
           (expected.substrs (list "Hello, %S." ;; Expected formatting string still w/ '%S'.
                                   test-name))  ;; Expected caller name string.
           (formatted (apply #'int<nub>:output:format test<nub>:user :error test-name fmt.args)))
      ;; Should have got back some sort of string.
      (should formatted)
      (should (stringp formatted))

      ;; Should have our message and our test name in it somewhere.
      (dolist (expected expected.substrs)
        (should (string-match-p expected formatted))))

    ;;------------------------------
    ;; Test different prefixes.
    ;;------------------------------
    (should-not (string= (int<nub>:output:format test-name test<nub>:user :error "Hello, %s.")
                         (int<nub>:output:format test-name test<nub>:user :debug "Hello, %s.")))))


;;------------------------------
;; nub:output
;;------------------------------

(ert-deftest test<nub/output>::nub:output ()
  "Test that `nub:output' behaves."

  ;; Squelch the actual output; still save to the test's output lists.
  (setq test<nub>:redirect/output:type nil)

  (test<nub>:fixture
      ;; Test name, nub user, setup func, teardown func.
      "test<nub/output>::nub:output"
      :user/auto
      nil
      nil

    ;;------------------------------
    ;; Test Error Level
    ;;------------------------------
    (nub:output test<nub>:user
                :error
                test-name
                '("Hello " "%s... You have a minor case of severe erroring.")
                "there")

    (test<nub>:assert:output test-name
                             :error
                             ;; Expect one error message with:
                             ;;   - test-name
                             ;;   - formatted output message
                             (list (list test-name "Hello there... You have a minor case of severe erroring.")))

    ;;------------------------------
    ;; Test Warning Level
    ;;------------------------------
    (nub:output test<nub>:user
                :warning
                test-name
                "Hello %s; %s."
                "there"
                "this is your final warning")

    (test<nub>:assert:output test-name
                             :warning
                             ;; Expect one warning message with:
                             ;;   - test-name
                             ;;   - formatted output message
                             (list (list test-name "Hello there; this is your final warning.")))

    ;;------------------------------
    ;; Test Debug Level
    ;;------------------------------
    (nub:output test<nub>:user
                :debug
                test-name
                "I'm afraid I'm infested with bugs, %s..."
                "Dave")

    (test<nub>:assert:output test-name
                             :debug
                             ;; Expect one debug message with:
                             ;;   - test-name
                             ;;   - formatted output message
                             (list (list test-name "I'm afraid I'm infested with bugs, Dave...")))))
