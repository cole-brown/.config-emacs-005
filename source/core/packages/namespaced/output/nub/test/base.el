;;; core/modules/output/nub/test/base.el --- Test Helpers -*- no-byte-compile: t; lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-10-21
;; Timestamp:  2023-08-16
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Test Helpers
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Base Functionality for Testing
;;------------------------------------------------------------------------------

;; Load our files.
(imp:load :feature  '(:nub alist)
          :filename "../alist")
(imp:load :feature  '(:nub utils)
          :filename "../utils")
(imp:load :feature  '(:nub variables)
          :filename "../variables")
(imp:load :feature  '(:nub output)
          :filename "../output")
(imp:load :feature  '(:nub debug)
          :filename "../debug")


;;------------------------------------------------------------------------------
;; Constants & Variables
;;------------------------------------------------------------------------------

(defvar test<nub>:user nil
  "User that the set-up/tear-down/etc will use.")


(defvar test<nub>:user:history nil
  "A list of all the users we have run tests using.")


(defvar test<nub>:output:error nil
  "A list of `:error' output messages if we are stealing `:error' verbosity.")


(defvar test<nub>:output:warning nil
  "A list of `:warning' output messages if we are stealing `:warning' verbosity.")
;; test<nub>:output:warning
;; (length test<nub>:output:warning)


(defvar test<nub>:output:debug nil
  "A list of `:debug' output messages if we are stealing `:debug' verbosity.
NOTE: Does not include /test/ debug messages - just the normal nub
debugging messages.")


;;------------------------------
;; Per-Test-Suite
;;------------------------------

(defvar test<nub>:suite:func/setup nil
  "Set-up function to run for the current testing suite.")


(defvar test<nub>:suite:func/teardown nil
  "Tear-down function to run for the current testing suite.")


(defvar test<nub>:should:marker/counter 0
  "Counter for `test<nub>:should:marker'.")


(defvar test<nub>:redirect/output:type:default t
  "Default for how to redirect output when testing.

nil     - Squelch outputs/error signals.
        - Save all outputs to lists.
:error  - Squelch warning/debug.
        - Allow error signals.
        - Save all outputs to lists.
t       - Allow all outputs to output normally.
        - Allow error signals.
        - Save all outputs to lists.")


(defvar test<nub>:redirect/output:type
  test<nub>:redirect/output:type:default
  "How to redirect output when testing.

Gets reset to `test<nub>:redirect/output:type:default' at the end of each test.

nil     - Squelch outputs/error signals.
        - Save all outputs to lists.
:error  - Squelch warning/debug.
        - Allow error signals.
        - Save all outputs to lists.
t       - Allow all outputs to output normally.
        - Allow error signals.
        - Save all outputs to lists.")


;;------------------------------
;; "ERT List of Should Forms" buffer help
;;------------------------------

(defun test<nub>:should:marker (test-name &rest args)
  "Put a `should' in the test which will evaluate to some searchable output for
marking where in a test you are.

Search for \"[MARK-\[0-9\]+]:\"."
  ;;---
  ;; Big noticeable banner.
  ;;---
  (let ((fmt/str (concat "\n"
                         "╔═════════════════════════════════════╗\n"
                         "╠══╣            MARK-%02d            ╠══╣\n"
                         "╚═╤═══════════════════════════════════╝\n"))
        formatted)

    ;;---
    ;; Test's name and the `args' to print.
    ;;---
    (if (null args)
        ;; No extra stuff in `fmt/str` when no ARGS.
        (setq fmt/str (concat "  └──┤ " test-name "\n"))

      ;; `test-name' and the args.
      (setq fmt/str (concat fmt/str
                            "  │\n"
                            "  ├──┤ " test-name "\n"
                            "  │\n"
                            "  └────┤ "
                            ;; String formatting if just a str, else "whatever" formatting.
                            (cond ((and (= 1 (length args))
                                        (stringp (nth 0 args)))
                                   "%s")
                                  (t
                                   "%S"))
                            "\n")))

    ;;---
    ;; Output
    ;;---

    ;; Eval string first so it's cleaner in the list of asserts.
    (setq formatted (apply #'format fmt/str
                           test<nub>:should:marker/counter
                           args))

    ;; Increment counter for next time.
    (setq test<nub>:should:marker/counter
          (1+ test<nub>:should:marker/counter))

    ;; Assert the string so it ends up in the list of asserts.
    (should formatted)))


;;------------------------------------------------------------------------------
;; Test Helpers: Users
;;------------------------------------------------------------------------------

(defun test<nub>:user:make (name)
  "Create a user based on test name."
  (intern (concat ":" name)))


(defun test<nub>:user:clear ()
  "Remove the previous test's user from nub.

This should be done at the start of a test so nub user, vars, state, etc can
be manually inspected after a test is run."
  (when test<nub>:user
    (nub:vars:terminate test<nub>:user)))


(defun test<nub>:user:set (user)
  "Set `test<nub>:user' and update `test<nub>:user:history'."
  (setq test<nub>:user user)
  (push user test<nub>:user:history))


(defun test<nub>:assert:users ()
  "Assert that only the current test user exists in `nub'."
  (should test<nub>:user)
  (should (memq test<nub>:user test<nub>:user:history))

  ;; None of the previous users should be in nub.
  (dolist (prev-user test<nub>:user:history)
    (unless (eq prev-user test<nub>:user)
      (should-not (int<nub>:user:exists? "test<nub>:assert:users"
                                         prev-user
                                         nil))))

  ;; Current user should exist, though.
  (should (int<nub>:user:exists? "test<nub>:assert:users"
                                 test<nub>:user
                                 nil)))


;;------------------------------------------------------------------------------
;; Test Helpers: Output
;;------------------------------------------------------------------------------

;;------------------------------
;; Sink Functions for Output.
;;------------------------------

(defun test<nub>:redirect/output:error (msg &rest args)
  "Steals all calls to `int<nub>:output' for `:error' level and puts them
into `test<nub>:output:error' list instead."
  (push (apply #'format msg args) test<nub>:output:error))


(defun test<nub>:redirect/output:warning (msg &rest args)
  "Steals all calls to `int<nub>:output' for `:warning' level and puts them
into `test<nub>:output:warning' list instead."
  (push (apply #'format msg args) test<nub>:output:warning))
;; (test<nub>:redirect/output:warning "hello %s" "there")


(defun test<nub>:redirect/output:debug (msg &rest args)
  "Steals all calls to `int<nub>:output' for `:debug' level and puts them
into `test<nub>:output:debug' list instead."
  (push (apply #'format msg args) test<nub>:output:debug))


(defconst test<nub>:redirect/output:sinks
  ;;------------------------------
  ;; Always direct to the test output sink lists!
  ;;------------------------------
  '(;; Allow to output as usual.
    (t   . ((:error   . (test<nub>:redirect/output:error   :default))
            (:warning . (test<nub>:redirect/output:warning :default))
            (:debug   . (test<nub>:redirect/output:debug   :default))))

    ;; Allow errors; squelch warning & debugs.
    (:errors . ((:error   . (test<nub>:redirect/output:error :default))
                (:warning . test<nub>:redirect/output:warning)
                (:debug   . test<nub>:redirect/output:debug)))

    ;; Squelch messages (only save to our lists).
    (nil . ((:error   . test<nub>:redirect/output:error)
            (:warning . test<nub>:redirect/output:warning)
            (:debug   . test<nub>:redirect/output:debug))))
  "Direct the `nub' output messages based on `test<nub>:redirect/output:type'.")


;;------------------------------
;; Set-Up / Tear-Down
;;------------------------------

(defun test<nub>:redirect/output:setup ()
  "Steals all calls to `int<nub>:output' and puts them into `test<nub>:output:...' lists instead.

Can also allow the normal output based on `test<nub>:redirect/output:type'"
  (nub:vars:init test<nub>:user
                 :path:root              nil ; String or nil ("this directory")
                 :list:debug:tags/common nil ; List: Common tags.
                 :alist:prefixes         nil ; Alist: Message prefixes.
                 :alist:enabled?         nil ; Alist: Enabled per level.
                 ;; Alist: Sinks.
                 ;;   - Decide based on `test<nub>:redirect/output:type'.
                 :alist:sinks (alist-get test<nub>:redirect/output:type
                                         test<nub>:redirect/output:sinks)))


(defun test<nub>:redirect/output:teardown ()
  "Removes our output override."
  (nub:vars:reset test<nub>:user))


(defun test<nub>:assert:output (caller level should-be)
  "Assert that LEVEL outputs were/were not issued during the test.

LEVEL should be one of: (:error :warning :debug)

SHOULD-BE can be:
  - a number
    + Number of LEVEL outputs must match this.
    + USING
  - a string
    + Exactly 1 LEVEL output messages.
    + The LEVEL message must match SHOULD-BE.
    + USING
  - a list of lists of strings
    + A list-of-strings is a list of expected substrings in a LEVEL output message.
    + List-of-strings #1 should be for message #1, list-of-strings #2 for message #2, etc.
  - nil/falsy
    + LEVEL should have no output messages."
  (let* ((outputs (pcase level
                    (:error   test<nub>:output:error)
                    (:warning test<nub>:output:warning)
                    (:debug   test<nub>:output:debug)
                    (_
                     (error "test<nub>:assert:output: Unknown level '%S'. Caller: %S"
                            level caller))))
         (func/assert-list (lambda ()
                             "Assert that the output list exists/is non-nil."
                             (should outputs)
                             (should (listp outputs)))))
    ;; See what level is when debugging.
    (test<nub>:should:marker caller
                             (format "level: %S, should-be: %S" level should-be))

    (cond
     ;;---
     ;; Falsy: Nothing should exist.
     ;;---
     ;; NOTE: Must be before listp since `nil' is a list.
     ((not should-be)
      (should-not outputs))

     ;;---
     ;; Number: Exactly that amount of messages in the output list.
     ;;---
     ((numberp should-be)
      ;; Zero is a special case; don't want to assert `outputs' exists.
      (when (> 0 should-be)
        (funcall func/assert-list))
      (should (= should-be
                 (length outputs))))

     ;;---
     ;; String: Should be exactly one message in the output list and it should equal this string.
     ;;---
     ((stringp should-be)
      (funcall func/assert-list)
      (should (= 1
                 (length outputs)))
      (should (string= should-be
                       (nth 0 outputs))))

     ;;---
     ;; List (of lists of strings): match substrings in output list.
     ;;---
     ((listp should-be) ;; Suck all lists into this case, then validate.
      ;;---
      ;; Must be valid list of lists of strings.
      ;;---
      ;; Should have the correct number of substring lists for the messages.
      (should (> (length should-be) 0))
      ;; Each element in list should be a sub-list...
      (should (-all? #'listp should-be))
      ;; ...and each element in each sub-list should be a string.
      (should (-all? (lambda (sublist) (-all? #'stringp sublist))
                     should-be))

      ;; Outputs should be valid and should match expected substring lists in length.
      (funcall func/assert-list)
      (should (= (length should-be)
                 (length outputs)))

      ;; Should find all the expected substrings in the expected message.
      (dotimes (i (length should-be)) ;; For each message...
        (let ((substrings (nth i should-be))
              (output (nth i outputs)))
          (dotimes (j (length substrings)) ;; For each expected substring...
            (should (string-match-p (nth j substrings) output))))))

     ;;---
     ;; Didn't match anything expected.
     ;;---
     (t
      (error "test<nub>:assert:output: Unknown SHOULD-BE: %S" should-be)))))
;; (test<nub>:assert:output "test" :warning nil)


;;------------------------------------------------------------------------------
;; Test Fixture (Set-Up / Tear-Down) Functions
;;------------------------------------------------------------------------------

;;------------------------------
;; Per-Test-Suite
;;------------------------------

(defun test<nub>:setup/suite (user func/setup func/teardown)
  "Hook in any per-suite set-up/tear-down functions.

FUNC/SETUP and FUNC/TEARDOWN should take args:
  - string: NAME - the test's name

FUNC/SETUP will run as last step in set-up.
FUNC/TEARDOWN will run as first step in tear-down."
  ;; Reset nub user for new test.
  (test<nub>:user:clear)
  (test<nub>:user:set user)

  (setq test<nub>:suite:func/setup    func/setup
        test<nub>:suite:func/teardown func/teardown))


;;------------------------------
;; Set-Up
;;------------------------------

(defun test<nub>:setup/vars ()
  "Any setup of consts/vars needed per test."
  ;; Reset these at start of test so they can be manually inspected after a test is run.
  (setq test<nub>:output:error   nil)
  (setq test<nub>:output:warning nil)
  (setq test<nub>:output:debug   nil)
  (setq test<nub>:should:marker/counter 0))


(defun test<nub>:setup (name user func/setup func/teardown)
  "Run setup for test named NAME.

FUNC/SETUP and FUNC/TEARDOWN will be run during set-up/tear-down if provided."
  ;; Create user from test name if no user provided.
  (test<nub>:setup/suite (if (or (null user)
                                 (eq user :user/auto))
                             (test<nub>:user:make name)
                           user)
                         func/setup
                         func/teardown)

  (test<nub>:setup/vars)
  (test<nub>:redirect/output:setup)


  ;; Always last:
  (when test<nub>:suite:func/setup
    (unwind-protect
        (funcall test<nub>:suite:func/setup name))))


;;------------------------------
;; Tear-Down
;;------------------------------

(defun test<nub>:teardown/vars ()
  "Clear out/clean up vars used during testing so next test or normal Emacs
usage isn't affected."

  ;; Reset our flag for how to direct output messages / error signals.
  (setq test<nub>:redirect/output:type test<nub>:redirect/output:type:default)

  ;;------------------------------
  ;; `test<nub>:output:...'
  ;;------------------------------
  ;; Reset these vars at set-up so they can be inspected after a test is run.
  ;; Do nothing to them here.
  )


(defun test<nub>:teardown (name)
  "Run teardown for tests."
  ;; Always first in actual tear-down:
  (when test<nub>:suite:func/teardown
    (unwind-protect
        (funcall test<nub>:suite:func/teardown name)))

  ;; Generally in reverse order from set-up.
  (test<nub>:redirect/output:teardown)
  (test<nub>:teardown/vars))


;;------------------------------
;; Test Fixture Macro
;;------------------------------

(defmacro test<nub>:fixture (name user func/setup func/teardown &rest body)
  "Run `test<nub>:setup', then BODY, then ensures `test<nub>:teardown' is
run no matter what happens in BODY. Binds NAME to symbol `test-name'.

If TEST-TEARDOWN-FN is non-nil, it is /always/ called after the test is run
(even if it errors out/fails/etc). TEST-TEARDOWN-FN should take one parameter:
NAME."
  (declare (indent 4))
  `(let ((test-name ,name))
     ;; `unwind-protect' lets us run teardown even if errors, etc.
     (unwind-protect
         ;; Run test set-up, then the test.
         (progn
           (test<nub>:setup test-name ,user ,func/setup ,func/teardown)

           ,@body)

       ;; Always run tear-down.
       (test<nub>:teardown test-name))))
