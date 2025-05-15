;;; core/modules/emacs/alist/test/base.el --- Testing Helpers -*- no-byte-compile: t; lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-12-15
;; Timestamp:  2023-06-21
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Testing Helpers
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Base Functionality for Testing
;;------------------------------------------------------------------------------

;; No always-load files.


;;------------------------------------------------------------------------------
;; Constants & Variables
;;------------------------------------------------------------------------------

;;------------------------------
;; Global Scoped Variable
;;------------------------------

(defvar test<alist>:var:alist/nil    nil
  "Alist that starts off nil.")


(defvar test<alist>:var:alist/values nil
  "Alist that starts off with values.")


(defun test<alist>:var:alist/get (type &optional local-symbol-name)
  "Function call that will return some alist based on TYPE.

TYPE:
  - :local
    + Returns LOCAL-SYMBOL-NAME.
  - :global/nil
    + Returns `test<alist>:var:alist/nil'.
  - :global/values
    + Returns `test<alist>:var:alist/values'.

Usage:
  (test<alist>:get :global)
    -> `test<alist>:alist'
  (test<alist>:get :local 'some-local-symbol-name)
    -> `some-local-symbol-name'"
  (cond ((eq type :local)
         local-symbol-name)
        ((eq type :global/nil)
         'test<alist>:var:alist/nil)
        ((eq type :global/values)
         'test<alist>:var:alist/values)
        (t
         (should-not "wrong input, idiot."))))
;; (test<alist>:var:alist/get :global/values)
;; (let ((alist/local '((:k . :v)))) (test<alist>:get :local 'alist/local))


(defun test<alist>:setup/vars:alists/global (_)
  "Create a global-scoped alist (as opposed to function scoped w/ `let') for some tests to use."
  (setq test<alist>:var:alist/values (list (cons :key-0 :value-0/initial)
                                           (cons :key-1 :value-1/initial)
                                           (cons :key-2 :value-2/initial)
                                           (cons :key-3 :value-3/initial)
                                           (cons :key-4 :value-4/initial)
                                           (cons :key-5 :value-5/initial)))
  (setq test<alist>:var:alist/nil nil)
  ;; (message "setup: %S" test<alist>:alist)
  )


(defun test<alist>:teardown/vars:alists/global (_)
  "Leave the global-scoped alists hanging around w/ whatever values tests modified to?"
  ;; (makunbound 'test<alist>:alist)
  ;; (unintern 'test<alist>:alist)
  ;; (message "teardown: %S" (condition-case _
  ;;                             test<alist>:alist
  ;;                           (void-variable "<void>")))
  )


;;------------------------------
;; Per-Test-Suite
;;------------------------------

(defvar test<alist>:suite:func/setup nil
  "Set-up function to run for the current testing suite.")


(defvar test<alist>:suite:func/teardown nil
  "Tear-down function to run for the current testing suite.")


(defvar test<alist>:should:marker/counter 0
  "Counter for `test<alist>:should:marker'.")


;;------------------------------
;; "ERT List of Should Forms" buffer help
;;------------------------------

(defun test<alist>:should:marker (test-name &rest args)
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
                           test<alist>:should:marker/counter
                           args))

    ;; Increment counter for next time.
    (setq test<alist>:should:marker/counter
          (1+ test<alist>:should:marker/counter))

    ;; Assert the string so it ends up in the list of asserts.
    (should formatted)))


;;------------------------------------------------------------------------------
;; Test Fixture (Set-Up / Tear-Down) Functions
;;------------------------------------------------------------------------------

;;------------------------------
;; Per-Test-Suite
;;------------------------------

(defun test<alist>:setup/suite (func/setup func/teardown)
  "Hook in any per-suite set-up/tear-down functions.

FUNC/SETUP and FUNC/TEARDOWN should take args:
  - string: NAME - the test's name

FUNC/SETUP will run as last step in set-up.
FUNC/TEARDOWN will run as first step in tear-down."
  (setq test<alist>:suite:func/setup    func/setup
        test<alist>:suite:func/teardown func/teardown))


;;------------------------------
;; Set-Up
;;------------------------------

(defun test<alist>:setup/vars (test-name)
  "Any setup of consts/vars needed per test."
  (test<alist>:setup/vars:alists/global test-name))


(defun test<alist>:setup (name func/setup func/teardown)
  "Run setup for test named NAME.

FUNC/SETUP and FUNC/TEARDOWN will be run during set-up/tear-down if provided."
  (test<alist>:setup/suite func/setup
                           func/teardown)

  (test<alist>:setup/vars name)

  ;; Always last:
  (when test<alist>:suite:func/setup
    (unwind-protect
        (funcall test<alist>:suite:func/setup name))))


;;------------------------------
;; Tear-Down
;;------------------------------

(defun test<alist>:teardown/vars (test-name)
  "Clear out/clean up vars used during testing so next test or normal Emacs
usage isn't affected."
  (test<alist>:teardown/vars:alists/global test-name))


(defun test<alist>:teardown (test-name)
  "Run teardown for tests."
  ;; Always first in actual tear-down:
  (when test<alist>:suite:func/teardown
    (unwind-protect
        (funcall test<alist>:suite:func/teardown test-name)))

  ;; Generally in reverse order from set-up.
  (test<alist>:teardown/vars test-name))


;;------------------------------
;; Test Fixture Macro
;;------------------------------

(defmacro test<alist>:fixture (name func/setup func/teardown &rest body)
  "Run `test<alist>:setup', then BODY, then ensures `test<alist>:teardown' is
run no matter what happens in BODY. Binds NAME to symbol `test-name'.

If TEST-TEARDOWN-FN is non-nil, it is /always/ called after the test is run
(even if it errors out/fails/etc). TEST-TEARDOWN-FN should take one parameter:
NAME."
  (declare (indent 3))
  `(let ((test-name ,name))
     ;; `unwind-protect' lets us run teardown even if errors, etc.
     (unwind-protect
         ;; Run test set-up, then the test.
         (progn
           (test<alist>:setup test-name ,func/setup ,func/teardown)

           ,@body)

       ;; Always run tear-down.
       (test<alist>:teardown test-name))))
