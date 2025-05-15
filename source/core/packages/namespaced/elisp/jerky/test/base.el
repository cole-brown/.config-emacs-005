;;; core/modules/elisp/jerky/test/base.el --- Test utils for Jerky. -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-06-14
;; Timestamp:  2023-06-21
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Test utils for Jerky.
;;
;;; Code:


;;==============================================================================
;; Hello there.
;;==============================
;; Evaluate 'init.el' before evaluating test files and running tests.
;;==============================================================================


;;------------------------------------------------------------------------------
;; Base Functionality for Testing
;;------------------------------------------------------------------------------

;; Load whatever should always be loaded for all tests.
(imp:test:load :feature:post '(:jerky debug)
               :filename     "../debug")
(imp:test:load :feature:post '(:jerky jerky)
               :filename     "../jerky")


;;------------------------------------------------------------------------------
;; Constants & Variables
;;------------------------------------------------------------------------------

;;------------------------------
;; Per-Test-Suite
;;------------------------------

(defvar test<jerky>:suite:func/setup nil
  "Set-up function to run for the current testing suite.")


(defvar test<jerky>:suite:func/teardown nil
  "Tear-down function to run for the current testing suite.")


(defvar test<jerky>:should:marker/counter 0
  "Counter for `test<jerky>:should:marker'.")


;;------------------------------
;; Jerky Variables
;;------------------------------

(defvar test<jerky>:repo:backup nil
  "Backup `int<jerky>:repo' here.

Allow tests to do whatever they want in the repo and then restore to its actual
values after the test runs.")


(defvar test<jerky>:repo:test nil
  "Copy of `int<jerky>:repo' created after a test.

Allow tester to check it for debugging if needed, without having to step through
or print.")


(defvar test<jerky>:namespaces:backup nil
  "Backup `int<jerky>:namespaces' here.

Allow tests to do whatever they want in the namespaces alist and then restore to
its actual values after the test runs.")


(defvar test<jerky>:namespaces:test nil
  "Copy of `int<jerky>:namespaces' created after a test.

Allow tester to check it for debugging if needed, without having to step through
or print.")


;;------------------------------------------------------------------------------
;; "ERT List of Should Forms" buffer help
;;------------------------------------------------------------------------------

(defun test<jerky>:should:marker (test-name &rest args)
  "Add a test-name-start type marker to the ERT `should' forms.

TEST-NAME should be the name of the `ert-deftest` function as a string.

ARGS will be printed if provided.

Search for regexp: \"MARK-[0-9]\{2,\}\""
  ;;---
  ;; Big noticeable banner.
  ;;---
  (let ((fmt:str (concat "\n"
                         "╔═════════════════════════════════════╗\n"
                         "╠══╣            MARK-%02d            ╠══╣\n"
                         "╚═╤═══════════════════════════════════╝\n"))
        formatted)

    ;;---
    ;; Test's name and the `args' to print.
    ;;---
    (if (null args)
        ;; No extra stuff in `fmt:str` when no ARGS.
        (setq fmt:str (concat fmt:str
                              "  └──┤ " test-name "\n"))

      ;; `test-name' and the args.
      (setq fmt:str (concat fmt:str
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
    (setq formatted (apply #'format fmt:str
                           test<jerky>:should:marker/counter
                           args))

    ;; Increment counter for next time.
    (setq test<jerky>:should:marker/counter
          (1+ test<jerky>:should:marker/counter))

    ;; Assert the string so it ends up in the list of asserts.
    (should formatted)))
;; (test<jerky>:should:marker "test name" "marker name")


(defun test<jerky>:should:marker:small (&rest args)
  "Add a small marker to the ERT `should' forms.

ARGS will be printed if provided.

Search for regexp: \"MARK-[0-9]\{2,\}\""
  ;;---
  ;; Smaller noticeable banner.
  ;;---
  (let ((fmt:str (concat "\n"
                         "───────────\n"
                         "  MARK-%02d\n"
                         (if args
                             "──┬────────\n"
                           "───────────\n")))
        formatted)

    ;;---
    ;; `args' to print?
    ;;---
    (when args
      (setq fmt:str (concat fmt:str
                            "  └──┤ "
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
    (setq formatted (apply #'format fmt:str
                           test<jerky>:should:marker/counter
                           args))

    ;; Increment counter for next time.
    (setq test<jerky>:should:marker/counter
          (1+ test<jerky>:should:marker/counter))

    ;; Assert the string so it ends up in the list of asserts.
    (should formatted)))
;; (test<jerky>:should:marker:small "mark name")


;;------------------------------------------------------------------------------
;; Test Fixture (Set-Up / Tear-Down) Functions
;;------------------------------------------------------------------------------

;;------------------------------
;; Per-Test-Suite
;;------------------------------

(defun test<jerky>:setup/suite (func/setup func/teardown)
  "Hook in any per-suite set-up/tear-down functions.

FUNC/SETUP and FUNC/TEARDOWN should take args:
  - string: NAME - the test's name

FUNC/SETUP will run as last step in set-up.
FUNC/TEARDOWN will run as first step in tear-down."
  (setq test<jerky>:suite:func/setup    func/setup
        test<jerky>:suite:func/teardown func/teardown))


;;------------------------------
;; Set-Up
;;------------------------------

(defun test<jerky>:setup/vars ()
  "Any setup of consts/vars needed per test."
  ;; Generally, reset vars at start of test so they can be manually inspected
  ;; after a test is run. Except `jerky:features', `jerky:features:locate' &
  ;; `jerky:path:roots', which needs to be reverted back to valid after every
  ;; test.
  ;;   - In that case, save the test's values off to `test<jerky>:[...]:test'.

  ;; Backup `int<jerky>:repo' & `int<jerky>' and clear it out for any tests that need to use it.
  (setq test<jerky>:repo:backup       int<jerky>:repo          ; 1. Make backup of var.
        int<jerky>:repo               (int<jerky>:repo:create) ; 2. Create new var for tests.
        test<jerky>:repo:test         nil                      ; 3. Clear last test's results.
        test<jerky>:namespaces:backup int<jerky>:namespaces
        int<jerky>:namespaces         (int<jerky>:namespaces:create)
        test<jerky>:namespaces:test   nil))


(defun test<jerky>:debug:marker:create (caller name:test type name:type)
  "Create debug marker string.

String used in:
  - `test<jerky>:debug:marker:begin'
  - `test<jerky>:debug:marker:end'

CALLER should be the string name of the test util function that calls this.

NAME:TEST should be the ERT test's name as a string.

TYPE should be `:top' or `:bottom'.

NAME:TYPE should be a string, generally \"START\" or \"END\"."
  (let* ((width:header 60)
         (border:char:horizontal ?═)
         (border:left  "╠══╣")
         (border:right "╠══╣")
         (length:middle:padding:min (+ (length border:left)
                                       1
                                       (length name:test)
                                       1
                                       (length border:right)))
         (name:truncated (if (> length:middle:padding:min width:header)
                             ;; Truncate name to fit.
                             ;; Truncate the head of the string? Tail is which test, exactly, it is...
                             (substring name:test
                                        (- length:middle:padding:min width:header)
                                        (length name:test))
                           ;; Name is fine as-is.
                           name:test))
         (width:padding (- width:header
                           (length border:left)
                           (length border:right)
                           (length name:truncated)))
         ;; Have at least one space on either side.
         (width:padding:centered (max 1 (/ width:padding 2.0)))
         (padding:left  (make-string (ceiling width:padding:centered) ? ))
         (padding:right (make-string (floor width:padding:centered)   ? ))
         ;; Top/Bottom with room for connection to Type
         (border:fill:top/bottom (make-string (- width:header 3) ?═ :multibyte)))
    (nub:debug
        :jerky
        caller
        '(:test)
      '("\n\n" ;; decent amount of space from previous output
        ;; "" or top type - no hard-coded "\n" in case of "".
        "%s"
        ;; "═" or "╧" and border:fill:top/bottom
        "╔%s%s╗\n"
        ;; left border, left padding, name (truncated?), right padding, right border: 5 "%s" fields
        "%s%s%s%s%s\n"
        ;; "═" or "╧" and border:fill:top/bottom
        "╚%s%s╝\n"
        ;; "" or bottom type - no hard-coded "\n" in case of "".
        "%s")
      ;; Top: Type
      (if (eq type :top)
          (format  " ┌──┤ %s\n" name:type)
        "")
      ;; Top: Border
      ;;   - Connect to top type or no.
      (if (eq type :top)
          "╧"
        "═")
      ;;   - Rest of border.
      border:fill:top/bottom
      ;; Middle: Name
      border:left
      padding:left
      name:truncated
      padding:right
      border:right
      ;; Bottom: Border
      ;;   - Connect to bottom type or no.
      (if (eq type :bottom)
          "╤"
        "═")
      ;;   - Rest of border.
      border:fill:top/bottom
      ;; Bottom: Type
      (if (eq type :bottom)
          (format  " └──┤ %s\n" name:type)
        "")
      ;;   - Leave to caller to fill.
      "%s")))
;; (test<jerky>:debug:marker:create "test" "test<jerky>:test:debug:marker:create" :bottom "START")
;; (test<jerky>:debug:marker:create "test" "test<jerky>:test:debug:marker:create" :top "END")


(defun test<jerky>:setup (name func/setup func/teardown)
  "Run setup for test named NAME.

FUNC/SETUP and FUNC/TEARDOWN will be run during set-up/tear-down if provided."
  ;; If debugging, give a nice marker in the debug output buffer:
  (test<jerky>:debug:marker:create "test<jerky>:setup"
                                   name
                                   :bottom
                                   "START")

  ;; Create user from test name if no user provided.
  (test<jerky>:setup/suite func/setup
                           func/teardown)

  (test<jerky>:setup/vars)

  ;; Always last:
  (when test<jerky>:suite:func/setup
    (unwind-protect
        (funcall test<jerky>:suite:func/setup name))))


;;------------------------------
;; Tear-Down
;;------------------------------

(defun test<jerky>:teardown/vars ()
  "Clean up so that next test or normal Emacs usage isn't affected by tests run."
  ;; Generally, reset vars at start of test so they can be manually inspected
  ;; after a test is run. Except `int<jerky>:repo' & `int<jerky>:namespaces',
  ;; which needs to be reverted back to valid after every test.
  ;;   - In that case, save the test's values off to `test<jerky>:[...]:test'.

  (setq test<jerky>:repo:test   int<jerky>:repo         ; 1. Save current to the test result var.
        int<jerky>:repo         test<jerky>:repo:backup ; 2. Restore backup to the jerky var.
        test<jerky>:repo:backup nil                     ; 3. Clear the backup.
        test<jerky>:namespaces:test   int<jerky>:namespaces
        int<jerky>:namespaces         test<jerky>:namespaces:backup
        test<jerky>:namespaces:backup nil))


(defun test<jerky>:teardown (name)
  "Run teardown for test.

NAME should be the name of the `ert-deftest` function as a string."
  ;; If debugging, give a nice marker in the debug output buffer:
  (test<jerky>:debug:marker:create "test<jerky>:teardown"
                                   name
                                   :top
                                   "END")

  ;; Always first in actual tear-down:
  (when test<jerky>:suite:func/teardown
    (unwind-protect
        (funcall test<jerky>:suite:func/teardown name)))

  ;; Generally in reverse order from set-up.
  (test<jerky>:teardown/vars))


;;------------------------------
;; Test Fixture Macro
;;------------------------------

(defmacro test<jerky>:fixture (name func/setup func/teardown &rest body)
  "Run set-up, test BODY, and tear-down.

Run:
  1. `test<jerky>:setup'
  2. BODY forms
  3. Ensure `test<jerky>:teardown' is run no matter what happens in BODY.

Lexically bind NAME to symbol `test-name'. NAME should be the name of the
`ert-deftest` function as a string.

If FUNC/SETUP is non-nil, it is called before the test is run.

If FUNC/TEARDOWN is non-nil, it is /always/ called after the test is run
\(even if it errors out/fails/etc).

FUNC/SETUP and FUNC/TEARDOWN should take one parameter:
  - name (string): ERT test's function name"
  (declare (indent 3))
  `(let ((test-name ,name))
     ;; `unwind-protect' lets us run teardown even if errors, etc.
     (unwind-protect
         ;; Run test set-up, then the test.
         (progn
           (test<jerky>:setup test-name ,func/setup ,func/teardown)

           ,@body)

       ;; Always run tear-down.
       (test<jerky>:teardown test-name))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :jerky 'test 'base)
