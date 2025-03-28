;;; core/modules/emacs/imp/test/base.el --- Test Helpers Too -*- no-byte-compile: t; lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-12-15
;; Timestamp:  2023-06-22
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Your Test Helpers are in 'imp/test/init.el'.
;; These are merely Test Helpers.
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
(test<imp>:init:load "../error")
(test<imp>:init:load "../debug")


;;------------------------------------------------------------------------------
;; Current test's file/dir.
;;------------------------------------------------------------------------------

(defun test<imp>:path/file:this ()
  "Filepath of caller file, depending on if this is being loaded or looked at."
  (if load-in-progress
      load-file-name
    (buffer-file-name)))
;; (test<imp>:path/file:this)


(defun test<imp>:path/dir:this ()
  "Filepath of caller file, depending on if this is being loaded or looked at."
  (file-name-directory (test<imp>:path/file:this)))


;;------------------------------------------------------------------------------
;; Constants & Variables
;;------------------------------------------------------------------------------

;;------------------------------
;; Per-Test-Suite
;;------------------------------

(defvar test<imp>:suite:func/setup nil
  "Set-up function to run for the current testing suite.")


(defvar test<imp>:suite:func/teardown nil
  "Tear-down function to run for the current testing suite.")


(defvar test<imp>:should:marker/counter 0
  "Counter for `test<imp>:should:marker'.")


;;------------------------------
;; `imp/features' & `imp/path-roots'
;;------------------------------

(defvar test<imp>:features:backup nil
  "Backup `imp/features' so we can test it and then restore to its actual values.")


(defvar test<imp>:features:test nil
  "Save `imp/features' after a test so we can check it for debugging if needed.")


(defvar test<imp>:features:locate:backup nil
  "Backup `imp/features-locate' so we can test it and then restore to its actual values.")


(defvar test<imp>:features:locate:test nil
  "Save `imp/features-locate' after a test so we can check it for debugging if needed.")


(defvar test<imp>:path:roots:backup nil
  "Backup `imp/path-roots' so we can test it and then restore to its actual values.")


(defvar test<imp>:path:roots:test nil
  "Save `imp/path-roots' after a test so we can check it for debugging if needed.")


;;------------------------------
;; Testing Dirs & Files
;;------------------------------

(defvar test<imp>:path:root:test (test<imp>:path/dir:this)
  "The 'imp/test' \"root\" directory.")


(defvar test<imp>:path:root:loading (concat (test<imp>:path/dir:this)
                                            "loading/")
  "The filename (or filepath) for our 'imp/test/loading/imp-init.el' file.")


(defvar test<imp>:file:loading:init "imp-init.el"
  "The filename (or filepath) for our 'imp/test/loading/imp-init.el' file.

NOTE: Should be `imp/path-filename-init', but can't load/require 'path.el' here.")


(defvar test<imp>:feature:loading :loading
  "The feature name for our 'imp/test/loading/' files.

NOTE: Provided by `test<imp>:file:loading:init'.")


(defvar test<imp>:file:loading:features "imp-features.el"
  "The filename (or filepath) for our 'imp/test/loading/imp-features.el' file.

NOTE: Should be `imp/path-filename-features', but can't load/require 'path.el' here.")


(defvar test<imp>:feature:loading:features '(:loading features)
  "The feature name for our 'imp/test/loading/imp-features' files.

NOTE: Provided by `test<imp>:file:loading:features'.")


(defvar test<imp>:file:loading:load "load"
  "The filename (or filepath) for our 'imp/test/loading/load.el' file.")


(defvar test<imp>:feature:loading:load '(:loading load)
  "The feature name for our 'imp/test/loading/load.el' file.")


(defvar test<imp>:file:loading:dont-load "dont-load"
  "The filename (or filepath) for our 'imp/test/loading/dont-load.el' file.")


(defvar test<imp>:feature:loading:dont-load '(:loading dont-load)
  "The feature name for our 'imp/test/loading/dont-load.el' file.")


(defvar test<imp>:feature:loading:doesnt-exist '(:loading doesnt-exist)
  "A feature name for 'imp/test/loading/doesnt-exist.el', which doesn't exist.")


(defvar test<imp>:file:loading:doesnt-exist "doesnt-exist"
  "A file name for 'imp/test/loading/doesnt-exist.el', which doesn't exist.")


;;------------------------------------------------------------------------------
;; "ERT List of Should Forms" buffer help
;;------------------------------------------------------------------------------

(defun test<imp>:should:marker (test-name &rest args)
  "Put a `should' in the test which will evaluate to some searchable output for
marking where in a test you are.

Search for \"[MARK-\[0-9\]+]:\"."
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
                           test<imp>:should:marker/counter
                           args))

    ;; Increment counter for next time.
    (setq test<imp>:should:marker/counter
          (1+ test<imp>:should:marker/counter))

    ;; Assert the string so it ends up in the list of asserts.
    (should formatted)))
;; (test<imp>:should:marker "test name" "marker name")


(defun test<imp>:should:marker:small (&rest args)
  "Put a `should' in the test which will evaluate to some searchable output for
marking where in a test you are.

Search for \"[MARK-\[0-9\]+]:\"."
  ;;---
  ;; Smaller noticeable banner.
  ;;---
  (let ((fmt:str (if args
                     (concat "\n"
                         "───────────\n"
                         "  MARK-%02d\n"
                         "──┬────────\n")
                   (concat "\n"
                           "───────────\n"
                           "  MARK-%02d\n"
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
                           test<imp>:should:marker/counter
                           args))

    ;; Increment counter for next time.
    (setq test<imp>:should:marker/counter
          (1+ test<imp>:should:marker/counter))

    ;; Assert the string so it ends up in the list of asserts.
    (should formatted)))
;; (test<imp>:should:marker:small "mark name")


;;------------------------------------------------------------------------------
;; Test Helpers
;;------------------------------------------------------------------------------


;;------------------------------
;; 'test/loading/' files
;;------------------------------

(defun test<imp>:setup/vars:loading ()
  "Deletes variable from 'test/loading/*.el' files."
  ;; `makunbound' is fine to call even if the vars don't exist.
  (makunbound 'test<imp>:file:loading?)

  (makunbound 'test<imp>:loading:init:loaded)
  (makunbound 'test<imp>:loading:features:loaded)
  (makunbound 'test<imp>:loading:load:loaded)
  (makunbound 'test<imp>:loading:dont-load:loaded))


(defun test<imp>:setup/root:loading ()
  "Puts the 'test/loading' stuff into `imp/path-roots'."
  ;; Need to have 'imp/path.el' functions, obviously, so do not use in tests
  ;; before 'imp/path.el' has been tested.
  (test<imp>:init:load "../path")
  (imp/path-root-set test<imp>:feature:loading
                     test<imp>:path:root:loading
                     test<imp>:file:loading:init
                     test<imp>:file:loading:features))


;;------------------------------------------------------------------------------
;; Test Fixture (Set-Up / Tear-Down) Functions
;;------------------------------------------------------------------------------

;;------------------------------
;; Per-Test-Suite
;;------------------------------

(defun test<imp>:setup/suite (func/setup func/teardown)
  "Hook in any per-suite set-up/tear-down functions.

FUNC/SETUP and FUNC/TEARDOWN should take args:
  - string: NAME - the test's name

FUNC/SETUP will run as last step in set-up.
FUNC/TEARDOWN will run as first step in tear-down."
  (setq test<imp>:suite:func/setup    func/setup
        test<imp>:suite:func/teardown func/teardown))


;;------------------------------
;; Set-Up
;;------------------------------

(defun test<imp>:setup/vars ()
  "Any setup of consts/vars needed per test."
  ;; Generally, reset vars at start of test so they can be manually inspected
  ;; after a test is run. Except `imp/features', `imp/features-locate' &
  ;; `imp/path-roots', which needs to be reverted back to valid after every
  ;; test.
  ;;   - In that case, save the test's values off to `test<imp>:[...]:test'.

  ;; Backup `imp/features' & `imp/path-roots' and clear it out for any tests that need to use it.
  (setq test<imp>:features:backup        imp/features
        test<imp>:features:locate:backup imp/features-locate
        test<imp>:path:roots:backup      imp/path-roots
        imp/features                     nil
        imp/features-locate              nil
        imp/path-roots                   nil
        test<imp>:features:test          nil
        test<imp>:features:locate:test   nil
        test<imp>:path:roots:test        nil)

  ;; Clear out vars loaded from 'imp/test/loading/...' files.
  (test<imp>:setup/vars:loading))


(defun test<imp>:debug:marker:create (caller name:test type name:type)
  "Create strings for `test<imp>:debug:marker:begin' and/or
`test<imp>:debug:marker:end'.

TYPE should be `:top' or `:bottom'."
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
    (imp--debug caller
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
;; (test<imp>:debug:marker:create "test" "test<imp>:test:debug:marker:create" :bottom "START")
;; (test<imp>:debug:marker:create "test" "test<imp>:test:debug:marker:create" :top "END")


(defun test<imp>:setup (name func/setup func/teardown)
  "Run setup for test named NAME.

FUNC/SETUP and FUNC/TEARDOWN will be run during set-up/tear-down if provided."
  ;; If debugging, give a nice marker in the debug output buffer:
  (test<imp>:debug:marker:create "test<imp>:setup"
                                 name
                                 :bottom
                                 "START")

  ;; Create user from test name if no user provided.
  (test<imp>:setup/suite func/setup
                         func/teardown)

  (test<imp>:setup/vars)

  ;; Always last:
  (when test<imp>:suite:func/setup
    (unwind-protect
        (funcall test<imp>:suite:func/setup name))))


;;------------------------------
;; Tear-Down
;;------------------------------

(defun test<imp>:teardown/vars ()
  "Clear out/clean up vars used during testing so next test or normal Emacs
usage isn't affected."
  ;; Generally, reset vars at start of test so they can be manually inspected
  ;; after a test is run. Except `imp/features', `imp/features-locate' &
  ;; `imp/path-roots', which needs to be reverted back to valid after every
  ;; test.
  ;;   - In that case, save the test's values off to `test<imp>:[...]:test'.

  ;; Restore `imp/path-roots', but save whatever testing did to the `test<imp>:path:roots:test' var.
  (setq test<imp>:features:test          imp/features
        test<imp>:features:locate:test   imp/features-locate
        test<imp>:path:roots:test        imp/path-roots
        ;; Restore `imp/features' & `imp/path-roots'.
        imp/features                     test<imp>:features:backup
        imp/features-locate              test<imp>:features:locate:backup
        imp/path-roots                   test<imp>:path:roots:backup
        test<imp>:features:backup        nil
        test<imp>:features:locate:backup nil
        test<imp>:path:roots:backup      nil))


(defun test<imp>:teardown (name)
  "Run teardown for tests."
  ;; If debugging, give a nice marker in the debug output buffer:
  (test<imp>:debug:marker:create "test<imp>:teardown"
                                 name
                                 :top
                                 "END")

  ;; Always first in actual tear-down:
  (when test<imp>:suite:func/teardown
    (unwind-protect
        (funcall test<imp>:suite:func/teardown name)))

  ;; Generally in reverse order from set-up.
  (test<imp>:teardown/vars))


;;------------------------------
;; Test Fixture Macro
;;------------------------------

(defmacro test<imp>:fixture (name func/setup func/teardown &rest body)
  "Run `test<imp>:setup', then BODY, then ensures `test<imp>:teardown' is
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
           (test<imp>:setup test-name ,func/setup ,func/teardown)

           ,@body)

       ;; Always run tear-down.
       (test<imp>:teardown test-name))))
