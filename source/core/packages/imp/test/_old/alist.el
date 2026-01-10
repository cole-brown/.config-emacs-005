;;; core/modules/emacs/imp/test/alist.el --- Tests for "alist.el" -*- no-byte-compile: t; lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2023-06-22
;; Timestamp:  2023-06-22
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Tests for "alist.el"
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Test Requirements
;;------------------------------------------------------------------------------

(test<imp>:init:load "base.el")

(test<imp>:init:load "../feature.el")
(test<imp>:init:load "../alist.el")


;;------------------------------------------------------------------------------
;; Test Debugging Helpers
;;------------------------------------------------------------------------------

;;------------------------------
;; Global Scoped Variable
;;------------------------------

(setq test<imp-alist>:alist/nil    nil)
(setq test<imp-alist>:alist/values nil)


(defun test<imp-alist>:alist:get (type &optional local-symbol-name)
  "Function call that will return something based on TYPE.

TYPE:
  - :local
    + Returns LOCAL-SYMBOL-NAME.
  - :global/nil
    + Returns `test<imp-alist>:alist/nil'.
  - :global/values
    + Returns `test<imp-alist>:alist/values'.


Usage:
  (test<imp-alist>:alist:get :global)
    -> `test<imp-alist>:alist'
  (test<imp-alist>:alist:get :local 'some-local-symbol-name)
    -> `some-local-symbol-name'"
  (cond ((eq type :local)
         local-symbol-name)
        ((eq type :global/nil)
         'test<imp-alist>:alist/nil)
        ((eq type :global/values)
         'test<imp-alist>:alist/values)
        (t
         (should-not "wrong input, idiot."))))
;; (test<imp-alist>:alist:get :global/values)
;; (let ((alist/local '((:k . :v)))) (test<imp-alist>:alist:get :local 'alist/local))


;;------------------------------
;; Set-Up / Tear-Down
;;------------------------------

(defun test<imp-alist>:setup (_)
  "Create a global-scoped alist (as opposed to function scoped w/ `let') for some tests to use."
  (setq test<imp-alist>:alist/values (list (cons :key-0 :value-0/initial)
                                           (cons :key-1 :value-1/initial)
                                           (cons :key-2 :value-2/initial)
                                           (cons :key-3 :value-3/initial)
                                           (cons :key-4 :value-4/initial)
                                           (cons :key-5 :value-5/initial)))
  (setq test<imp-alist>:alist/nil nil)
  ;; (message "setup: %S" test<imp-alist>:alist)
  )


(defun test<imp-alist>:teardown (test-name)
  "Leave the global-scoped alists hanging around w/ whatever values tests modified to?"
  ;; (makunbound 'test<imp-alist>:alist)
  ;; (unintern 'test<imp-alist>:alist)
  ;; (message "teardown: %S" (condition-case _
  ;;                             test<imp-alist>:alist
  ;;                           (void-variable "<void>")))
  )


;; ╔═════════════════════════════╤═══════════╤═════════════════════════════════╗
;; ╟─────────────────────────────┤ ERT TESTS ├─────────────────────────────────╢
;; ╠════════════════════════╤════╧═══════════╧════╤════════════════════════════╣
;; ╟────────────────────────┤ Imps are people too!├────────────────────────────╢
;; ╚════════════════════════╧═════════════════════╧════════════════════════════╝


;;------------------------------------------------------------------------------
;; Tests: Standard/General 'alist' Functions
;;------------------------------------------------------------------------------

;;------------------------------
;; imp--alist-get-value
;;------------------------------

(ert-deftest test<imp-alist>::imp--alist-get-value ()
  "Test that `imp--alist-get-value' behaves appropriately."
  (test<imp>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<imp-alist>::imp--alist-get-value"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===
    (let* ((value/expected:0 "42")
           (value/expected:1 :1337)
           (value/expected:2 9001)
           (alist/cons (list (cons :key-0 value/expected:0)
                             (cons :key-1 value/expected:1)
                             (cons :key-2 value/expected:2)))
           (alist/list (list (list :key-0 value/expected:0)
                             (list :key-1 value/expected:1)
                             (list :key-2 value/expected:2)))
           value/get)

      ;;------------------------------
      ;; Check the 'cons' alist.
      ;;------------------------------
      (test<imp>:should:marker test-name "cons: key-0")
      (setq value/get (imp--alist-get-value :key-0 alist/cons))
      (should value/get)
      (should (stringp value/get))
      (should (string= value/get value/expected:0))

      (test<imp>:should:marker test-name "cons: key-1")
      (setq value/get (imp--alist-get-value :key-1 alist/cons))
      (should value/get)
      (should (keywordp value/get))
      (should (eq value/get value/expected:1))

      (test<imp>:should:marker test-name "cons: key-2")
      (setq value/get (imp--alist-get-value :key-2 alist/cons))
      (should value/get)
      (should (integerp value/get))
      (should (= value/get value/expected:2))

      ;;------------------------------
      ;; Check the 'list' alist.
      ;;------------------------------
      (test<imp>:should:marker test-name "list: key-0")
      (setq value/get (imp--alist-get-value :key-0 alist/list))
      (should value/get)
      (should (listp value/get))
      (should (= 1 (length value/get)))
      (setq value/get (nth 0 value/get))
      (should (stringp value/get))
      (should (string= value/get value/expected:0))

      (test<imp>:should:marker test-name "list: key-1")
      (setq value/get (imp--alist-get-value :key-1 alist/list))
      (should value/get)
      (should (listp value/get))
      (should (= 1 (length value/get)))
      (setq value/get (nth 0 value/get))
      (should (keywordp value/get))
      (should (eq value/get value/expected:1))

      (test<imp>:should:marker test-name "list: key-2")
      (setq value/get (imp--alist-get-value :key-2 alist/list))
      (should value/get)
      (should (listp value/get))
      (should (= 1 (length value/get)))
      (setq value/get (nth 0 value/get))
      (should (integerp value/get))
      (should (= value/get value/expected:2)))))


;;------------------------------
;; imp--alist-get-pair
;;------------------------------

(ert-deftest test<imp-alist>::imp--alist-get-pair ()
  "Test that `imp--alist-get-pair' behaves appropriately."
  (test<imp>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<imp-alist>::imp--alist-get-pair"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===
    (let* ((value/expected:0 "42")
           (value/expected:1 :1337)
           (value/expected:2 9001)
           (alist/cons (list (cons :key-0 value/expected:0)
                             (cons :key-1 value/expected:1)
                             (cons :key-2 value/expected:2)))
           (alist/list (list (list :key-0 value/expected:0)
                             (list :key-1 value/expected:1)
                             (list :key-2 value/expected:2)))
           value/get
           value/get:key
           value/get:value)

      ;;------------------------------
      ;; Check the 'cons' alist.
      ;;------------------------------
      (let ((expected:key  :key-0)
            (expected:value value/expected:0))
        (test<imp>:should:marker test-name "cons: key-0")
        (setq value/get (imp--alist-get-pair expected:key alist/cons))
        (should value/get)
        (should (consp value/get))
        (should (listp value/get)) ;; Cons are also lists.
        (setq value/get:key   (car value/get)
              value/get:value (cdr value/get))
        (should (keywordp value/get:key))
        (should (eq value/get:key expected:key))
        (should (stringp value/get:value))
        (should (string= value/get:value expected:value)))

      (let ((expected:key  :key-1)
            (expected:value value/expected:1))
        (test<imp>:should:marker test-name "cons: key-1")
        (setq value/get (imp--alist-get-pair expected:key alist/cons))
        (should value/get)
        (should (consp value/get))
        (should (listp value/get)) ;; Cons are also lists.
        (setq value/get:key   (car value/get)
              value/get:value (cdr value/get))
        (should (keywordp value/get:key))
        (should (eq value/get:key expected:key))
        (should (keywordp value/get:value))
        (should (eq value/get:value expected:value)))

      (let ((expected:key  :key-2)
            (expected:value value/expected:2))
        (test<imp>:should:marker test-name "cons: key-2")
        (setq value/get (imp--alist-get-pair expected:key alist/cons))
        (should value/get)
        (should (consp value/get))
        (should (listp value/get)) ;; Cons are also lists.
        (setq value/get:key   (car value/get)
              value/get:value (cdr value/get))
        (should (keywordp value/get:key))
        (should (eq value/get:key expected:key))
        (should (integerp value/get:value))
        (should (= value/get:value expected:value)))

      ;;------------------------------
      ;; Check the 'list' alist.
      ;;------------------------------
      (let ((expected:key  :key-0)
            (expected:value value/expected:0))
        (test<imp>:should:marker test-name "list: key-0")
        (setq value/get (imp--alist-get-pair expected:key alist/list))
        (should value/get)
        (should (listp value/get))
        (setq value/get:key   (nth 0 value/get)
              value/get:value (nth 1 value/get))
        (should (keywordp value/get:key))
        (should (eq value/get:key expected:key))
        (should (stringp value/get:value))
        (should (string= value/get:value expected:value)))

      (let ((expected:key :key-1)
            (expected:value value/expected:1))
        (test<imp>:should:marker test-name "list: key-1")
        (setq value/get (imp--alist-get-pair expected:key alist/list))
        (should value/get)
        (should (listp value/get))
        (setq value/get:key   (nth 0 value/get)
              value/get:value (nth 1 value/get))
        (should (keywordp value/get:key))
        (should (eq value/get:key expected:key))
        (should (keywordp value/get:value))
        (should (eq value/get:value expected:value)))

      (let ((expected:key :key-2)
            (expected:value value/expected:2))
        (test<imp>:should:marker test-name "list: key-2")
        (setq value/get (imp--alist-get-pair expected:key alist/list))
        (should value/get)
        (should (listp value/get))
        (setq value/get:key   (nth 0 value/get)
              value/get:value (nth 1 value/get))
        (should (keywordp value/get:key))
        (should (eq value/get:key expected:key))
        (should (integerp value/get:value))
        (should (= value/get:value expected:value))))))


;;------------------------------
;; imp--alist-update - local alist (defined in a `let')
;;------------------------------

(ert-deftest test<imp-alist>::imp--alist-update--local ()
  "Test that `imp--alist-update' will add/overwrite values in a local alist correctly."
  (test<imp>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<imp-alist>::imp--alist-update--local"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===
    (let ((alist/cons (list (cons :key-0 :value-0/initial)
                            (cons :key-1 :value-1/initial)
                            (cons :key-2 :value-2/initial)))
          (alist/list (list (list :key-0 :value-0/initial)
                            (list :key-1 :value-1/initial)
                            (list :key-2 :value-2/initial)))
          alist/updated
          value/get)

      ;;------------------------------
      ;; Add new key/values.
      ;;------------------------------
      (should-not (imp--alist-get-value :key-3 alist/cons))
      (should-not (imp--alist-get-value :key-3 alist/list))

      (test<imp>:should:marker test-name "cons: New Key/Value")
      (setq alist/updated (imp--alist-update :key-3 :value-3/new alist/cons))
      (should alist/updated)

      ;; Our return value should be our alist.
      (should (eq alist/updated alist/cons))
      (setq value/get (imp--alist-get-value :key-3 alist/cons))
      (should value/get)
      (should (keywordp value/get))
      (should (eq value/get :value-3/new))

      (test<imp>:should:marker test-name "list: New Key/Value")
      ;; Add the new value as a list, since it's the `alist/list'.
      (setq alist/updated (imp--alist-update :key-3 '(:value-3/new) alist/list))
      (should alist/updated)
      ;; Our return value should be our alist.
      (should (eq alist/updated alist/list))
      (setq value/get (imp--alist-get-value :key-3 alist/list))
      (should value/get)
      (should (listp value/get))
      (should (= 1 (length value/get)))
      (setq value/get (nth 0 value/get))
      (should (keywordp value/get))
      (should (eq value/get :value-3/new))

      ;;------------------------------
      ;; Update existing key's value.
      ;;------------------------------
      (let ((value/cons (imp--alist-get-value :key-0 alist/cons))
            (value/list (imp--alist-get-value :key-0 alist/list)))
        (should value/cons)
        (should value/list)
        (should (eq value/cons :value-0/initial))
        (should (equal value/list '(:value-0/initial)))

        (test<imp>:should:marker test-name "cons: Update Existing Key/Value")
        (setq alist/updated (imp--alist-update :key-0 :value-0/updated alist/cons))
        (should alist/updated)
        ;; Our return value should be our alist.
        (should (eq alist/updated alist/cons))
        (setq value/get (imp--alist-get-value :key-0 alist/cons))
        (should value/get)
        (should (keywordp value/get))
        (should (eq value/get :value-0/updated))

        (test<imp>:should:marker test-name "list: Update Existing Key/Value")
        ;; Add the new value as a list, since it's the `alist/list'.
        (setq alist/updated (imp--alist-update :key-0 '(:value-0/updated) alist/list))
        (should alist/updated)
        ;; Our return value should be our alist.
        (should (eq alist/updated alist/list))
        (setq value/get (imp--alist-get-value :key-0 alist/list))
        (should value/get)
        (should (listp value/get))
        (should (= 1 (length value/get)))
        (setq value/get (nth 0 value/get))
        (should (keywordp value/get))
        (should (eq value/get :value-0/updated))))))


;;------------------------------
;; imp--alist-update - non-local alist
;;------------------------------

(ert-deftest test<imp-alist>::imp--alist-update--global ()
  "Test that `imp--alist-update' will add/overwrite values in a global alist correctly."
  (test<imp>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<imp-alist>::imp--alist-update--global"
      (lambda (_)
        "Set up globals for this test."
        (setq test<imp-alist>:alist/cons (list (cons :key-0 :value-0/initial)
                                               (cons :key-1 :value-1/initial)
                                               (cons :key-2 :value-2/initial)))
        (setq test<imp-alist>:alist/list (list (list :key-0 :value-0/initial)
                                               (list :key-1 :value-1/initial)
                                               (list :key-2 :value-2/initial)))
        )
      (lambda (_)
        "Clean up the alist of lists."
        ;; (unintern test<imp-alist>:alist/cons)
        ;; (unintern test<imp-alist>:alist/list)
        )

    ;;===
    ;; Run the test.
    ;;===
    (let* (alist/updated
           value/get)

      ;;------------------------------
      ;; Add new key/values.
      ;;------------------------------
      (should-not (imp--alist-get-value :key-3 test<imp-alist>:alist/cons))
      (should-not (imp--alist-get-value :key-3 test<imp-alist>:alist/list))

      (test<imp>:should:marker test-name "cons: New Key/Value")
      (setq alist/updated (imp--alist-update :key-3 :value-3/new test<imp-alist>:alist/cons))
      (should alist/updated)

      ;; Our return value should be our alist.
      (should (eq alist/updated test<imp-alist>:alist/cons))
      (setq value/get (imp--alist-get-value :key-3 test<imp-alist>:alist/cons))
      (should value/get)
      (should (keywordp value/get))
      (should (eq value/get :value-3/new))

      (test<imp>:should:marker test-name "list: New Key/Value")
      ;; Add the new value as a list, since it's the `test<imp-alist>:alist/list'.
      (setq alist/updated (imp--alist-update :key-3 '(:value-3/new) test<imp-alist>:alist/list))
      (should alist/updated)
      ;; Our return value should be our alist.
      (should (eq alist/updated test<imp-alist>:alist/list))
      (setq value/get (imp--alist-get-value :key-3 test<imp-alist>:alist/list))
      (should value/get)
      (should (listp value/get))
      (should (= 1 (length value/get)))
      (setq value/get (nth 0 value/get))
      (should (keywordp value/get))
      (should (eq value/get :value-3/new))

      ;;------------------------------
      ;; Update existing key's value.
      ;;------------------------------
      (let ((value/cons (imp--alist-get-value :key-0 test<imp-alist>:alist/cons))
            (value/list (imp--alist-get-value :key-0 test<imp-alist>:alist/list)))
        (should value/cons)
        (should value/list)
        (should (eq value/cons :value-0/initial))
        (should (equal value/list '(:value-0/initial)))

        (test<imp>:should:marker test-name "cons: Update Existing Key/Value")
        (setq alist/updated (imp--alist-update :key-0 :value-0/updated test<imp-alist>:alist/cons))
        (should alist/updated)
        ;; Our return value should be our alist.
        (should (eq alist/updated test<imp-alist>:alist/cons))
        (setq value/get (imp--alist-get-value :key-0 test<imp-alist>:alist/cons))
        (should value/get)
        (should (keywordp value/get))
        (should (eq value/get :value-0/updated))

        (test<imp>:should:marker test-name "list: Update Existing Key/Value")
        ;; Add the new value as a list, since it's the `test<imp-alist>:alist/list'.
        (setq alist/updated (imp--alist-update :key-0 '(:value-0/updated) test<imp-alist>:alist/list))
        (should alist/updated)
        ;; Our return value should be our alist.
        (should (eq alist/updated test<imp-alist>:alist/list))
        (setq value/get (imp--alist-get-value :key-0 test<imp-alist>:alist/list))
        (should value/get)
        (should (listp value/get))
        (should (= 1 (length value/get)))
        (setq value/get (nth 0 value/get))
        (should (keywordp value/get))
        (should (eq value/get :value-0/updated))))))


;;------------------------------
;; imp--alist-delete - local alist (defined in a `let')
;;------------------------------

(ert-deftest test<imp-alist>::imp--alist-delete--local ()
  "Test that `imp--alist-delete' will delete keys from the alist correctly."
  (test<imp>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<imp-alist>::imp--alist-delete--local"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===
    (let* ((alist/cons (list (cons :key-0 :value-0)
                             (cons :key-1 :value-1)
                             (cons :key-2 :value-2)))
           (alist/list (list (list :key-0 :value-0)
                             (list :key-1 :value-1)
                             (list :key-2 :value-2)))
           alist/deleted)

      ;;------------------------------
      ;; Delete keys from the alists.
      ;;------------------------------
      (test<imp>:should:marker test-name "cons: `:key-0'")
      (setq alist/deleted (imp--alist-delete :key-0 alist/cons))
      (should alist/deleted)
      ;; Our return value should be our alist.
      (should (eq alist/deleted alist/cons))
      (should-not (imp--alist-get-value :key-0 alist/cons))

      (test<imp>:should:marker test-name "list: `:key-0'")
      (setq alist/deleted (imp--alist-delete :key-0 alist/list))
      (should alist/deleted)
      ;; Our return value should be our alist.
      (should (eq alist/deleted alist/list))
      (should-not (imp--alist-get-value :key-0 alist/list)))))


;;------------------------------
;; imp--alist-delete - global alist
;;------------------------------

(ert-deftest test<imp-alist>::imp--alist-delete--global ()
  "Test that `imp--alist-delete' will delete keys from the alist correctly."
  (test<imp>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<imp-alist>::imp--alist-delete--global"
      (lambda (_)
        "Set up globals for this test."
        (setq test<imp-alist>:alist/cons (list (cons :key-0 :value-0/initial)
                                               (cons :key-1 :value-1/initial)
                                               (cons :key-2 :value-2/initial)))
        (setq test<imp-alist>:alist/list (list (list :key-0 :value-0/initial)
                                               (list :key-1 :value-1/initial)
                                               (list :key-2 :value-2/initial)))
        )
      (lambda (_)
        "Clean up the alist of lists."
        ;; (unintern test<imp-alist>:alist/cons)
        ;; (unintern test<imp-alist>:alist/list)
        )

    ;;===
    ;; Run the test.
    ;;===
    (let* (alist/deleted)

      ;;------------------------------
      ;; Delete keys from the alists.
      ;;------------------------------
      (test<imp>:should:marker test-name "cons: `:key-0'")
      (setq alist/deleted (imp--alist-delete :key-0 test<imp-alist>:alist/cons))
      (should alist/deleted)
      ;; Our return value should be our alist.
      (should (eq alist/deleted test<imp-alist>:alist/cons))
      (should-not (imp--alist-get-value :key-0 test<imp-alist>:alist/cons))

      (test<imp>:should:marker test-name "list: `:key-0'")
      (setq alist/deleted (imp--alist-delete :key-0 test<imp-alist>:alist/list))
      (should alist/deleted)
      ;; Our return value should be our alist.
      (should (eq alist/deleted test<imp-alist>:alist/list))
      (should-not (imp--alist-get-value :key-0 test<imp-alist>:alist/list)))))


;;------------------------------------------------------------------------------
;; Tests: Regression Tests
;;----------
;; Test that bugs don't resurface.
;;------------------------------------------------------------------------------

;;------------------------------
;; imp--alist-update
;;------------------------------

(ert-deftest test<imp-alist>::imp--alist-update--regression-call-for-alist ()
  "Test that `imp--alist-update' can work if you use a macro call that
returns a symbol-name as a parameter.

Bug came from:
  (imp--layout-unbind :debug :testing :common '(:n \"s\" :layout:common:undefined))

----------

[BUG]:
  Calling with a function call (that returns an alist's symbol name) raises an error.

[FIX]:
  - Simplified macros for update and delete a lot: moved updating/deleting to a
    helper function and macro just saves results back to provided symbol."
  (test<imp>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<imp-alist>::imp--alist-update--regression-call-for-alist"
      #'test<imp-alist>:setup
      #'test<imp-alist>:teardown

    ;;===
    ;; Run the test.
    ;;===
    (should test<imp-alist>:alist/values)
    (should-error alist/values)

    (let* ((alist/values (list (cons :key-0 :value-0/initial)
                               (cons :key-1 :value-1/initial)
                               (cons :key-2 :value-2/initial)))
           (alist/nil nil))

      ;;------------------------------
      ;; Update should work when passing in the symbol itself.
      ;;------------------------------
      (should (imp--alist-update :key-3
                                     :value-3/00
                                     ;; Passes in value of `alist/values'.
                                     alist/values))
      (should (imp--alist-update :key-3
                                     :value-3/01
                                     ;; Passes in value of `alist/nil'.
                                     alist/nil))
      (should (imp--alist-update :key-3
                                     :value-3/02
                                     ;; Passes in value of `alist/nil'.
                                     test<imp-alist>:alist/values))

      ;;------------------------------
      ;; Update should work w/ a macro call which returns the symbol name.
      ;;------------------------------
      (should (eq 'alist/values
                  (test<imp-alist>:alist:get :local 'alist/values)))
      (should (eq 'test<imp-alist>:alist/nil
                  (test<imp-alist>:alist:get :global/nil)))
      (should (eq 'test<imp-alist>:alist/values
                  (test<imp-alist>:alist:get :global/values)))

      ;; Dunno how to get lexicals to work? :(
      (should-error (imp--alist-update :key-3
                                           :value-3/07
                                           (test<imp-alist>:alist:get :local 'alist/values)))

      ;; Should update a list if given the values variable name.
      (should (imp--alist-update :key-3
                                     :value-3/08
                                     (test<imp-alist>:alist:get :global/values)))
      (should (eq :value-3/08
                  (imp--alist-get-value :key-3 test<imp-alist>:alist/values)))

      ;; Should create a list if given the nil variable name.
      (should (eq nil
                  test<imp-alist>:alist/nil))
      (should (imp--alist-update :key-3
                                     :value-3/09
                                     (test<imp-alist>:alist:get :global/nil)))
      (should (eq :value-3/09
                  (imp--alist-get-value :key-3 test<imp-alist>:alist/nil))))))


