;;; core/modules/emacs/alist/test/generic.el --- Tests for "generic.el" -*- no-byte-compile: t; lexical-binding: t; -*-
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
;; Test Requirements
;;------------------------------------------------------------------------------

(imp:test:load :filename "base.el")


;; ╔═════════════════════════════╤═══════════╤═════════════════════════════════╗
;; ╟─────────────────────────────┤ ERT TESTS ├─────────────────────────────────╢
;; ╠══════════╤══════════════════╧═══════════╧══════════════════╤══════════════╣
;; ╟──────────┤ Because Emacs needs to give Alists some love... ├──────────────╢
;; ╚══════════╧═════════════════════════════════════════════════╧══════════════╝


;;------------------------------------------------------------------------------
;; Tests: Standard/General 'alist' Functions
;;------------------------------------------------------------------------------

;;------------------------------
;; alist:alist?
;;------------------------------

(ert-deftest test<alist/generic>::alist:alist? ()
  "Test that `alist:alist?' behaves appropriately."
  (test<alist>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<alist/generic>::alist:alist?"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===
    (let ((alist/cons (list (cons :key-0 :value-0)
                            (cons :key-1 :value-1)
                            (cons :key-2 :value-2)))
          (alist/list (list (list :key-0 :value-0)
                            (list :key-1 :value-1)
                            (list :key-2 :value-2))))

      ;;------------------------------
      ;; Not alists.
      ;;------------------------------
      (should-not (alist:alist? t))
      (should-not (alist:alist? :jeff))
      (should-not (alist:alist? #'ignore))
      (should-not (alist:alist? 'some-symbol))

      ;; Quoting our vars is just providing a symbol name, which is
      ;; not providing an alist to the predicate function.
      (should-not (alist:alist? 'alist/cons))
      (should-not (alist:alist? 'alist/list))

      ;;------------------------------
      ;; Are alists.
      ;;------------------------------
      ;; `nil' is a valid (empty) list, and empty lists are valid alists.
      (should (alist:alist? nil))

      ;; Our alist vars should be alists.
      (should (alist:alist? alist/cons))
      (should (alist:alist? alist/list))

      ;; Alists themselves should also be alists.
      (should (alist:alist? (list (cons :key-0 :value-0)
                                  (cons :key-1 :value-1)
                                  (cons :key-2 :value-2))))
      (should (alist:alist? (list (list :key-0 :value-0)
                                  (list :key-1 :value-1)
                                  (list :key-2 :value-2))))

      ;; Alists which have more than one thing as the value: still alists.
      (should (alist:alist? (list (cons :key-0 '(:value-00 :value-01 :value-02))
                                  (cons :key-1 '(:value-11 :value-11 :value-12))
                                  (cons :key-2 '(:value-22 :value-21 :value-22)))))
      (should (alist:alist? (list (list :key-0 :value-00 :value-01 :value-02)
                                  (list :key-1 :value-11 :value-11 :value-12)
                                  (list :key-2 :value-22 :value-21 :value-22)))))))


;;------------------------------
;; alist:generic:get/value
;;------------------------------

(ert-deftest test<alist/generic>::alist:generic:get/value ()
  "Test that `alist:generic:get/value' behaves appropriately."
  (test<alist>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<alist/generic>::alist:generic:get/value"
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
      (test<alist>:should:marker test-name "cons: key-0")
      (setq value/get (alist:generic:get/value :key-0 alist/cons))
      (should value/get)
      (should (stringp value/get))
      (should (string= value/get value/expected:0))

      (test<alist>:should:marker test-name "cons: key-1")
      (setq value/get (alist:generic:get/value :key-1 alist/cons))
      (should value/get)
      (should (keywordp value/get))
      (should (eq value/get value/expected:1))

      (test<alist>:should:marker test-name "cons: key-2")
      (setq value/get (alist:generic:get/value :key-2 alist/cons))
      (should value/get)
      (should (integerp value/get))
      (should (= value/get value/expected:2))

      ;;------------------------------
      ;; Check the 'list' alist.
      ;;------------------------------
      (test<alist>:should:marker test-name "list: key-0")
      (setq value/get (alist:generic:get/value :key-0 alist/list))
      (should value/get)
      (should (listp value/get))
      (should (= 1 (length value/get)))
      (setq value/get (nth 0 value/get))
      (should (stringp value/get))
      (should (string= value/get value/expected:0))

      (test<alist>:should:marker test-name "list: key-1")
      (setq value/get (alist:generic:get/value :key-1 alist/list))
      (should value/get)
      (should (listp value/get))
      (should (= 1 (length value/get)))
      (setq value/get (nth 0 value/get))
      (should (keywordp value/get))
      (should (eq value/get value/expected:1))

      (test<alist>:should:marker test-name "list: key-2")
      (setq value/get (alist:generic:get/value :key-2 alist/list))
      (should value/get)
      (should (listp value/get))
      (should (= 1 (length value/get)))
      (setq value/get (nth 0 value/get))
      (should (integerp value/get))
      (should (= value/get value/expected:2)))))


;;------------------------------
;; alist:generic:get/pair
;;------------------------------

(ert-deftest test<alist/generic>::alist:generic:get/pair ()
  "Test that `alist:generic:get/pair' behaves appropriately."
  (test<alist>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<alist/generic>::alist:generic:get/pair"
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
        (test<alist>:should:marker test-name "cons: key-0")
        (setq value/get (alist:generic:get/pair expected:key alist/cons))
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
        (test<alist>:should:marker test-name "cons: key-1")
        (setq value/get (alist:generic:get/pair expected:key alist/cons))
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
        (test<alist>:should:marker test-name "cons: key-2")
        (setq value/get (alist:generic:get/pair expected:key alist/cons))
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
        (test<alist>:should:marker test-name "list: key-0")
        (setq value/get (alist:generic:get/pair expected:key alist/list))
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
        (test<alist>:should:marker test-name "list: key-1")
        (setq value/get (alist:generic:get/pair expected:key alist/list))
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
        (test<alist>:should:marker test-name "list: key-2")
        (setq value/get (alist:generic:get/pair expected:key alist/list))
        (should value/get)
        (should (listp value/get))
        (setq value/get:key   (nth 0 value/get)
              value/get:value (nth 1 value/get))
        (should (keywordp value/get:key))
        (should (eq value/get:key expected:key))
        (should (integerp value/get:value))
        (should (= value/get:value expected:value))))))


;;------------------------------
;; alist:generic:update - local alist (defined in a `let')
;;------------------------------

(ert-deftest test<alist/generic>::alist:generic:update::local ()
  "Test that `alist:generic:update' will add/overwrite values in a local alist correctly."
  (test<alist>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<alist/generic>::alist:generic:update::local"
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
      (should-not (alist:generic:get/value :key-3 alist/cons))
      (should-not (alist:generic:get/value :key-3 alist/list))

      (test<alist>:should:marker test-name "cons: New Key/Value")
      (setq alist/updated (alist:generic:update :key-3 :value-3/new alist/cons))
      (should alist/updated)

      ;; Our return value should be our alist.
      (should (eq alist/updated alist/cons))
      (setq value/get (alist:generic:get/value :key-3 alist/cons))
      (should value/get)
      (should (keywordp value/get))
      (should (eq value/get :value-3/new))

      (test<alist>:should:marker test-name "list: New Key/Value")
      ;; Add the new value as a list, since it's the `alist/list'.
      (setq alist/updated (alist:generic:update :key-3 '(:value-3/new) alist/list))
      (should alist/updated)
      ;; Our return value should be our alist.
      (should (eq alist/updated alist/list))
      (setq value/get (alist:generic:get/value :key-3 alist/list))
      (should value/get)
      (should (listp value/get))
      (should (= 1 (length value/get)))
      (setq value/get (nth 0 value/get))
      (should (keywordp value/get))
      (should (eq value/get :value-3/new))

      ;;------------------------------
      ;; Update existing key's value.
      ;;------------------------------
      (let ((value/cons (alist:generic:get/value :key-0 alist/cons))
            (value/list (alist:generic:get/value :key-0 alist/list)))
        (should value/cons)
        (should value/list)
        (should (eq value/cons :value-0/initial))
        (should (equal value/list '(:value-0/initial)))

        (test<alist>:should:marker test-name "cons: Update Existing Key/Value")
        (setq alist/updated (alist:generic:update :key-0 :value-0/updated alist/cons))
        (should alist/updated)
        ;; Our return value should be our alist.
        (should (eq alist/updated alist/cons))
        (setq value/get (alist:generic:get/value :key-0 alist/cons))
        (should value/get)
        (should (keywordp value/get))
        (should (eq value/get :value-0/updated))

        (test<alist>:should:marker test-name "list: Update Existing Key/Value")
        ;; Add the new value as a list, since it's the `alist/list'.
        (setq alist/updated (alist:generic:update :key-0 '(:value-0/updated) alist/list))
        (should alist/updated)
        ;; Our return value should be our alist.
        (should (eq alist/updated alist/list))
        (setq value/get (alist:generic:get/value :key-0 alist/list))
        (should value/get)
        (should (listp value/get))
        (should (= 1 (length value/get)))
        (setq value/get (nth 0 value/get))
        (should (keywordp value/get))
        (should (eq value/get :value-0/updated))))))


;;------------------------------
;; alist:generic:update - non-local alist
;;------------------------------

(ert-deftest test<alist/generic>::alist:generic:update::global ()
  "Test that `alist:generic:update' will add/overwrite values in a global alist correctly."
  (test<alist>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<alist/generic>::alist:generic:update::global"
      (lambda (_)
        "Set up globals for this test."
        (setq test<alist>:var:alist/cons (list (cons :key-0 :value-0/initial)
                                               (cons :key-1 :value-1/initial)
                                               (cons :key-2 :value-2/initial)))
        (setq test<alist>:var:alist/list (list (list :key-0 :value-0/initial)
                                               (list :key-1 :value-1/initial)
                                               (list :key-2 :value-2/initial)))
        )
      (lambda (_)
        "Clean up the alist of lists."
        ;; (unintern test<alist>:var:alist/cons)
        ;; (unintern test<alist>:var:alist/list)
        )

    ;;===
    ;; Run the test.
    ;;===
    (let* (alist/updated
           value/get)

      ;;------------------------------
      ;; Add new key/values.
      ;;------------------------------
      (should-not (alist:generic:get/value :key-3 test<alist>:var:alist/cons))
      (should-not (alist:generic:get/value :key-3 test<alist>:var:alist/list))

      (test<alist>:should:marker test-name "cons: New Key/Value")
      (setq alist/updated (alist:generic:update :key-3 :value-3/new test<alist>:var:alist/cons))
      (should alist/updated)

      ;; Our return value should be our alist.
      (should (eq alist/updated test<alist>:var:alist/cons))
      (setq value/get (alist:generic:get/value :key-3 test<alist>:var:alist/cons))
      (should value/get)
      (should (keywordp value/get))
      (should (eq value/get :value-3/new))

      (test<alist>:should:marker test-name "list: New Key/Value")
      ;; Add the new value as a list, since it's the `test<alist>:var:alist/list'.
      (setq alist/updated (alist:generic:update :key-3 '(:value-3/new) test<alist>:var:alist/list))
      (should alist/updated)
      ;; Our return value should be our alist.
      (should (eq alist/updated test<alist>:var:alist/list))
      (setq value/get (alist:generic:get/value :key-3 test<alist>:var:alist/list))
      (should value/get)
      (should (listp value/get))
      (should (= 1 (length value/get)))
      (setq value/get (nth 0 value/get))
      (should (keywordp value/get))
      (should (eq value/get :value-3/new))

      ;;------------------------------
      ;; Update existing key's value.
      ;;------------------------------
      (let ((value/cons (alist:generic:get/value :key-0 test<alist>:var:alist/cons))
            (value/list (alist:generic:get/value :key-0 test<alist>:var:alist/list)))
        (should value/cons)
        (should value/list)
        (should (eq value/cons :value-0/initial))
        (should (equal value/list '(:value-0/initial)))

        (test<alist>:should:marker test-name "cons: Update Existing Key/Value")
        (setq alist/updated (alist:generic:update :key-0 :value-0/updated test<alist>:var:alist/cons))
        (should alist/updated)
        ;; Our return value should be our alist.
        (should (eq alist/updated test<alist>:var:alist/cons))
        (setq value/get (alist:generic:get/value :key-0 test<alist>:var:alist/cons))
        (should value/get)
        (should (keywordp value/get))
        (should (eq value/get :value-0/updated))

        (test<alist>:should:marker test-name "list: Update Existing Key/Value")
        ;; Add the new value as a list, since it's the `test<alist>:var:alist/list'.
        (setq alist/updated (alist:generic:update :key-0 '(:value-0/updated) test<alist>:var:alist/list))
        (should alist/updated)
        ;; Our return value should be our alist.
        (should (eq alist/updated test<alist>:var:alist/list))
        (setq value/get (alist:generic:get/value :key-0 test<alist>:var:alist/list))
        (should value/get)
        (should (listp value/get))
        (should (= 1 (length value/get)))
        (setq value/get (nth 0 value/get))
        (should (keywordp value/get))
        (should (eq value/get :value-0/updated))))))


;;------------------------------
;; alist:generic:delete - local alist (defined in a `let')
;;------------------------------

(ert-deftest test<alist/generic>::alist:generic:delete::local ()
  "Test that `alist:generic:delete' will delete keys from the alist correctly."
  (test<alist>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<alist/generic>::alist:generic:delete::local"
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
      (test<alist>:should:marker test-name "cons: `:key-0'")
      (setq alist/deleted (alist:generic:delete :key-0 alist/cons))
      (should alist/deleted)
      ;; Our return value should be our alist.
      (should (eq alist/deleted alist/cons))
      (should-not (alist:generic:get/value :key-0 alist/cons))

      (test<alist>:should:marker test-name "list: `:key-0'")
      (setq alist/deleted (alist:generic:delete :key-0 alist/list))
      (should alist/deleted)
      ;; Our return value should be our alist.
      (should (eq alist/deleted alist/list))
      (should-not (alist:generic:get/value :key-0 alist/list)))))


;;------------------------------
;; alist:generic:delete - global alist
;;------------------------------

(ert-deftest test<alist/generic>::alist:generic:delete::global ()
  "Test that `alist:generic:delete' will delete keys from the alist correctly."
  (test<alist>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<alist/generic>::alist:generic:delete::global"
      (lambda (_)
        "Set up globals for this test."
        (setq test<alist>:var:alist/cons (list (cons :key-0 :value-0/initial)
                                               (cons :key-1 :value-1/initial)
                                               (cons :key-2 :value-2/initial)))
        (setq test<alist>:var:alist/list (list (list :key-0 :value-0/initial)
                                               (list :key-1 :value-1/initial)
                                               (list :key-2 :value-2/initial)))
        )
      (lambda (_)
        "Clean up the alist of lists."
        ;; (unintern test<alist>:var:alist/cons)
        ;; (unintern test<alist>:var:alist/list)
        )

    ;;===
    ;; Run the test.
    ;;===
    (let* (alist/deleted)

      ;;------------------------------
      ;; Delete keys from the alists.
      ;;------------------------------
      (test<alist>:should:marker test-name "cons: `:key-0'")
      (setq alist/deleted (alist:generic:delete :key-0 test<alist>:var:alist/cons))
      (should alist/deleted)
      ;; Our return value should be our alist.
      (should (eq alist/deleted test<alist>:var:alist/cons))
      (should-not (alist:generic:get/value :key-0 test<alist>:var:alist/cons))

      (test<alist>:should:marker test-name "list: `:key-0'")
      (setq alist/deleted (alist:generic:delete :key-0 test<alist>:var:alist/list))
      (should alist/deleted)
      ;; Our return value should be our alist.
      (should (eq alist/deleted test<alist>:var:alist/list))
      (should-not (alist:generic:get/value :key-0 test<alist>:var:alist/list)))))


;;------------------------------------------------------------------------------
;; Tests: Regression Tests
;;----------
;; Test that bugs don't resurface.
;;------------------------------------------------------------------------------

;;------------------------------
;; alist:generic:update
;;------------------------------

(ert-deftest test<alist/generic>::alist:generic:update::regression/call-for-alist ()
  "Test that `alist:generic:update' can work if you use a macro call that
returns a symbol-name as a parameter.

Bug came from:
  (int<alist>:layout:unbind :debug :testing :common '(:n \"s\" :layout:common:undefined))

----------

[BUG]:
  Calling with a function call (that returns an alist's symbol name) raises an error.

[FIX]:
  - Simplified macros for update and delete a lot: moved updating/deleting to a
    helper function and macro just saves results back to provided symbol."
  (test<alist>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<alist/generic>::alist:generic:update::regression/call-for-alist"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===
    (should test<alist>:var:alist/values)
    (should-error alist/values)

    (let* ((alist/values (list (cons :key-0 :value-0/initial)
                               (cons :key-1 :value-1/initial)
                               (cons :key-2 :value-2/initial)))
           (alist/nil nil))

      ;;------------------------------
      ;; Update should work when passing in the symbol itself.
      ;;------------------------------
      (should (alist:generic:update :key-3
                            :value-3/00
                            ;; Passes in value of `alist/values'.
                            alist/values))
      (should (alist:generic:update :key-3
                            :value-3/01
                            ;; Passes in value of `alist/nil'.
                            alist/nil))
      (should (alist:generic:update :key-3
                            :value-3/02
                            ;; Passes in value of `alist/nil'.
                            test<alist>:var:alist/values))

      ;;------------------------------
      ;; Update should work w/ a macro call which returns the symbol name.
      ;;------------------------------
      (should (eq 'alist/values
                  (test<alist>:var:alist/get :local 'alist/values)))
      (should (eq 'test<alist>:var:alist/nil
                  (test<alist>:var:alist/get :global/nil)))
      (should (eq 'test<alist>:var:alist/values
                  (test<alist>:var:alist/get :global/values)))

      ;; Dunno how to get lexicals to work? :(
      (should-error (alist:generic:update :key-3
                                  :value-3/07
                                  (test<alist>:var:alist/get :local 'alist/values)))

      ;; Should update a list if given the values variable name.
      (should (alist:generic:update :key-3
                            :value-3/08
                            (test<alist>:var:alist/get :global/values)))
      (should (eq :value-3/08
                  (alist:generic:get/value :key-3 test<alist>:var:alist/values)))

      ;; Should create a list if given the nil variable name.
      (should (eq nil
                  test<alist>:var:alist/nil))
      (should (alist:generic:update :key-3
                            :value-3/09
                            (test<alist>:var:alist/get :global/nil)))
      (should (eq :value-3/09
                  (alist:generic:get/value :key-3 test<alist>:var:alist/nil))))))

