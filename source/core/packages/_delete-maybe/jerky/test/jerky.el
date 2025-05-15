;;; core/modules/elisp/jerky/test/jerky.el --- Tests for "jerky/jerky.el" -*- no-byte-compile: t; lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-06-15
;; Timestamp:  2023-06-29
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Tests for "jerky/jerky.el".
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Test Requirements
;;------------------------------------------------------------------------------

(imp:test:load :feature:post '(:jerky test base)
               :filename     "base")


;; ╔═════════════════════════════╤═══════════╤═════════════════════════════════╗
;; ╟─────────────────────────────┤ ERT TESTS ├─────────────────────────────────╢
;; ╠════════════════════╤════════╧═══════════╧════════╤════════════════════════╣
;; ╟────────────────────┤ Does the Jerky taste right? ├────────────────────────╢
;; ╚════════════════════╧═════════════════════════════╧════════════════════════╝


;;------------------------------------------------------------------------------
;; Tests: Jerky Key/Value Repo
;;------------------------------------------------------------------------------

;;------------------------------
;; int<jerky>:parse
;;------------------------------

(ert-deftest test<jerky/jerky>::int<jerky>:parse ()
  "Test that `int<jerky>:parse' behaves appropriately."
  (test<jerky>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<jerky/jerky>::int<jerky>:parse"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    (should (equal
             '("foo/bar/baz" :namespace qux :value 1 :docstr nil)
             (int<jerky>:parse '(foo bar baz :namespace qux :value 1)
                               ;; t means "default of `:namespace', `:value', and `:docstr', please"
                               t)))

    (should (equal
             '("foo/bar/baz" :namespace qux :value 1 :docstr nil :DNE nil :baz "hello")
             (int<jerky>:parse '(foo bar baz :namespace qux :value 1 :baz "hello")
                               ;; `:namespace', `:value', and `:docstr'...
                               ;;   ...plus `:baz' and `:DNE'.
                               t :baz :DNE)))))


;;------------------------------
;; int<jerky>:namespace:valid?
;;------------------------------

(ert-deftest test<jerky/jerky>::int<jerky>:namespace:valid? ()
  "Test that `int<jerky>:namespace:valid?' behaves appropriately."
  (test<jerky>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<jerky/jerky>::int<jerky>:namespace:valid?"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    (should (int<jerky>:namespace:valid? :jeff))
    (should-not (int<jerky>:namespace:valid? 'jeff t))
    (should-error (int<jerky>:namespace:valid? 'jeff))))


;;------------------------------
;; int<jerky>:namespace:entry:get
;;------------------------------

(ert-deftest test<jerky/jerky>::int<jerky>:namespace:entry:get ()
  "Test that `int<jerky>:namespace:entry:get' behaves appropriately."
  (test<jerky>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<jerky/jerky>::int<jerky>:namespace:entry:get"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    (let ((entry (int<jerky>:namespace:entry:get :default)))
      (should entry)
      (should (listp entry))

      (should (keywordp (nth 0 entry)))
      (should (stringp (nth 1 entry)))
      (should (stringp (nth 2 entry)))
      (should (listp (nth 3 entry)))

      (should (eq :default
                  (nth 0 entry)))
      (should (string= "Default/Fallback Namespace"
                       (nth 1 entry)))
      (should (string= "Default namespace for jerky. Other namespaces default to this for fallbacks."
                       (nth 2 entry)))
      (should (eq 1
                  (length (nth 3 entry))))
      (should (eq :no-fallback
                  (nth 0 (nth 3 entry)))))))


;;------------------------------
;; int<jerky>:namespace:entry/namespace:get
;;------------------------------

(ert-deftest test<jerky/jerky>::int<jerky>:namespace:entry/namespace:get ()
  "Test that `int<jerky>:namespace:entry/namespace:get' behaves appropriately."
  (test<jerky>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<jerky/jerky>::int<jerky>:namespace:entry/namespace:get"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    (let ((entry (int<jerky>:namespace:entry:get :default)))
      (should entry)
      (should (listp entry))

      (should (keywordp (nth 0 entry)))

      (should (eq :default
                  (nth 0 entry)))

      (should (eq :default
                  (int<jerky>:namespace:entry/namespace:get entry))))))


;;------------------------------
;; int<jerky>:namespace:entry/title:get
;;------------------------------

(ert-deftest test<jerky/jerky>::int<jerky>:namespace:entry/title:get ()
  "Test that `int<jerky>:namespace:entry/title:get' behaves appropriately."
  (test<jerky>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<jerky/jerky>::int<jerky>:namespace:entry/title:get"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    (let ((entry (int<jerky>:namespace:entry:get :default)))
      (should entry)
      (should (listp entry))

      (should (stringp (nth 1 entry)))

      (should (string= "Default/Fallback Namespace"
                       (nth 1 entry)))

      (should (string= "Default/Fallback Namespace"
                       (int<jerky>:namespace:entry/title:get entry))))))


;;------------------------------
;; int<jerky>:namespace:entry/docstr:get
;;------------------------------

(ert-deftest test<jerky/jerky>::int<jerky>:namespace:entry/docstr:get ()
  "Test that `int<jerky>:namespace:entry/docstr:get' behaves appropriately."
  (test<jerky>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<jerky/jerky>::int<jerky>:namespace:entry/docstr:get"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    (let ((entry (int<jerky>:namespace:entry:get :default)))
      (should entry)
      (should (listp entry))

      (should (stringp (nth 2 entry)))

      (should (string= "Default namespace for jerky. Other namespaces default to this for fallbacks."
                       (nth 2 entry)))

      (should (string= "Default namespace for jerky. Other namespaces default to this for fallbacks."
                       (int<jerky>:namespace:entry/docstr:get entry))))))


;;------------------------------
;; int<jerky>:namespace:entry/fallback:get
;;------------------------------

(ert-deftest test<jerky/jerky>::int<jerky>:namespace:entry/fallback:get ()
  "Test that `int<jerky>:namespace:entry/fallback:get' behaves appropriately."
  (test<jerky>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<jerky/jerky>::int<jerky>:namespace:entry/fallback:get"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    (let ((entry (int<jerky>:namespace:entry:get :default)))
      (should entry)
      (should (listp entry))

      (should (listp (nth 3 entry)))
      (should (listp (int<jerky>:namespace:entry/fallback:get entry)))

      (should (eq 1
                  (length (nth 3 entry))))
      (should (eq 1
                  (length (int<jerky>:namespace:entry/fallback:get entry))))

      (should (eq :no-fallback
                  (nth 0 (nth 3 entry))))
      (should (eq :no-fallback
                  (nth 0 (int<jerky>:namespace:entry/fallback:get entry)))))))


;;------------------------------
;; int<jerky>:namespace:entry:set
;;------------------------------

(ert-deftest test<jerky/jerky>::int<jerky>:namespace:entry:set ()
  "Test that `int<jerky>:namespace:entry:set' behaves appropriately."
  (test<jerky>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<jerky/jerky>::int<jerky>:namespace:entry:set"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    (let ((namespace :test)
          (title "Test Namespace Title")
          (docstr "Hello there.")
          (fallbacks '(:foo :default))
          entry)

      (setq entry (int<jerky>:namespace:entry:set namespace title docstr fallbacks))

      (should entry)
      (should (listp entry))

      (should (eq namespace
                  (int<jerky>:namespace:entry/namespace:get entry)))

      (should (string= title
                       (int<jerky>:namespace:entry/title:get entry)))

      (should (string= docstr
                       (int<jerky>:namespace:entry/docstr:get entry)))

      (should (listp (int<jerky>:namespace:entry/fallback:get entry)))
      (should (= 2 (length (int<jerky>:namespace:entry/fallback:get entry))))
      (should (eq :foo
                  (nth 0 (int<jerky>:namespace:entry/fallback:get entry))))
      (should (eq :default
                  (nth 1 (int<jerky>:namespace:entry/fallback:get entry)))))))


;;------------------------------
;; int<jerky>:namespace:set
;;------------------------------

(ert-deftest test<jerky/jerky>::int<jerky>:namespace:set ()
  "Test that `int<jerky>:namespace:set' behaves appropriately."
  (test<jerky>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<jerky/jerky>::int<jerky>:namespace:set"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    ;;------------------------------
    ;; Test create.
    ;;------------------------------
    (let* ((namespace :test:00)
           (title "Test Namespace Title 00")
           (docstr "Hello there, 00.")
           (fallbacks '(:default))
           (entry (int<jerky>:namespace:entry:set namespace title docstr fallbacks))
           entry/updated
           entry/get)

      (setq entry/updated (int<jerky>:namespace:set entry))
      (should entry/updated)
      (should (listp entry/updated))

      (setq entry/get (int<jerky>:namespace:entry:get namespace))
      (should entry/get)
      (should (listp entry/get))

      (should (eq namespace
                  (int<jerky>:namespace:entry/namespace:get entry/updated)))
      (should (eq (int<jerky>:namespace:entry/namespace:get entry/updated)
                  (int<jerky>:namespace:entry/namespace:get entry/get)))

      (should (string= title
                       (int<jerky>:namespace:entry/title:get entry/updated)))
      (should (string= (int<jerky>:namespace:entry/title:get entry/updated)
                       (int<jerky>:namespace:entry/title:get entry/get)))

      (should (string= docstr
                       (int<jerky>:namespace:entry/docstr:get entry/updated)))
      (should (string= (int<jerky>:namespace:entry/docstr:get entry/updated)
                       (int<jerky>:namespace:entry/docstr:get entry/get)))

      (should (listp (int<jerky>:namespace:entry/fallback:get entry/updated)))
      (should (listp (int<jerky>:namespace:entry/fallback:get entry/get)))
      (should (= 1 (length (int<jerky>:namespace:entry/fallback:get entry/updated))))
      (should (= 1 (length (int<jerky>:namespace:entry/fallback:get entry/get))))
      (should (eq :default
                  (nth 0 (int<jerky>:namespace:entry/fallback:get entry/updated))))
      (should (eq :default
                  (nth 0 (int<jerky>:namespace:entry/fallback:get entry/get)))))

    ;; Need another one so we can have fallbacks...
    (let* ((namespace :test:01)
           (title "Test Namespace Title 01")
           (docstr "Hello there, 01.")
           (fallbacks '(:default))
           (entry (int<jerky>:namespace:entry:set namespace title docstr fallbacks))
           entry/updated)

      (setq entry/updated (int<jerky>:namespace:set entry))
      (should entry/updated)
      (should (listp entry/updated))

      (setq entry/get (int<jerky>:namespace:entry:get namespace))
      (should entry/get)
      (should (listp entry/get))

      (should (eq namespace
                  (int<jerky>:namespace:entry/namespace:get entry/updated)))
      (should (eq (int<jerky>:namespace:entry/namespace:get entry/updated)
                  (int<jerky>:namespace:entry/namespace:get entry/get)))

      (should (string= title
                       (int<jerky>:namespace:entry/title:get entry/updated)))
      (should (string= (int<jerky>:namespace:entry/title:get entry/updated)
                       (int<jerky>:namespace:entry/title:get entry/get)))

      (should (string= docstr
                       (int<jerky>:namespace:entry/docstr:get entry/updated)))
      (should (string= (int<jerky>:namespace:entry/docstr:get entry/updated)
                       (int<jerky>:namespace:entry/docstr:get entry/get)))

      (should (listp (int<jerky>:namespace:entry/fallback:get entry/updated)))
      (should (listp (int<jerky>:namespace:entry/fallback:get entry/get)))
      (should (= 1 (length (int<jerky>:namespace:entry/fallback:get entry/updated))))
      (should (= 1 (length (int<jerky>:namespace:entry/fallback:get entry/get))))
      (should (eq :default
                  (nth 0 (int<jerky>:namespace:entry/fallback:get entry/updated))))
      (should (eq :default
                  (nth 0 (int<jerky>:namespace:entry/fallback:get entry/get)))))

    ;;------------------------------
    ;; Test update/overwrite.
    ;;------------------------------
    (let* ((namespace :test:00)
           (title "New Test Namespace Title")
           (docstr "New Docstr.")
           (fallbacks '(:test:01 :default))
           (entry (int<jerky>:namespace:entry:set namespace title docstr fallbacks))
           entry/updated
           entry/get)

      (setq entry/updated (int<jerky>:namespace:set entry))
      (should entry/updated)
      (should (listp entry/updated))

      (setq entry/get (int<jerky>:namespace:entry:get namespace))
      (should entry/get)
      (should (listp entry/get))

      (should (eq namespace
                  (int<jerky>:namespace:entry/namespace:get entry/updated)))
      (should (eq (int<jerky>:namespace:entry/namespace:get entry/updated)
                  (int<jerky>:namespace:entry/namespace:get entry/get)))

      (should (string= title
                       (int<jerky>:namespace:entry/title:get entry/updated)))
      (should (string= (int<jerky>:namespace:entry/title:get entry/updated)
                       (int<jerky>:namespace:entry/title:get entry/get)))

      (should (string= docstr
                       (int<jerky>:namespace:entry/docstr:get entry/updated)))
      (should (string= (int<jerky>:namespace:entry/docstr:get entry/updated)
                       (int<jerky>:namespace:entry/docstr:get entry/get)))

      (should (listp (int<jerky>:namespace:entry/fallback:get entry/updated)))
      (should (listp (int<jerky>:namespace:entry/fallback:get entry/get)))
      (should (= 2 (length (int<jerky>:namespace:entry/fallback:get entry/updated))))
      (should (= 2 (length (int<jerky>:namespace:entry/fallback:get entry/get))))
      (should (eq :test:01
                  (nth 0 (int<jerky>:namespace:entry/fallback:get entry/updated))))
      (should (eq :test:01
                  (nth 0 (int<jerky>:namespace:entry/fallback:get entry/get))))
      (should (eq :default
                  (nth 1 (int<jerky>:namespace:entry/fallback:get entry/updated))))
      (should (eq :default
                  (nth 1 (int<jerky>:namespace:entry/fallback:get entry/get)))))

    ;;------------------------------
    ;; Test delete.
    ;;------------------------------
    (let* ((namespace :test:00)
           (title "New Test Namespace Title")
           (docstr "New Docstr.")
           (fallbacks '(:default))
           (entry (int<jerky>:namespace:entry:set namespace title docstr fallbacks))
           entry/updated
           entry/get)

      ;;---
      ;; Check returned entry and also entry from get.
      ;;---
      ;; DELETE!!!
      (setq entry/updated (int<jerky>:namespace:set entry jerky:action/delete))
      (should-not entry/updated)

      (setq entry/get (int<jerky>:namespace:entry:get namespace))
      (should-not entry/get))

    ;;------------------------------
    ;; Test create without fallbacks.
    ;;------------------------------
    ;; Should get `:default' as the fallback.
    (let* ((namespace :test:nil-fallbacks)
           (title "Test Nil Fallbacks")
           (docstr "null docstr")
           (fallbacks nil)
           (entry (int<jerky>:namespace:entry:set namespace title docstr fallbacks))
           entry/updated
           entry/get)

      (setq entry/updated (int<jerky>:namespace:set entry))
      (should entry/updated)
      (should (listp entry/updated))

      (setq entry/get (int<jerky>:namespace:entry:get namespace))
      (should entry/get)
      (should (listp entry/get))

      (should (eq namespace
                  (int<jerky>:namespace:entry/namespace:get entry/updated)))

      (should (string= title
                       (int<jerky>:namespace:entry/title:get entry/updated)))
      (should (string= (int<jerky>:namespace:entry/title:get entry/updated)
                       (int<jerky>:namespace:entry/title:get entry/get)))

      (should (string= docstr
                       (int<jerky>:namespace:entry/docstr:get entry/updated)))
      (should (string= (int<jerky>:namespace:entry/docstr:get entry/updated)
                       (int<jerky>:namespace:entry/docstr:get entry/get)))

      (should (listp (int<jerky>:namespace:entry/fallback:get entry/updated)))
      (should (listp (int<jerky>:namespace:entry/fallback:get entry/get)))
      (should (= 1 (length (int<jerky>:namespace:entry/fallback:get entry/updated))))
      (should (= 1 (length (int<jerky>:namespace:entry/fallback:get entry/get))))
      (should (eq :default
                  (nth 0 (int<jerky>:namespace:entry/fallback:get entry/updated))))
      (should (eq :default
                  (nth 0 (int<jerky>:namespace:entry/fallback:get entry/get)))))))


;;------------------------------
;; jerky:namespace:create
;;------------------------------

(ert-deftest test<jerky/jerky>::jerky:namespace:create ()
  "Test that `jerky:namespace:create' behaves appropriately."
  (test<jerky>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<jerky/jerky>::jerky:namespace:create"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    ;;------------------------------
    ;; Create w/o fallbacks.
    ;;------------------------------
    (let* ((namespace :test:00)
           (title "Zeroith Namespace Title")
           (docstr "Hello there, Double Ought.")
           (fallbacks '(:default)) ;; What we want as fallbacks from not supplying any fallbacks.
           entry/created)

      ;; Don't even supply `:fallbacks' - should get `:default'
      (setq entry/created (jerky:namespace:create namespace
                                                  :title title
                                                  :docstr docstr))
      (should entry/created)
      (should (listp entry/created))

      (should (eq namespace
                  (int<jerky>:namespace:entry/namespace:get entry/created)))

      (should (string= title
                       (int<jerky>:namespace:entry/title:get entry/created)))

      (should (string= docstr
                       (int<jerky>:namespace:entry/docstr:get entry/created)))

      (should (listp (int<jerky>:namespace:entry/fallback:get entry/created)))
      (should (= 1 (length (int<jerky>:namespace:entry/fallback:get entry/created))))
      (should (eq :default
                  (nth 0 (int<jerky>:namespace:entry/fallback:get entry/created)))))

    ;;------------------------------
    ;; Create w/ nil fallbacks.
    ;;------------------------------
    (let* ((namespace :test:01)
           (title "First Namespace Title")
           (docstr "Hello there, Number One.")
           (fallbacks nil)
           entry/created)

      (setq entry/created (jerky:namespace:create namespace
                                                  :title title
                                                  :docstr docstr
                                                  :fallbacks fallbacks))
      (should entry/created)
      (should (listp entry/created))

      (should (eq namespace
                  (int<jerky>:namespace:entry/namespace:get entry/created)))

      (should (string= title
                       (int<jerky>:namespace:entry/title:get entry/created)))

      (should (string= docstr
                       (int<jerky>:namespace:entry/docstr:get entry/created)))

      (should (listp (int<jerky>:namespace:entry/fallback:get entry/created)))
      (should (= 1 (length (int<jerky>:namespace:entry/fallback:get entry/created))))
      (should (eq :default
                  (nth 0 (int<jerky>:namespace:entry/fallback:get entry/created)))))


    ;;------------------------------
    ;; Create w/ fallbacks.
    ;;------------------------------
    (let* ((namespace :test:01)
           (title "First Namespace Title")
           (docstr "Hello there, Number One.")
           (fallbacks '(nil :foo nil :bar nil)) ;; nils should get filtered out.
           entry/created)

      (setq entry/created (jerky:namespace:create namespace
                                                  :title title
                                                  :docstr docstr
                                                  :fallbacks fallbacks))
      (should entry/created)
      (should (listp entry/created))

      (should (eq namespace
                  (int<jerky>:namespace:entry/namespace:get entry/created)))

      (should (string= title
                       (int<jerky>:namespace:entry/title:get entry/created)))

      (should (string= docstr
                       (int<jerky>:namespace:entry/docstr:get entry/created)))

      (should (listp (int<jerky>:namespace:entry/fallback:get entry/created)))
      (should (= 2 (length (int<jerky>:namespace:entry/fallback:get entry/created))))
      (should (eq :foo
                  (nth 0 (int<jerky>:namespace:entry/fallback:get entry/created))))
      (should (eq :bar
                  (nth 1 (int<jerky>:namespace:entry/fallback:get entry/created)))))))


;;------------------------------
;; jerky:namespace:has
;;------------------------------

(ert-deftest test<jerky/jerky>::jerky:namespace:has ()
  "Test that `jerky:namespace:has' behaves appropriately."
  (test<jerky>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<jerky/jerky>::jerky:namespace:has"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    ;;------------------------------
    ;; Create namespaces to test for.
    ;;------------------------------
    (should (jerky:namespace:create :test:00
                                    :title "title 00"
                                    :docstr "docstr 00"))
    (should (jerky:namespace:create :test:01
                                    :title "title 01"
                                    :docstr "docstr 01"))

    ;;------------------------------
    ;; Test existance of namespaces.
    ;;------------------------------
    (should (jerky:namespace:has :test:00))
    (should (jerky:namespace:has :test:01))
    (should (jerky:namespace:has :default))

    (should-not (jerky:namespace:has :foo))
    (should-not (jerky:namespace:has :bar))
    (should-not (jerky:namespace:has :test:02))))


;;------------------------------
;; int<jerky>:namespace/ordered
;;------------------------------

(ert-deftest test<jerky/jerky>::int<jerky>:namespace/ordered ()
  "Test that `int<jerky>:namespace/ordered' behaves appropriately."
  (test<jerky>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<jerky/jerky>::int<jerky>:namespace/ordered"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    ;; Non-existant namespaces can't be fallbacks.
    (should-error (jerky:namespace:create :test:dne
                                    :title "title 02"
                                    :docstr "docstr 02"
                                    :fallbacks '(:foo :bar)))

    ;;------------------------------
    ;; Create namespaces to test for.
    ;;------------------------------
    ;; Just default fallback.
    (should (jerky:namespace:create :test:00
                                    :title "title 00"
                                    :docstr "docstr 00"))

    ;; Multiple fallbacks: :test:00 & :default
    (should (jerky:namespace:create :test:01
                                    :title "title 01"
                                    :docstr "docstr 01"
                                    :fallbacks '(:test:00)))

    (should (jerky:namespace:create :test:02
                                    :title "title 02"
                                    :docstr "docstr 02"
                                    :fallbacks (list jerky:namespace/no-fallback)))

    ;; Cascading fallbacks.
    (should (jerky:namespace:create :test:03/with-default
                                    :title "title 03"
                                    :docstr "docstr 03"
                                    :fallbacks '(:test:02 :test:01 :default)))

    (should (jerky:namespace:create :test:03/no-default
                                    :title "title 03"
                                    :docstr "docstr 03"
                                    :fallbacks '(:test:02 :test:01)))

    ;;------------------------------
    ;; Test Fallback Ordering.
    ;;------------------------------

    ;; If namespace isn't found, we should get the default.
    (should (equal '(:default)
                   (int<jerky>:namespace/ordered :does-not-exist)))

    ;; The rest should exist and so returned ordered list should start with the
    ;; requested namespace.

    ;; This is the default: a namespace and `:default' fallback.
    (should (equal '(:test:00 :default)
                   (int<jerky>:namespace/ordered :test:00)))

    ;; These are non-defaults.
    (should (equal '(:test:01 :test:00 :default)
                   (int<jerky>:namespace/ordered :test:01)))
    (should (equal '(:test:02)
                   (int<jerky>:namespace/ordered :test:02)))
    (should (equal '(:test:03/with-default :test:02 :test:01 :test:00 :default)
                   (int<jerky>:namespace/ordered :test:03/with-default)))
    (should (equal '(:test:03/no-default :test:02 :test:01 :test:00)
                   (int<jerky>:namespace/ordered :test:03/no-default)))))


;;------------------------------
;; int<jerky>:key:normalize
;;------------------------------

(ert-deftest test<jerky/jerky>::int<jerky>:key:normalize ()
  "Test that `int<jerky>:key:normalize' behaves appropriately."
  (test<jerky>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<jerky/jerky>::int<jerky>:key:normalize"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    ;; nil remains nil...
    (should-not (int<jerky>:key:normalize nil))

    ;; ...but a list with nil in it becomes empty string.
    (should (string= ""
                     (int<jerky>:key:normalize '(nil))))

    (should (string= "a/b"
                     (int<jerky>:key:normalize "a/b")))
    (should (string= (int<jerky>:key:normalize "a/b")
                     (int<jerky>:key:normalize '("a/b"))))

    (should (string= "a/b/c"
                     (int<jerky>:key:normalize '("a/b" "c"))))
    (should (string= "base/a/b/c"
                     (int<jerky>:key:normalize '(:base "a/b" "c"))))))


;;------------------------------
;; jerky:key:string
;;------------------------------

(ert-deftest test<jerky/jerky>::jerky:key:string ()
  "Test that `jerky:key:string' behaves appropriately."
  (test<jerky>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<jerky/jerky>::jerky:key:string"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    (should (string= ""
                     (jerky:key:string nil)))

    (should (string= "a/b"
                     (jerky:key:string "a/b")))
    (should (string= (jerky:key:string "a/b")
                     (jerky:key:string '("a/b"))))

    (should (string= "a/b/c"
                     (jerky:key:string '("a/b" "c"))))
    (should (string= "base/a/b/c"
                     (jerky:key:string '(:base "a/b" "c"))))))


;;------------------------------
;; int<jerky>:repo/record/namespace:set
;;------------------------------

(ert-deftest test<jerky/jerky>::int<jerky>:repo/record/namespace:set ()
  "Test that `int<jerky>:repo/record/namespace:set' behaves appropriately."
  (test<jerky>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<jerky/jerky>::int<jerky>:repo/record/namespace:set"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    ;;------------------------------
    ;; Create from scratch.
    ;;------------------------------
    (let ((record/initial nil)
          record/updated)
      (setq record/updated
            (int<jerky>:repo/record/namespace:set :create
                                                  "only value"
                                                  "only docstring"
                                                  record/initial))

      (should record/updated)
      (should (equal '((:create "only value" "only docstring"))
                     record/updated)))

    ;;------------------------------
    ;; Add new namespace to existing.
    ;;------------------------------
    (let ((record/initial '((:default "default value" "default docstring")))
          record/updated)

      (setq record/updated
            (int<jerky>:repo/record/namespace:set :test
                                                  "another namespace value"
                                                  "another namespace docstring"
                                                  record/initial))

      (should record/updated)
      (should (equal '((:test "another namespace value" "another namespace docstring")
                       (:default "default value" "default docstring"))
                     record/updated))

      ;;------------------------------
      ;; Delete existing namespace.
      ;;------------------------------
      (setq record/initial record/updated)

      (should (equal '((:test "another namespace value" "another namespace docstring")
                       (:default "default value" "default docstring"))
                     record/initial))

      (setq record/updated
            (int<jerky>:repo/record/namespace:set :test
                                                  jerky:action/delete
                                                  "another namespace docstring"
                                                  record/initial))

      (should (equal '((:default "default value" "default docstring"))
                     record/updated)))))


;;------------------------------
;; int<jerky>:repo/key:set
;;------------------------------

(ert-deftest test<jerky/jerky>::int<jerky>:repo/key:set ()
  "Test that `int<jerky>:repo/key:set' behaves appropriately."
  (test<jerky>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<jerky/jerky>::int<jerky>:repo/key:set"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    ;; Can't guarentee that the input plist was updated or not, so don't test for that.

    ;;------------------------------
    ;; Create plist from nil input.
    ;;------------------------------
    (let ((key :test)
          (plist/initial nil)
          plist/updated)

      (setq plist/updated
            (int<jerky>:repo/key:set key plist/initial))

      (should plist/updated)
      (should (equal (list :key key)
                  plist/updated)))

    ;;------------------------------
    ;; Set existing plist.
    ;;------------------------------
    (let ((key :test/updated)
          (plist/initial '(:key :test/initial))
          plist/updated)

      (setq plist/updated
            (int<jerky>:repo/key:set key plist/initial))

      (should plist/updated)
      (should (equal (list :key key)
                     plist/updated)))))


;;------------------------------
;; int<jerky>:repo/record:set
;;------------------------------

(ert-deftest test<jerky/jerky>::int<jerky>:repo/record:set ()
  "Test that `int<jerky>:repo/record:set' behaves appropriately."
  (test<jerky>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<jerky/jerky>::int<jerky>:repo/record:set"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    ;; Can't guarentee that the input plist was updated or not, so don't test for that.

    ;;------------------------------
    ;; Create plist from nil input.
    ;;------------------------------
    (let ((record :test)
          (plist/initial nil)
          plist/updated)

      (setq plist/updated
            (int<jerky>:repo/record:set record plist/initial))

      (should plist/updated)
      (should (equal (list :record record)
                  plist/updated)))

    ;;------------------------------
    ;; Set existing plist.
    ;;------------------------------
    (let ((record :test/updated)
          (plist/initial '(:record :test/initial))
          plist/updated)

      (setq plist/updated
            (int<jerky>:repo/record:set record plist/initial))

      (should plist/updated)
      (should (equal (list :record record)
                     plist/updated)))

    ;;------------------------------
    ;; Delete.
    ;;------------------------------
    (let ((record jerky:action/delete)
          (plist/initial '(:record :test/initial))
          ;; Delete should return value of `jerky:action/delete' so that
          ;; `int<jerky>:repo/update' functions correctly.
          (output/expected jerky:action/delete)
          plist/updated)

      (setq plist/updated
            (int<jerky>:repo/record:set record plist/initial))

      (should plist/updated)
      (should (eq output/expected
                  plist/updated)))))


;;------------------------------
;; int<jerky>:repo:set
;;------------------------------

(ert-deftest test<jerky/jerky>::int<jerky>:repo:set ()
  "Test that `int<jerky>:repo:set' behaves appropriately."
  (test<jerky>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<jerky/jerky>::int<jerky>:repo:set"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    ;;------------------------------
    ;; Should have an empty Jerky Repo to start...
    ;;------------------------------
    (should int<jerky>:repo)
    (should (hash-table-p int<jerky>:repo))
    (should (hash-table-empty-p int<jerky>:repo))

    ;;------------------------------
    ;; Should not set invalid records.
    ;;------------------------------
    (should-error (int<jerky>:repo:set :test/initial
                                       nil))
    (should-error (int<jerky>:repo:set :test/initial
                                       '(:key :test/initial)))
    (should-error (int<jerky>:repo:set :test/initial
                                       '(:record :test/initial)))

    ;;------------------------------
    ;; Set a record.
    ;;------------------------------
    (let* ((key "test/00")
           (record '((:ns/00   :value/00      "namespace 00 docstring")
                     (:default :value/default "default docstring")))
           (plist/input (list :key key :record record))
           plist/repo)

      (should (int<jerky>:repo:set key plist/input))

      ;; Do we have the thing in `int<jerky>:repo' now?
      (setq plist/repo (gethash key int<jerky>:repo))
      (should plist/repo)
      (should (listp plist/repo))
      (should (plist-member plist/repo :key))
      (should (plist-member plist/repo :record))
      (should (equal key
                     (plist-get plist/repo :key)))
      (should (equal record
                     (plist-get plist/repo :record)))

      ;;------------------------------
      ;; Delete a record.
      ;;------------------------------
      ;; Delete should return nil.
      (should-not (int<jerky>:repo:set key jerky:action/delete))

      ;; Is it deleted now?
      (setq plist/repo (gethash key int<jerky>:repo))
      (should-not plist/repo))))


;;------------------------------
;; int<jerky>:repo/update
;;------------------------------

(ert-deftest test<jerky/jerky>::int<jerky>:repo/update ()
  "Test that `int<jerky>:repo/update' behaves appropriately."
  (test<jerky>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<jerky/jerky>::int<jerky>:repo/update"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    ;;------------------------------
    ;; Should have an empty Jerky Repo to start...
    ;;------------------------------
    (should int<jerky>:repo)
    (should (hash-table-p int<jerky>:repo))
    (should (hash-table-empty-p int<jerky>:repo))

    ;;------------------------------
    ;; Create the record.
    ;;------------------------------
    (let* ((key "test/repo/update")
           (namespace :test:repo/update)
           (value 9001)
           (docstr "Hello there.")
           (record/expected (list :key key
                                  :record (list (list namespace value docstr))))
           plist/repo/create)

      (setq plist/repo/create (int<jerky>:repo/update key namespace value docstr))

      (should plist/repo/create)
      (should (listp plist/repo/create))
      (should (plist-member plist/repo/create :key))
      (should (plist-member plist/repo/create :record))
      (should (equal (plist-get record/expected   :key)
                     (plist-get plist/repo/create :key)))
      (should (equal (plist-get record/expected   :record)
                     (plist-get plist/repo/create :record)))

      ;;------------------------------
      ;; Update/overwrite the record.
      ;;------------------------------
      (let* ((value/update :overwritten)
             (record/expected (list :key key
                                    :record (list (list namespace value/update docstr))))
             plist/repo/update)

        ;; Change the value.
        (setq plist/repo/update (int<jerky>:repo/update key namespace value/update docstr))

        (should plist/repo/update)
        (should (listp plist/repo/update))
        (should (plist-member plist/repo/update :key))
        (should (plist-member plist/repo/update :record))
        (should (equal (plist-get record/expected   :key)
                       (plist-get plist/repo/update :key)))
        (should (equal (plist-get record/expected   :record)
                       (plist-get plist/repo/update :record)))

        ;;------------------------------
        ;; Delete the record.
        ;;------------------------------
        (let* (plist/repo/delete)
          (setq plist/repo/delete (int<jerky>:repo/update key namespace jerky:action/delete docstr))
          (should-not plist/repo/delete))))))


;;------------------------------
;; jerky:set
;;------------------------------

(ert-deftest test<jerky/jerky>::jerky:set ()
  "Test that `jerky:set' behaves appropriately."
  (test<jerky>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<jerky/jerky>::jerky:set"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    ;;------------------------------
    ;; Should have an empty Jerky Repo to start...
    ;;------------------------------
    (should int<jerky>:repo)
    (should (hash-table-p int<jerky>:repo))
    (should (hash-table-empty-p int<jerky>:repo))

    ;;------------------------------
    ;; Create the record.
    ;;------------------------------
    (let* ((key/expected "test/jerky/set")
           (namespace    :test:jerky:set)
           (value        9001)
           (docstr       "Hello there.")
           (record/expected (list :key key/expected
                                  :record (list (list namespace value docstr))))
           plist/repo/create)

      (setq plist/repo/create (jerky:set 'test 'jerky 'set
                                         :value     value
                                         :namespace namespace
                                         :docstr    docstr))

      (should plist/repo/create)
      (should (listp plist/repo/create))
      (should (plist-member plist/repo/create     :key))
      (should (plist-member plist/repo/create     :record))
      (should (equal (plist-get record/expected   :key)
                     (plist-get plist/repo/create :key)))
      (should (equal (plist-get record/expected   :record)
                     (plist-get plist/repo/create :record)))

      ;;------------------------------
      ;; Update/overwrite the record.
      ;;------------------------------
      (let* ((value/update :overwritten)
             (record/expected (list :key key/expected
                                    :record (list (list namespace value/update docstr))))
             plist/repo/update)

        ;; Change the value.
        (setq plist/repo/update (jerky:set 'test 'jerky 'set
                                           :value     value/update
                                           :namespace namespace
                                           :docstr    docstr))

        (should plist/repo/update)
        (should (listp plist/repo/update))
        (should (plist-member plist/repo/update     :key))
        (should (plist-member plist/repo/update     :record))
        (should (equal (plist-get record/expected   :key)
                       (plist-get plist/repo/update :key)))
        (should (equal (plist-get record/expected   :record)
                       (plist-get plist/repo/update :record))))

      ;;------------------------------
      ;; Delete the record.
      ;;------------------------------
      (let* (plist/repo/delete)
        (setq plist/repo/delete (jerky:set 'test 'jerky 'set
                                           :value     jerky:action/delete
                                           :namespace namespace
                                           :docstr    docstr))

        (should-not plist/repo/delete)))))


;;------------------------------
;; int<jerky>:repo:get
;;------------------------------

(ert-deftest test<jerky/jerky>::int<jerky>:repo:get ()
  "Test that `int<jerky>:repo:get' behaves appropriately."
  (test<jerky>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<jerky/jerky>::int<jerky>:repo:get"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    ;;------------------------------
    ;; Should have an empty Jerky Repo to start...
    ;;------------------------------
    (should int<jerky>:repo)
    (should (hash-table-p int<jerky>:repo))
    (should (hash-table-empty-p int<jerky>:repo))

    ;;------------------------------
    ;; Create the repo entry to get.
    ;;------------------------------
    (let* ((key/expected "test/repo/get")
           (namespace    :test:repo/get)
           (value        9001)
           (docstr       "Hello there.")
           (record/expected (list :key key/expected
                                  :record (list (list namespace value docstr))))
           plist/repo/create
           plist/repo/get)

      (setq plist/repo/create (jerky:set 'test 'repo 'get
                                         :value     value
                                         :namespace namespace
                                         :docstr    docstr))
      (should plist/repo/create)

      ;;------------------------------
      ;; Now we can test getting it.
      ;;------------------------------

      (setq plist/repo/get (int<jerky>:repo:get key/expected))
      (should plist/repo/get)
      (should (listp plist/repo/get))
      (should (plist-member plist/repo/get        :key))
      (should (plist-member plist/repo/get        :record))
      (should (equal (plist-get record/expected   :key)
                     (plist-get plist/repo/create :key)))
      (should (equal (plist-get record/expected   :record)
                     (plist-get plist/repo/create :record)))

      (should (equal plist/repo/create
                     plist/repo/get)))))


;;------------------------------
;; int<jerky>:repo/key:get
;;------------------------------

(ert-deftest test<jerky/jerky>::int<jerky>:repo/key:get ()
  "Test that `int<jerky>:repo/key:get' behaves appropriately."
  (test<jerky>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<jerky/jerky>::int<jerky>:repo/key:get"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    ;;------------------------------
    ;; Create one repo entry for testing.
    ;;------------------------------
    (let* ((key "test/repo/key/get")
           (namespace    :test:repo/key/get)
           (value        9001)
           (docstr       "Hello there.")
           (entry (list :key key
                        :record (list (list namespace value docstr)))))

      (should (listp entry))
      (should (plist-member entry :key))
      (should (plist-member entry :record))

      ;;------------------------------
      ;; Test it.
      ;;------------------------------

      (should (string= key
                       (int<jerky>:repo/key:get entry))))))


;;------------------------------
;; int<jerky>:repo/record:get
;;------------------------------

(ert-deftest test<jerky/jerky>::int<jerky>:repo/record:get ()
  "Test that `int<jerky>:repo/record:get' behaves appropriately."
  (test<jerky>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<jerky/jerky>::int<jerky>:repo/record:get"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    ;;------------------------------
    ;; Create one repo entry for testing.
    ;;------------------------------
    (let* ((key "test/repo/record/get")
           (namespace    :test:repo/record/get)
           (value        9001)
           (docstr       "Hello there.")
           (entry (list :key key
                        :record (list (list namespace value docstr))))
           record)

      (should (listp entry))
      (should (plist-member entry :key))
      (should (plist-member entry :record))

      ;;------------------------------
      ;; Test it.
      ;;------------------------------

      (setq record (int<jerky>:repo/record:get entry))

      (should (listp record))
      (should (seq-every-p #'listp record))
      (should (equal (plist-get entry :record)
                     record)))))


;;------------------------------
;; int<jerky>:repo/record/namespace:get
;;------------------------------

(ert-deftest test<jerky/jerky>::int<jerky>:repo/record/namespace:get ()
  "Test that `int<jerky>:repo/record/namespace:get' behaves appropriately."
  (test<jerky>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<jerky/jerky>::int<jerky>:repo/record/namespace:get"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    ;;------------------------------
    ;; Create one repo entry for testing.
    ;;------------------------------
    (let* ((key "test/repo/record/namespace/get")
           (namespace/00    :test:namespace/00)
           (value/00        9001)
           (docstr/00       "Hello there, 00.")
           (namespace/01    :test:namespace/01)
           (value/01        "over nine thousand")
           (docstr/01       "Hi.")
           (value/default    'default)
           (docstr/default   "Default.")
           (record (list (list namespace/00 value/00 docstr/00)
                         (list namespace/01 value/01 docstr/01)
                         (list jerky:namespace/default value/default docstr/default)))
           namespace-assoc)

      (should (listp record))
      (should (alist-get namespace/00 record))

      ;;------------------------------
      ;; Test single namespace.
      ;;------------------------------

      (setq namespace-assoc (int<jerky>:repo/record/namespace:get namespace/00 record))
      (should (equal (list namespace/00 value/00 docstr/00)
                     namespace-assoc))

      (setq namespace-assoc (int<jerky>:repo/record/namespace:get namespace/01 record))
      (should (equal (list namespace/01 value/01 docstr/01)
                     namespace-assoc))

      ;;------------------------------
      ;; Test list of namespaces.
      ;;------------------------------
      ;; Give it a list of namespaces, and it should return first match found.

      ;; 00 first
      (setq namespace-assoc (int<jerky>:repo/record/namespace:get (list namespace/00 namespace/01 :does-not-exist)
                                                                  record))
      (should (equal (list namespace/00 value/00 docstr/00)
                     namespace-assoc))

      ;; 01 first
      (setq namespace-assoc (int<jerky>:repo/record/namespace:get (list namespace/01 namespace/00 :does-not-exist)
                                                                  record))
      (should (equal (list namespace/01 value/01 docstr/01)
                     namespace-assoc))

      ;; Something it won't find first, then 01, so... expect 01.
      (setq namespace-assoc (int<jerky>:repo/record/namespace:get (list :does-not-exist namespace/01 namespace/00)
                                                                  record))
      (should (equal (list namespace/01 value/01 docstr/01)
                     namespace-assoc))

      ;; Unknown -> nil.
      (setq namespace-assoc (int<jerky>:repo/record/namespace:get :does-not-exist
                                                                  record))
      (should-not namespace-assoc))))


;;------------------------------
;; int<jerky>:record/namespace:get
;;------------------------------

(ert-deftest test<jerky/jerky>::int<jerky>:record/namespace:get ()
  "Test that `int<jerky>:record/namespace:get' behaves appropriately."
  (test<jerky>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<jerky/jerky>::int<jerky>:record/namespace:get"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    (let* ((namespace    :test:namespace)
           (value        9001)
           (docstr       "Hello there, 00.")
           (record (list namespace value docstr)))

      (should (eq namespace
                  (int<jerky>:record/namespace:get record))))))


;;------------------------------
;; int<jerky>:record/value:get
;;------------------------------

(ert-deftest test<jerky/jerky>::int<jerky>:record/value:get ()
  "Test that `int<jerky>:record/value:get' behaves appropriately."
  (test<jerky>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<jerky/jerky>::int<jerky>:record/value:get"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    (let* ((namespace    :test:namespace)
           (value        9001)
           (docstr       "Hello there, 00.")
           (record (list namespace value docstr)))

      (should (= value
                 (int<jerky>:record/value:get record))))))


;;------------------------------
;; int<jerky>:record/docstr:get
;;------------------------------

(ert-deftest test<jerky/jerky>::int<jerky>:record/docstr:get ()
  "Test that `int<jerky>:record/docstr:get' behaves appropriately."
  (test<jerky>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<jerky/jerky>::int<jerky>:record/docstr:get"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    (let* ((namespace    :test:namespace)
           (value        9001)
           (docstr       "Hello there, 00.")
           (record (list namespace value docstr)))

      (should (string= docstr
                       (int<jerky>:record/docstr:get record))))))


;;------------------------------
;; jerky:get
;;------------------------------

(ert-deftest test<jerky/jerky>::jerky:get ()
  "Test that `jerky:get' behaves appropriately."
  (test<jerky>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<jerky/jerky>::jerky:get"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    ;;------------------------------
    ;; Set up test values.
    ;;------------------------------

    (let* ((value/default :value:default)
           (docstr/default "Docstring default.")
           (namespace :testspace)
           (value/namespace :value:namespace)
           (docstr/namespace "Docstring namespace."))

      (jerky:namespace:create namespace
                              :title "Test Namespace Title"
                              :docstr "This is the test namespace docstring.")

      ;;---
      ;; Set values into same jerky keys.
      ;;---
      ;; Without namespace.
      (should (jerky:set :test 'jerky:get "00"
                         :value     value/default
                         :docstr    docstr/default))

      ;; With namespace.
      (should (jerky:set :test 'jerky:get "00"
                         :namespace namespace
                         :value     value/namespace
                         :docstr    docstr/namespace))

      ;;------------------------------
      ;; Test getting (no namespace).
      ;;------------------------------

      (should (string= value/default
                       (jerky:get :test 'jerky:get "00")))
      (should (string= value/default
                       (jerky:get :test 'jerky:get "00"
                                  :field :value)))

      (should (string= jerky:namespace/default
                       (jerky:get :test 'jerky:get "00"
                                  :field :namespace)))

      (should (string= docstr/default
                       (jerky:get :test 'jerky:get "00"
                                  :field :docstr)))


      ;;------------------------------
      ;; Test getting (namespaced).
      ;;------------------------------

      (should (string= value/namespace
                       (jerky:get :test 'jerky:get "00"
                                  :namespace namespace)))
      (should (string= value/namespace
                       (jerky:get :test 'jerky:get "00"
                                  :namespace namespace
                                  :field :value)))

      (should (string= namespace
                       (jerky:get :test 'jerky:get "00"
                                  :namespace namespace
                                  :field :namespace)))

      (should (string= docstr/namespace
                       (jerky:get :test 'jerky:get "00"
                                  :namespace namespace
                                  :field :docstr))))))


;;------------------------------------------------------------------------------
;; Untested Functions
;;------------------------------------------------------------------------------
;; There could be more, but these don't have tests:
;;  - jerky:namespace:get
;;  - int<jerky>:search/filter
;;  - jerky:has


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :jerky 'test 'jerky)
