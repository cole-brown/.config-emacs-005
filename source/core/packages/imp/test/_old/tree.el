;;; core/modules/emacs/imp/test/tree.el --- Tests for "tree.el" -*- no-byte-compile: t; lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-05-07
;; Timestamp:  2023-06-22
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Tests for "tree.el"
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Test Requirements
;;------------------------------------------------------------------------------

(test<imp>:init:load "base.el")

(test<imp>:init:load "../feature.el")
(test<imp>:init:load "../alist.el")
(test<imp>:init:load "../tree.el")


;; ╔═════════════════════════════╤═══════════╤═════════════════════════════════╗
;; ╟─────────────────────────────┤ ERT TESTS ├─────────────────────────────────╢
;; ╠════════════════════════╤════╧═══════════╧════╤════════════════════════════╣
;; ╟────────────────────────┤ Imps are people too!├────────────────────────────╢
;; ╚════════════════════════╧═════════════════════╧════════════════════════════╝


;;------------------------------------------------------------------------------
;; Tests: Predicates
;;------------------------------------------------------------------------------

;;------------------------------
;; imp--tree-node?
;;------------------------------

(ert-deftest test<imp-tree>::imp--tree-node? ()
  "Tests that the `imp--tree-node?' predicate functions correctly."
  ;;---
  ;; Not node or tree - false.
  ;;---
  (should (equal (imp--tree-node? nil)
                 nil))
  (should (equal (imp--tree-node? :root)
                 nil))

  ;;---
  ;; A node - true.
  ;;---
  (should (equal (imp--tree-node? '(:root))
                 t))

  ;;---
  ;; A tree - false.
  ;;---
  (should (equal (imp--tree-node? '((:root)))
                 nil)))


;;------------------------------
;; imp--tree-tree?
;;------------------------------

(ert-deftest test<imp-tree>::imp--tree-tree? ()
  "Tests that the `imp--tree-tree?' predicate functions correctly."
  ;;---
  ;; Not node or tree - false.
  ;;---
  (should (equal (imp--tree-tree? nil)
                 nil))
  (should (equal (imp--tree-tree? :root)
                 nil))

  ;;---
  ;; A node - false.
  ;;---
  (should (equal (imp--tree-tree? '(:root))
                 nil))

  ;;---
  ;; A tree - true/false depending on if all children are nodes.
  ;;---
  (should (equal (imp--tree-tree? '((:root)))
                 t))
  ;; Currently only checks direct children, so this is true.
  (should (equal (imp--tree-tree? '((:root (:one (:two))) (:boo (:a :b :c))))
                 t)))


;;------------------------------
;; imp--tree-chain?
;;------------------------------

(ert-deftest test<imp-tree>::imp--tree-chain? ()
  "Tests that the `imp--tree-chain?' predicate functions correctly."
  ;;---
  ;; Just a symbol - false.
  ;;---
  (should (equal (imp--tree-chain? :root)
                 nil))
  (should (equal (imp--tree-chain? 'root)
                 nil))

  ;;---
  ;; A list (don't care about rooted) - true.
  ;;---
  (should (equal (imp--tree-chain? '(:root))
                 t))
  (should (equal (imp--tree-chain? '(root))
                 t))
  (should (equal (imp--tree-chain? '(:root :beer root beer))
                 t))

  ;;---
  ;; A list (/do/ care about rooted) - depends.
  ;;---
  (should (equal (imp--tree-chain? '(:root) t)
                 t))
  (should (equal (imp--tree-chain? '(root) t)
                 nil))
  (should (equal (imp--tree-chain? '(:root :beer root beer) t)
                 t))
  (should (equal (imp--tree-chain? '(root beer :root :beer) t)
                 nil)))


;;------------------------------
;; imp--tree-key-exists?
;;------------------------------

(ert-deftest test<imp-tree>::imp--tree-key-exists? ()
  "Tests that the `imp--tree-key-exists?' predicate functions correctly.

It should only search the root level of the tree for the key."
  (let ((tree '((:root0) (:root1 (:one (:two (:leaf)))))))

    ;;---
    ;; Root of `tree':
    ;;---
    ;; `:root0' should exist in the root of `tree'.
    (should (imp--tree-key-exists? :root0 tree))

    ;; `:root1' should exist in the root of `tree' as well.
    (should (imp--tree-key-exists? :root1 tree))

    ;; Other things should not exist here.
    (should-not (imp--tree-key-exists? :root2 tree))
    (should-not (imp--tree-key-exists? :one tree))
    (should-not (imp--tree-key-exists? :two tree))
    (should-not (imp--tree-key-exists? :leaf tree))

    ;;---
    ;; Branches of `tree':
    ;;---
    (let ((tree (imp--alist-get-value :root1 tree)))
      ;; Previous roots no longer exist.
      (should-not (imp--tree-key-exists? :root0 tree))
      (should-not (imp--tree-key-exists? :root1 tree))

      ;; Other things:
      (should-not (imp--tree-key-exists? :root2 tree))
      (should (imp--tree-key-exists? :one tree))
      (should-not (imp--tree-key-exists? :two tree))
      (should-not (imp--tree-key-exists? :leaf tree))

      ;; And even deeper:
      (let ((tree (imp--alist-get-value :one tree)))
        ;; Previous roots no longer exist.
        (should-not (imp--tree-key-exists? :root0 tree))
        (should-not (imp--tree-key-exists? :root1 tree))
        (should-not (imp--tree-key-exists? :one tree))

        ;; Other things:
        (should-not (imp--tree-key-exists? :root2 tree))
        (should (imp--tree-key-exists? :two tree))
        (should-not (imp--tree-key-exists? :leaf tree))))))


;;------------------------------
;; imp--tree-chain
;;------------------------------

(ert-deftest test<imp-tree>::imp--tree-chain ()
  "Tests that the `imp--tree-chain' predicate functions correctly."

  (should (equal '(:root)
                 (imp--tree-chain '(:root) nil)))

  (should (equal '(:root (:one (:two (:three (:leaf-node)))))
                 (imp--tree-chain '(:root :one :two :three) :leaf-node))))


;;------------------------------
;; imp--tree-create
;;------------------------------

(ert-deftest test<imp-tree>::imp--tree-create ()
  "Tests that the `imp--tree-create' predicate functions correctly.
It should basically make a list of the `imp--tree-chain' output."
  (should (equal '((:root))
                 (imp--tree-create '(:root) nil)))

  (should (equal '((:root (:one (:two (:three (:leaf-node))))))
                 (imp--tree-create '(:root :one :two :three) :leaf-node))))


;;------------------------------
;; imp--tree-branch-update
;;------------------------------

(ert-deftest test<imp-tree>::imp--tree-branch-update ()
  "Tests that the `imp--tree-branch-update' predicate functions correctly."
  (should (equal '((:two (:leaf-node1) (:three (:leaf-node0))))
                 (imp--tree-branch-update '(:two (:leaf-node1))
                                              '((:two (:three (:leaf-node0))))))))


;;------------------------------
;; imp--tree-update-helper
;;------------------------------

(ert-deftest test<imp-tree>::imp--tree-update-helper ()
  "Tests that the `imp--tree-update-helper' predicate functions correctly.

This is not guarenteed to set the tree variable passed in to the updated tree."

  ;;---
  ;; Chain splits from tree:
  ;;---
  (let* ((tree/orig     (imp--tree-create '(:root :one :two :three) :leaf-node0))
         (tree/working  (imp--tree-create '(:root :one :two :three) :leaf-node0))
         (tree/expected '((:root (:one (:two (:free (:leaf-node1)) (:three (:leaf-node0))))))))
    (should (equal tree/orig tree/working))
    (should-not (equal tree/orig tree/expected))

    (should (equal tree/expected
                   (imp--tree-update-helper '(:root :one :two :free)
                                                :leaf-node1
                                                tree/working))))

  ;;---
  ;; Tree doesn't exist:
  ;;---
  (let* ((tree/orig     nil)
         (tree/working  nil)
         (tree/expected '((:root (:one (:two (:free (:leaf-node1))))))))
    (should (equal tree/orig tree/working))
    (should-not (equal tree/orig tree/expected))

    (should (equal tree/expected
                   (imp--tree-update-helper '(:root :one :two :free)
                                                :leaf-node1
                                                tree/working)))

    ;; `imp--tree-update-helper' does not set `tree/working'.
    (should (equal tree/orig tree/working))
    (should-not (equal tree/expected tree/working)))

  ;;---
  ;; Chain doesn't exist in tree:
  ;;---
  (let* ((tree/orig    (imp--tree-create '(:root0 :one :two :three) :leaf-node0))
         (tree/working (imp--tree-create '(:root0 :one :two :three) :leaf-node0))
         (tree/expected '((:root1 (:won (:too (:free (:leaf-node1)))))
                          (:root0 (:one (:two (:three (:leaf-node0))))))))
    (should (equal tree/orig tree/working))
    (should-not (equal tree/orig tree/expected))

    (should (equal tree/expected
                   (imp--tree-update-helper '(:root1 :won :too :free)
                                                :leaf-node1
                                                tree/working))))

  ;;---
  ;; Chain pre-exists in tree:
  ;;---
  (let* ((tree/orig     (imp--tree-create '(:root :one :two :three) :leaf-node0))
         (tree/working  (imp--tree-create '(:root :one :two :three) :leaf-node0))
         (tree/expected '((:root (:one (:two (:leaf-node1) (:three (:leaf-node0))))))))
    (should (equal tree/orig tree/working))
    (should-not (equal tree/orig tree/expected))

    (should (equal tree/expected
                   (imp--tree-update-helper '(:root :one :two)
                                                :leaf-node1
                                                tree/working))))

  ;;---
  ;; Reach end of tree before end of chain:
  ;;---
  (let* ((tree/orig     (imp--tree-create '(:root :one :two :three) :leaf-node0))
         (tree/working  (imp--tree-create '(:root :one :two :three) :leaf-node0))
         (tree/expected '((:root (:one (:two (:three (:four (:leaf-node1)) (:leaf-node0))))))))
    (should (equal tree/orig tree/working))
    (should-not (equal tree/orig tree/expected))

    (should (equal tree/expected
                   (imp--tree-update-helper '(:root :one :two :three :four)
                                                :leaf-node1
                                                tree/working))))

  ;;---
  ;; Chain w/ null value:
  ;;---
  (let* ((tree/orig     (imp--tree-create '(:root :one :two :three) :leaf-node0))
         (tree/working  (imp--tree-create '(:root :one :two :three) :leaf-node0))
         (tree/expected '((:root (:one (:two (:free) (:three (:leaf-node0))))))))
    (should (equal tree/orig tree/working))
    (should-not (equal tree/orig tree/expected))

    (should (equal tree/expected
                   (imp--tree-update-helper '(:root :one :two :free)
                                                nil
                                                tree/working)))))


;;------------------------------
;; imp--tree-update
;;------------------------------

(ert-deftest test<imp-tree>::imp--tree-update ()
  "Tests that the `imp--tree-update' predicate functions correctly.

This should guarenteed setting the tree variable passed in to the updated tree."

  ;;---
  ;; Chain splits from tree:
  ;;---
  (let* ((tree/orig     (imp--tree-create '(:root :one :two :three) :leaf-node0))
         (tree/working  (imp--tree-create '(:root :one :two :three) :leaf-node0))
         (tree/expected '((:root (:one (:two (:free (:leaf-node1)) (:three (:leaf-node0))))))))
    (should (equal tree/orig tree/working))
    (should-not (equal tree/orig tree/expected))

    (should (equal tree/expected
                   (imp--tree-update '(:root :one :two :free)
                                         :leaf-node1
                                         tree/working)))

    ;; Tree should be updated.
    (should-not (equal tree/orig tree/working))
    (should (equal tree/expected tree/working)))

  ;;---
  ;; Tree doesn't exist:
  ;;---
  (let* ((tree/orig     nil)
         (tree/working  nil)
         (tree/expected '((:root (:one (:two (:free (:leaf-node1))))))))
    (should (equal tree/orig tree/working))
    (should-not (equal tree/orig tree/expected))

    (should (equal tree/expected
                   (imp--tree-update '(:root :one :two :free)
                                         :leaf-node1
                                         tree/working)))

    ;; Tree should be updated.
    (should-not (equal tree/orig tree/working))
    (should (equal tree/expected tree/working)))

  ;;---
  ;; Chain doesn't exist in tree:
  ;;---
  (let* ((tree/orig    (imp--tree-create '(:root0 :one :two :three) :leaf-node0))
         (tree/working (imp--tree-create '(:root0 :one :two :three) :leaf-node0))
         (tree/expected '((:root1 (:won (:too (:free (:leaf-node1)))))
                          (:root0 (:one (:two (:three (:leaf-node0))))))))
    (should (equal tree/orig tree/working))
    (should-not (equal tree/orig tree/expected))

    (should (equal tree/expected
                   (imp--tree-update '(:root1 :won :too :free)
                                         :leaf-node1
                                         tree/working)))

    ;; Tree should be updated.
    (should-not (equal tree/orig tree/working))
    (should (equal tree/expected tree/working)))

  ;;---
  ;; Chain pre-exists in tree:
  ;;---
  (let* ((tree/orig     (imp--tree-create '(:root :one :two :three) :leaf-node0))
         (tree/working  (imp--tree-create '(:root :one :two :three) :leaf-node0))
         (tree/expected '((:root (:one (:two (:leaf-node1) (:three (:leaf-node0))))))))
    (should (equal tree/orig tree/working))
    (should-not (equal tree/orig tree/expected))

    (should (equal tree/expected
                   (imp--tree-update '(:root :one :two)
                          :leaf-node1
                          tree/working)))

    ;; Tree should be updated.
    (should-not (equal tree/orig tree/working))
    (should (equal tree/expected tree/working)))

  ;;---
  ;; Reach end of tree before end of chain:
  ;;---
  (let* ((tree/orig     (imp--tree-create '(:root :one :two :three) :leaf-node0))
         (tree/working  (imp--tree-create '(:root :one :two :three) :leaf-node0))
         (tree/expected '((:root (:one (:two (:three (:four (:leaf-node1)) (:leaf-node0))))))))
    (should (equal tree/orig tree/working))
    (should-not (equal tree/orig tree/expected))

    (should (equal tree/expected
                   (imp--tree-update '(:root :one :two :three :four)
                                         :leaf-node1
                                         tree/working)))

    ;; Tree should be updated.
    (should-not (equal tree/orig tree/working))
    (should (equal tree/expected tree/working)))

  ;;---
  ;; Chain w/ null value:
  ;;---
  (let* ((tree/orig     (imp--tree-create '(:root :one :two :three) :leaf-node0))
         (tree/working  (imp--tree-create '(:root :one :two :three) :leaf-node0))
         (tree/expected '((:root (:one (:two (:free) (:three (:leaf-node0))))))))
    (should (equal tree/orig tree/working))
    (should-not (equal tree/orig tree/expected))

    (should (equal tree/expected
                   (imp--tree-update '(:root :one :two :free)
                                         nil
                                         tree/working)))

    ;; Tree should be updated.
    (should-not (equal tree/orig tree/working))
    (should (equal tree/expected tree/working))))


;;------------------------------
;; imp--tree-contains?
;;------------------------------

(ert-deftest test<imp-tree>::imp--tree-contains? ()
  "Tests that the `imp--tree-contains?' predicate functions correctly."

  (let* ((tree  (imp--tree-create '(:root :one :two :three) :leaf-node0)))

    (imp--tree-contains? '(:root :one :two)
                             tree)

    (imp--tree-contains? '(:root :one :two :free)
                             tree)

    (imp--tree-contains? '(:root1 :one :two)
                             tree))

  (imp--tree-contains? '(:imp test)
                           '((:imp (ort (something (here))) (test)))))
