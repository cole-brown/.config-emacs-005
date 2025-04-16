;;; imp/tree.el --- imp feature tree helpers -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2023-06-22
;; Timestamp:  2024-10-02
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;                                 ──────────
;; ╔════════════════════════════════════════════════════════════════════════╗
;; ║                                  Tree                                  ║
;; ╚════════════════════════════════════════════════════════════════════════╝
;;                                   ──────
;;                       noun: A woody perennial plant.
;;                                 ──────────
;;
;;; Code:


;;--------------------------------------------------------------------------------
;; Debugging
;;--------------------------------------------------------------------------------
;; Tree debugging will flood out other debugging, so enable/disable separately.

(defvar imp--tree-debug-flag nil
  "Allow `imp--tree-debug' to call `imp--debug'?")


(defun imp--tree-debug (caller string &rest args)
  "Debug function for tree, so it can be disabled normally."
  (when imp--tree-debug-flag
    (apply #'imp--debug
           caller
           string
           args)))


;;------------------------------------------------------------------------------
;; Predicates
;;------------------------------------------------------------------------------

(defun imp--tree-node? (node)
  "Is NODE an imp tree node?

Return t if NODE is a node; nil otherwise (including if NODE is a
tree or nil)."
  ;; A node is:
  ;;   - A list...
  ;;   - ...which has not-a-list as the first item (so not an alist).
  ;;   - First item should be a symbol/keyword.
  (and (not (null node))
       (listp node)
       (not (null (car node)))
       (not (listp (car node)))
       (symbolp (car node))))
;; (imp--tree-node? nil)
;; (imp--tree-node? :root)
;; (imp--tree-node? '(:root))
;; (imp--tree-node? '((alist-key alist-value)))


(defun imp--tree-tree? (tree)
  "Is TREE an imp tree?

Return t if TREE is a tree; nil otherwise (including if TREE is a node)."
  ;; A tree is:
  ;;   - A list...
  ;;   - ...which has nodes for each item in it (so an alist of nodes).
  (if (or (null tree)
          (not (listp tree)))
      nil
    ;; Each item in list must be a node.
    (not
     (seq-contains-p (mapcar #'imp--tree-node? tree)
                     nil))))
;; symbol: no
;;   (imp--tree-tree? :root)
;; node: no
;;   (imp--tree-tree? '(:root))
;; list of node: yes
;;   (imp--tree-tree? '((:root)))
;;   (imp--tree-tree? '((:root :ignored) (:root02) (:root03)))


(defun imp--tree-chain? (chain &optional rooted)
  "Is CHAIN a chain of tree keys?

Return t if CHAIN is a chain of tree keys; nil otherwise (including if CHAIN
is nil)."
  (let (is-chain
        (is-rooted t)) ;; default to true for when not interested in rootedness.

    ;; A chain is:
    ;;   - A list...
    ;;   - ...which has symbols as the only items in it.
    ;; A
    (setq is-chain (if (or (null chain)
                           (not (listp chain)))
                       nil
                     ;; Each item in chain must be a symbol.
                     (seq-every-p #'symbolp chain)))

    ;; Additionally, a rooted chain must have a keyword as the first symbol.
    (when (and is-chain rooted)
      (setq is-rooted (keywordp (car chain))))

    ;; Chain?
    (and is-chain is-rooted)))
;; symbol: no
;;   (imp--tree-chain? :root)
;; list: yes
;;   (imp--tree-chain? '(:root))
;;   (imp--tree-chain? '(root))
;;   (imp--tree-chain? '(:root) t)
;; list, but not rooted: no
;;   (imp--tree-chain? '(root) t)


(defun imp--tree-key-exists? (key tree)
  "Return non-nil if key exists in tree, even if it has no children.

Return value is KEY's entry in TREE, or nil if KEY does not exist."
  (imp--alist-get-pair key tree))
;; (imp--tree-key-exists? :root1 (imp--tree-update '(:root1) nil (imp--tree-create '(:root0 :one :two) :leaf)))


;;------------------------------------------------------------------------------
;; Helper Functions
;;------------------------------------------------------------------------------

(defun imp--tree-chain (chain value)
  "Create an alist entry for a tree from CHAIN and VALUE."
  (unless (listp chain) ;; nil ok too
    (imp--error "imp--tree-chain"
                "Chain cannot be created - expected list, got: %S"
                chain))
  ;; Set-up: Need to build in reverse.
  (let* ((backwards (reverse chain))
         ;; Entry starts off as the final link w/ the value (if the value is not
         ;; nil).
         (entry (if value
                    (list (car backwards) (list value))
                  (list (car backwards))))
         ;; And we still need to process these.
         (link (cadr backwards))
         (remaining (cddr backwards)))

    (imp--tree-debug "imp--tree-chain" "entry:     %S" entry)
    (imp--tree-debug "imp--tree-chain" "link:      %S" link)
    (imp--tree-debug "imp--tree-chain" "remaining: %S" remaining)

    ;; Grow entry by remaining links.
    (while link
      ;; This makes entry an alist of alist(s).
      (setq entry (list link entry))
      ;; Cycle link/remaining to next.
      (setq link (car remaining))
      (setq remaining (cdr remaining)))

    ;; Return the chain of alists for chain/value.
    entry))
;; (imp--tree-chain '(:root) nil)
;; (imp--tree-chain '(:root :one :two :three) :leaf-node)
;; (alist-get :root (list (imp--tree-chain '(:root :one :two :three) :leaf-node)))
;; (alist-get :one (alist-get :root (list (imp--tree-chain '(:root :one :two :three) :leaf-node))))
;; (alist-get :two (alist-get :one (alist-get :root (list (imp--tree-chain '(:root :one :two :three) :leaf-node)))))
;; (alist-get :three (alist-get :two (alist-get :one (alist-get :root (list (imp--tree-chain '(:root :one :two :three) :leaf-node))))))


(defun imp--tree-create (chain value)
  "Creates a TREE with CHAIN and VALUE as the starting content."
  (list (imp--tree-chain chain value)))
;; (imp--tree-create '(:root :one :two :three) :leaf-node)


(defun imp--tree-branch-update (entry branch)
  "Add ENTRY to BRANCH.

ENTRY must be an alist entry for a tree; e.g. a return value from
`imp--tree-chain'.

BRANCH must be a tree - an alist of alists. ENTRY will be added to root of
BRANCH."
  (imp--tree-debug "imp--tree-branch-update" "->entry:  %S" entry)
  (imp--tree-debug "imp--tree-branch-update" "->branch: %S" branch)
  (let* ((key (car entry))
         ;; Need the entry, not the alist, of key's children.
         ;; Need '(:value), not '((:value)).
         (value (cadr entry))
         (siblings (imp--alist-get-value key branch)))
    (imp--tree-debug "imp--tree-branch-update" "  key:      %S" key)
    (imp--tree-debug "imp--tree-branch-update" "  vaule:    %S" value)
    (imp--tree-debug "imp--tree-branch-update" "  siblings: %S" siblings)

    ;; Add new value to its new siblings, update branch and done.
    (push value siblings)
    (imp--tree-debug "imp--tree-branch-update" "updated siblings: %S" siblings)
    (setq branch (imp--alist-update key siblings branch))
    (imp--tree-debug "imp--tree-branch-update" "updated branch: %S" branch)
    branch))
;; (imp--tree-branch-update '(:two (:leaf-node1)) '((:two (:three (:leaf-node0)))))
;; (alist-get :two (imp--tree-branch-update '(:two (:leaf-node1)) '((:two (:three (:leaf-node0))))))
;; (alist-get :leaf-node1 (alist-get :two (imp--tree-branch-update '(:two (:leaf-node1)) '((:two (:three (:leaf-node0)))))))


;;------------------------------------------------------------------------------
;; API for imp:
;;------------------------------------------------------------------------------

(defun imp--tree-update-helper (chain value tree)
  "Adds CHAIN with final VALUE to TREE.

CHAIN should be a list of symbols and/or keywords.
VALUE should be a symbol or keyword.
TREE should be a list or symbol that holds the list.

If VALUE is nil, just adds chain - does not add a nil child.

Returns an updated copy of tree."

  ;;------------------------------
  ;; Error Checking
  ;;------------------------------
  ;; Don't allow a null chain.
  (when (or (null chain)
            (not (imp--tree-chain? chain)))
    (imp--error "imp--tree-update" "CHAIN is not a chain: %S" chain))

  ;; Valid tree?
  (when (and (not (null tree)) ;; We can deal with a tree of nil.
             (not (imp--tree-tree? tree))) ;; We can't deal with an invalid tree.
    (imp--error "imp--tree-update" "TREE is not a tree: %S" tree))

  ;;------------------------------
  ;; Create or update?
  ;;------------------------------
  (if (null tree)
      ;;------------------------------
      ;; Create a tree with chain.
      ;;------------------------------
      (imp--tree-create chain value)

    ;;------------------------------
    ;; Update tree with chain.
    ;;------------------------------

    ;; Need to splice this chain into the tree at whatever branch it shoots off
    ;; from existing.

    (let ((branch tree)
          (link (car chain))
          (remaining (cdr chain))
          ;; Keep track of upstream.
          parent-branches
          parent-links)
      (while (and branch
                  remaining)
        ;; Get branch for current link; save off branch/link.
        ;; 'parent-branch' for the 'parent-link' should include the link as an key
        ;; - we need it for easier updating on the way back up to the root.
        (push branch parent-branches)
        (setq branch (imp--alist-get-value link branch))
        (push link parent-links)
        (imp--tree-debug "imp--tree-update" "%S = %S" parent-links parent-branches)

        ;; Update link/remaining for next round.
        (setq link (car remaining))
        (setq remaining (cdr remaining)))

      (imp--tree-debug "imp--tree-update" "found final branch:")
      (imp--tree-debug "imp--tree-update" "  branch:    %S" branch)
      (imp--tree-debug "imp--tree-update" "  remaining: %S" remaining)
      (imp--tree-debug "imp--tree-update" "  link:      %S" link)

      ;;------------------------------
      ;; Error Check: Invalid chain after all?
      ;;------------------------------
      ;; Do not allow adding a chain that wants to continue off from a value

      ;; link and branch should now be at the end of the known existing chain in
      ;; tree. Need to add whatever the rest is to this branch now.
      (let ((entry (imp--tree-chain (cons link remaining) value))
            branch-update)

        ;;------------------------------
        ;; New Branch or Add Here?
        ;;------------------------------
        (if (imp--alist-get-value link branch)
            ;;------------------------------
            ;; Add Here.
            ;;------------------------------
            (setq branch-update (imp--tree-branch-update entry branch))

          ;;------------------------------
          ;; New Branch.
          ;;------------------------------
          (imp--tree-debug "imp--tree-update" "branch: %S" branch)
          (imp--tree-debug "imp--tree-update" "link: %S" link)
          (imp--tree-debug "imp--tree-update" "new: %S" entry)
          (imp--tree-debug "imp--tree-update" "  key:   %S" (car entry))
          (imp--tree-debug "imp--tree-update" "  value: %S" (cdr entry))
          (setq branch-update (imp--alist-update (car entry)
                                                 (cdr entry)
                                                 branch)))

        ;;------------------------------
        ;; Finish by updating tree.
        ;;------------------------------
        (imp--tree-debug "imp--tree-update" "branch-update: %S" branch-update)

        (imp--tree-debug "imp--tree-update" "Branch found/updated. Walk update up to root...\n")

        ;; Now backtrack up the tree to the root - have to update every branch
        ;; along the way to save the new value.
        ;; ;;
        ;; ;; First, drop the leading values from the parents lists. They are what we
        ;; ;; already updated.
        ;; (setq parent-links (cdr parent-links))
        ;; (setq parent-branches (cdr parent-branches))
        (while parent-links
          ;; Grab what this level is.
          (setq link (car parent-links))
          (setq parent-links (cdr parent-links))
          (setq branch (car parent-branches))
          (setq parent-branches (cdr parent-branches))
          (imp--tree-debug "imp--tree-update" "link: %S" link)
          (imp--tree-debug "imp--tree-update" "branch: %S" branch)

          ;; Push updated branch of tree into place.
          (setq branch-update (imp--alist-update link branch-update branch))
          (imp--tree-debug "imp--tree-update" "branch-update: %S" branch-update))
        branch-update))))
;; Chain splits from tree:
;;   (imp--tree-update '(:root :one :two :free) :leaf-node1 (imp--tree-create '(:root :one :two :three) :leaf-node0))
;; Tree doesn't exist:
;;   (imp--tree-update '(:root :one :two :free) :leaf-node1 nil)
;; Chain doesn't exist in tree:
;;   (imp--tree-update '(:root1 :won :too :free) :leaf-node1 (imp--tree-create '(:root0 :one :two :three) :leaf-node0))
;; Chain pre-exists in tree:
;;   (imp--tree-update '(:root :one :two) :leaf-node1 (imp--tree-create '(:root :one :two :three) :leaf-node0))
;; Reach end of tree before end of chain:
;;   (imp--tree-update '(:root :one :two :three :four) :leaf-node1 (imp--tree-create '(:root :one :two :three) :leaf-node0))
;; Chain w/ null value:
;;   (imp--tree-update '(:root :one :two :free) nil (imp--tree-create '(:root :one :two :three) :leaf-node0))
;; This func does not update the variable; use `imp--tree-update' for that.
;;   (setq test<imp>:tree nil)
;;   (imp--tree-update-helper '(:foo bar baz) 'qux test<imp>:tree)
;;   test<imp>:tree


(defmacro imp--tree-update (chain value tree)
  "Adds CHAIN with final VALUE to TREE.

CHAIN should be a list of symbols and/or keywords.
VALUE should be a symbol or keyword.
TREE should be a list or symbol that holds the list.

If VALUE is nil, just adds chain - does not add a nil child."
  ;; Have to eval ,tree up to twice to set it, but we can avoid more than that?
  `(let ((macro<imp>:tree ,tree))
     (cond
      ((listp macro<imp>:tree)
       (setq ,tree
             (imp--tree-update-helper ,chain ,value ,tree)))
      ((symbolp macro<imp>:tree)
       (set macro<imp>:tree
            (imp--tree-update-helper ,chain ,value (eval macro<imp>:tree))))

      (t
       (imp--error "imp--tree-update"
                   "Unable to update tree: not a list or a symbol: %S (type: %S)"
                   macro<imp>:tree
                   (typeof macro<imp>:tree))))))
;; (setq test<imp>:tree nil)
;; (imp--tree-update '(:foo bar baz) 'qux test<imp>:tree)
;; test<imp>:tree


(defun imp--tree-contains? (chain tree)
  "Returns non-nil if TREE contains the CHAIN of symbols/keywords."
  ;;------------------------------
  ;; Error Checking
  ;;------------------------------
  ;; Don't allow a null chain.
  (when (or (null chain)
            (not (imp--tree-chain? chain)))
    (imp--error "imp--tree-contains" "CHAIN is not a chain: %S" chain))

  ;; Valid tree?
  (when (or (null tree)
            (not (imp--tree-tree? tree)))
    (imp--error "imp--tree-contains" "TREE is not a tree: %S" tree))

  (imp--tree-debug "imp--tree-contains?" "CHAIN and TREE verified as valid.\n  chain: %S\n  tree:\n    %S"
                   chain tree)

  ;;------------------------------
  ;; Does tree contain the chain?
  ;;------------------------------
  (let ((branch tree) ;; Start at root of the tree.
        entry)
    (dolist (link chain)
      (imp--tree-debug "imp--tree-contains?" "  link:   %S" link)
      (imp--tree-debug "imp--tree-contains?" "  branch: %S" branch)
      (setq entry (imp--tree-key-exists? link branch))
      (imp--tree-debug "imp--tree-contains?" "  entry: %S" entry)
      ;; Next branch will be entry's children.
      (setq branch (cdr entry)))

    ;; Final entry:
    (imp--tree-debug "imp--tree-contains?" "final entry for link '%S': %S"
                     (car (last chain)) entry)
    (imp--tree-debug "imp--tree-contains?" "final branch: %S"
                     branch)

    ;; Return whatever we found after walking that whole chain. Will be either
    ;; a tree entry or nil, so that satisfies our predicate nature.
    entry))
;; (imp--tree-contains? '(:root :one :two) (imp--tree-create '(:root :one :two :three) :leaf-node0))
;; (imp--tree-contains? '(:root :one :two :free) (imp--tree-create '(:root :one :two :three) :leaf-node0))
;; (imp--tree-contains? '(:root1 :one :two) (imp--tree-create '(:root :one :two :three) :leaf-node0))
;; (imp--tree-contains? '(:imp test) '((:imp (ort (something (here))) (test))))
