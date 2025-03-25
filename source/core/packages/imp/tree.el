;;; core/modules/emacs/imp/tree.el --- imp feature tree helpers -*- lexical-binding: t; -*-
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
;; Constants & Variables
;;--------------------------------------------------------------------------------

(defvar int<imp>:tree:debug:flag nil
  "Allow `int<imp>:tree:debug' to call `int<imp>:debug'?")


(defun int<imp>:tree:debug (caller string &rest args)
  "Debug function for tree, so it can be disabled normally."
  (when int<imp>:tree:debug:flag
    (apply #'int<imp>:debug
           caller
           string
           args)))


;;------------------------------------------------------------------------------
;; Predicates: Types
;;------------------------------------------------------------------------------

(defun int<imp>:tree:node? (node)
  "Retuns t if NODE is a node; nil otherwise (including if NODE is a
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
;; (int<imp>:tree:node? :root)
;; (int<imp>:tree:node? '(:root))


(defun int<imp>:tree:tree? (tree)
  "Retuns t if TREE is a tree; nil otherwise (including if TREE is a node)."
  ;; A tree is:
  ;;   - A list...
  ;;   - ...which has nodes for each item in it (so an alist of nodes).
  (if (or (null tree)
          (not (listp tree)))
      nil
    ;; Each item in list must be a node.
    (not
     (seq-contains-p (mapcar #'int<imp>:tree:node? tree)
                     nil))))
;; symbol: no
;;   (int<imp>:tree? :root)
;; node: no
;;   (int<imp>:tree? '(:root))
;; list of node: yes
;;   (int<imp>:tree? '((:root)))
;;   (int<imp>:tree? '((:root :ignored) (:root02) (:root03)))


(defun int<imp>:tree:chain? (chain &optional rooted)
  "Retuns t if CHAIN is a chain of tree keys; nil otherwise (including if CHAIN
is nil)."
  (let (is-chain
        (is-rooted t)) ;; default to true for when not interested in rootedness.

    ;; A tree is:
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
;;   (int<imp>:tree:chain? :root)
;; list: yes
;;   (int<imp>:tree:chain? '(:root))
;;   (int<imp>:tree:chain? '(root))
;;   (int<imp>:tree:chain? '(:root) t)
;; list, but not rooted: no
;;   (int<imp>:tree:chain? '(root) t)


(defun int<imp>:tree:key/exists? (key tree)
  "Returns non-nil if key exists in tree, even if it has no children.

Return value is KEY's entry in TREE, or nil if KEY does not exist."
  (int<imp>:alist:get/pair key tree))
;; (int<imp>:tree:key/exists? :root1 (int<imp>:tree:update '(:root1) nil (int<imp>:tree:create '(:root0 :one :two) :leaf)))


;;------------------------------------------------------------------------------
;; Helper Functions
;;------------------------------------------------------------------------------

(defun int<imp>:tree:chain (chain value)
  "Create an alist entry for a tree from CHAIN and VALUE."
  (unless (listp chain) ;; nil ok too
    (int<imp>:error "int<imp>:tree:chain"
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

    (int<imp>:tree:debug "int<imp>:tree:chain" "entry:     %S" entry)
    (int<imp>:tree:debug "int<imp>:tree:chain" "link:      %S" link)
    (int<imp>:tree:debug "int<imp>:tree:chain" "remaining: %S" remaining)

    ;; Grow entry by remaining links.
    (while link
      ;; This makes entry an alist of alist(s).
      (setq entry (list link entry))
      ;; Cycle link/remaining to next.
      (setq link (car remaining))
      (setq remaining (cdr remaining)))

    ;; Return the chain of alists for chain/value.
    entry))
;; (int<imp>:tree:chain '(:root) nil)
;; (int<imp>:tree:chain '(:root :one :two :three) :leaf-node)
;; (alist-get :root (list (int<imp>:tree:chain '(:root :one :two :three) :leaf-node)))
;; (alist-get :one (alist-get :root (list (int<imp>:tree:chain '(:root :one :two :three) :leaf-node))))
;; (alist-get :two (alist-get :one (alist-get :root (list (int<imp>:tree:chain '(:root :one :two :three) :leaf-node)))))
;; (alist-get :three (alist-get :two (alist-get :one (alist-get :root (list (int<imp>:tree:chain '(:root :one :two :three) :leaf-node))))))


(defun int<imp>:tree:create (chain value)
  "Creates a TREE with CHAIN and VALUE as the starting content."
  (list (int<imp>:tree:chain chain value)))
;; (int<imp>:tree:create '(:root :one :two :three) :leaf-node)


(defun int<imp>:tree:branch/update (entry branch)
  "Add ENTRY to BRANCH.

ENTRY must be an alist entry for a tree; e.g. a return value from
`int<imp>:tree:chain'.

BRANCH must be a tree - an alist of alists. ENTRY will be added to root of
BRANCH."
  (int<imp>:tree:debug "int<imp>:tree:branch/update" "->entry:  %S" entry)
  (int<imp>:tree:debug "int<imp>:tree:branch/update" "->branch: %S" branch)
  (let* ((key (car entry))
         ;; Need the entry, not the alist, of key's children.
         ;; Need '(:value), not '((:value)).
         (value (cadr entry))
         (siblings (int<imp>:alist:get/value key branch)))
    (int<imp>:tree:debug "int<imp>:tree:branch/update" "  key:      %S" key)
    (int<imp>:tree:debug "int<imp>:tree:branch/update" "  vaule:    %S" value)
    (int<imp>:tree:debug "int<imp>:tree:branch/update" "  siblings: %S" siblings)

    ;; Add new value to its new siblings, update branch and done.
    (push value siblings)
    (int<imp>:tree:debug "int<imp>:tree:branch/update" "updated siblings: %S" siblings)
    (setq branch (int<imp>:alist:update key siblings branch))
    (int<imp>:tree:debug "int<imp>:tree:branch/update" "updated branch: %S" branch)
    branch))
;; (int<imp>:tree:branch/update '(:two (:leaf-node1)) '((:two (:three (:leaf-node0)))))
;; (alist-get :two (int<imp>:tree:branch/update '(:two (:leaf-node1)) '((:two (:three (:leaf-node0))))))
;; (alist-get :leaf-node1 (alist-get :two (int<imp>:tree:branch/update '(:two (:leaf-node1)) '((:two (:three (:leaf-node0)))))))


;;------------------------------------------------------------------------------
;; API for imp:
;;------------------------------------------------------------------------------

(defun int<imp>:tree:update/helper (chain value tree)
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
            (not (int<imp>:tree:chain? chain)))
    (int<imp>:error "int<imp>:tree:update" "CHAIN is not a chain: %S" chain))

  ;; Valid tree?
  (when (and (not (null tree)) ;; We can deal with a tree of nil.
             (not (int<imp>:tree:tree? tree))) ;; We can't deal with an invalid tree.
    (int<imp>:error "int<imp>:tree:update" "TREE is not a tree: %S" tree))

  ;;------------------------------
  ;; Create or update?
  ;;------------------------------
  (if (null tree)
      ;;------------------------------
      ;; Create a tree with chain.
      ;;------------------------------
      (int<imp>:tree:create chain value)

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
        (setq branch (int<imp>:alist:get/value link branch))
        (push link parent-links)
        (int<imp>:tree:debug "int<imp>:tree:update" "%S = %S" parent-links parent-branches)

        ;; Update link/remaining for next round.
        (setq link (car remaining))
        (setq remaining (cdr remaining)))

      (int<imp>:tree:debug "int<imp>:tree:update" "found final branch:")
      (int<imp>:tree:debug "int<imp>:tree:update" "  branch:    %S" branch)
      (int<imp>:tree:debug "int<imp>:tree:update" "  remaining: %S" remaining)
      (int<imp>:tree:debug "int<imp>:tree:update" "  link:      %S" link)

      ;;------------------------------
      ;; Error Check: Invalid chain after all?
      ;;------------------------------
      ;; Do not allow adding a chain that wants to continue off from a value

      ;; link and branch should now be at the end of the known existing chain in
      ;; tree. Need to add whatever the rest is to this branch now.
      (let ((entry (int<imp>:tree:chain (cons link remaining) value))
            branch-update)

        ;;------------------------------
        ;; New Branch or Add Here?
        ;;------------------------------
        (if (int<imp>:alist:get/value link branch)
            ;;------------------------------
            ;; Add Here.
            ;;------------------------------
            (setq branch-update (int<imp>:tree:branch/update entry branch))

          ;;------------------------------
          ;; New Branch.
          ;;------------------------------
          (int<imp>:tree:debug "int<imp>:tree:update" "branch: %S" branch)
          (int<imp>:tree:debug "int<imp>:tree:update" "link: %S" link)
          (int<imp>:tree:debug "int<imp>:tree:update" "new: %S" entry)
          (int<imp>:tree:debug "int<imp>:tree:update" "  key:   %S" (car entry))
          (int<imp>:tree:debug "int<imp>:tree:update" "  value: %S" (cdr entry))
          (setq branch-update (int<imp>:alist:update (car entry)
                                                     (cdr entry)
                                                     branch)))

        ;;------------------------------
        ;; Finish by updating tree.
        ;;------------------------------
        (int<imp>:tree:debug "int<imp>:tree:update" "branch-update: %S" branch-update)

        (int<imp>:tree:debug "int<imp>:tree:update" "Branch found/updated. Walk update up to root...\n")

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
          (int<imp>:tree:debug "int<imp>:tree:update" "link: %S" link)
          (int<imp>:tree:debug "int<imp>:tree:update" "branch: %S" branch)

          ;; Push updated branch of tree into place.
          (setq branch-update (int<imp>:alist:update link branch-update branch))
          (int<imp>:tree:debug "int<imp>:tree:update" "branch-update: %S" branch-update))
        branch-update))))
;; Chain splits from tree:
;;   (int<imp>:tree:update '(:root :one :two :free) :leaf-node1 (int<imp>:tree:create '(:root :one :two :three) :leaf-node0))
;; Tree doesn't exist:
;;   (int<imp>:tree:update '(:root :one :two :free) :leaf-node1 nil)
;; Chain doesn't exist in tree:
;;   (int<imp>:tree:update '(:root1 :won :too :free) :leaf-node1 (int<imp>:tree:create '(:root0 :one :two :three) :leaf-node0))
;; Chain pre-exists in tree:
;;   (int<imp>:tree:update '(:root :one :two) :leaf-node1 (int<imp>:tree:create '(:root :one :two :three) :leaf-node0))
;; Reach end of tree before end of chain:
;;   (int<imp>:tree:update '(:root :one :two :three :four) :leaf-node1 (int<imp>:tree:create '(:root :one :two :three) :leaf-node0))
;; Chain w/ null value:
;;   (int<imp>:tree:update '(:root :one :two :free) nil (int<imp>:tree:create '(:root :one :two :three) :leaf-node0))
;; This func does not update the variable; use `int<imp>:tree:update' for that.
;;   (setq test<imp>:tree nil)
;;   (int<imp>:tree:update/helper '(:foo bar baz) 'qux test<imp>:tree)
;;   test<imp>:tree


(defmacro int<imp>:tree:update (chain value tree)
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
             (int<imp>:tree:update/helper ,chain ,value ,tree)))
      ((symbolp macro<imp>:tree)
       (set macro<imp>:tree
            (int<imp>:tree:update/helper ,chain ,value (eval macro<imp>:tree))))

      (t
       (int<imp>:error "int<imp>:tree:update"
                       "Unable to update tree: not a list or a symbol: %S (type: %S)"
                       macro<imp>:tree
                       (typeof macro<imp>:tree))))))
;; (setq test<imp>:tree nil)
;; (int<imp>:tree:update '(:foo bar baz) 'qux test<imp>:tree)
;; test<imp>:tree


(defun int<imp>:tree:contains? (chain tree)
  "Returns non-nil if TREE contains the CHAIN of symbols/keywords."
  ;;------------------------------
  ;; Error Checking
  ;;------------------------------
  ;; Don't allow a null chain.
  (when (or (null chain)
            (not (int<imp>:tree:chain? chain)))
    (int<imp>:error "int<imp>:tree:contains" "CHAIN is not a chain: %S" chain))

  ;; Valid tree?
  (when (or (null tree)
            (not (int<imp>:tree:tree? tree)))
    (int<imp>:error "int<imp>:tree:contains" "TREE is not a tree: %S" tree))

  (int<imp>:tree:debug "int<imp>:tree:contains?" "CHAIN and TREE verified as valid.\n  chain: %S\n  tree:\n    %S"
                  chain tree)

  ;;------------------------------
  ;; Does tree contain the chain?
  ;;------------------------------
  (let ((branch tree) ;; Start at root of the tree.
        entry)
    (dolist (link chain)
      (int<imp>:tree:debug "int<imp>:tree:contains?" "  link:   %S" link)
      (int<imp>:tree:debug "int<imp>:tree:contains?" "  branch: %S" branch)
      (setq entry (int<imp>:tree:key/exists? link branch))
      (int<imp>:tree:debug "int<imp>:tree:contains?" "  entry: %S" entry)
      ;; Next branch will be entry's children.
      (setq branch (cdr entry)))

    ;; Final entry:
    (int<imp>:tree:debug "int<imp>:tree:contains?" "final entry for link '%S': %S"
                    (car (last chain)) entry)
    (int<imp>:tree:debug "int<imp>:tree:contains?" "final branch: %S"
                    branch)

    ;; Return whatever we found after walking that whole chain. Will be either
    ;; a tree entry or nil, so that satisfies our predicate nature.
    entry))
;; (int<imp>:tree:contains? '(:root :one :two) (int<imp>:tree:create '(:root :one :two :three) :leaf-node0))
;; (int<imp>:tree:contains? '(:root :one :two :free) (int<imp>:tree:create '(:root :one :two :three) :leaf-node0))
;; (int<imp>:tree:contains? '(:root1 :one :two) (int<imp>:tree:create '(:root :one :two :three) :leaf-node0))
;; (int<imp>:tree:contains? '(:imp test) '((:imp (ort (something (here))) (test))))
