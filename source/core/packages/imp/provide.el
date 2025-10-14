;;; core/modules/emacs/imp/provide.el --- Provide a feature to imp, maybe Emacs. -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-05-07
;; Timestamp:  2025-10-13
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;                                 ──────────
;; ╔════════════════════════════════════════════════════════════════════════╗
;; ║                            Provide Features                            ║
;; ╚════════════════════════════════════════════════════════════════════════╝
;;                                   ──────
;;       Provide Imp feature symbol paths & also Emacs feature symbols.
;;                                 ──────────
;;
;; Provide to just Emacs:  `provide'
;; Provide to just imp:    `imp-provide' w/ `imp-only' flag
;; Provide to imp & Emacs: `imp-provide'
;;
;;; Code:

(require 'seq)

;;------------------------------------------------------------------------------
;; Public API: Provide
;;------------------------------------------------------------------------------

;; (defun imp-provide-loading? (&optional file-name)
;;   "Returns true if loading file.

;; If FILE-NAME is nil, returns true if loading any file.
;; If FILE-NAME is a string, returns true if loading that exact
;; (full path to) file name."
;;   (if file-name
;;       ;; Exactly* that file loading?
;;       ;;   * Exactly for this platform (OS).
;;       (and load-in-progress
;;            (string=
;;             (imp--path-platform-agnostic load-file-name)
;;             (imp--path-platform-agnostic file-name)))
;;     ;; Just anything loading?
;;     load-in-progress))


(defalias 'imp-provided? 'imp-feature?)
(defalias 'imp-providedp 'imp-feature?)


(defun imp-provide-to-imp (&rest feature)
  "Record FEATURE in `imp-features' as having been provided.

Each FEATURE should be one of:
  - A keyword.
  - A symbol.
  - A string (will be passed through `imp-feature-normalize').

If you want to provide the feature to emacs as well, you can either:
  1. Use `imp-provide-with-emacs' instead of this to have it automatically
     happen.
     - imp will translate the FEATURE symbol chain via `imp-feature-normalize-imp->emacs'.
  2. Do it yourself by also calling Emacs' `provide' with a symbol of your
     choosing."
  (let ((feature-imp (imp--feature-normalize-chain feature)))
    (if (null feature-imp)
        (imp--error "imp-provide"
                    '("No features to provide? "
                      "input: %S, normalized: %S")
                    feature
                    feature-imp)

      (imp--debug "imp-provide" "Providing feature '%S'..."
                  feature-imp)
      (imp--feature-add feature-imp)
      ;; Return nomalized feature name instead of `imp-features' tree.
      feature-imp)))
;; (imp-provide-to-imp :package 'module 'submodule 'feature)


(defun imp-provide-to-emacs (&rest feature)
  "Record FEATURE in `imp-features' and in Emacs' `features' (via
Emacs' `provide') as having been provided.

Each FEATURE should be one of:
  - A keyword.
  - A symbol.
  - A string to be passed through `imp-feature-normalize'.

imp will translate the FEATURE symbol chain via `imp-feature-normalize' and use
the result for the call to Emacs' `provide'.

Returns the Emacs feature symbol created/used."
  (let ((feature-emacs (imp-feature-normalize feature)))
    (imp--debug "imp--provide-to-emacs" "Providing to emacs as '%S'..."
                feature-emacs)
    (provide feature-emacs)
    ;; return normalized feature name
    feature-emacs))


(defun imp-provide (&rest feature)
  "Record FEATURE as available to imp and emacs.

imp:   See tree `imp-features'
Emacs: See list `features' (via `provide')

Each FEATURE should be one of:
  - A keyword.
  - A symbol.
  - A string (will be passed through `imp-feature-normalize').

If you want to provide the feature to emacs as well, you can either:
  1. Use `imp-provide-with-emacs' instead of this to have it automatically
     happen.
     - imp will translate the FEATURE symbol chain via `imp-feature-normalize-imp->emacs'.
  2. Do it yourself by also calling Emacs' `provide' with a symbol of your
     choosing."
  (apply #'imp-provide-to-emacs feature)
  (apply #'imp-provide-to-imp feature))


(defun imp--unprovide-feature-from-emacs (&rest feature)
  "Delete FEATURE from Emacs' `features'.

Delete exactly/only FEATURE."
  (let ((func-name "imp--unprovide-feature-from-emacs")
        (normalized (imp-feature-normalize feature)))
    (when (featurep normalized)
      ;; Filter it out of the `features' list.
      (setq features (seq-filter (lambda (f) (not (eq f normalized))) features))
      (imp--debug func-name
                  "Unprovided `%s' from `features'"
                  normalized))))


(defun imp--unprovide-tree-from-emacs (normalized tree)
  "Unprovide NORMALIZED and its subfeatures TREE from Emacs' `features'."
  (imp--tree-map #'imp--unprovide-from-emacs
                 (reverse normalized)
                 tree))


(defun imp-unprovide (&rest feature)
  "Ninja-delete FEATURE from feature lists.

Delete from:
  - imp: `imp-features'
  - emacs: `features'"
  (let* ((feature-imp (imp--feature-normalize-chain feature))
         (tree (imp--feature-get-tree feature-imp)))
    ;; Does imp know about this?
    (if (null tree)
        ;; No, imp doesn't know about that.
        ;; Make some half-hearted sort of effort to delete from Emacs'
        ;; features by deleting only FEATURE; no subfeatures harmed.
        (imp--unprovide-from-emacs feature)

      ;; Yeah, imp knows about that.
      (imp--feature-delete feature-imp)
      ;; Well... imp used to.

      ;; Delete from Emacs' `features' smarter:
      ;; Delete it and all is subfeatures.
      (imp--unprovide-tree-from-emacs feature-imp tree))))
;; (imp-unprovide :test)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide :imp 'provide)
