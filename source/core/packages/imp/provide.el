;;; core/modules/emacs/imp/provide.el --- Provide a feature to imp, maybe Emacs. -*- lexical-binding: t; -*-
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
;;                                 ──────────
;; ╔════════════════════════════════════════════════════════════════════════╗
;; ║                            Provide Features                            ║
;; ╚════════════════════════════════════════════════════════════════════════╝
;;                                   ──────
;;       Provide Imp feature symbol paths & also Emacs feature symbols.
;;                                 ──────────
;;
;; Provide to just Emacs:  `provide'
;; Provide to just imp:    `imp-provide'
;; Provide to imp & Emacs: `imp-provide' w/ `imp-only' flag
;;
;;; Code:


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
  (let ((feature-imp (imp--feature-normalize-to-list feature)))
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

imp will translate the FEATURE symbol chain via `imp-feature-normalize-imp->emacs' and use
the result for the call to Emacs' `provide'.

Returns the Emacs feature symbol created/used."
  (let ((feature-emacs (imp-feature-normalize-for-emacs feature)))
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


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide :imp 'provide)
