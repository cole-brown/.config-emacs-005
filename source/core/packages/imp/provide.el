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
;; Provide to just imp:    `imp:provide'
;; Provide to imp & Emacs: `imp:provide:with-emacs'
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Public API: Provide
;;------------------------------------------------------------------------------

(defun imp:provide:loading? (&optional file-name)
  "Returns true if loading file.

If FILE-NAME is nil, returns true if loading any file.
If FILE-NAME is a string, returns true if loading that exact
(full path to) file name."
  (if file-name
      ;; Exactly* that file loading?
      ;;   * Exactly for this platform (OS).
      (and load-in-progress
           (string=
            (int<imp>:path:platform-agnostic load-file-name)
            (int<imp>:path:platform-agnostic file-name)))
    ;; Just anything loading?
    load-in-progress))


(defalias 'imp:provided? 'imp:feature?)
(defalias 'imp:providedp 'imp:feature?)


(defun imp:provide (&rest feature)
  "Record FEATURE in `imp:features' as having been provided.

Each FEATURE should be one of:
  - A keyword.
  - A symbol.
  - A string (will be passed through `imp:feature:normalize').

If you want to provide the feature to emacs as well, you can either:
  1. Use `imp:provide:with-emacs' instead of this to have it automatically
     happen.
     - imp will translate the FEATURE symbol chain via `imp:feature:normalize:imp->emacs'.
  2. Do it yourself by also calling Emacs' `provide' with a symbol of your
     choosing."
  (let ((feature/imp (int<imp>:feature:normalize feature)))
    (if (null feature/imp)
        (int<imp>:error "imp:provide"
                        '("No features to provide? "
                          "input: %S, normalized: %S")
                        feature
                        feature/imp)

      (int<imp>:debug "imp:provide" "Providing feature '%S'..."
                      feature/imp)
      (int<imp>:feature:add feature/imp))))
;; (imp:provide :package 'module 'submodule 'feature)


(defun imp:provide:with-emacs (&rest feature)
  "Record FEATURE in `imp:features' and in Emacs' `features' (via
Emacs' `provide') as having been provided.

Each FEATURE should be one of:
  - A keyword.
  - A symbol.
  - A string to be passed through `imp:feature:normalize'.

imp will translate the FEATURE symbol chain via `imp:feature:normalize:imp->emacs' and use
the result for the call to Emacs' `provide'.

Returns the Emacs feature symbol created/used."
  (imp:provide feature)
  (let ((feature/emacs (imp:feature:normalize:imp->emacs feature)))
    (int<imp>:debug "imp:provide:with-emacs" "Providing to emacs as '%S'..."
                    feature/emacs)
    (provide feature/emacs)
    feature/emacs))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide:with-emacs :imp 'provide)
