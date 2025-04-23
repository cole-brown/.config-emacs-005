;;; core/modules/emacs/imp/package.el --- `imp' + `use-package' = `imp-use-package' -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-04-20
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
;; ║               Use a Package, Evaluate After a Package...               ║
;; ╚════════════════════════════════════════════════════════════════════════╝
;;                                   ──────
;;                            Free(ish) Shipping!
;;                                 ──────────
;;
;; `imp' + `use-package' = `imp-use-package'
;;
;;; Code:


(require 'cl-lib)


;;------------------------------------------------------------------------------
;; Use-Package: imp keyword
;;------------------------------------------------------------------------------

;; TODO: `use-package' keyword instead of replacement.

;; https://www.gnu.org/software/emacs/manual/html_node/use-package/Creating-an-extension.html

use-package-keywords
(:pin :ensure :disabled :load-path :requires :defines :functions :preface :if :when :unless :vc :no-require :catch :after :custom :custom-face :bind :bind* :bind-keymap :bind-keymap* :interpreter :mode :magic :magic-fallback :hook :commands :autoload :init :defer :demand :load :config :diminish :delight)

;;------------------------------------------------------------------------------
;; Use-Package: (old) use-package + imp timings replacement macro
;;------------------------------------------------------------------------------

(defmacro imp-use-package (name &rest args)
  "Wrap `use-package' in imp timing.

NAME and ARGS should be exactly as `use-package' requires.

Does not load `use-package'; call should load it prior to using this.

Available macro variables:
  - `macro<imp>:path/file'
  - `macro<imp>:path/dir'
  - `macro<imp>:name/file'"
  (declare (indent 1))
  (let ((macro<imp>:feature (list :use-package name)))
    `(let ((macro<imp>:path/file ,(imp-path-abbreviate (imp-path-current-file)))
           (macro<imp>:path/dir  ,(imp-path-abbreviate (imp-path-current-dir)))
           (macro<imp>:name/file ,(imp-file-current)))
       (imp-timing
           (quote ,macro<imp>:feature)
           macro<imp>:path/file
           macro<imp>:path/dir
         (use-package ,name
           ,@args)))))
;; (imp-use-package test-foo)
;; (imp-use-package test-foo
;;   :init
;;   (message "hello %S" macro<imp>:path/file))


;;------------------------------------------------------------------------------
;; Evaluate After Package(s)/Feature(s) Load
;;------------------------------------------------------------------------------
;; Happily stolen from Doom. This is Doom's `after!' macro (from
;; "core/core-lib.el"), broken up so I could grok it.

;; TODO: Currently only supports stuff provided to Emacs as a feature.
;;   - Is that acceptable or should it work also on imp-only features?
(defmacro imp-eval-after (feature &rest body)
  "Evaluate BODY after FEATURE(s) have loaded.

FEATURE is a symbol or list of them. These are package/feature names, not modes,
functions or variables. It can be:

- An unquoted imp or Emacs feature symbol (the name of a package/feature)
    (imp-eval-after helm BODY...)
    (imp-eval-after :foo BODY...)
- An unquoted imp feature symbol list
    (imp-eval-after (:foo bar) BODY...)
- An unquoted, nested list of compound feature lists, using any combination of
  :or/:any and :and/:all
    (imp-eval-after (:or feature-a feature-b ...)  BODY...)
    (imp-eval-after (:or :jeff (:foo bar) ...)  BODY...)
    (imp-eval-after (:and feature-a feature-b ...) BODY...)
    (imp-eval-after (:and feature-a (:or feature-b feature-c) ...) BODY...)

NOTE: FEATURE must ultimately be provided to Emacs; cannot work on imp-only
features.

This is a wrapper around `eval-after-load' that:
1. Suppresses warnings for disabled features at compile-time
2. Supports compound feature statements
3. Prevents eager expansion pulling in autoloaded macros all at once"
  (declare (indent 1) (debug t))
  ;;------------------------------
  ;; Error Cases
  ;;------------------------------
  (cond ((null feature)
         (error "imp-eval-after- FEATURE must not be null! Got: %S" feature))

        ((and (listp feature)
              (eq 'quote (car feature)))
         (error "imp-eval-after- FEATURE should not be quoted! Got: %S" feature))

        ;;------------------------------
        ;; Single Feature
        ;;------------------------------
        ((symbolp feature)
         (list (if (or (not (bound-and-true-p byte-compile-current-file))
                       ;; `imp-require' will check Emacs and imp for:
                       ;;   1) Is the feature already provided?
                       ;;   2) Can the feature be provided right now?
                       ;; It signals an `error' if the answers are no, which
                       ;; we need to prevent; this is just a check for if it's
                       ;; ready right now.
                       (ignore-error '(error user-error)
                         (imp-require feature)))
                   #'progn
                 #'with-no-warnings)
               ;; We intentionally avoid `with-eval-after-load' to prevent eager
               ;; macro expansion from pulling (or failing to pull) in autoloaded
               ;; macros/features.
               `(eval-after-load ',(if (keywordp feature)
                                       (imp-feature-normalize-for-emacs feature)
                                     feature)
                  ',(macroexp-progn body))))

        ((and (listp feature)
              (not (memq (car feature) '(:and :all :or :any))))
         ;; Convert imp feature list to Emacs feature symbol & recurse to hit the above case.
         `(imp-eval-after ,(apply #'imp-feature-normalize-for-emacs feature) ,@body))

        ;;------------------------------
        ;; Multiple Features
        ;;------------------------------
        ;; Figure out `:and'/`:or' condition, then recurse.
        (t
         (let ((condition (car feature))
               (rest      (cdr feature)))
           ;;---
           ;; OR
           ;;---
           (cond ((memq condition '(:or :any))
                  ;; Make an `imp-eval-after' for each feature and let them each
                  ;; run after their feature loads, so... can/will eval BODY multiple
                  ;; times.
                  (macroexp-progn
                   (cl-loop for next in rest
                            collect `(imp-eval-after ,next ,@body))))
                 ;;---
                 ;; AND
                 ;;---
                 ((memq condition '(:and :all))
                  ;; Chain `imp-eval-after' for the features in the order they
                  ;; are supplied. So... Waits for the first feature to be loaded
                  ;; before waiting for the second, etc.
                  (dolist (next (reverse rest) (car body))
                    (setq body `((imp-eval-after ,next ,@body)))))

                 ;;---
                 ;; ERROR?!
                 ;;---
                 ;; Should not get here, since anything that is not
                 ;; `:and'/`:all'/`:or'/`:any' should be considered an imp feature
                 ;; keyword, but to be complete: signal an error.
                 (t
                  `(imp--error "imp-eval-after"
                                   "Unhandled condition `%S' for features: %S"
                                   condition
                                   feature)))))))
;; (imp-eval-after nil (message "hi"))
;; (imp-eval-after :imp (message "hi"))
;; (imp-eval-after imp (message "hi"))
;; (imp-eval-after (:imp package) (message "hi"))
;; (imp-eval-after (:and :imp (imp package)) (message "hi"))
;; Incorrect:
;;   (imp-eval-after ':imp (message "hi"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide :imp 'package)
