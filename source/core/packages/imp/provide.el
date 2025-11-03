;;; imp/provide.el --- Provide a feature. -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-05-07
;; Timestamp:  2025-11-03
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;                                 ──────────
;; ╔══════════════════════════════════════════════════════════════════════════╗
;; ║                             Provide Features                             ║
;; ╚══════════════════════════════════════════════════════════════════════════╝
;;                                   ──────
;;             Provide feature to `imp-features' and `features'.
;;                                 ──────────
;;; Code:


(require 'seq)

;;------------------------------------------------------------------------------
;; Public API: Provide
;;------------------------------------------------------------------------------

(defalias 'imp-provided? 'imp-feature?)
(defalias 'imp-provided-p 'imp-feature?)

(defun imp--provide-parse (args)
  "Normalize the Lisp value given by ARGS."
  (seq-map (lambda (arg)
             (cond ((null arg) nil)
                   ((eq t arg) t)
                   ((and arg (symbolp arg))
                    (if (and (boundp arg) arg)
                        (symbol-value arg)
                      arg))
                   ((functionp arg)
                    (funcall arg))
                   (t
                    (eval arg))))
           args))
;; (imp--provide-parse '(foo))
;; (imp--provide-parse '(foo bar (imp-file-current :no-ext)))


(defmacro imp-provide (&rest feature)
  `(let* ((funcname 'imp-provide)
          (feature* ',feature)
          (feature-norm (imp-feature-normalize (imp--provide-parse feature*))))
     (unless feature-norm
       (imp--error funcname
                   '("No features to provide? "
                     "input: %S, normalized: %S")
                   feature*
                   feature-norm))

     ;; Provide to `imp-features' tree.
     (imp--feature-add feature-norm)

     ;; Provide to Emacs `features' list.
     (provide feature-norm)

     feature-norm))
;; (imp-provide foo)
;; (imp-provide foo bar (imp-file-current :no-ext))


;;------------------------------------------------------------------------------
;; Debug / Test Helpers: Unprovide
;;------------------------------------------------------------------------------

(defmacro imp-unprovide (&rest feature)
  "Ninja-delete FEATURE root & descendants from feature lists.

Delete from:
  - imp: `imp-features'
    - Given FEATURE of \\='(foo bar baz), delete from root of
      feature tree (`foo', see func `imp-feature-first').
  - emacs: `features'
    - Given FEATURE of \\='(foo bar baz), delete all that either:
      1) start with \"foo:/\" or \"foo/\"
      2) equal the entire string \"foo\""
  `(let* ((feature* ',feature)
          (feature-norm (apply #'imp-feature-normalize feature*))
          (feature-first (imp-feature-first feature-norm)))
     (unless feature-norm
       (imp--error "imp-unprovide"
                   '("No features to unprovide? "
                     "input: %S, normalized: %S")
                   feature*
                   feature-norm))

     (message "unproviding all of `%S' from `imp-features' & `features'."
              feature-first)

     ;; imp: Delete from `imp-features'.
     (imp--feature-delete feature-norm)

     ;; Emacs: Delete from `features'.
     (let* ((first-str (imp--feature-string feature-first))
            ;; Given `first-str' of "foo", features must either:
            ;;   1) start with "foo:/", "foo/"
            ;;   2) be the entire string "foo"
            (regex (rx-to-string `(seq string-start
                                       (or (and ,first-str
                                                (optional ,imp--feature-separator-root)
                                                ,imp--feature-separator-chain)
                                           (and ,first-str
                                                string-end)))
                                 :no-group)))
       (setq features (seq-filter (lambda (f) (not (string-match-p regex (symbol-name f))))
                                  features)))))
;; (pp imp-features)
;; (imp-provide 'foo 'bar)
;; (provide 'foo:/bar)
;; (provide 'foo/bar)
;; (imp-provide '(test or something here))
;; (pp imp-features)
;; (pp features)
;; (imp-unprovide foo/bar)
;; (imp-unprovide foo)
;; (pp imp-features)
;; (imp-unprovide test)
;; (pp imp-features)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide imp provide)
