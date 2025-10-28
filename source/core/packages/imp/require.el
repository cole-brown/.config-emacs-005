;;; imp/provide.el --- Require a feature from imp. -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-05-07
;; Timestamp:  2025-10-28
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;                                 ──────────
;; ╔════════════════════════════════════════════════════════════════════════╗
;; ║                            Require Features                            ║
;; ╚════════════════════════════════════════════════════════════════════════╝
;;                                   ──────
;;                     Require imp feature symbol paths.
;;                                 ──────────
;;
;; Require from Emacs: `require'
;; Require from imp:   `imp-require'
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Public API: Require
;;------------------------------------------------------------------------------

(defun imp--require-parse (args)
  "Normalize the Lisp value given by ARGS."
  (seq-map (lambda (arg)
             (cond ((null arg) nil)
                   ((eq t arg) t)
                   ((and arg (symbolp arg))
                    (if (bound-and-true-p arg)
                        (symbol-value arg)
                      arg))
                   ((functionp arg)
                    (funcall arg))
                   (t
                    (eval arg))))
           args))
;; (imp--require-parse '(foo))
;; (imp--require-parse '(foo bar (imp-file-current :no-ext)))
;; (imp-feature-normalize (imp--require-parse '(foo bar (imp-file-current :no-ext))))


(defmacro imp-require (&rest feature)
  "Ensure file(s) for FEATURE are provided.

Return non-nil on success."
  `(let* ((funcname 'imp-require)
          (feature* ',feature)
          (feature-normal (imp-feature-normalize (imp--require-parse feature*))))
    ;; Already provided?
    (cond ((imp-feature? feature-normal))
          ((featurep feature-normal))

          ;; TODO(require): try to `imp-load' it if we know where `feature-base' is.
          ;; TODO(require): try to `require' it.
          ;; TODO(require): for now, error
          (t
           (imp--error funcname
                       "TODO: try to actually load/require %S"
                       feature-normal))

          ;; ;; Can we load it?
          ;; ((progn
          ;;    (condition-case err
          ;;        (imp--load-feature feature-normal)
          ;;      ;; If loading by feature failed, then user needs to check their
          ;;      ;; order of loading/requiring things. Let's give them some more
          ;;      ;; info.
          ;;      (error
          ;;       (imp--error "imp-require"
          ;;                   '("Failed to find/load required feature: \n"
          ;;                     "  input feature: %S\n"
          ;;                     "  normalized:    %S\n"
          ;;                     "Check your order of providing/loading, "
          ;;                     "or make sure it initializes its root and "
          ;;                     "features with imp first.\n"
          ;;                     "Error: %S\n"
          ;;                     "  %S")
          ;;                   feature*
          ;;                   feature-normal
          ;;                   (car err)
          ;;                   (cdr err))))
          ;;    ;; Yes; so add to imp's feature tree.
          ;;    (imp--feature-add feature-normal)))

          ;; ;; Nope; return nil.
          ;; (t
          ;;  nil)
          )))
;; (imp-require 'test 'this)


(defalias #'imp-require-assert #'imp-feature-assert)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide imp require)
