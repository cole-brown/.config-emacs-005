;;; imp/load.el --- Load Files -*- lexical-binding: t; -*-
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
;; ║                               Load Files                               ║
;; ╚════════════════════════════════════════════════════════════════════════╝
;;                                   ──────
;;                      This is, like, the whole point.
;;                                 ──────────
;;
;; Special thanks/apologies/condolences to Doom's `load!' macro,
;; which this was originally based off of, but is not anymore.
;;
;; Flagrantly stolen from `use-package-core.el',
;;  - https://github.com/jwiegley/use-package/blob/cba23de38d0a0c361f591be450d97dfc277469d3/use-package-core.el
;; And lovingly beaten to death.
;;   (apologies to John Wiegley)
;;
;;; Code:

(require 'cl-lib)

;;------------------------------------------------------------------------------
;; The Main Macro(es)
;;------------------------------------------------------------------------------

(defmacro imp-core (feature args)
  `(let* ((feature* (imp-parser-normalize-feature ,feature))
          (args* (imp-parser-normalize-keywords feature* ,args))
          (imp-parser--form
           (if (eq imp-parser-verbose 'debug)
               (concat "\n\n"
                       (pp-to-string `(imp-parser feature* ,@,args))
                       "\n  -->\n\n"
                       (pp-to-string `(imp-parser feature* ,@args*))
                       "\n  ==>\n\n"
                       (pp-to-string
                        (macroexp-progn
                         (let ((imp-parser-verbose 'errors)
                               (imp-parser-expand-minimally t))
                           (imp-parser-process-keywords feature* args*)))))
             "")))
     (when (eq imp-parser-verbose 'debug)
       (message (make-string 40 ?━))
       (message "%s" feature*)
       (message (make-string 40 ?━))
       (message "%s" imp-parser--form))

     (imp-parser-process-keywords feature* args*)))


;;;###autoload
(defmacro imp (feature &rest args)
  "Load FEATURE.

Usage:

  (imp-parser feature
     [:keyword [option]]...)

:disabled      Flag. Nothing happens; FEATURE is ignored completely if this keyword
               is present.

:path PATH     Absolute, relative, or rooted path to file.
               PATH can be:
                 - A path string.
                   - absolute or relative
                   - prefixes:
                     - \"./\" - relative to `imp-path-current-dir'
                     - \":/\" - relative to FEATURE's root
                 - A symbol whose value is a path string.
                   - example: `user-emacs-directory'
                 - A symbol whose name is known.
                   - examples:
                     - `pwd' - `imp-path-current-dir'
                     - `.emacs' - `user-emacs-directory'
                     - `root' - `imp-feature-root'
                   - See func `imp-parser-normalize-path-symbol'
                 - A form/function that evaluates to a path string.
                   - (imp-path-join user-emacs-directory \"path/to/imp\")
TODO(path): Do we want this to be the solution for lists?
                 - A list of the above to join into a path.
TODO(path): Alternative is a list of paths to try for locating the file.

:root ROOT     Create an imp feature root for ROOT at PATH given in `:path'.
               See var `imp-path-roots' and func `imp-path-root-set'.
               If ROOT is t or a flag (arg-less), use first part of FEATURE.
               Example:
                 (imp-parser imp/init
                             :path (imp-path-join user-emacs-directory
                                                  \"path/to/imp\")
                             :root)
                 => imp-path-roots: '((imp \"~/.config/emacs/path/to/imp\") ...)

:error ERR     Value (aka ERROR) can be:
                 - nil
                 - non-nil (default)
                 - A form that should evaluate to one of the above.
               If ERROR is nil, the function will not raise an error if:
                 - The file doesn't exist.
                 - FEATURE isn't provided after loading the file.
               It will still raise an error if:
                 - It cannot parse the inputs.
                 - It cannot determine where to /look/ for the file.

:optional OPT  Load FEATURE if it exists; do nothing if it does not.
               OPT can be:
                 - nil (default)
                 - non-nil
                 - A form that should evaluate to one of the above.

:requires REQ  Assert that a feature already exists in `features'.
               If assert fails, handle according to ERROR/OPTIONAL.
               REQ can be:
                 - nil
                 - symbol
                 - A form that should evaluate to one of the above.

:if EXPR       Initialize and load only if EXPR evaluates to a non-nil value.
:when EXPR     See `:if'.
:unless EXPR   Opposite of `:if'.
               Initialize and load only if EXPR evaluates to a nil value.

:after AFTER   Delay the effect of the imp-parser declaration until after the
               named features have loaded. Before they have been loaded, no
               other keyword has any effect at all, and once they have been
               loaded it is as if `:after' was not specified."
  (declare (indent defun))
  (unless (memq :disabled args)
    (macroexp-progn
     (imp-parser-concat
      ;; (when imp-parser-compute-statistics
      ;;   `((imp-parser-statistics-gather :imp-parser ',feature nil)))

      ;; Let errors bubble up for now. They're more useful for debugging
      ;; and I don't have `imp-parser' integrated with imp's preexisting
      ;; debug/error stuff.
      (imp-parser-core feature args)

      ;; (if (memq imp-parser-verbose '(errors debug))
      ;;     (imp-parser-core feature args)
      ;;   (condition-case-unless-debug err
      ;;       (imp-parser-core feature args)
      ;;     (error
      ;;      (ignore
      ;;       (display-warning
      ;;        'imp-parser
      ;;        (format "Failed to parse package %s: %s"
      ;;                feature (error-message-string err))
      ;;        :error)))))

      ;; (when imp-parser-compute-statistics
      ;;   `((imp-parser-statistics-gather :imp-parser ',feature t)))
      ))))


;;------------------------------------------------------------------------------
;; The Other Main Macro
;;------------------------------------------------------------------------------



;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide imp load)
