;;; core/modules/emacs/imp/test.el --- Load Files for Unit Tests -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-01-31
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
;; ║                       Load Files for Unit Tests                        ║
;; ╚════════════════════════════════════════════════════════════════════════╝
;;                                   ──────
;;                       Tests can have imps too, yes?
;;                                 ──────────
;;
;; `imp-test-load': Similar to `imp-load', but with some kwargs for testing.
;;
;;; Code:



;;------------------------------------------------------------------------------
;; `imp-test-load' Helpers
;;------------------------------------------------------------------------------

(defun imp--test-load-parse (caller path:current-dir plist-symbol-name plist)
  "Parses `imp-test-load' args. See `imp-test-load' for details.

CALLER should be \"imp-test-load\".

PATH:CURRENT-DIR should be the return value of `(imp-path-current-dir)',
executed in the context of the file calling CALLER.
  - That is, CALLER is probably a macro.

PLIST-SYMBOL-NAME should be \"load-args-plist\".
PLIST should be `load-args-plist'.

Returns a plist:
  - :path
    + Path string to load file.
  - :feature:pre
    + imp feature keyword/symbol list
  - :feature:post
    + imp feature keyword/symbol list
  - :error
    - t/nil"
  ;; Valid keys:
  (let ((keys:valid '(:path :filename :feature:pre :feature:post :error))
        ;; Parsing vars.
        keys:parsed
        parsing:done
        ;; Input parsed values:
        in:path
        in:filename
        in:feature/pre
        in:feature/post
        in:error
        ;; Output default values:
        out:path
        out:feature/pre
        out:feature/post
        (out:error t))

    (imp--debug caller
                    '("inputs:\n"
                      "caller:            %S\n"
                      "path:current-dir:  %S\n"
                      "plist-symbol-name: %S\n"
                      "plist:\n"
                      "%s")
                    caller
                    path:current-dir
                    plist-symbol-name
                    (pp-to-string plist))

    ;;------------------------------
    ;; Validate PLIST.
    ;;------------------------------
    (unless (= 0 (% (length plist) 2))
      (imp--error caller
                      '("Malformed %s plist! "
                        "PLIST must have pairs of keywords & values, "
                        "so expecting a length that's a multiple of 2. "
                        "Got: %S")
                      plist-symbol-name
                      key))

    ;;------------------------------
    ;; Parse Inputs
    ;;------------------------------
    ;; Parse PLIST for expected keys. Error on unexpected.
    ;; Dismantle PLIST itself as we parse.
    (while (and plist
                (not parsing:done))
      (imp--debug caller
                      '("\n"
                        "parse plist:\n"
                        "%s")
                      (pp-to-string plist))

      (let ((key   (car plist))
            (value (cadr plist)))
        (imp--debug caller
                        '("\n"
                          "    key:   %S\n"
                          "    value: %S")
                        key value)
        (imp--debug caller
                        "Valid keys are `%S'; is `%S' a valid key? %S"
                        keys:valid
                        key
                        (memq key keys:valid))

        ;;---
        ;; Sanity checks:
        ;;---
        (unless (keywordp key)
          (imp--error caller
                          '("Malformed %s plist! "
                            "Parsing plist expected a keyword but got: %S")
                          plist-symbol-name
                          key))
        (unless (memq key keys:valid)
          (imp--error caller
                          '("Unknown keyword %S in %s plist! "
                            "Valid keywords are: %S")
                          key
                          plist-symbol-name
                          keys:valid))
        (when (memq key keys:parsed)
          (imp--error caller
                          '("Duplicate key `%S' in %s plist! "
                            "Already have `%S' value: %S")
                          key
                          plist-symbol-name
                          key
                          (cond ((eq key :path)
                                 path)
                                ((eq key :filename)
                                 filename)
                                ((eq key :feature)
                                 feature)
                                ((eq key :error)
                                 error))))
        (imp--debug caller
                        '("Passed sanity checks:\n"

                          "    key:   %S\n"
                          "    value: %S")
                        key value)

        ;;---
        ;; Update variables for next loop's processing.
        ;;---
        (setq plist (cddr plist))
        (push key keys:parsed)

        ;;---
        ;; Valid `key'; just save value.
        ;;---
        ;; Verify value later if necessary.
        (cond ((eq key :path)
               (setq in:path value))
              ((eq key :filename)
               (setq in:filename value))
              ((eq key :feature:pre)
               ;; Allow FEATURE:PRE to be a single thing, a flat list, or a list that needs flattened...
               (setq in:feature/pre (imp--list-flatten value)))
              ((eq key :feature:post)
               ;; Allow FEATURE:POST to be a single thing, a flat list, or a list that needs flattened...
               (setq in:feature/post (imp--list-flatten value)))
              ((eq key :error)
               (setq in:error value)))))

    ;;------------------------------
    ;; Check for required inputs.
    ;;------------------------------

    (unless (or (memq :path keys:parsed)
                (memq :filename keys:parsed))
      (imp--error caller
                      '("No file inputs? "
                        "Either `:path', `:filename', or both are required in plist "
                        "`%s': %S")
                      plist-symbol-name
                      plist))

    ;;------------------------------
    ;; Prep Outputs:
    ;;------------------------------
    ;;---
    ;; Process FEATURE:PRE and FEATURE:POST.
    ;;---
    (imp--debug caller
                    "Normalize `feature:pre'...")
    (when in:feature/pre
      (condition-case-unless-debug err
          (if (eq :imp-dne (car in:feature/pre))
              ;; Don't let `:imp-dne' get normalized to `:impdne'.
              (setq out:feature/pre (list :imp-dne
                                          (imp--feature-normalize (cdr in:feature/pre))))
            ;; Normalize to a list.
            (setq out:feature/pre (imp--feature-normalize in:feature/pre)))

        ;; Handlers:
        ((error user-error) (imp--error caller
                                            "Failed to normalize `:feature:pre': %S"
                                            (cdr err))))
      (imp--debug caller "out:feature/pre: %S" out:feature/pre))
    (imp--debug caller
                    "Normalize `feature:post'...")
    (when in:feature/post
      (condition-case-unless-debug err
          (if (eq :imp-dne (car in:feature/post))
              ;; Don't let `:imp-dne' get normalized to `:impdne'.
              (setq out:feature/post (list :imp-dne
                                           (imp--feature-normalize (cdr in:feature/post))))
            ;; Normalize to a list.
            (setq out:feature/post (imp--feature-normalize in:feature/post)))

        ;; Handlers:
        ((error user-error) (imp--error caller
                                            "Failed to normalize `:feature:post': %S"
                                            (cdr err))))
      (imp--debug caller "out:feature/post: %S" out:feature/post))

    ;;---
    ;; Process PATH & FILENAME into single output path.
    ;;---
    ;; Need FEATURE:PRE & FEATURE:POST before doing this stuff.

    ;; 0) Ease-of-use: Promote PATH to FILENAME if only PATH was provided.
    ;;---
    (unless in:filename
      (setq in:filename in:path
            in:path nil))

    ;; 1) Check PATH first so we can have it for FILENAME if needed.
    ;;---
    ;; Prefer provided path, then look for the root, then use `path:current-dir'.
    (let ((path (or in:path
                    (imp--path-root-dir (nth 0 out:feature/post) :no-error)
                    path:current-dir)))
      (unless (stringp path)
        (imp--error caller
                        '("Could not determine a path to look for filename: '%s' "
                          "PATH and current directory are not strings. path: %S, current-dir: %S")
                        in:filename
                        in:path
                        path:current-dir))
      ;; Update input path to final value.
      (setq in:path path))

    ;; 2) Finalize output path, using PATH if FILENAME is a relative path.
    ;;---
    (setq out:path (expand-file-name in:filename in:path))
    (imp--debug caller "out:path:    %S" out:path)

    ;;---
    ;; ERROR
    ;;---
    ;; It just needs to be nil or not.
    ;; NOTE: Make sure to use existing `out:error' as default value if no in:error!
    ;;   - So we need to know if that key was encountered.
    (if (not (memq :error keys:parsed))
        ;; Not encountered; leave as the default.
        (imp--debug caller "out:error:   %S (default)" out:error)

      ;; Parsed explicitly - set exactly.
      (setq out:error (not (null in:error)))
      (imp--debug caller "out:error:   %S (parsed)" out:error))

    ;;------------------------------
    ;; Return:
    ;;------------------------------
    (list :path         out:path
          :feature:pre  out:feature/pre
          :feature:post out:feature/post
          :error        out:error)))
;; (let ((load-args-plist '(:feature:pre (:imp-dne (:foo bar))
;;                          :feature:post (:foo bar)
;;                          :path "init.el"
;;                          ;; :path
;;                          ;; :filename
;;                          ;; :error nil
;;                          )))
;;   ;; (message "%S" load-args-plist))
;;   (imp--test-load-parse "imp-test-load"
;;                             (imp-path-current-dir)
;;                             (upcase "load-args-plist")
;;                             load-args-plist))


(defun imp--test-load-verify-feature (caller pre/post feature:pre feature:post path error?)
  "Verify conditions for either FEATURE:PRE or FEATURE:POST.

Verifies FEATURE:PRE if PRE/POST is `:feature:pre'.
Verifies FEATURE:POST if PRE/POST is `:feature:post'.

Raises error signal or returns error string, based on ERROR? value."
  (let* ((feature:verify (cond ((eq :feature:pre pre/post)
                               feature:pre)
                              ((eq :feature:post pre/post)
                               feature:post)
                              (t
                               (imp--error "imp--test-load-verify-feature"
                                 "Bad value for PRE/POST: %S"
                                 pre/post))))
         error-encountered)

    ;;------------------------------
    ;; Verify the feature's conditions.
    ;;------------------------------
    (cond ((null feature:verify)
           t)

          ;;---
          ;;"Does Not Exist" Check
          ;;---
          ((eq :imp-dne (nth 0 feature:verify))
           (let ((feature (nth 1 feature:verify)))
             (when (imp-provided? feature)
               (if error?
                   (imp--error caller
                                   '("`%S' of `%S' is defined but `:imp-dne' suggests it should not be!\n"
                                     "  feature:pre:  %S\n"
                                     "  feature:post: %S\n"
                                     "  path:         %S")
                                   pre/post
                                   feature
                                   feature:pre
                                   feature:post
                                   path)
                 (setq error-encountered (format (concat "`%S' of `%S' is defined but `:imp-dne'"
                                                         "suggests it should not be!\n"
                                                         "  feature:pre:   %S\n"
                                                         "  feature:post:  %S\n"
                                                         "  path:          %S")
                                                 pre/post
                                                 feature
                                                 feature:pre
                                                 feature:post
                                                 path))))))

          ;;---
          ;; "_DOES_ Exist" Check
          ;;---
          (t
           (unless (imp-provided? feature:verify)
             (if error?
                 (imp--error caller
                                 '("`%S' of `%S' should exist, but it is not a feature!\n"
                                   "  feature:pre:  %S\n"
                                   "  feature:post: %S\n"
                                   "  path:         %S")
                                 pre/post
                                 feature:verify
                                 feature:pre
                                 feature:post
                                 path)
               (setq error-encountered (format (concat "`%S' of `%S' should exist, "
                                                       "but it is not a feature!\n"
                                                       "  feature:pre:   %S\n"
                                                       "  feature:post:  %S\n"
                                                       "  path:          %S")
                                               pre/post
                                               feature:verify
                                               feature:pre
                                               feature:post
                                               path))))))

    ;; If we were set to error, it already happend or not.
    ;; So just need to notify of errors when not raising signals.
    ;; This is an error string or nil:
    error-encountered))


;;------------------------------------------------------------------------------
;; Test Load API
;;------------------------------------------------------------------------------

;; Based off of the `imp-load' macro.
(defmacro imp-test-load (&rest load-args-plist)
  "Load a file relative to the current executing file (`load-file-name').

LOAD-ARGS-PLIST is a plist of load args:
  - One or both:
    + `:filename'
    + `:path'
  - Optional:
    + `:feature:pre'
    + `:feature:post'
    + `:error'
      - Defaults to `t'; supply `:error nil' to change.

`:filename' value (aka FILENAME) can be:
  - A path string (to a file).
  - A list of strings to join into a path (to a file).
  - A form that should evaluate to one of the above.

When FILENAME is a relative path and PATH is nil, this looks
for FILENAME relative to the 'current file' (see below).

`:path' value (aka PATH) can be:
  - A path string.
  - A list of strings to join into a path.
  - A form that should evaluate to one of the above.

PATH is (nominally) where to look for the file (a string representing a
directory path). If omitted, the lookup is relative to either
`load-file-name', `byte-compile-current-file' or `buffer-file-name'
(checked in that order).

NOTE: If FILENAME is nil but PATH refers to a file, PATH will be use as FILENAME.

`:feature:pre' and `:feature:post' values should be nil, or a list of keywords
and symbols.
  - example: '(:imp load)
  - If supplied, will check that the feature does (not) exist:
    - `feature:pre':  before the file is loaded.
    - `feature:post': after the file is loaded.

To make sure a feature doesn't exist and then does exist after the load, start
the `:feature:pre' list with the keyword `:imp-dne' (does not exist).
  Example 1:
    (imp-test-load
     [...]
     :feature:pre '(:imp-dne (:imp load))
     :feature:post '(:imp load))
  Example 2:
    (imp-test-load
     [...]
     :feature:pre '(:imp-dne :imp load)
     :feature:post '(:imp load))

Or you can just use `:feature:pre' as a check for a different prerequisite
feature. (Or that a different feature does /not/ exist via `:imp-dne'.)

`:error' value (aka ERROR) can be:
  - nil
  - non-nil (default)
If ERROR is nil, the function will not raise an error if:
  - The file doesn't exist.
  - The FEATURE isn't provided after loading the file.
It will still raise an error if:
  - It cannot parse the inputs.
  - It cannot determine where to /look/ for the file.

Always loads the file."
  (let ((macro:path:current-dir (imp-path-current-dir)))
    `(let* ((macro:func/name "imp-test-load")
            (macro:parsed (imp--test-load-parse macro:func/name
                                                    ,macro:path:current-dir
                                                    (upcase "load-args-plist")
                                                    (list ,@load-args-plist)))
            (macro:path          (plist-get macro:parsed :path))
            (macro:path:filename (imp--path-filename macro:path))
            (macro:path:parent   (imp-path-parent        macro:path))
            (macro:feature:pre   (plist-get macro:parsed :feature:pre))
            (macro:feature:post  (plist-get macro:parsed :feature:post))
            ;; Invert for `load' parameter NO-ERROR.
            (macro:error? (plist-get macro:parsed :error))
            ;; Set `file-name-handler-alist' to nil to speed up loading.
            file-name-handler-alist
            error-encountered
            load-result)
       (imp--debug macro:func/name
                       '("parsed:\n"
                         "  path: %s\n"
                         "    -> dir:     %s\n"
                         "    -> file:    %s\n"
                         "  feature:pre:  %S\n"
                         "  feature:post: %S\n"
                         "  error?:       %S")
                       macro:path
                       macro:path:parent
                       macro:path:filename
                       macro:feature:pre
                       macro:feature:post
                       macro:error?)

       ;;------------------------------
       ;; Feature Prereqs Check
       ;;------------------------------
       (setq error-encountered (imp--test-load-verify-feature macro:func/name
                                                                  :feature:pre
                                                                  macro:feature:pre
                                                                  macro:feature:post
                                                                  macro:path
                                                                  macro:error?))

       (if error-encountered
           (progn
             ;; Best we can do is debug? Or message maybe?
             ;; Try debug.
             (imp--debug macro:func/name
                             "Error during feature prereqs check:\n%s"
                             error-encountered)
             ;; Return nil for errors.
             nil)

         ;;------------------------------
         ;; Always Load!
         ;;------------------------------
         ;; Load w/ timing info if desired.
         (imp-timing
             macro:feature:post
             macro:path:filename
             macro:path:parent
           ;; Actually do the load.
           (setq load-result (load macro:path
                                   (not macro:error?)
                                   'nomessage)))

         ;;------------------------------
         ;; Feature Postreqs Check
         ;;------------------------------
         (setq error-encountered (imp--test-load-verify-feature macro:func/name
                                                                    :feature:post
                                                                    macro:feature:pre
                                                                    macro:feature:post
                                                                    macro:path
                                                                    macro:error?))
         (if error-encountered
             (progn
               ;; Best we can do is debug? Or message maybe?
               ;; Try debug.
               (imp--debug macro:func/name
                               "Error during feature postreq check:\n%s"
                               error-encountered)
               ;; Return nil for errors.
               nil)

           ;;------------------------------
           ;; Non-Error Result:
           ;;------------------------------
           load-result)))))
