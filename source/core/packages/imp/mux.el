;;; imp/mux.el --- Multiplex via hashes. -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2020-10-28
;; Timestamp:  2025-12-16
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Multiplex via hashes.
;; Useful for config-per-system.
;;
;; TODO: commentary
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Constants & Things
;;------------------------------------------------------------------------------

(defconst imp--mux-hash-algorithm 'sha512
  "Hashing function used in `imp-mux' hashing functions.")

(defconst imp--mux-hash-slice 6
  "Default hashing slice size to use for `imp--mux-hash-truncate'.")

(defconst imp--mux-hash-join "-"
  "Default slice join string for `imp--mux-hash-truncate'.")

(defconst imp--mux-hash-identifiers '("imp" . "mux")
  "So `imp' can recognize a hash in the path.
Even if it's buried in a dir name.")

(defconst imp--mux-hash-identifiers-join "-"
  "Default slice join string for `imp-mux-hash-system'.")

(defconst imp--mux-hash-rx (rx-to-string `(seq word-boundary
                                               ,(car imp--mux-hash-identifiers)
                                               (optional ,imp--mux-hash-identifiers-join)
                                               (= ,imp--mux-hash-slice hex)
                                               (optional ,imp--mux-hash-join)
                                               (= ,imp--mux-hash-slice hex)
                                               (optional ,imp--mux-hash-identifiers-join)
                                               ,(cdr imp--mux-hash-identifiers)
                                               word-boundary)
                                         :no-group))
;; (string-match-p imp--mux-hash-rx (imp-mux-system))
;; (string-match-p imp--mux-hash-rx (imp--mux-hash-truncate (system-name) system-type))


(defun imp--mux-system-rx ()
  (concat
   (rx-to-string 'word-boundary :no-group)
   (string-replace imp--mux-hash-identifiers-join
                   (rx-to-string `(optional ,imp--mux-hash-identifiers-join) :no-group)
                   (if (not (string= imp--mux-hash-identifiers-join imp--mux-hash-join))
                       (string-replace imp--mux-hash-join
                                       (rx-to-string `(optional ,imp--mux-hash-join) :no-group)
                                       (imp-mux-system))
                     (imp-mux-system)))
   (rx-to-string 'word-boundary :no-group)))
;; (imp--mux-system-rx)


(defconst imp--mux-placeholder
  (if (= imp--mux-hash-slice)
      ;; This is easier to read as a placeholder.
      (concat
       (car imp--mux-hash-identifiers)
       imp--mux-hash-identifiers-join
       "dabb1e"
       imp--mux-hash-join
       "babb1e"
       imp--mux-hash-identifiers-join
       (cdr imp--mux-hash-identifiers))
    (imp-mux-hash 'imp 'mux)))


;;------------------------------------------------------------------------------
;; General Hashing Functions
;;------------------------------------------------------------------------------

(defun imp--mux-hash-full (&rest inputs)
  "Return the full hash string of INPUTS.

Hash algorithm is `imp--mux-hash-algorithm', which should be one of the symbols
returned from `secure-hash-algorithms', which, see for available algorithms."
  (let ((inputs-filtered (seq-keep #'identity inputs)) ; remove nils
        (input-string (mapconcat #'imp-parser-normalize-string-eval inputs)))
    ;;------------------------------
    ;; Error Checks
    ;;------------------------------
    ;; Make sure hash exists as a supported algorithm.
    (unless (member imp--mux-hash-algorithm (secure-hash-algorithms))
      (imp--error (which-function) "Unknown hash: %s" hash))

    (unless (> (length inputs-filtered) 0)
      (imp--error (which-function)
                  "Cannot hash nil. INPUTS: %S -> %S"
                  inputs
                  inputs-filtered))

    (unless (stringp input-string)
      (imp--error (which-function)
                  "Cannot hash INPUT. Normalization did not produce a string. Inputs: %s '%s' -> %s '%s'"
                  (type-of inputs)
                  inputs
                  (type-of input-string)
                  input-string))

    ;;------------------------------
    ;; Hash!
    ;;------------------------------
    (secure-hash imp--mux-hash-algorithm input-string)))
;; (string= (imp--mux-hash-full "jeff") (imp--mux-hash-full 'jeff))
;; (imp--mux-hash-full 'jeff 'jeff)
;; (imp--mux-hash-full '(jeff jeff))
;; (imp--mux-hash-full nil)
;; (imp--mux-hash-full (list (system-name) system-type))


(defun imp--mux-hash-truncate (&rest inputs)
  "Hash INPUTS, keep first and last bits of hash string.

(imp--mux-hash-full 'foo)
  => \"f7fbba6e0636f890e56fbbf3283e524c6fa3204ae298382d624741d0dc6638326e282c41be5e4254d8820772c5518a2c5a8c0c7f7eda19594a7eb539453e1ed7\"
(imp--mux-hash-truncate 'foo)
  => \"f7fbba-3e1ed7\""
  (let* ((hash-full (apply #'imp--mux-hash-full inputs)))
    (concat (substring hash-full 0 imp--mux-hash-slice)
            imp--mux-hash-join
            (substring hash-full (- imp--mux-hash-slice) nil))))
;; (imp--mux-hash-truncate "jeff")
;; (imp--mux-hash-truncate (list (system-name) system-type))
;; (imp--mux-hash-full (list (system-name) system-type))


(defun imp-mux-hash (&rest inputs)
  "Hash INPUTS, keep first and last bits of hash string, and bookend.

(imp--mux-hash-full 'foo)
  => \"f7fbba6e0636f890e56fbbf3283e524c6fa3204ae298382d624741d0dc6638326e282c41be5e4254d8820772c5518a2c5a8c0c7f7eda19594a7eb539453e1ed7\"
(imp--mux-hash-truncate 'foo)
  => \"f7fbba-3e1ed7\"
(imp-mux-hash 'foo)
  => \"imp-f7fbba-3e1ed7-mux\""
  (concat (car imp--mux-hash-identifiers)
          imp--mux-hash-identifiers-join
          (apply #'imp--mux-hash-truncate inputs)
          imp--mux-hash-identifiers-join
          (cdr imp--mux-hash-identifiers)))
;; (imp-mux-hash 'imp 'mux)


;;------------------------------------------------------------------------------
;; System Hash
;;------------------------------------------------------------------------------

(defun imp-mux-system ()
  "Return this system's imp-mux hash as a string."
  (imp-mux-hash (system-name) system-type))
;; (imp-mux-system)


(defun imp-mux-system-info ()
  "Print this system's info and hash.

If interactive, write string to `*Messages*' buffer.
Else, return string."
  (interactive)
  (let ((info (format "%s + %s = %s"
                      (system-name)
                      system-type
                      (imp-mux-system))))
    (if (called-interactively-p)
        (message "%s: %s"
                 (which-function)
                 info)
      info)))
;; (imp-mux-system-info)


;;------------------------------------------------------------------------------
;; Path
;;------------------------------------------------------------------------------

(defun imp--mux-path? (path)
  "Does PATH have a imp-mux hash in it?

Return result from regex match: nil or integer (start of match in string)"
  (string-match-p imp--mux-hash-rx path))


(defun imp-mux-path (root &optional rest)
  "Return a path with an imp-mux placeholder."
  (imp-path-join root
                 imp--mux-placeholder
                 rest))
;; (imp-mux-path "/path/to/mux" "and/then/init.el")
;; (imp-mux-path nil "and/then/init.el")
;; (imp-mux-path nil "init.el")


(defun imp--mux-path-split (path)
  "Split PATH into 3 paths: '(prefix imp-mux-system-hash-dir postfix)

Split on directories.
  (imp--mux-path-split \"path/to/jeff-work-2025_imp-123456-abcdef-mux/foo/bar.el\")
    => '(\"path/to\"
         \"jeff-work-2025_mux-123456-abcdef\"
         \"foo/bar.el\")

If multiple hashes present, split on first hash.

If no imp-mux system hash in PATH, return just '(PATH)"
  (if (and path
           (stringp path)
           (imp--mux-path? path))
      (let ((names (imp-path-split path))
            found? ; did we find a hash yet?
            prefix ; path we're building for `car' of segments
            segments) ; return value: '(prefix imp-mux-system-hash-dir postfix)
        (while (and names (not found?))
          (let ((name (pop names)))
            (if (string-match-p imp--mux-hash-rx name)
                (progn
                  ;; set prefix as whatever we found so far
                  (push prefix segments)
                  ;; set this name as the hash dir
                  (push name segments)
                  ;; rest will be added after while loop
                  (setq found? t))
              (setq prefix (imp-path-join prefix name)))))
        ;; set postfix
        (push (apply #'imp-path-join names) segments)
        (nreverse segments))
    ;; Not found. Just return PATH
    (list path)))
;; (imp--mux-path-split "path/to/jeff-work-2025_imp-123456-abcdef-mux/foo/bar.el")
;; (imp--mux-path-split "path/to/jeff-work-2025_imp123456abcdefmux/foo/bar.el")
;; (imp--mux-path-split "path/to/jeff-work-2025_/foo/bar.el")
;; (imp--mux-path-split "path/to/foo/bar/jeff-work-2025_imp-123456-abcdef-mux")
;; (imp--mux-path-split "jeff-work-2025_imp123456abcdefmux/path/to/foo/bar.el")


(defun imp--mux-path-find (path)
  "Find this system's path given possibly mux'd absolute PATH."
  (let ((mux-path (imp--mux-path-split path)))
    (when (and (nth 0 mux-path) ; path root str exists
               (nth 1 mux-path) ; path is mux'd
               (file-exists-p (nth 0 mux-path))) ; path root exists in file system
      (imp-path-join (nth 0 mux-path)
                     (nth 0 (directory-files (nth 0 mux-path) nil (imp--mux-system-rx) t))
                     (nth 2 mux-path)))))
;; (imp--mux-path-find (concat "/path/to/secret/mux/" imp--mux-placeholder))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
