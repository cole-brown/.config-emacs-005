;;; core/modules/output/nub/utils.el --- Utility Nubs -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-10-22
;; Timestamp:  2023-08-18
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Utility Functions for Nub
;;
;;; Code:

(imp:require :nub 'internal)
(imp:require :nub 'alist)


;;------------------------------------------------------------------------------
;; Utility Functions & Such That Don't Really Fit Elsewhere
;;------------------------------------------------------------------------------

(defun nub:format:callers (caller &optional parents)
  "Build a 'caller' string.

Builds from CALLER (string) and PARENTS (string or nil).

Returns a string."
  ;;------------------------------
  ;; Error Checks:
  ;;------------------------------
  ;; CALLER must be a string.
  (cond ((not (stringp caller))
         (int<nub>:error "int<nub>:format:callers"
                         "Invalid CALLER param. Expected a string; got: caller: %S, parents: %S"
                         caller parents))
        ;; PARENTS must be a string if not nil.
        ((and parents
              (not (stringp parents)))
         (int<nub>:error "int<nub>:format:callers"
                         "Invalid CALLER param. Expected a string; got: parents: %S, caller: %S"
                         parents caller))

        ;;------------------------------
        ;; Valid: Concat caller w/ parents.
        ;;------------------------------
        (t
         (if parents
             (concat caller " <-via- " parents)
           caller))))
;; (nub:format:callers "bob" nil)
;; (nub:format:callers "bob" "alice")
;; (nub:format:callers "C" (nub:format:callers "B" "A"))
;; (nub:format:callers nil nil)


;;------------------------------------------------------------------------------
;; Normalization
;;------------------------------------------------------------------------------

(defun int<nub>:normalize->string (input)
  "Normalize INPUT to a layout string.

If INPUT is:
  - String:  Remove \"+layout/\" prefix,
             (then remove \":\" prefix if exists), and return.
  - Keyword: Get symbol name, remove \":\" prefix, and return.
  - Symbol:  Get symbol name, remove \"+layout/\" prefix,
             (then remove \":\" prefix if exists), and return.
E.g.
  1) \"dvorak\" -> \"dvorak\"
  2) `:dvorak' -> \"dvorak\"
  3) `+layout/dvorak' -> \"dvorak\""
  (replace-regexp-in-string
   ;; Must match start of string.
   (rx string-start
       ;; ':' from keywords (`:dvorak').
       ;; '+' from module flag (`+layout/dvorak').
       (zero-or-one (any ":" "+"))
       ;; 'layout/' from module flag (`+layout/dvorak') or directory path ('layout/+dvorak').
       (zero-or-one "layout/")
       ;; '+' from directory path ('layout/+dvorak').
       (zero-or-one "+"))
   ;; Replace with nothing.
   ""
   ;; Make sure we've got a string (or we've errored out on invalid input type.
   (if (stringp input)
       input
     (symbol-name input))))
;; (int<nub>:normalize->string '+layout/spydez)
;; (int<nub>:normalize->string :spydez)
;; (int<nub>:normalize->string "spydez")
;; (int<nub>:normalize->string "+spydez")


(defun int<nub>:normalize->keyword (input)
  "Convert INPUT to a keyboard layout keyword.

If INPUT is `nil', `nil' will be returned (allows for default args).

Otherwise INPUT is normalized to a string and then converted to a keyword.
  - Uses `int<nub>:normalize->string'.

E.g. `+layout/dvorak' -> `:dvorak'."
  (if (null input)
      nil
    (intern (concat ":"
                    (int<nub>:normalize->string input)))))
;; (int<nub>:normalize->keyword '+layout/spydez)
;; (int<nub>:normalize->keyword :spydez)
;; (int<nub>:normalize->keyword "spydez")
;; (int<nub>:normalize->keyword nil)


;;--------------------------------------------------------------------------------
;; Paths
;;--------------------------------------------------------------------------------

(defun int<nub>:path:current:file ()
  "Return the path of the file this function is called from."
  (cond
   ;;------------------------------
   ;; Look for a valid "current file" variable.
   ;;------------------------------
   ((bound-and-true-p byte-compile-current-file))

   ((bound-and-true-p load-file-name))

   ((stringp (car-safe current-load-list))
    (car current-load-list))

   ;; Indirect buffers don't have a `buffer-file-name'; you need to get their
   ;; base buffer first. But direct buffers have a `nil' base buffer, so... this
   ;; works for both direct and indirect buffers:
   ((buffer-file-name (buffer-base-buffer)))

   ;;------------------------------
   ;; Failure Case!
   ;;------------------------------
   (t
    ;; Do not error. We need to log the message more than we need to complain about this.
    nil)))
;; (int<nub>:path:current:file)


(defun int<nub>:path:current:dir ()
  "Return the directory path of the file this is called from."
  (when-let (path (int<nub>:path:current:file))
    (directory-file-name (file-name-directory path))))


(defun int<nub>:path:current:file/relative (user)
  "Return the relative path of the file this function is called from.

Path will be relative to `nub' USER's root directory."
  (condition-case _
      (let* ((path/root (int<nub>:var:path user))
             (path/here (int<nub>:path:current:file))
             ;; Don't like `file-relative-name' as it can return weird things when it
             ;; goes off looking for actual directories and files...
             (path/relative (replace-regexp-in-string
                             ;; Make sure root dir has ending slash.
                             path/root ;; Look for root directory path...
                             ""        ;; Replace with nothing to get a relative path.
                             path/here
                             :fixedcase
                             :literal)))

        ;; Do we care if it's not really relative?
        ;; It'd be nice to care, but we cannot error out during the messaging
        ;; functions (`nub:error', `nub:debug', etc).
        ;; (when (string= path/relative path/here)
        ;;   (int<imp>:error "int<nub>:path:current:file/relative"
        ;;                   '("Current directory is not relative to FEATURE-OR-ROOT!\n"
        ;;                     "  FEATURE-OR-ROOT: %S\n"
        ;;                     "  root path:    %s\n"
        ;;                     "  curr path:    %s\n"
        ;;                     "---> result:    %s")
        ;;                   feature-or-root
        ;;                   path/root
        ;;                   path/here
        ;;                   path/relative))

        ;; Return (hopefully) relative path.
        path/relative)

    ;; Error Handling: Don't error out of the debug/error/whatever message being created.
    (error nil)))


(defun int<nub>:caller-or-path (user caller)
  "Return CALLER if valid, else return a path string relative to USER's root.

USER must be the `nub' user's keyword.

CALLER must be a string or nil.

If CALLER is a string:
  - Return as-is.

Else:
  - If inside `user-emacs-directory', return a path relative to it.
  - If inside a project, return a path relative to the project root."
  ;; Use CALLER as-is?
  (cond ((stringp caller)
         caller)
        ;; Can we find a relative path?
        ((int<nub>:path:current:file/relative user))
        ;; Can we find any path?
        ((int<nub>:path:current:file))

        ;;------------------------------
        ;; Failure Case!
        ;;------------------------------
        ;; Never gonna:
        ;;   - Error out.
        ;;   - Return nil.
        ;;   - Turn around and desert you.
        ;; Gonna just:
        ;;   - Give up instead?
        ;;   - Hope someone else sorts it out.
        (t
         "<unknown>")))


;;------------------------------------------------------------------------------
;; The End
;;------------------------------------------------------------------------------
(imp:provide :nub 'utils)
