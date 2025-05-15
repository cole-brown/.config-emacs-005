;;; core/modules/emacs/dlv/path.el --- Path helpers for DLV. -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-10-05
;; Timestamp:  2023-06-21
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Path helpers for DLV.
;;
;;; Code:


(imp:require :nub)
(imp:require :dlv 'debug)


;;------------------------------------------------------------------------------
;; DLV Directory Path
;;------------------------------------------------------------------------------

(defun int<dlv>:path:expand (path &optional as-dir)
  "Expand PATH using `expand-file-name', then reverts any \"~\" expansion.

Normalize paths for DLV:
  - Lowercases Windows drive letters.
  - Converts Windows backslashes to forwardslashes.
  - Resolves any \"..\" and \".\" in the path.
  - etc.

If AS-DIR is non-nil, return a directory path (end in a slash).
Else return path as type (dir/file) provided."
  (when (or (null path)
            (not (stringp path)))
    (error "int<dlv>:path:expand: PATH must be a string! Got: %S %S"
           (type-of path)
           path))

  (let ((path/abs (expand-file-name
                   (if as-dir
                       (file-name-as-directory path)
                     path))))
    ;; Check _original_ `path' for tilde.
    (if (string-prefix-p "~" path)
        ;; Return expanded but with "~" still.
        (replace-regexp-in-string (expand-file-name "~")
                                  "~"
                                  path/abs)
      ;; Return expanded as-is.
      path/abs)))
;; (int<dlv>:path:expand "~/foo/../bar")
;; (int<dlv>:path:expand "~/foo/../bar" :dir)
;; Windows:
;;   (int<dlv>:path:expand "d:/home/work/foo/../bar")
;; Error:
;;   (int<dlv>:path:expand nil)

(defun int<dlv>:dir:normalize (dir &optional preserve-tilde)
  "Normalize DIR into an absolute path with '/' separators and ending in a '/'.

Example:
  (int<dlv>:dir:normalize \"~/foo/bar\")
    -> \"/home/work/foo/bar/\"
    NOTE: Loses the leading \"~\" unless PRESERVE-TILDE is non-nil!

  (int<dlv>:dir:normalize \"~/foo/bar\" t)
    -> \"~/foo/bar/\"

  (int<dlv>:dir:normalize \"~/foo/bar/\" t)
    -> \"~/foo/bar/\"

  (int<dlv>:dir:normalize \"D:\\foo\\bar\")
    -> \"d:/foo/bar/\"

  (int<dlv>:dir:normalize \"D:\\home\\work\\foo\\bar\")
    -> \"d:/home/work/foo/bar/\""
  ;; Finish by ensuring we have a trailing '/'.
  (file-name-as-directory
   ;; Do we want to preserve "~"?
   (if preserve-tilde
       (int<dlv>:path:expand dir)
     ;; `expand-file-name' lowercases Windows drive letters and converts backslash to forwardslash.
     ;; It also converts "~" into the absolute path, which isn't really desired in this case...
     (expand-file-name dir))))
;; (int<dlv>:dir:normalize "D:\\foo\\bar")
;; (int<dlv>:dir:normalize "~/foo/bar")
;; (int<dlv>:dir:normalize "~/foo/bar" :preserve)


(defun int<dlv>:path:multiplex (path &optional as-dir)
  "Return PATH split up into: '(prefix-list . rest-str)

If AS-DIR is non-nil, PATH will be resolved as a directory path.

`prefix-list' will be either list of strings or nil.
`rest-str' will be either remainder of path (to concat onto prefix list members)
or entire expanded path.

Primarily for converting paths under user's home into both the tilde path and
the expanded \"/home/username/\" path.

Example:
  (int<dlv>:path:multiplex \"~/foo/bar\")
    -> '((\"~/\" \"/home/jeff/\") . \"foo/bar\")

  (int<dlv>:path:multiplex \"/home/jeff\")
    -> '((\"~/\" \"/home/jeff/\") . \"\")

  (int<dlv>:path:multiplex \"/some/path/foo/bar\")
    -> '((nil) . \"/some/path/foo/bar\")"
  (let ((func/name "int<dlv>:path:multiplex")
        (func/tags '(:path)))
    (nub:debug:func/start
        :dlv
        func/name
        func/tags
      (cons 'path      path)
      (cons 'as-dir    as-dir))

    ;;------------------------------
    ;; Error Checks
    ;;------------------------------
    (when (null path)
      (error "%s: PATH must not be nil! path: %S"
             func/name
             path))

    (when (not (stringp path))
      (error "%s: PATH must be a string! path: %S %S"
             func/name
             (type-of path)
             path))

    ;;------------------------------
    ;; Figure out path/paths.
    ;;------------------------------
    (let* ((path/home.abbrev "~")
           (path/home.abs (expand-file-name "~"))
           (path/abs (int<dlv>:path:expand path as-dir)))

      (nub:debug:func/return
          :dlv
          func/name
          func/tags
        ;; If it's one of the home paths, output two paths.
        ;;   - "~" needs to be both "~" and the absolute/expanded path.
        ;;   - ...and vice versa.
        (cond
         ;; "~" in resolved PATH.
         ((string-prefix-p path/home.abbrev path/abs)
          (let ((dir/prefix
                 (file-name-as-directory path/home.abbrev)))
            ;; Remove leading part of path that is covered by the prefixes.
            (cons (list dir/prefix
                        (file-name-as-directory path/home.abs))
                  ;; Remove prefix.
                  (replace-regexp-in-string (concat dir/prefix "?") ;; May or may not have the final "/".
                                            ""
                                            path/abs))))

         ;; "/home/jeff" in resolved PATH.
         ((string-prefix-p path/home.abs path/abs)
          (let ((dir/prefix
                 (file-name-as-directory path/home.abs)))
            ;; Remove leading part of path that is covered by the prefixes.
            (cons (list (file-name-as-directory path/home.abbrev)
                        dir/prefix)
                  ;; Remove prefix.
                  (replace-regexp-in-string (concat dir/prefix "?") ;; May or may not have the final "/".
                                            ""
                                            path/abs))))
         ;; Not a home path; ok as-is - just make it fit the return.
         (t
          (cons nil
                path/abs)))))))
;; (int<dlv>:path:multiplex "~/foo/bar")
;; (int<dlv>:path:multiplex "d:/home/work")
;; (int<dlv>:path:multiplex "d:/some/path/foo/bar")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :dlv 'path)
