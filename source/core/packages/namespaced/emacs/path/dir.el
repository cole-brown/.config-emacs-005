;;; core/modules/emacs/path/dir.el --- Directory Functions -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2023-04-07
;; Timestamp:  2023-06-22
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Directory Functions
;;
;;; Code:


(require 'cl-lib)


(imp:require :path 'path)


;;------------------------------------------------------------------------------
;; Dir Name
;;------------------------------------------------------------------------------

(defun dir:name (path &rest segment)
  "Return the directory name from PATH & SEGMENT(s).

Example:
  (dir:name \"/foo\" \"bar.tar.gz\")
  (dir:name \"/foo/bar.tar.gz\")
  (dir:name \"c:/foo/bar.tar.gz\")
    -> \"bar.tar.gz\""
  (file-name-nondirectory (apply #'path:canonicalize:file path segment)))
;; (dir:name "/foo" "bar.tar.gz")
;; (dir:name "/path/to/foo/")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :path 'dir)
