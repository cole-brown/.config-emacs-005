;;; core/modules/emacs/imp/init.el --- Structured IMPort/export of elisp features  -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2025-03-28
;; Timestamp:  2025-03-28
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Chill with the symbol names a bit.
;;
;; Convert from:
;;   > imp:provide:with-emacs
;;   > imp:path:root/set
;;   > int<imp>:load:parse
;;   >
;;   > test<imp/load>::helper::int<imp>:load:parse
;;
;; Convert to:
;;   > imp/provide-with-emacs
;;   > imp/path-root-set
;;   > imp--load-parse
;;   >
;;   > test<imp/load>::helper::imp--load-parse
;;
;;; Code:

(require 'cl-lib)

(defun _refactor-file (namespace file)
  "Process a single FILE for NAMESPACE refactoring."
  (let* ((existing-buffer (find-buffer-visiting file))
         (regex (rx-to-string
                 `(and
                   (or
                    (and "int<"
                         ,namespace
                         ">")
                    ,namespace)
                   ":"
                   (group (+ (any alnum ":" "/"))))
                 'no-group)))
    (save-window-excursion
      (save-mark-and-excursion
        (if existing-buffer
            (switch-to-buffer existing-buffer)
          (find-file file))
        (goto-char (point-min))

        (while (re-search-forward regex nil t)
          (replace-match (concat namespace
                                 (if (string-prefix-p "int<" (match-string 0))
                                     "--" ; private
                                   "/") ; public
                                 (replace-regexp-in-string "[/:]" "-" (match-string 1)))))
        (save-buffer)
        (unless existing-buffer
          (kill-buffer))))))

(defun _refactor-directory (namespace dir)
  "Process all *.el files in DIR recursively for refactoring."
  (dolist (file (directory-files-recursively dir "\\.el$"))
    (_refactor-file namespace file)))

(defun _refactor-shit (namespace path)
  "Prompt the user for a PATH and refactor namespace patterns within it."
  (interactive (string-join
                '("sNamespace: "
                  "GPath to file or directory: ")
                "\n"))
  (cond ((file-exists-p path)
         (_refactor-file namespace path))
        ((file-directory-p path)
         (_refactor-directory namespace path))
        (t
         (error "The path does not exist!"))))
