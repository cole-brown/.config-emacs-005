;;; core/modules/output/nub/info.el --- Info-Level Output -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-06-07
;; Timestamp:  2023-08-15
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Info-Level Output
;;
;;; Code:


(imp:require :nub 'output)


;;------------------------------------------------------------------------------
;; Infos!
;;------------------------------------------------------------------------------

(defun nub:info (user caller message &rest args)
  "Output a MESSAGE for USER at `:info' level.

Format to standard message output for the USER with CALLER info, then output
the message with FORMATTING and ARGS to the correct place according to the
current verbosity of `:info' (e.g. `info', by default).

If CALLER is nil, uses relative path from `user-emacs-directory' to
the caller's file.
  Examples:
    - \"init.el\"
    - \"core/modules/output/nub/foo.el\"
    - \"/some/path/outside/user-emacs-directory/file.el\"

MSG should be the `message' formatting string.

ARGS should be the `message' arguments."
  (declare (indent 2))

  (let* ((caller (int<nub>:caller-or-path user caller))
         (func/name (nub:format:callers "nub:info" caller)))

    (int<nub>:user:exists? func/name user :info)
    (apply #'nub:output
           user
           :info
           caller
           message
           args)))
;; (nub:info :default "test-func" "hello %s" "there")
;; (nub:info :default "test-func" '(:derive) (message "test"))
;; (nub:info :default "test-func" '(:jeff) (message "test"))
;; (let ((caller "test-func")
;;       (tags '(:derive))
;;       (msg "test message"))
;;   (nub:debug caller tags msg))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :nub 'info)
