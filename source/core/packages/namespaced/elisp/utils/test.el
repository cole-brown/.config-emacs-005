;;; elisp/utils/test.el --- Test Utils -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2020-07-14
;; Timestamp:  2023-06-21
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; ERT (Emacs Regression Test) Utilities
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Delete ERT Tests
;;------------------------------------------------------------------------------

(defun ert:delete/all ()
  "Delete all ERT tests by calling `ert-delete-all-tests'."
  (ert-delete-all-tests))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :elisp 'utils 'test)
