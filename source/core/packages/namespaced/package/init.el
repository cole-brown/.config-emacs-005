;;; namespaced/package/init.el --- Emacs Packages -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-02-16
;; Timestamp:  2025-11-10
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;; Code:

(require 'rx)


;;------------------------------------------------------------------------------
;; Setup
;;------------------------------------------------------------------------------

(imp-path-root-set 'package (imp-path-current-dir))


(defgroup package nil
  "Package upgrade & manipulation functions."
  :group 'tools)


;;------------------------------------------------------------------------------
;; Load files.
;;------------------------------------------------------------------------------

(imp-timing
    'package
    (imp-path-current-file)

  (imp package:/upgrade/mode ; requires `rx'
    :path "./upgrade-mode.el")
  (imp package:/upgrade/command ; requires `rx', `package:/upgrade/mode'
    :path "./upgrade-command.el"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide package)
