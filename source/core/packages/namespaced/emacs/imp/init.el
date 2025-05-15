;;; core/modules/emacs/imp/init.el --- Structured IMPort/export of elisp features  -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-05-07
;; Timestamp:  2024-10-02
;; Version:    1.1.20220413
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; ------------------------------------------------------------------------------
;; Usage
;; ------------------------------------------------------------------------------
;;
;; Require
;; ------
;; (imp:require <symbol/keyword0> ...)
;;   - If a root is set for <symbol/keyword0>, this can (try to) find the file
;;     required.
;;
;; Provide
;; -------
;; (imp:provide <symbol/keyword0> ...)            ; Provide via imp only.
;; (imp:provide:with-emacs <symbol/keyword0> ...) ; Provide via imp and Emacs.
;;
;; (Optional) Set-Up:
;; ------
;; (imp:path:root/set <symbol/keyword0>
;;                    <path-to-root-dir-absolute>
;;                    &optional <path-to-root-file-relative-or-absolute>)
;;   - Setting a root for <symbol/keyword0> allows later `imp:require' calls to
;;     try to find the file if not already provided.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Function for to Load our Files...
;;------------------------------------------------------------------------------

(defun int<imp>:init:load (filename)
  "Load a FILENAME relative to the current file."
  (let (file-name-handler-alist)
    (load (expand-file-name
           filename
           (directory-file-name
            (file-name-directory
             (cond ((bound-and-true-p byte-compile-current-file))
                   (load-file-name)
                   ((stringp (car-safe current-load-list))
                    (car current-load-list))
                   (buffer-file-name)
                   ((error "Cannot find filepath for filename '%s'"))))))
          nil
          ;; NOTE: Commenting out this `nomessage' can help debug Emacs start-up.
          'nomessage)))


;;------------------------------------------------------------------------------
;; Load our files...
;;------------------------------------------------------------------------------

;;------------------------------
;; Required by debug.
;;------------------------------
;; Try not to have too many things here.
(int<imp>:init:load "error")


;;------------------------------
;; Debug ASAP!..
;;------------------------------
(int<imp>:init:load "debug")
(int<imp>:debug:init)


;;------------------------------
;; Order matters.
;;------------------------------
(int<imp>:init:load "feature")
(int<imp>:init:load "alist")
(int<imp>:init:load "tree")
(int<imp>:init:load "path")
(int<imp>:init:load "+flag")   ;; Currently optional but always loaded. Could make fully optional or required.
(int<imp>:init:load "+timing") ;; Optional, but always load it - it'll time or not time based on settings.
(int<imp>:init:load "provide")
(int<imp>:init:load "load")
(int<imp>:init:load "require")
(int<imp>:init:load "package")
(int<imp>:init:load "commands")


;;------------------------------------------------------------------------------
;; Initialization
;;------------------------------------------------------------------------------

;; These are needed earlier than provide, so now that everything is ready, let
;; them provide their feature and do other set-up.
(int<imp>:path:init)
(int<imp>:flag:init)
(int<imp>:load:init)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
;; Not strictly necessary to provide to emacs, since provide and require both
;; provide to emacs as well, but does help when requiring via Emacs.
(imp:provide:with-emacs :imp)
;;; imp/init.el ends here
