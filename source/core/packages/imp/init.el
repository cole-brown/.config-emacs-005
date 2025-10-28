;;; imp/init.el --- Structured IMPort of elisp features  -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-05-07
;; Timestamp:  2025-10-28
;; Version:    1.1.20220413
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; ------------------------------------------------------------------------------
;; Add to Your .emacs
;; ------------------------------------------------------------------------------
;;
;; (load (expand-file-name "path/to/imp/init.el" user-emacs-directory))
;;
;; ;; optional customizations
;; (customize-set-variable 'imp-timing-enabled? t)
;;
;; ------------------------------------------------------------------------------
;; Usage
;; ------------------------------------------------------------------------------
;;
;; TODO Load
;; ------
;; (imp-load TODO: TODO)
;;
;; Require
;; ------
;; (imp-require <symbol/keyword0> ...)
;;   - If a root is set for <symbol/keyword0>, this can (try to) find the file
;;     required.
;;
;; Provide
;; -------
;; (imp-provide <symbol/keyword0> ...)            ; Provide via imp only.
;; (imp-provide-with-emacs <symbol/keyword0> ...) ; Provide via imp and Emacs.
;;
;; (Optional) Set-Up:
;; ------
;; (imp-path-root-set <symbol/keyword0>
;;                    <path-to-root-dir-absolute>
;;                    &optional <path-to-root-file-relative-or-absolute>)
;;   - Setting a root for <symbol/keyword0> allows later `imp-require' calls to
;;     try to find the file if not already provided.
;;
;;; Code:

(require 'cl-macs)


;;------------------------------------------------------------------------------
;; Function for to Load our own Files...
;;------------------------------------------------------------------------------

(cl-flet ((imp--init-load (path-relative)
            ;; Given PATH-RELATIVE, find the absolute path and load the file.
            (let (file-name-handler-alist)
              (load (expand-file-name
                     path-relative
                     (directory-file-name
                      (file-name-directory
                       ;; this is just `imp-path-current-file'
                       (cond ((bound-and-true-p byte-compile-current-file))
                             (load-file-name)
                             ((stringp (car-safe current-load-list))
                              (car current-load-list))
                             (buffer-file-name)
                             ((error "Cannot find filepath for filename '%s'"))))))
                    nil
                    ;; NOTE: Commenting out this `nomessage' can help debug Emacs start-up.
                    'nomessage))))

  ;;------------------------------------------------------------------------------
  ;; Customize
  ;;------------------------------------------------------------------------------

  ;; Look here for all the defcustoms.
  (imp--init-load "settings")


  ;;------------------------------------------------------------------------------
  ;; Load our files...
  ;;------------------------------------------------------------------------------

  ;;------------------------------
  ;; Required by debug.
  ;;------------------------------
  (imp--init-load "fundamental")
  (imp--init-load "output")
  (imp--init-load "error")


  ;;------------------------------
  ;; Debug: Get it initialized ASAP, cuz I bug a lot.
  ;;------------------------------
  (imp--init-load "debug")


  ;;------------------------------
  ;; Order matters.
  ;;------------------------------
  (imp--init-load "alist")
  (imp--init-load "tree")
  (imp--init-load "feature")
  (imp--init-load "path")
  (imp--init-load "flag")
  (imp--init-load "timing")
  (imp--init-load "provide")
  (imp--init-load "load")
  (imp--init-load "require")
  (imp--init-load "package")
  (imp--init-load "commands"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide imp)
;;; imp/init.el ends here
