;;; imp/init.el --- Structured IMPort of elisp features  -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-05-07
;; Timestamp:  2025-10-29
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
;;                    <path-to-root-dir-absolute>)
;;   - Setting a root for <symbol/keyword0> allows later `imp-require' calls to
;;     try to find the file if not already provided.
;;
;;; Code:

;; Require everything that any imp file uses here to keep track of
;; external dependencies?
(require 'cl-macs)
(require 'cl-lib)
(require 'seq)


;;------------------------------------------------------------------------------
;; Constants & Variables
;;------------------------------------------------------------------------------

(defconst imp-version "0.3.0"
  "This version of `imp'.")


(defvar imp-roots nil
  "Alist of require/provide root keywords to a cons of: (root-dir . root-file).

Example:
  `imp' entry is: '(imp \"/path/to/imp/\")")
;; imp-roots
;; (setq imp-roots nil)


(defvar imp-features nil
  "Features that have been loaded by `imp-provide'.

It is a tree; an alist of alists of ad nauseam. Provided features are the
leaves, and their feature names should be built from the path traversed to get
to them.
  - I.e. directory structures w/ files as leaves.

For example:
  '((:imp
     (provide)
     (require))
    (:metasyntactic
     (foo (bar (baz (qux (quux (quuux (quuuux (quuuuux))))))
               (thud (grunt))
               (bletch)
               (fum)
               (bongo)
               (zot)))
     (bazola (ztesch))
     (fred (jim (sheila (barney))))
     (corge (grault (flarp)))
     (zxc (spqr (wombat)))
     (shme)
     (spam (eggs))
     (snork)
     (blarg (wibble))
     (toto (titi (tata (tutu))))
     (pippo (pluto (paperino)))
     (aap (noot (mies)))
     (oogle (foogle (boogle (zork (gork (bork)))))))
    (:pinky (narf (zort (poit (egad (troz (fiddely-posh))))))))
    - is a tree with 3 'roots':
      - :imp
        - provide
        - require
      - :metasyntactic
        - ...
      - :pinky
        - ...")
;; (setq imp-features nil)


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
                       ;; This is just `imp-path-current-file' as of 2025-10-29
                       (cond ((bound-and-true-p byte-compile-current-file))
                             ((bound-and-true-p load-file-name))
                             ((stringp (car-safe current-load-list))
                              (car current-load-list))
                             ((buffer-file-name (buffer-base-buffer)))
                             ((error "Cannot find filepath for filename '%s'"))))))
                    nil
                    ;; NOTE: Commenting out this `nomessage' can help debug Emacs start-up.
                    'nomessage))))

  ;;------------------------------------------------------------------------------
  ;; Load our files...
  ;;------------------------------------------------------------------------------

  ;; Look here for all the defcustoms.
  (imp--init-load "settings")

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
  (imp--init-load "parser")
  (imp--init-load "list")
  (imp--init-load "tree")
  (imp--init-load "feature")
  (imp--init-load "path")
  (imp--init-load "timing")
  (imp--init-load "provide")
  (imp--init-load "load")

  ;; TODO: Do we really need these?
  (imp--init-load "require")
  (imp--init-load "package")
  (imp--init-load "commands"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide imp)
;;; imp/init.el ends here
