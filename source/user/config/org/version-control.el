;;; mantle/config/org/version-control.el --- Configure version control stuff for Org-Mode -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2023-03-16
;; Timestamp:  2023-06-29
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Configure version control stuff for Org-Mode.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Orgit: Link to Magit buffers from Org docs
;;------------------------------------------------------------------------------
;; https://github.com/magit/orgit
;; Use the usual `org-store-link' & `org-insert-link' to link to Magit buffers.
;;
;;---
;; Format
;;---
;; https://github.com/magit/orgit#format
;;
;; orgit:/path/to/repo/            links to a magit-status buffer
;; orgit-log:/path/to/repo/::REV   links to a magit-log buffer
;; orgit-rev:/path/to/repo/::ARGS  links to a magit-revision buffer
;;
;;---
;; Export
;;---
;; https://github.com/magit/orgit#export
;;
;; When an Org file containing such links is exported, then the url of the
;; remote configured with `orgit-remote' is used to generate a web url according
;; to `orgit-export-alist'. That webpage should present approximately the same
;; information as the Magit buffer would.
;;
;; Both the remote to be considered the public remote, as well as the actual web
;; urls can be defined in individual repositories using Git variables.
;;
;; To use a remote different from `orgit-remote' but still use `orgit-export-alist'
;; to generate the web urls, use:
;;   git config orgit.remote REMOTE-NAME
;;
;; To explicitly define the web urls, use something like:
;;   git config orgit.status http://example.com/repo/overview
;;   git config orgit.log http://example.com/repo/history/%r
;;   git config orgit.rev http://example.com/repo/revision/%r

(imp:use-package orgit
  :after (:and org magit))


;;--------------------------------------------------------------------------------
;; Groot: Link to files as git repo name & relative path
;;--------------------------------------------------------------------------------

(defvar innit:path:package:groot (path:join innit:path:packages:user "groot")
  "`use-package' doesn't like having a function call for `:load-path', thus this.")

(imp:use-package groot
  ;; This is my own package, so...
  ;;   1) Don't try to install.
  :ensure nil
  ;;   2) Here's where it is; add this dir to the `load-path'.
  :load-path innit:path:package:groot
  ;;   ...?
  ;;   3) I guess I need to make requirements explicit since we aren't trying to
  ;;      install anything?
  :after org


  ;;------------------------------
  :config
  ;;------------------------------

  ;; NOTE: `groot-repositories' set in secrets.

  ;; This is a defvar, not a defcustom, as it's only intended for the dev (me).
  (setq groot--org-api-warn-on-error t))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'org 'version-control)
