;;; mantle/config/org/toggl.el --- Toggl time-tracking integration with Org-Mode -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2023-10-11
;; Timestamp:  2023-10-16
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Toggl time-tracking integration with Org-Mode
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Toggl
;;------------------------------------------------------------------------------

;;------------------------------
;; `org-toggl' Usage:
;;-------
;; If you use Org mode (which is the recommended way to interact with
;; org-toggl), put the point on a heading and `M-x org-toggl-set-project' to
;; associate that heading (and its subheadings, if
;; `org-toggl-inherit-toggl-properties' is non-nil) with a project. Then, if
;; `org-toggl-integration-mode' minor mode is on, clocking in/out of such a
;; heading will start/stop the respective Toggl entry.
;;------------------------------

;; https://github.com/mbork/org-toggl
(imp:use-package org-toggl
  ;; Not on a package repository so get it from GitHub:
  :straight (:type git
             :host github
             :repo "mbork/org-toggl"
             :fork (:host github
                    :repo "cole-brown/org-toggl"))
  :after org

  ;;------------------------------
  :custom
  ;;------------------------------

  ;; Should a sub-heading in `org-mode' inherit the `org-toggl' properties of its parent(s)?
  (org-toggl-inherit-toggl-properties t)

  ;; Auth Credentials
  (toggl-workspace-id (plist-get secret:key:toggl :workspace))
  (toggl-auth-token   (plist-get secret:key:toggl :key))


  ;;------------------------------
  :config
  ;;------------------------------

  (toggl-get-projects)
  (org-toggl-integration-mode +1))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'org 'toggl)
