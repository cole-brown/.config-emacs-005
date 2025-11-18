;;; user/config/emacs/no-littering.el --- PICK UP THAT CAN. -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2025-11-17
;; Timestamp:  2025-11-17
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  For a cleaner `user-emacs-directory', use `no-littering'.
;;
;; Keep the `user-emacs-directory' clean by changing where Emacs & packages
;; store their data. Move it from various & sundry places in and under
;; `user-emacs-directory' to be in one of two `user-emacs-directory'
;; sub-directories:
;;   - `no-littering-etc-directory'
;;   - `no-littering-var-directory'
;;
;;; Code:

;;------------------------------------------------------------------------------
;; `no-littering'
;;------------------------------------------------------------------------------

(use-package no-littering
  ;; Make sure this loads ASAP.
  ;; It's dictating where other packages can write their shit to.
  :demand t

  ;;------------------------------
  :config
  ;;------------------------------
  ;; Suggested settings: https://github.com/emacscollective/no-littering#suggested-settings

  ;; backups
  ;;--------
  ;; Backups like auto-saves, `undo-tree' history, etc. should go in the
  ;; `no-littering' directory. They should not be littered in the same dir as
  ;; their real actual file.
  ;; NOTE: "theme" is overloaded here. It has nothing to do with `zenburn' or
  ;; whatever is theming my UI. It is "them[ing] locations where backups of
  ;; various sorts are created".
  (no-littering-theme-backups)

  ;; `recentf'
  ;;----------
  ;; `recentf' should ignore the files in the `no-littering' dirs.
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude no-littering-etc-directory)
    (add-to-list 'recentf-exclude no-littering-var-directory))

  ;; 'custom.el'
  ;;------------
  ;; TODO: send to /dev/null instead. Get code for that from 2023's repo.
  ;; NOTE: Not doing this because I want everything in my init.  No package or
  ;; face will be added manually. I want my .emacs to be reproducable. To get
  ;; a new computer up and running should just be:
  ;;   1) install Emacs
  ;;   2) pull this repo to the correct place
  ;;   3) start Emacs
  ;;---
  ;; ;; Move `custom-set-variables' and `custom-set-faces' out if this file.
  ;; ;; Put in `etc/' instead.
  ;; (setq custom-file (no-littering-expand-etc-file-name "custom.el"))

  ;; Native Compliation (Emacs 29+)
  ;;-------------------------------
  (when (fboundp 'startup-redirect-eln-cache)
    (startup-redirect-eln-cache
     (convert-standard-filename
      (no-littering-expand-var-file-name "eln-cache/")))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide user (imp-path-relative 'user (imp-path-sans-extension (imp-path-current-file))))
