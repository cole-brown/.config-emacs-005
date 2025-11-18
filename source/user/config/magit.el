;;; user/config/magit.el --- magit -*- lexical-binding: t; -*-
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
;;  Magit & Friends
;;
;; The best Git Porcelain:
;;   https://magit.vc/
;;   https://github.com/magit/magit
;;
;; Plus some other version control things.
;; Whatever.
;; The important thing is: Magit.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Magit: Git Front-End (Porcelain)
;;------------------------------------------------------------------------------
;; The best Git Porcelain.
;;   https://magit.vc/
;;   https://github.com/magit/magit

(use-package magit

  ;;------------------------------
  :init
  ;;------------------------------

  ;; (defun mantle:user:magit:buffer:kill ()
  ;;   "Kill all magit buffers."
  ;;   (interactive)
  ;;   (message "[%s] Kill all 'magit' buffers..."
  ;;            (datetime:format 'rfc-3339 'datetime))
  ;;   (buffer:kill:matching ".*magit.*"
  ;;                         :internal
  ;;                         :modified
  ;;                         :process))
  )


;;------------------------------------------------------------------------------
;; Git Gutter
;;------------------------------------------------------------------------------
;; https://github.com/emacsorphanage/git-gutter-fringe

(use-package git-gutter-fringe
  ;; NOTE: `git-gutter-fringe' does not work in the terminal. Use `git-gutter' if
  ;; needed there.
  :when (display-graphic-p)
  :diminish

  ;;------------------------------
  :config
  ;;------------------------------
  (global-git-gutter-mode +1))

;;------------------------------------------------------------------------------
;; Git File Modes
;;------------------------------------------------------------------------------

(use-package git-modes
  :defer t

  ;;------------------------------
  :config
  ;;------------------------------

  ;; Docker's ignore file is basically the same format as .gitignore, so use the
  ;; `gitignore-mode' for it.
  (add-to-list 'auto-mode-alist
               (cons "/.dockerignore\\'" 'gitignore-mode)))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide user (imp-path-relative 'user (imp-path-sans-extension (imp-path-current-file))))
