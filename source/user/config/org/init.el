;;; source/user/org/init.el --- Configure Org & Friends -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2023-03-16
;; Timestamp:  2025-10-28
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Configure `org-mode', `org-journal', etc.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Org Helpers
;;------------------------------------------------------------------------------

(imp-load :feature  '(:user mode org)
          :path     "mode/org" ;; (imp-path-current-dir-relative :user)
          :filename "init")


;;------------------------------------------------------------------------------
;; Org-Mode Itself
;;------------------------------------------------------------------------------

(imp-load :feature  '(:user config org mode)
          :path     "config/org" ;; (imp-path-current-dir-relative :user)
          :filename "mode")

;; ...And now theme org mode itself...
(imp-eval-after (:and zenburn-theme org)
  (imp-load :feature  '(:user config org theme zenburn)
            :path     "config/org" ;; (imp-path-current-dir-relative :user)
            :filename "theme/zenburn"))
;; TODO: another one for `hc-zenburn-theme'

;; Skip agenda for now... It doesn't jive well with hundreds of org files...
;; (imp-load :feature  '(:user config org agenda)
;;           :path     "source/user/org" ;; (imp-path-current-dir-relative :user)
;;           :filename "agenda")


;; ;;------------------------------------------------------------------------------
;; ;; Org-Journal
;; ;;------------------------------------------------------------------------------

;; (imp-load :feature  '(:user config org journal)
;;           :path     "source/user/org" ;; (imp-path-current-dir-relative :user)
;;           :filename "journal")


;; ;;------------------------------------------------------------------------------
;; ;; Etc.
;; ;;------------------------------------------------------------------------------

;; (imp-load :feature  '(:user config org contacts)
;;           :path     "source/user/org" ;; (imp-path-current-dir-relative :user)
;;           :filename "contacts")

;; (imp-load :feature  '(:user config org pretty)
;;           :path     "source/user/org" ;; (imp-path-current-dir-relative :user)
;;           :filename "pretty")


;; ;;------------------------------------------------------------------------------
;; ;; Integration with Other Parts of Emacs
;; ;;------------------------------------------------------------------------------

;; (imp-load :feature  '(:user config org version-control)
;;           :path     "source/user/org" ;; (imp-path-current-dir-relative :user)
;;           :filename "version-control")


;; ;;--------------------------------------------------------------------------------
;; ;; Integration with Apps / Services
;; ;;--------------------------------------------------------------------------------

;; (imp-load :feature  '(:user config org toggl)
;;           :path     "source/user/org" ;; (imp-path-current-dir-relative :user)
;;           :filename "toggl")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide user config org)
