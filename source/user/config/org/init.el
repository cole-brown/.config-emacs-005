;;; source/user/org/init.el --- Configure Org & Friends -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2023-03-16
;; Timestamp:  2025-11-18
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

(imp user:/mode/org/init)


;;------------------------------------------------------------------------------
;; Org-Mode Itself
;;------------------------------------------------------------------------------

(imp ./mode)

;; This has been moved to user/theme/zenburn/org-mode.el.
;; ;; ...And now theme org mode itself...
;; (imp ./theme/zenburn
;;   :after zenburn-theme
;;   :after org)

;; Skip agenda for now... It doesn't jive well with hundreds of org files...
;; (imp agenda :path pwd)


;; ;;------------------------------------------------------------------------------
;; ;; Org-Journal
;; ;;------------------------------------------------------------------------------

;; (imp ./journal)


;; ;;------------------------------------------------------------------------------
;; ;; Etc.
;; ;;------------------------------------------------------------------------------

;; (imp ./contacts)

;; (imp ./pretty)


;; ;;------------------------------------------------------------------------------
;; ;; Integration with Other Parts of Emacs
;; ;;------------------------------------------------------------------------------

;; (imp ./version-control)


;; ;;--------------------------------------------------------------------------------
;; ;; Integration with Apps / Services
;; ;;--------------------------------------------------------------------------------

;; (imp ./toggl)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide user config org)
