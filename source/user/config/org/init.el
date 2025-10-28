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

(imp-parser user:/mode/org/init)


;;------------------------------------------------------------------------------
;; Org-Mode Itself
;;------------------------------------------------------------------------------

(imp-parser mode :path pwd)

;; ...And now theme org mode itself...
(imp-parser theme/zenburn
  :path pwd
  :after zenburn-theme
  :after org)

;; Skip agenda for now... It doesn't jive well with hundreds of org files...
;; (imp-parser agenda :path pwd)


;; ;;------------------------------------------------------------------------------
;; ;; Org-Journal
;; ;;------------------------------------------------------------------------------

;; (imp-parser journal :path pwd)


;; ;;------------------------------------------------------------------------------
;; ;; Etc.
;; ;;------------------------------------------------------------------------------

;; (imp-parser contacts :path pwd)

;; (imp-parser pretty :path pwd)


;; ;;------------------------------------------------------------------------------
;; ;; Integration with Other Parts of Emacs
;; ;;------------------------------------------------------------------------------

;; (imp-parser version-control :path pwd)


;; ;;--------------------------------------------------------------------------------
;; ;; Integration with Apps / Services
;; ;;--------------------------------------------------------------------------------

;; (imp-parser toggl :path pwd)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide user config org)
