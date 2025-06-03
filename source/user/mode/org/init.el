;;; modules/mode/org/init.el --- Org-Mode Extras -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2020-11-16
;; Timestamp:  2023-06-29
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Org-Mode Extras
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Org-Mode Stuff
;;------------------------------------------------------------------------------

(imp-timing
 '(:mode org)
 (imp-file-current)
 (imp-path-current-dir)


 (imp-load :feature  '(:mode org keyword)
           :path     (imp-path-current-dir)
           :filename "keyword")
 (imp-load :feature  '(: mode org link)
           :path     (imp-path-current-dir)
           :filename "link"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide :mode 'org)
