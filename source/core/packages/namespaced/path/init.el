;;; namespaced/path/init.el --- Nicer path & file functions. -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2020-10-22
;; Timestamp:  2025-11-04
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Nicer path & file functions.
;;
;; Why?
;;   - Emacs' functions have funky names that don't seem to really indicate what
;;     they do half the time.
;;   - `f.el' has a strange fixation on paths actually existing.
;;   - No one seems to want to deal with paths from some other operating system.
;;   - These are namespaced, which makes searching/finding/browsing them in
;;     Emacs help much nicer.
;;     - `path:' - path functions (including filepaths & dirpaths)
;;     - `file:' - filename functions
;;     - `dir:'  - dirname functions
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Set imp Root
;;------------------------------------------------------------------------------

(imp-path-root-set 'path (imp-path-current-dir))


;;------------------------------------------------------------------------------
;; Set up custom vars.
;;------------------------------------------------------------------------------

(defgroup path nil
  "Path functions."
  :group 'tools)


;;------------------------------------------------------------------------------
;; Load Files
;;------------------------------------------------------------------------------

(imp-timing
    'path
    (imp-path-current-file)

  (imp path:/path)      ; requires `str', `elisp:/functions'
  (imp path:/files)     ; requires `path:/path'
  (imp path:/dir)       ; requires `path:/path'
  (imp path:/regex)     ; requires `list/alist:/type/string'
  (imp path:/git)       ; requires `path:/path'
  (imp path:/buffer)    ; requires `path:/path', `path:/git'
  (imp path:/+uniquify) ; requires `path:/path', `path:/git', `path:/buffer'

  ;; End load timing.
  )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide path)
