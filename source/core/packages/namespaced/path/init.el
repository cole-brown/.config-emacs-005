;;; core/modules/emacs/path/init.el --- Nicer path & file functions. -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2020-10-22
;; Timestamp:  2023-08-11
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

(imp:path:root/set :path
                   (imp:path:current:dir)
                   "init.el")


;;------------------------------------------------------------------------------
;; Set up custom vars.
;;------------------------------------------------------------------------------

(defgroup path:group nil
  "Path functions."
  :prefix "path:"
  :group 'tools)


;;------------------------------------------------------------------------------
;; Load Files
;;------------------------------------------------------------------------------

(imp:timing
    :path
    "init.el"
    (imp:path:current:dir)


  ;; Always load.
  (imp:load :feature  '(:path path)
            :filename "path")
  (imp:load :feature  '(:path files)
            :filename "files")
  (imp:load :feature  '(:path dir)
            :filename "dir")
  (imp:load :feature  '(:path regex)
            :filename "regex")
  (imp:load :feature  '(:path git)
            :filename "git")
  (imp:load :feature  '(:path buffer)
            :filename "buffer")
  (imp:load :feature  '(:path +uniquify)
            :filename "+uniquify")


  ;; End load timing.
  )


;;--------------------------------------------------------------------------------
;; Init
;;--------------------------------------------------------------------------------

(int<path>:uniquify:nub:init)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :path)
