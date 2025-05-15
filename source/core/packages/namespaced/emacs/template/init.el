;;; core/modules/emacs/template/init.el --- Non-Interactive Snippets -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2023-04-26
;; Timestamp:  2023-06-26
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Non-Interactive Snippets
;;    - TODO: Name NISnippet to mimic YASnippet?
;;
;;; Code:


(imp:path:root/set :template
                   (imp:path:current:dir)
                   "init.el")

;;------------------------------------------------------------------------------
;; Load Files
;;------------------------------------------------------------------------------

(imp:timing
    :template
    "init.el"
    (imp:path:current:dir)

  (imp:load :feature  '(:template template)
            :filename "template"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :template)
