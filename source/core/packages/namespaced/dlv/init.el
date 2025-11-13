;;; core/modules/emacs/dlv/init.el --- Directory Local Variables -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-09-10
;; Timestamp:  2025-11-12
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Code-Defined Directory Local Variables
;; Code-Defined File Local Variables
;;
;;
;; `dlv' looks for these `imp:flag' flags:
;;   - `:dlv +debug' : Default to enabling `:info' and `:debug' log statements.
;;   - Enable/Disable Settings (mutually exclusive; pick one):
;;     - `:dlv -enabled'      : Not enabled == disabled.
;;     - `:dlv +enabled/safe' : Only safe DLVs allowed!
;;     - `:dlv +enabled/all'  : Always allow anything - potentially dangerous!
;;     - `:dlv +enabled/flag' : Always ask the user - potentially annoying!
;;   - `:dlv -display'
;;     - Don't load the commands for displaying DLVs & related info.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Set-Up.
;;------------------------------------------------------------------------------

(imp:path:root/set :dlv
                   (imp:path:current:dir)
                   (imp:file:current))


;;------------------------------------------------------------------------------
;; Load Files.
;;------------------------------------------------------------------------------

(imp:timing
    :dlv
    (imp:file:current)
    (imp:path:current:dir)

  ;;------------------------------
  ;; Debugging
  ;;------------------------------

  (imp:load :feature  '(:dlv debug)
            :filename "debug")
  ;; Initialize debugging before going any further.
  (int<dlv>:debug:init)

  ;;------------------------------
  ;; Required
  ;;------------------------------

  (imp:load :feature  '(:dlv path)
            :filename "path")
  (imp:load :feature  '(:dlv class)
            :filename "class")
  (imp:load :feature  '(:dlv dlv)
            :filename "dlv")

  ;;------------------------------
  ;; Optional Files
  ;;------------------------------

  ;; Always load unless specifically removed.
  (unless (imp:flag? :dlv -display)
    (imp:load :feature  '(:dlv +display)
              :filename "+display")))


;;------------------------------------------------------------------------------
;; Optional Functionality
;;------------------------------------------------------------------------------

;; Only run the optional functionality checks/enables when loading this file.
(when (imp:provide:loading?)
  ;;------------------------------
  ;; Enable/disable DLVs?
  ;;------------------------------
  ;; Check for a feature flag for how to enable DLVs.
  ;; NOTE: Currently considering these flags mutually exclusive.
  (cond ((imp:flag? :dlv -enabled) ;; Not enabled == disabled.
         (dlv:enable :disable))
        ((imp:flag? :dlv +enabled/safe) ;; Only safe DLVs allowed!
         (dlv:enable :safe))
        ((imp:flag? :dlv +enabled/all) ;; Always allow anything - potentially dangerous!
         (dlv:enable :all))
        ((imp:flag? :dlv +enabled/prompt) ;; Always ask the user.
         (dlv:enable :prompt))
        ;; Default: Always enable DLVs unless specifically told not to.
        (t
         (dlv:enable :enable))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide:with-emacs :dlv)
