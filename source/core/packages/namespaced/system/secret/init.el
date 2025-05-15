;;; core/modules/system/secret/init.el --- Set-Up Secrets Separately -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2020-08-28
;; Timestamp:  2023-06-26
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; ┌───────────────────────────────────────────────────────────────────────────┐
;; │                      What's the Secret Password?                          │
;; └───────────────────────────────────────────────────────────────────────────┘
;;
;; Separate secrets from the standard Emacs initialization.
;;
;; Allow multiple systems (computers) to use the same init with small
;; differences.
;;
;; NOTE: Both these functions must be called:
;;   1. `system:secret:init'
;;   2. `system:secret:config'
;;
;; They should be called in the init code before anything that may require
;; secrets.
;;
;; `system:secret:init' must be called very early on.
;;
;; `system:secret:config' could be called just after `system:secret:init', or
;; later on in config, depending on how you use your secrets' init/config.
;;
;; Existance of secrets for this system is first checked for in
;; `system:secret:init'.
;;
;;; Code:




;;------------------------------------------------------------------------------
;; Load our files.
;;------------------------------------------------------------------------------

(imp:timing
    '(:system secret)
    (imp:file:current)
    (imp:path:current:dir)

  (imp:load :feature  '(:system secret debug)
            :path     (imp:path:current:dir/relative :system)
            :filename "debug")
  ;; Initialize debugging before going any further.
  (int<system/secret>:nub:init)

  (imp:load :feature  '(:system secret functions)
            :path     (imp:path:current:dir/relative :system)
            :filename "functions")

  (imp:load :feature  '(:system secret load)
            :path     (imp:path:current:dir/relative :system)
            :filename "load"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :system 'secret)
