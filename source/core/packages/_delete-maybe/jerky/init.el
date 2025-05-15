;;; core/modules/elisp/jerky/init.el --- Tree/Value Store -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2020-07-14
;; Timestamp:  2023-06-21
;; Version:    3.2
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; You've heard of "Key/Value Store", well Jerky is a Tree/Value Store, sort of.
;; Fancy hash table, technically.
;;
;; Store values at the leaf nodes of a tree.
;;
;;; Code:


;; ┌───────────────────────────────┬───────┬───────────────────────────────────┐
;; ├───────────────────────────────┼ jerky ┼───────────────────────────────────┤
;; │ Named after ~dirky~, as in "dir key", which needed to be more general...  │
;; │ So... Generlky? Genky?... Jenky?                                          │
;; │ Jerky?                                                                    │
;; │                                 Jerky.                                    │
;; └───────────────────────────────────────────────────────────────────────────┘


;;------------------------------------------------------------------------------
;; Set-Up.
;;------------------------------------------------------------------------------

(imp:path:root/set :jerky
                   (imp:path:current:dir)
                   (imp:file:current))


;;------------------------------------------------------------------------------
;; Load Jerky Files.
;;------------------------------------------------------------------------------

(imp:timing
    :jerky
    (imp:file:current)
    (imp:path:current:dir)

  (imp:load :feature  '(:jerky debug)
            :filename "debug")
  ;; Initialize debugging before going any further.
  (int<jerky>:debug:init)

  (imp:load :feature  '(:jerky utils)
            :filename "utils")

  (imp:load :feature  '(:jerky jerky)
            :filename "jerky")

  ;; Always load `dlv' unless specifically removed.
  (when (int<jerky>:using:dlv?)
    (imp:load :feature  '(:jerky +dlv)
              :filename "+dlv")))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide:with-emacs :jerky)
