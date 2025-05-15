;;; namespaced/str/init.el --- String Functions -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-02-16
;; Timestamp:  2025-05-15
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; String Functions
;;
;; Why?
;;   - These are namespaced, which makes searching/finding/browsing them in
;;     Emacs help much nicer.
;;   - Many more string case types.
;;   - Other reasons at the time that weren't written down.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Set up imp.
;;------------------------------------------------------------------------------

(imp-path-root-set :str
                   (imp-path-current-dir)
                   "init.el")


;;------------------------------------------------------------------------------
;; Set up custom vars.
;;------------------------------------------------------------------------------

(defgroup str:group nil
  "String manipulation functions."
  :group 'tools)


;;------------------------------------------------------------------------------
;; Load files.
;;------------------------------------------------------------------------------

(imp:timing
    :str
    "init.el"
    (imp:path:current:dir)

  (imp:load :feature  '(:str normalize)
            :filename "normalize")
  ;; (imp:load :feature  '(:str regex)
  ;;           :filename "regex")
  ;; (imp:load :feature  '(:str buffer)
  ;;           :filename "buffer")
  ;; (imp:load :feature  '(:str string)
  ;;           :filename "string")
  ;; (imp:load :feature  '(:str propertize)
  ;;           :filename "propertize")
  ;; (imp:load :feature  '(:str hash)
  ;;           :filename "hash")

  ;; ;; Requires 'normalize', 'regex', and 'string'.
  ;; (unless (imp:flag? :str -case)
  ;;   (imp:load :feature  '(:str +case)
  ;;             :filename "+case")

  ;;   (unless (imp:flag? :str -hydra)
  ;;     (imp:load :feature  '(:str +hydra +case)
  ;;               :filename "+case-hydra")))

  ;; ;; Requires 'string'.
  ;; (unless (imp:flag? :str -random)
  ;;   (imp:load :feature  '(:str +random)
  ;;             :filename "+random"))

  ;; End load timing.
  )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide :str)
