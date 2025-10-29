;;; namespaced/str/init.el --- String Functions -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-02-16
;; Timestamp:  2025-10-29
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
                   (imp-path-current-dir))

;;------------------------------------------------------------------------------
;; Set up custom vars.
;;------------------------------------------------------------------------------

(defgroup str:group nil
  "String manipulation functions."
  :group 'tools)


;;------------------------------------------------------------------------------
;; Load files.
;;------------------------------------------------------------------------------

(imp-timing
    :str
    (imp-path-current-file)

  (imp str:/normalize)
  ;; TODO: refactor the rest of 'em.
  ;; (imp str:/regex)
  ;; (imp str:/buffer)
  ;; (imp str:/string)
  ;; (imp str:/propertize)
  ;; (imp str:/hash)

  ;; ;; Requires 'normalize', 'regex', and 'string'.
  ;; (imp str:/+case)
  ;; (imp str:/+case-hydra)

  ;; ;; Requires 'string'.
  ;; (imp str:/+random)

  ;; End load timing.
  )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide str)
