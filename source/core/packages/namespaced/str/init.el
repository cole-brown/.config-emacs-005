;;; namespaced/str/init.el --- String Functions -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-02-16
;; Timestamp:  2025-10-28
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

  (imp-parser str:/normalize)
  ;; TODO: refactor the rest of 'em.
  ;; (imp-parser str:/regex)
  ;; (imp-parser str:/buffer)
  ;; (imp-parser str:/string)
  ;; (imp-parser str:/propertize)
  ;; (imp-parser str:/hash)

  ;; ;; Requires 'normalize', 'regex', and 'string'.
  ;; (imp-parser str:/+case)
  ;; (imp-parser str:/+case-hydra)

  ;; ;; Requires 'string'.
  ;; (imp-parser str:/+random)

  ;; End load timing.
  )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide str)
