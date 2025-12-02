;;; namespaced/str/hash.el --- String Hashes -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2020-10-28
;; Timestamp:  2025-11-20
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; String Hashes
;;
;;; Code:


(imp-require str:/normalize)
(imp-require str:/string)


;;------------------------------------------------------------------------------
;; Constants & Variables
;;------------------------------------------------------------------------------

(defconst str:hash:default 'sha512
  "Default hashing function to use for str:hash-input.")

(defconst str:hash:slice 6
  "Default hashing slice size to use for `str:hash:pretty'.")

(defconst str:hash:join/slices "-"
  "Default slice join string for `str:hash:pretty'.")

(defconst str:hash:join/prefixes "/"
  "Default join string to use for joining prefixes together in `str:hash'.")

(defconst str:hash:join/prepend "::"
  "Default join string to use for joining prefixes to hash in `str:hash'.")


;;------------------------------------------------------------------------------
;; Hashes
;;------------------------------------------------------------------------------

(defun str:hash:full (input &optional hash)
  "Return the full hash string of INPUT.

INPUT can be string, symbol or list of strings/symbols.

HASH algorithm should be one of the symbols returned from
`secure-hash-algorithms', which, see for available algorithms.

If HASH is nil, default to `str:hash:default'."
  ;; Set hash to default if unspecified.
  (let ((hash (or hash str:hash:default))
        ;; Normalize INPUT before error checking.
        (input-string (str:normalize:join input)))
    ;;------------------------------
    ;; Error Checks
    ;;------------------------------
    ;; Make sure hash exists as a supported algorithm.
    (unless (member hash (secure-hash-algorithms))
      (error "Unknown hash: %s" hash))

    (unless (stringp input-string)
      (error "Cannot hash INPUT. Normalization did not produce a string. Input: %s '%s' -> %s '%s'"
             (type-of input)
             input
             (type-of input-string)
             input-string))

    ;;------------------------------
    ;; Hash!
    ;;------------------------------
    (secure-hash hash input-string)))
;; (str:hash:full "jeff")
;; (str:hash:full 'jeff)
;; (str:hash:full '(jeff jeff))
;; (str:hash:full nil)
;; (str:hash:full (list (system-name) system-type))


(defun str:hash:short (input &optional hash slice join)
  "Hash INPUT (can be string, symbol or list of strings/symbols).

If HASH is non-nil, will use that HASH algorithm (see `(secure-hash-algorithms)'
for available algorithms. Otherwise uses `str:hash:default'.

If SLICE is non-nil, will use that (integer) to break the hash up into slices.
Otherwise uses `str:hash:slice'. The slices it uses are the first SLICE
characters and the last SLICE characters.

If JOIN is non-nil, will use that (string) to join back together the hash
slices. Otherwise uses `str:hash:join/slices'."
  (let* ((hash-full (str:hash:full input hash))
         (slice (or slice str:hash:slice))
         (join (or join str:hash:join/slices)))

    (concat (substring hash-full 0 slice)
            join
            (substring hash-full (- slice) nil))))
;; (str:hash:short "jeff")
;; (str:hash:short (list (system-name) system-type))
;; (str:hash:full (list (system-name) system-type))


(defun str:hash:recreate (prefixes pretty-hash)
  "Like `str:hash', with PRETTY-HASH instead of a created hash.

PREFIXES should be: string, symbol, or list of strings/symbols.

PRETTY-HASH should be a string like `str:hash:short' outputs.

This joins all PREFIXES together into a string separated with
`str:hash:join/prefixes'.

Then it joins the prefix string to the PRETTY-HASH string with
`str:hash:join/prepend'."
  ;; Create prepend string from prefixes...
  (concat (apply #'str:join str:hash:join/prefixes
                 (apply #'str:normalize:each prefixes))
          ;; ...add prepend separator...
          str:hash:join/prepend
          ;; ...and finish with the pretty hash.
          pretty-hash))
;; (str:hash:recreate '(jeff compy) "cab3d6-bad38c")


(defun str:hash:pretty (prefixes inputs)
  "PREFIXES and INPUTS should be: string, symbol, or list of strings/symbols.

This joins all PREFIXES together into a string separated with
`str:hash:join/prefixes'.

Then it hash INPUTS using `str:hash:short' function.

Finally, it join prefixes string and inputs hash string with
`str:hash:join/prepend'."
  ;; Create prepend string from prefixes...
  (concat (apply #'str:join str:hash:join/prefixes
                 (apply #'str:normalize:each prefixes))
          ;; ...add prepend separator...
          str:hash:join/prepend
          ;; ...and finish with the pretty hash.
          (str:hash:short inputs)))
;; (str:hash:pretty '(jeff compy) 'laptop-2020)


(defun str:hash:split (hash:pretty)
  "Split a pretty hash back up into input prefixes and the pretty hash."
  (let ((split (split-string hash:pretty str:hash:join/prepend)))
    ;; Return: '((prefixes ...) hash)
    (list (split-string (nth 0 split) str:hash:join/prefixes)
          (nth 1 split))))
;; (str:hash:split (str:hash '(jeff compy) 'laptop-2020))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide str hash)
