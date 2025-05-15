;;; core/modules/emacs/buffer/yank.el --- Cut, Copy, Paste, Yank, Kill -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2023-10-06
;; Timestamp:  2023-10-10
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Cut, Copy, Paste, Yank, Kill
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Yank
;;------------------------------------------------------------------------------

(defun int<yank>:region/delete (start end &optional require-active?)
  "Delete region from START to END in preparation for yank.

If REQUIRE-ACTIVE? is non-nil, region must be active in order to be deleted."
  ;; Only delete region if we in fact have one.
  (when (and (or (and require-active? (region-active-p))
                 (not require-active?))
               (integerp start) ; `integerp' is true for points as well as integers
               (integerp end)
               (/= start end))
      ;; Be at start of region so that you yank into where region was after you
      ;; delete region if for some reason you aren't at/in your region when this
      ;; is called, which you should be unless you're not calling interactively?
      (goto-char (min start end))

      ;; `delete-region' does not add it to the kill ring.
      ;; `kill-region' does, but then you'd need to (yank 2).
      (delete-region start end)))


(defun int<yank>:replace (start end &optional trim/start? trim/end?)
  "Replace any selected region with yanked text.

START should be the start of the region (int or point) or nil.
END should be the end of the region (int or point) or nil.

If TRIM/START? is non-nil, trim whitespace from beginning of yanked text before
inserting.

If TRIM/END? is non-nil, trim whitespace from end of yanked text before
inserting."
  (when-let ((killed (current-kill 0 t))
             (replacement (cond ((and trim/start? trim/end?)
                                 (string-trim killed))
                                (trim/start?
                                 (string-trim-left killed))
                                (trim/end?
                                 (string-trim-right killed))
                                (t
                                 killed))))
    ;; Only delete region if we in fact have one.
    (int<yank>:region/delete start end :require-active)

    ;; ...and yank our (trimmed?) text.
    (insert replacement)))


(defun yank:replace (start end)
  "Replace any selected region with yanked text.

START should be the start of the region (int or point) or nil.
END should be the end of the region (int or point) or nil."
  (interactive "*r")

  ;; ;; Using meow and have a region selected?
  ;; (if (and (bound-and-true-p meow-mode)
  ;;          (region-active-p))
  ;;       ;; Use meow's replace-with-yanked-text function.
  ;;       ;; NOTE: Errors if no region selected, which is why we hide behind
  ;;       ;; `region-active-p'.
  ;;       (meow-replace)
  ;;
  ;;   (int<yank>:replace start end))

  ;; Don't use `meow-replace' because that trims newlines and I don't know why
  ;; you would want to trim newlines..?
  (int<yank>:replace start end))


(defun yank/pop:replace (&optional start end prefix)
  "Replace any selected region with yanked text from kill ring like `yank-pop'.

Otherwise select string from the kill ring and insert it.

START should be the start of the region (int or point) or nil.
END should be the end of the region (int or point) or nil.

See `yank-pop' for the meaning of PREFIX arg, which changes what is yanked &
inserted from kill buffer.

Will use `consult-yank-from-kill-ring' if `consult' is a feature."
  ;; `interactive' string can't do region _and_ prefix?
  ;; So do it ourselves:
  (interactive (list
                (when (region-active-p) (region-beginning))
                (when (region-active-p) (region-end))
                current-prefix-arg))

  ;; Not using `interactive' codes, so we can't use "*" to check for read-only
  ;; buffer, so check ourselves:
  (barf-if-buffer-read-only)

  ;;------------------------------
  ;; If we just yanked, shortcut into `yank-pop'.
  ;;------------------------------
  (cond ((memq last-command '(yank
                              ; `yank-pop' pretends to be `yank'; so no extra check for it.
                              yank:replace
                              meow-replace
                              meow-yank))
         (yank-pop (or prefix 1)))

        ;;------------------------------
        ;; In general, try to use the fancy `consult' yank function.
        ;;------------------------------
        ((and (featurep 'consult)
              (fboundp 'consult-yank-from-kill-ring))
         ;;---
         ;; First, delete the selected region?
         ;;---
         (int<yank>:region/delete start end :require-active)

         ;;---
         ;; Fancy Yank
         ;;---
         (call-interactively #'consult-yank-from-kill-ring))

        ;;------------------------------
        ;; Fallback: Just try to `yank-pop'?
        ;;------------------------------
        (t
         (yank-pop (or prefix 1)))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :buffer 'yank)
