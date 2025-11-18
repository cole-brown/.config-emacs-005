;;; user/config/emacs/timestamp.el --- Auto-timestamp timestamps. -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2025-11-17
;; Timestamp:  2025-11-17
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Auto-timestamp timestamps.
;;  Like that ";; Timestamp:" line above.
;;
;;; Code:

  (require 'rx)


;;------------------------------------------------------------------------------
;; Emacs's `time-stamp'
;;------------------------------------------------------------------------------

  (use-package time-stamp
    :ensure nil ; This is an Emacs built-in feature; don't need to install the package.

    ;;------------------------------
    ;; NOTE: Modes that want timestamps, do something like this:
    ;;------------------------------
    ;; Example: `org-mode':
    ;; ;;------------------------------
    ;; :hook
    ;; ;;------------------------------
    ;; ((org-mode-hook    . --/hook/time-stamp/settings
    ;;  (before-save-hook . --/hook/time-stamp/before-save))


    ;;------------------------------
    ;; WARNING
    ;;------------------------------
    ;; The docstrings for the \\[time-stamp] custom vars says that you
    ;; should not change them globally:
    ;;   > These variables are best changed with file-local variables.
    ;;   > If you were to change `time-stamp-pattern', `time-stamp-line-limit',
    ;;   > `time-stamp-start', or `time-stamp-end' in your init file, you
    ;;   > would be incompatible with other people's files.
    ;;
    ;; The file comments also warn:
    ;;   > ;;; Do not change time-stamp-line-limit, time-stamp-start,
    ;;   > ;;; time-stamp-end, time-stamp-pattern, time-stamp-inserts-lines,
    ;;   > ;;; or time-stamp-count in your .emacs or you will be incompatible
    ;;   > ;;; with other people's files!  If you must change them, do so only
    ;;   > ;;; in the local variables section of the file itself.
    ;;
    ;; See: /usr/share/emacs/28.1/lisp/time-stamp.el.gz
    ;;
    ;; So we'll make hook funcs to set them locally.


    ;;------------------------------
    :init
    ;;------------------------------

    (defun --/hook/time-stamp/settings ()
      "Timestamp settings hook function for major modes.

Set `time-stamp' local vars.

Add to mode's hook variable (usually `MODE-hook', sometimes
something else).
e.g. for `use-package org'
```
  :hook
  ((org-mode-hook    . --/hook/time-stamp/settings
   (before-save-hook . --/hook/time-stamp/before-save))
```

NOTE: This assumes you have set `use-package-hook-name-suffix' to nil:
  (customize-set-variable 'use-package-hook-name-suffix nil)"
      ;; `time-stamp-pattern' (effective) default:
      ;;    "8/Time-stamp:[ \t]+\\\\?[\"<]+%:y-%02m-%02d %02H:%02M:%02S %u\\\\?[\">]"
      ;;  Example:
      ;;    "Time-stamp: <2987-06-05 10:11:12 username>"
      ;;
      ;; ...but `time-stamp-pattern' is a cluttered and confusing combination of
      ;; 3 or more separate variables.  Set the separate variables instead and
      ;; let `time-stamp' figure it out.

      ;;---
      ;; Search Limits
      ;;---
      ;; How far to search a file for the time-stamp?
      ;;   - Positive: from start of file
      ;;   - Negative: from end of file
      ;; default: 8
      (setq-local time-stamp-line-limit 20)

      ;;---
      ;; Search Regexes
      ;;---
      ;; Start!
      ;;   default: "Time-stamp:[ \t]+\\\\?[\"<]+"
      ;;   aka:     (rx "Time-stamp:"
      ;;                (one-or-more (any " " "\t"))
      ;;                (optional "\\")
      ;;                (one-or-more (any ?\" "<")))
      ;; Should we try to be compatible while also letting ourself do timestamps like?:
      ;;   "Timestamp: 2310-03-04" (most files?)
      ;;   "TIMESTAMP: 2310-03-04" (org-mode file property)
      (setq-local time-stamp-start
                  (rx (or "Timestamp" "Time-stamp" "Time-Stamp" "TIMESTAMP")
                      ":"
                      (one-or-more (any " " "\t"))
                      (optional "\\") ; ...wut? Escape char in some file types for "<" maybe?
                      (optional (any ?\" "<"))))

      ;; End!
      ;;   default: "\\\\?[\">]"
      ;;   aka:     (rx (optional "\\") (any ?\" ">"))
      ;;
      ;; Should we try to be compatible while also letting ourself do timestamps like?:
      ;;   "Timestamp: 2310-03-04"
      (setq-local time-stamp-end
                  (rx (or (and (optional "\\")
                               (any ?\" ">"))
                          "\n")))

      ;;---
      ;; Time Format
      ;;---
      ;; default: "%Y-%02m-%02d %02H:%02M:%02S %l"
      (setq-local time-stamp-format "%Y-%02m-%02d"))


    ;; This hook is merely a namespacing so that it's easier for me to remember.
    ;; Could just as easily do (in use-package):
    ;;   :hook
    ;;   ((org-mode-hook . --/hook/time-stamp/settings
    ;;    (before-save-hook . time-stamp))
    ;; But it's easier to grok when both of 'em look like they belong together:
    ;;   :hook
    ;;   ((org-mode-hook    . --/hook/time-stamp/settings
    ;;    (before-save-hook . --/hook/time-stamp/before-save))
    (defun --/hook/time-stamp/before-save ()
      "Auto-timestamp files before save.

Add to hook variable `before-save-hook'.
e.g. for `use-package org'
```
  :hook
  ((org-mode-hook    . --/hook/time-stamp/settings
   (before-save-hook . --/hook/time-stamp/before-save))
```

NOTE: This assumes you have set `use-package-hook-name-suffix' to nil:
  (customize-set-variable 'use-package-hook-name-suffix nil)"

      ;; This hook is merely a namespacing so that it's easier for me to remember.
      ;; Could just as easily do (in use-package):
      ;;   :hook
      ;;   ((org-mode-hook . --/hook/time-stamp/settings
      ;;    (before-save-hook . time-stamp))
      ;; But it's easier to grok when both of 'em look like they belong together:
      ;;   :hook
      ;;   ((org-mode-hook    . --/hook/time-stamp/settings
      ;;    (before-save-hook . --/hook/time-stamp/before-save))
      (time-stamp)))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide user (imp-path-relative 'user (imp-path-sans-extension (imp-path-current-file))))
