;;; mantle/config/files.el --- File & Directory Settings -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-07-29
;; Timestamp:  2025-09-22
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  File & Directory Settings
;;
;;; Code:


;;------------------------------------------------------------------------------
;; recentf for recent files
;;------------------------------------------------------------------------------


(use-package recentf
  :ensure nil ; This is an Emacs built-in feature.
  :demand t
  :after tramp ; `--/recentf/handler/file/truename' requires tramp

  ;;------------------------------
  :init
  ;;------------------------------

  (defun --/recentf/handler/file/truename (file)
    "Proudly nicked from Doom's 'core/core-editor.el'."
    (if (or (not (file-remote-p file))
            (equal "sudo" (file-remote-p file 'method)))
        (abbreviate-file-name
         (file-truename
          (if (fboundp #'tramp-file-name-localname)
              (tramp-file-name-localname file)
            file)))
      file))


  ;;------------------------------
  :custom
  ;;------------------------------

  ;;`recentf-auto-cleanup'
  ;;----------------------
  ;; Clean up the recent list when Emacs has been idle for over 30 seconds.
  (recentf-auto-cleanup 30)

  ;; `recentf-max-saved-items'
  ;;--------------------------
  ;; Default is 20. Doom sets to 200 and that was occasionally too low.
  (recentf-max-saved-items 1000)

  ;; `recentf-max-menu-items'
  ;;-------------------------
  ;; How many saved items to /show/.
  (recentf-max-menu-items 20)


  ;;------------------------------
  :config
  ;;------------------------------

  ;; Resolve symlinks, strip out the /sudo:X@ prefix in local tramp paths, and
  ;; abbreviate $HOME -> ~ in filepaths (more portable, more readable, & saves
  ;; space)
  (add-to-list 'recentf-filename-handlers #'--/recentf/handler/file/truename)

  ;; Text properties inflate the size of recentf's files, and there is
  ;; no purpose in persisting them.
  ;; NOTE: Must be first in the list!
  (add-to-list 'recentf-filename-handlers #'substring-no-properties)

  ;; Periodically save the list of recent files: https://www.emacswiki.org/emacs/RecentFiles#toc1
  ;; Otherwise they're only saved during a graceful shutdown.
  (run-with-timer (* 30 60) ;; TODO: need `ns:unit': (unit:second 30 'mins) ;; Wait 30 mins to run.
                  (* 30 60) ;; TODO: need `ns:unit': (unit:second 30 'mins) ;; Repeat every 30 mins.
                  'recentf-save-list)

  ;; Recentf and TRAMP need some peace-keeping to get along.
  ;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2007-07/msg00019.html
  (add-to-list 'recentf-keep 'file-remote-p)

  ;; TODO: a squelch function for this.
  ;; ;; Don't want the minibuffer to always say
  ;; ;;   "Cleaning up the recentf list...done (NN removed)"
  ;; ;; when Emacs has been idle a while and `recentf' has run the auto-cleanup.
  ;; ;;
  ;; ;; NOTE: `recentf-auto-cleanup' just sets up a timer to call
  ;; ;; `recentf-cleanup', which I do want to output messages when called
  ;; ;; interactively... so only squelch if it's _not_ called interactively.
  ;; (define-advice recentf-cleanup (:around (fn &rest args) mantle:user:squelch)
  ;;   "`recentf-auto-cleanup' should not allow `recentf-cleanup' to be chatty."
  ;;   (innit:squelch/unless :interactive? t
  ;;                         (apply fn args)))

  ;; TODO: a squelch function for this.
  ;; ;; Don't want a bunch of `load-file' messages when `recentf-load-list' runs.
  ;; (define-advice recentf-load-list (:around (fn &rest args) mantle:user:squelch)
  ;;   "Don't want a bunch of `load-file' messages when `recentf-load-list' runs."
  ;;   (innit:squelch/unless :interactive? t
  ;;                         (apply fn args)))

  ;; Excluded Files/Dirs:
  ;;---
  ;; NOTE: `no-littering' has the set-up for adding its dirs to `recentf-exclude'.
  ;;   See:    core/boot/10-init/00-bootstrap.el
  ;;   Search: recentf-exclude
  ;;---
  ;;
  ;; No others to exclude that I know of, currently.
  ;; (add-to-list 'recentf-exclude <path>)

  ;; Enable!
  (recentf-mode +1))


;;------------------------------------------------------------------------------
;; Search: Deadgrep (uses Ripgrep)
;;------------------------------------------------------------------------------

;;------------------------------
;; ripgrep
;;------------------------------
;; https://github.com/BurntSushi/ripgrep
;;
;; This needs installed on the computer separately from Emacs.
;;
;;   "ripgrep is a line-oriented search tool that recursively searches your
;; current directory for a regex pattern. By default, ripgrep will respect your
;; .gitignore and automatically skip hidden files/directories and binary files.
;; ripgrep has first class support on Windows, macOS and Linux, with binary
;; downloads available for every release. ripgrep is similar to other popular
;; search tools like The Silver Searcher, ack and grep."


;;------------------------------
;; deadgrep
;;------------------------------
;; https://github.com/Wilfred/deadgrep
;; "Deadgrep is the fast, beautiful text search that your Emacs deserves."
(use-package deadgrep
  :demand t

  ;;------------------------------
  :init
  ;;------------------------------

  (defun --/deadgrep/default-directory (search-term)
    "Search for SEARCH-TERM with `deadgrep' at `default-directory'."
    (interactive (list (deadgrep--read-search-term)))
    (call-interactively #'deadgrep
                        search-term
                        default-directory))


  (defun --/deadgrep/buffer/kill-all ()
    "Kill all deadgrep buffers."
    (interactive)
    (message "[%s] Kill 'deadgrep' buffers..."
             (datetime:format 'rfc-3339 'datetime))
    (buffer:kill:matching ".*deadgrep.*"
                          :internal
                          :modified
                          :process))

  ;; ;;------------------------------
  ;; :config
  ;; ;;------------------------------
  ;;
  ;; ;;---
  ;; ;; Project Root Overrides
  ;; ;;---
  ;; ;; TODO: Search per-comp configs for `deadgrep-project-root' to find what's set?
  ;; ;;   - `deadgrep-project-root-overrides'
  ;; ;;   - `deadgrep-project-root-function'
  )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide :user 'config 'files)
