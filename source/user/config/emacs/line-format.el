;;; user/config/emacs/line-format.el --- mode & header line formats -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    TODO:(datetime:timestamp:insert :rfc-3339:date)
;; Timestamp:  2026-06-25
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  `mode-line-format'
;;  `header-line-format'
;;
;; See Info node `(elisp) Mode Line Format'
;; See URL https://www.gnu.org/software/emacs/manual/html_node/elisp/Mode-Line-Format.html
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Mode Line
;;------------------------------------------------------------------------------

(with-eval-after-load 'vc-hooks
;; TODO: Move to magit.el?
  (define-advice vc-mode-line-state (:around (func state) --/advice/vc/nerd-icons/state)
    "Change INDICATOR from ASCII string to nerd-font icons string.

example: \"@\" -> \"󱓊\""
    ;; destructuring bind of advised func's return values
    (pcase-let ((`(,state-echo ,face ,indicator) (funcall func state)))
      ;; Return STATE-ECHO and FACE unchanged
      (list
       state-echo
       face
       ;; Convert INDICATOR
       (cond ((eq state 'up-to-date)
              (/icon/solo "nf-md-file_check") ; 󰈖
              ;; (/icon/solo "nf-md-source_branch_check") ; 󱓏
              ;; "nf-md-source_branch" ; 󰘬
              )

             ((eq state 'needs-update)
              (/icon/solo "nf-md-file_restore") ; 󰙰
              ;; (/icon/solo "nf-md-file_sync") ; 󱈖
              ;; (/icon/solo "nf-md-source_branch_sync") ; 󱓎
              )

             ((stringp state) ; file locked by USER
              (/icon/solo "nf-md-file_lock") ; 󰈡
              ;; (/icon/solo "nf-md-lock") ; 󰌾
              ;; "nf-md-source_branch_remove" ; 󱓌
              )

             ((eq state 'added)
              (/icon/solo "nf-md-file_plus") ; 󰝒
              ;; (/icon/solo "nf-md-source_branch_plus") ; 󱓊
              )

             ((eq state 'conflict)
              (/icon/solo "nf-md-file_alert") ; 󰩋
              ;; (/icon/solo "nf-fa-triangle_exclamation") ; 
              ;; "nf-md-source_branch_remove" ; 󱓌
              )

             ((eq state 'removed)
              (/icon/solo "nf-md-file_minus") ; 󱪡
              ;; (/icon/solo "nf-md-source_branch_minus") ; 󱓋
              )

             ((eq state 'missing)
              (/icon/solo "nf-md-file_hidden") ; 󰘓
              ;; (/icon/solo "nf-md-file_question") ; 󰡯
              ;; (/icon/solo "nf-md-call_missed") ; 󰃹
              ;; "nf-fa-question" ; 
              ;; ;; "main" vs "?main"
              ;; "?" ; normal text question mark
              ;; "nf-cod-question" ; 
              ;; "nf-md-source_branch_refresh" ; 󱓍
              ;; "nf-md-source_branch_minus" ; 󱓋
              )

             ((eq state 'ignored)
              (/icon/solo "nf-md-file_remove") ; 󰮘
              ;; (/icon/solo "nf-seti-ignored") ; 
              )

             ((eq state 'needs-merge)
              (/icon/solo "nf-md-file_swap") ; 󰾴
              ;; (/icon/solo "nf-oct-git_merge_queue") ; 
              ;; "nf-md-merge" ; 󰽜
              ;; "nf-cod-merge" ; 
              ;; "nf-md-source_merge" ; 󰘭
              )

             ((eq state 'edited)
              (/icon/solo "nf-md-file_edit") ; 󱇧
              ;; "nf-md-file_document_edit" ; 󰷈
              ;; "nf-cod-edit" ; 
              ;; "nf-fa-edit" ; 
              )

             (t
              (/icon/solo "nf-fa-question") ; 
              ;; "_"
              ;; (/icon/solo "nf-md-source_branch") ; 󰘬
              ))))))

(with-eval-after-load 'vc-git
  (define-advice vc-git-mode-line-string (:override (file) --/advice/vc/nerd-icons/git)
    "Use `nerd-icons' icons & change string format ordering.

copy/paste/edit of function `vc-git-mode-line-string' in
file '/usr/share/emacs/30.2/lisp/vc/vc-git.el.gz'"
    (pcase-let* ((backend-name "Git")
                 (state (vc-state file))
                 (`(,state-echo ,face ,indicator)
                  (vc-mode-line-state state))
                 (rev (vc-working-revision file 'Git))
                 (disp-rev (or (vc-git--symbolic-ref file)
                               (and rev (substring rev 0 7))))
                 (state-string (concat (unless (eq vc-display-status 'no-backend)
                                         (concat (/icon/solo "nf-md-git")
                                                 (/icon/solo "nf-md-source_branch")))
                                       disp-rev
                                       " "
                                       indicator)))
      (propertize state-string
                  'face face
                  'help-echo (concat state-echo " under the " backend-name
                                     " version control system"
                                     "\nCurrent revision: " rev)))))

(with-eval-after-load 'vc-hooks
  (setq mode-line-format
        ;; We moved `vc-mode' to `header-line-format'.
        (assq-delete-all 'vc-mode mode-line-format)))

(with-eval-after-load 'which-func
  (setq mode-line-misc-info
        ;; Remove the current function name from the mode line, because it's
        ;; mostly invisible here anyway.
        (assq-delete-all 'which-function-mode mode-line-misc-info)))

;; (format-mode-line mode-line-format)


;;------------------------------------------------------------------------------
;; Header Line
;;------------------------------------------------------------------------------

;; Enable to align start of header to where line numbers end.
;; NOTE: requires `header-line-format' to start with: "" header-line-indent
;;
;; TODO: This keeps getting turned off? Do I need to put it in a mode hook or
;; something?!
(header-line-indent-mode +1)

;; Move `which-function' info from mode-line to header-line.
;; Move `vc-mode' info from mode-line to header-line (right aligned).
(setq-default header-line-format
              '(;; Align start of header to where line numbers end if
                ;; `header-line-indent-mode' is enabled.
                "" header-line-indent

                ;; Left Side Info
                (which-func-mode ("" which-func-format " "))

                ;; If you want to separate aligment:
                ;; ;; Align to Right Side
                ;; (:eval
                ;;  (when (bound-and-true-p vc-mode)
                ;;    (propertize
                ;;     " "
                ;;     'display
                ;;     `(space :align-to (- right-fringe ,(1+ (length vc-mode))))))
                ;;  )

                ;; Align to Right Side & Right Side Info
                (:eval
                 (when (bound-and-true-p vc-mode)
                   (let* ((text (string-trim-left vc-mode)))
                     (concat
                      (propertize
                       " "
                       'display
                       ;; TODO: `string-width' or `string-pixel-width' instead of `length'?
                       ;; TODO: `right' vs `right-fringe'?
                       `(space :align-to (- right-fringe ,(length text))))
                      text))))))
;; (format-mode-line header-line-format)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide user (imp-path-relative 'user (imp-path-sans-extension (imp-path-current-file))))
