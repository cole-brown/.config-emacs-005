;;; modules/mode/org/link.el --- Org-Mode Link Stuff -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2023-06-29
;; Timestamp:  2023-06-29
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Org-Mode Link Stuff
;;   - Create a link named "here" from the org-mode links.
;;   - Create a link named "here" form the kill ring.
;;
;;; Code:


(imp-require :buffer 'region)


;;------------------------------------------------------------------------------
;; Commands
;;------------------------------------------------------------------------------

;; Easy paste of e.g. URLs.
(defun --/org/cmd/here/yank (start end)
  "Insert item from kill ring as an org-mode link with description 'here'."
  (interactive "r")
  ;; (let ((text "here")
  ;;       (link (current-kill 0))) ; Like `yank' but returns a string instead of inserting.
  ;;   (if (buffer:region:active?)
  ;;       (progn
  ;;         (message "Not active")
  ;;         (setq text (or (buffer:region:get :properties? nil)
  ;;                        text))
  ;;         (replace-region-contents (buffer:region:min start end)
  ;;                                  (buffer:region:max start end)
  ;;                                  (lambda () (org-link-make-string link text))))
  ;;     (insert (org-link-make-string link text))))

  ;; TODO: Does this work well all the time? If so, delete above.
  (org-insert-link nil
                   (current-kill 0) ; Like `yank' but returns a string instead of inserting.
                   (or (buffer:region:get :properties? nil)
                       "here")))


(defun --/org/cmd/here/link (start end)
  "Insert stored org link as an org-mode link with description 'here'."
  (interactive "r")
  (org-insert-link nil
                   (car (car org-stored-links))
                   (or (buffer:region:get :properties? nil)
                       "here")))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
;; TODO: what to name this?
;; :emacs.d source mode org link?
(imp-provide :mode 'org 'link)
