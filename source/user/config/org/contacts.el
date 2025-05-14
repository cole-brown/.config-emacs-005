;;; mantle/config/org/contacts.el --- Configure `org-contacts' for Org-Mode -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2023-03-16
;; Timestamp:  2023-06-29
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Configure `org-contacts' for Org-Mode.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Org Contacts
;;------------------------------------------------------------------------------

;; TODO-org: Get org-contacts actually working like it never really was on vanilla?
;; (imp:use-package org-contacts
;;   ;; Use `org-contacts' from GNU ELPA, not MELPA.
;;   :pin gnu
;;   :after org
;;   :disabled   ; is this making things slow?
;;   :ensure nil ; auto-install?
;;   :demand t
;;
;;   :custom
;;   (org-contacts-files (path:abs:file
;;                        (spy:dirky/path :secrets :secrets/org)
;;                        "contacts.org"))
;;
;;   :config
;;   ;; https://github.com/tkf/org-mode/blob/master/contrib/lisp/org-contacts.el
;;   (add-to-list 'org-capture-templates
;;                `("c" "Contacts" entry
;;                   (file+headline org-contacts-files
;;                                  "INBOX")
;;                   ,(concat "* %(org-contacts-template-name)\n"
;;                            ":PROPERTIES:\n"
;;                            ":EMAIL: %(org-contacts-template-email)\n"
;;                            ":END:\n"))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'org 'contacts)
