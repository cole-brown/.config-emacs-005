;;; core/modules/emacs/innit/os.el --- Operating System Stuff -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2023-08-22
;; Timestamp:  2023-08-22
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Operating System Stuff
;;
;;; Code:


(imp:require :path)


;;------------------------------------------------------------------------------
;; Constants
;;------------------------------------------------------------------------------

(defconst innit:os:types/valid '(:linux :mac :bsd :windows)
  "A reduced set of OSes compared to what `system-type' provides.")


(defconst innit:os:linux?   (eq system-type 'gnu/linux))
(defconst innit:os:mac?     (eq system-type 'darwin))
(defconst innit:os:windows? (memq system-type '(cygwin windows-nt ms-dos)))
(defconst innit:os:bsd?     (or innit:os:mac? (eq system-type 'berkeley-unix)))


;;--------------------------------------------------------------------------------
;; Functions
;;--------------------------------------------------------------------------------

(defun innit:os:type ()
  "Normalize `system-type' down to a keyword from `innit:os:types/valid' or nil."
  (cond (innit:os:linux?   :linux)
        (innit:os:mac?     :mac)
        (innit:os:bsd?     :bsd)
        (innit:os:windows? :windows)
        (t                 nil)))
;; (innit:os:type)


(defun innit:os:linux/distro (&optional error?)
  "Get the Linux distro's name from `lsb_release'.

Return lowercased string of `lsb_release -si' output.
Return nil if not Linux or if `lsb_release' is not installed.
If ERROR? is non-nil, signal an error instead of returning nil."
  (let ((func/name "innit:os:linux/distro"))
    ;; You're on Linux, right?
    (cond ((not innit:os:linux?)
           ;; Not even a linux; die with an error or nil.
           (when error?
             (nub:error
                 :innit
                 func/name
               "OS type must be `:linux'! Got: %S"
               innit:os:type)))

          ;; Do you have `lsb_release' installed?
          ((not (executable-find "lsb_release"))
           (when error?
             (nub:error
                 :innit
                 func/name
               "`lsb_release' executable not found; don't know how to figure out the Linux distro!")))

          (t
           (downcase
            (string-trim ; Lose the newline.
             ;; Returns "Ubuntu\n" if on, say, Ubuntu 20.04.6 LTS.
             (shell-command-to-string "lsb_release -si")))))))
;; (innit:os:linux/distro)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :innit 'os)
