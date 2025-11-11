;;; namespaced/system/os.el --- Operating System Stuff -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2023-08-22
;; Timestamp:  2025-11-10
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

(imp-require path)


;;------------------------------------------------------------------------------
;; Constants & Variables
;;------------------------------------------------------------------------------

(defconst system:os:linux? (eq system-type 'gnu/linux)
  "Is this a Linux system according to `system-type'?")

(defconst system:os:mac? (eq system-type 'darwin)
  "Is this an Apple macOS system according to `system-type'?")

(defconst system:os:windows? (eq system-type 'windows-nt)
  "Is this a Windows system according to `system-type'?")

(defconst system:os:windows/cygwin? (eq system-type 'cygwin)
  "Is this a Cygwin inux system according to `system-type'?")

(defconst system:os:type system-type
  "See `system-type' docstr for details.")


;;--------------------------------------------------------------------------------
;; Functions
;;--------------------------------------------------------------------------------

(defun system:os:linux/distro (&optional error?)
  "Get the Linux distro's name from `lsb_release'.

Return lowercased string of `lsb_release -si' output.
Return nil if not Linux or if `lsb_release' is not installed.
If ERROR? is non-nil, signal an error instead of returning nil."
  (let ((funcname 'system:os:linux/distro))
    ;; You're on Linux, right?
    (cond ((not system:os:linux?)
           ;; Not even a linux; die with an error or nil.
           (when error?
             (error "%S: OS type must be `:linux'! Got: %S"
                    funcname
                    system:os:type)))

          ;; Do you have an `/etc/os-release' file?
          ((file-readable-p "/etc/os-release")
           (with-temp-buffer
             (insert-file-contents "/etc/os-release")
             ;; Find the "ID=" line
             (when (re-search-forward "^ID=\\(.*\\)$" nil t)
               (downcase (match-string 1)))))

          ;; Do you have `lsb_release' installed?
          ((executable-find "lsb_release")
           (downcase
            (string-trim ; Lose the newline.
             ;; Returns "Ubuntu\n" if on Ubuntu 20.04.6 LTS
             ;;         "Pop\n"    if on Pop_OS! 22.04
             (shell-command-to-string "lsb_release -si"))))

          (t
           (when error?
             (error "%S: Don't know how to figure out the Linux distro! %S"
                    funcname
                    "1. `/etc/os-release' file not found/readable"
                    "2. `lsb_release' executable not found"))))))
;; (system:os:linux/distro)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide system os)
