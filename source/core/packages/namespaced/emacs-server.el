;;; namespaced/emacs-server.el --- Daemons, Servers, and Demonic Waitresses -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-03-22
;; Timestamp:  2025-11-11
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; `server-running-p' doesn't work.
;;   - http://emacshorrors.com/posts/determining-if-the-server-is-started-or-the-wonders-of-server-running-p.html
;;     -> http://lists.gnu.org/archive/html/bug-gnu-emacs/2018-06/msg00723.html
;; So this is actually not useful:
;;   (unless (server-running-p) (server-start))
;;
;; These are some functions for figuring out if Emacs Server is running, so you
;; can actually do things like that smells like it should be doing. Then maybe
;; Emacs will collect itself into one instance, and then maybe drag-drop, send
;; from Visual Studio, etc will work as expected.
;;
;; For daemonic things, see:
;;   https://www.emacswiki.org/emacs/EmacsAsDaemon
;;
;;; Code:


(require 'server)

(imp-require path)


;;------------------------------------------------------------------------------
;; Variables
;;------------------------------------------------------------------------------

(defvaralias 'emacs-server:dir:socket 'server-socket-dir)
(defvaralias 'emacs-server:dir:auth   'server-auth-dir)
(defvaralias 'emacs-server:name       'server-name)
(defvaralias 'emacs-server:process    'server-process)


;;------------------------------------------------------------------------------
;; Functions
;;------------------------------------------------------------------------------

(defun emacs-server:path ()
  "Return absolute path to server file/socket if one exists, else return nil."
  ;; I can't see any useful `server-*' vars/funcs that will actually just tell
  ;; me which one (if any) is _in use_... So just check both.
  (let ((path/socket (path:join server-socket-dir server-name))
        (path/auth   (path:join server-auth-dir server-name)))
    ;; NOTE: Can be a socket, so don't use a TYPE param (e.g. `:file').
    (cond ((path:exists? path/socket)
           path/socket)
          ((path:exists? path/auth)
           path/auth)
          (t
           nil))))
;; (emacs-server:path)


;; `server-running-p' doesn't work.
;;   - http://emacshorrors.com/posts/determining-if-the-server-is-started-or-the-wonders-of-server-running-p.html
;;     -> http://lists.gnu.org/archive/html/bug-gnu-emacs/2018-06/msg00723.html
;; (unless (server-running-p) (server-start))
(defun emacs-server:process? ()
  "Return non-nil if this Emacs has a server process running."
  (bound-and-true-p server-process))
;; (emacs-server:process?)


(defun emacs-server:running? ()
  "Return non-nil if this Emacs has a server running.

Check for either a process (`server-process') or a socket/file."
  ;; Is your server running?
  ;; ...well you better go catch it!
  (and
   (emacs-server:process?)
   (emacs-server:path)))
;; (emacs-server:running?)


;;------------------------------------------------------------------------------
;; The End!
;;------------------------------------------------------------------------------
(imp-provide emacs-server)
