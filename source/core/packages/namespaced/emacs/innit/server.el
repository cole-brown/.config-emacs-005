;;; core/modules/emacs/innit/server.el --- Daemons, Servers, and Demonic Waitresses -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-03-22
;; Timestamp:  2023-06-22
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
;; For daemony things, see:
;;   https://www.emacswiki.org/emacs/EmacsAsDaemon
;;
;; Flavors Theoretically* Possible:
;;   - Daemons
;;   - Servers
;;   - Server Daemons
;;   - Serverless Cloud Daemons
;;   - Waitress Daemons
;;
;; Flavors Currently Possible:
;;   - Servers?
;;
;;; Code:


(require 'server)

(imp:require :path)


;;------------------------------------------------------------------------------
;; Emacs Server
;;------------------------------------------------------------------------------

(defun innit:emacs/server:path ()
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
;; (innit:emacs/server:path)


;; `server-running-p' doesn't work.
;;   - http://emacshorrors.com/posts/determining-if-the-server-is-started-or-the-wonders-of-server-running-p.html
;;     -> http://lists.gnu.org/archive/html/bug-gnu-emacs/2018-06/msg00723.html
;; (unless (server-running-p) (server-start))
(defun innit:emacs/server:process? ()
  "Return non-nil if this Emacs has a server process running."
  (bound-and-true-p server-process))
;; (innit:emacs/server:process?)


(cl-defun innit:emacs/server:running? (&key (process t)
                                            (file-or-socket nil))
  "Return non-nil if this Emacs has a server running.

Two keyword params:
  - `:process'        - nil/non-nil (default t)
  - `:file-or-socket' - nil/non-nil (default nil)

If PROCESS is non-nil, this Emacs must have a `server-process'.
If FILE-OR-SOCKET is non-nil, this Emacs must have an existing file/socket."
  ;;------------------------------
  ;; Sanity Check
  ;;------------------------------
  ;; Have to check for _something_...
  (unless (or process file-or-socket)
    (error "innit:emacs/server:running?: PROCESS or FILE-OR-SOCKET must be non-nil! Got: `:process' = %S, `:file-or-socket' = %S"
           process
           file-or-socket))

  ;;------------------------------
  ;; Is your server running?
  ;;------------------------------
  ;; ...well you better go catch it!
  (and
   ;; Check for process?
   (or (not process)
       (innit:emacs/server:process?))
   ;; Check for file?
   (or (not file-or-socket)
       (innit:emacs/server:path))))
;; (innit:emacs/server:running?)
;; (innit:emacs/server:running? :process t)
;; (innit:emacs/server:running? :process nil)
;; (innit:emacs/server:running? :file-or-socket t)
;; (innit:emacs/server:running? :process nil :file-or-socket t)
;; (innit:emacs/server:running? :process 'yep :file-or-socket 'yep)


;;------------------------------------------------------------------------------
;; The End!
;;------------------------------------------------------------------------------
(imp:provide :innit 'server)
