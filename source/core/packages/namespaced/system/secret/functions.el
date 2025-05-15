;;; core/modules/system/secret/functions.el --- Functions -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2020-10-28
;; Timestamp:  2023-06-26
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Functions
;;
;;; Code:


(imp:require :nub)
(imp:require :path)
(imp:require :system 'multiplexer)


;;------------------------------------------------------------------------------
;; Getters
;;------------------------------------------------------------------------------

;;------------------------------
;; Identity
;;------------------------------

(defun system:secret:hash ()
  "Get this system's secret hash, which is the same as the system's hash."
  (system:multiplexer:hash))
;; (system:secret:hash)
;; (string= (system:secret:hash) (system:multiplexer:hash))


(defun system:secret:id ()
  "Get this system's secrets ID.

Returns nil if no secrets ID for this system."
  (system:multiplexer:get :hash 'this
                          :key  'id))
;; (system:secret:id)


;;------------------------------
;; Paths
;;------------------------------

(defun system:secret:path/root ()
  "Get secrets' root dir for all systems."
  (system:multiplexer:get :hash 'this
                          :key '(path secret root)))


(defun system:secret:path/load ()
  "Get secrets' Emacs Lisp load dir for all systems."
  (system:multiplexer:get :hash 'this
                          :key '(path secret emacs)))


(defun system:secret:path/system ()
  "Get this specific system's secrets' load dir."
  (system:multiplexer:get :hash 'this
                          :key '(path secret system)))


;;------------------------------------------------------------------------------
;; Validation
;;------------------------------------------------------------------------------

(defun system:secret:validate (path/type filepath)
  "Validate that secrets exist for this system.

PATH/TYPE should be one of:
  - `:root'   - FILEPATH is relative to secret directory.
  - `:load'   - FILEPATH is relative to root init/load secret directory.
  - `:system' - FILEPATH is relative to system's secret directory.

FILEPATH should not include file extension. \".el\" will be appended!

Validate that:
  - system ID & hash exist
  - secret root directory exists
  - system's secret directory exists
  - FILEPATH exists"
  (let* ((success t)
         reason
         (hash           (system:secret:hash))
         (id             (system:secret:id))
         (path/root      (system:secret:path/root))
         (path/load      (system:secret:path/load))
         (path/system    (system:secret:path/system))
         path/file/load
         path/file/name)

    ;;------------------------------
    ;; Determine file paths.
    ;;------------------------------
    (cond ((eq path/type :root)
           (setq path/file/load (path:join path/root filepath)))
          ((eq path/type :load)
           (setq path/file/load (path:join path/load filepath)))
          ((eq path/type :system)
           (setq path/file/load (path:join path/system filepath)))
          ;; Unknown PATH/TYPE.
          (t
           (setq success nil
                 reason (format "Unknown PATH/TYPE `%S'; expected one of: %S"
                                path/type
                                '(:root :load :system)))))

    ;;------------------------------
    ;; Check for validity.
    ;;------------------------------
    ;; If we already failed, don't bother with more.
    (when success
      ;; Valid `path/type'; fill in `path/file/name' by adding file extension.
      (setq path/file/name (concat path/file/load ".el"))

      ;; Validity checks.
      (cond
       ;;---
       ;; Hash, ID, basic stuff.
       ;;---
       ((null hash)
        (setq success nil
              reason (format "Secrets hash is null for this system.")))

       ((null id)
        (setq success nil
              reason (format "Secrets ID is null for this system.")))

       ;;---
       ;; Paths
       ;;---
       ((or (null path/root)
            (not (stringp path/root)))
        (setq success nil
              reason (format "Secrets %s for this system (%s) is invalid: %s"
                             "root directory"
                             id
                             path/root)))

       ((or (null path/load)
            (not (stringp path/load)))
        (setq success nil
              reason (format "Secrets %s for this system (%s) is invalid: %s"
                             "load directory"
                             id
                             path/load)))

       ((or (null path/system)
            (not (stringp path/system)))
        (setq success nil
              reason (format "Secrets %s for this system (%s) is invalid: %s"
                             "system directory"
                             id
                             path/system)))

       ((not (file-directory-p path/root))
        (setq success nil
              reason (format "Secrets %s for this system (%s) does not exist: %s"
                             "root directory"
                             id
                             path/root)))

       ((not (file-directory-p path/load))
        (setq success nil
              reason (format "Secrets %s for this system (%s) does not exist: %s"
                             "load directory"
                             id
                             path/load)))

      ((not (file-directory-p path/system))
        (setq success nil
              reason (format "Secrets %s for this system (%s) does not exist: %s"
                             "system directory"
                             id
                             path/system)))

       ;;---
       ;; File
       ;;---
       ;; What about the filepath?
       ;; Add ".el" for actual file check.
       ((not (file-exists-p path/file/name))
        (setq success nil
              reason (format "Secrets %s for this system (%s) does not exist: %s"
                             "file"
                             id
                             path/file/name)))

       ;; File exists; return the load path...
       (t
        (setq success t
              reason nil))))

    ;;------------------------------
    ;; Return plist of vars and success/reason.
    ;;------------------------------
    (list :success         success
          :reason          reason
          :hash            hash
          :id              id
          :path/dir/root   path/root
          :path/dir/load   path/load
          :path/dir/system path/system
          :path/file/load  path/file/load
          :path/file/name  path/file/name)))
;; (system:secret:validate :load "init")


;;------------------------------------------------------------------------------
;; Helpers
;;------------------------------------------------------------------------------

(defun system:secret:has ()
  "Return non-nil if this system has secrets.

The system is considered to have secrets if:
  - It has a hash.
  - It has an ID.
  - And it has a secrets 'init.el' file."
  (condition-case _
      (not (null (plist-get (system:secret:validate :load "init") :success)))
    (error nil)))
;; (system:secret:has)


(defmacro system:secret:if (caller messages &rest body)
  "Do BODY if this system has secrets.

Used to guard config blocks that depend on secrets.

CALLER and MESSAGES are used only for printing 'nub' message/warning.
  - MESSAGES should be nil, or a plist with `:skip' and/or `:eval' keys.
    - Values should be either a string or a list of format string & args.
    - Messages will be prefixed with \"[SKIP] \" or \"[EVAL] \"

If skipping (and a `:skip' message is present in MESSAGES), the `:skip' MESSAGE
and CALLER are formatted into a 'nub' `:warning' level message for info about
skipped config.

If not skipping (and an `:eval' message is present in MESSAGES), the `:eval'
MESSAGE and CALLER are formatted into a 'nub' `:warning' level for info about
skipped config."
  (declare (indent 2))
  ;; Only eval inputs once.
  `(let ((macro<system/secret>:caller (or ,caller
                                          (nub:format:callers "system:secret:if" "unlabeled-caller")))
         (macro<system/secret>:messages ,messages))
     (if (system:secret:has)
         ;;------------------------------
         ;; Evaluate
         ;;------------------------------
         (progn
           ;; Only display the `:eval' MESSAGE if it exists.
           (let ((macro<system/secret>:message (plist-get macro<system/secret>:messages :eval)))
             ;; (message "eval msg: %S" macro<system/secret>:message)
             (cond ((and (stringp macro<system/secret>:message)
                         (string= "" macro<system/secret>:message))
                    ;; Just an empty string for a "message" - don't display.
                    nil)

                   ((stringp macro<system/secret>:message)
                    (nub:warning
                        :system/secret
                        caller
                      macro<system/secret>:message))

                   ((listp macro<system/secret>:message)
                    (nub:warning
                        :system/secret
                        caller
                      (apply #'format macro<system/secret>:message)))

                   (t
                    nil)))

           ;; Annnd... Run the BODY forms.
           ,@body)

       ;;------------------------------
       ;; Skip
       ;;------------------------------
       (let ((macro<system/secret>:message (plist-get macro<system/secret>:messages :skip)))
         ;; (message "skip msg: %S" macro<system/secret>:message)
         ;; Always display a warning; use a default message if none supplied.
         (nub:warning
             :system/secret
             caller
           (concat "[SKIP]: %s")
           (cond ((listp macro<system/secret>:message)
                  (apply #'format macro<system/secret>:message))
                 ((stringp macro<system/secret>:message)
                  macro<system/secret>:message)
                 (t
                  "Skipping...")))))))
;; (system:secret:if "testing" "skip string" (message "I should not see this."))
;; (system:secret:if nil "skip string" (message "I should not see this."))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :system 'secret 'functions)
