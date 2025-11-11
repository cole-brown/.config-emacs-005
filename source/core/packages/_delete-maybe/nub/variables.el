;;; core/modules/output/nub/variables.el --- Variables, Setters, Init -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-12-01
;; Timestamp:  2023-08-16
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;                                 ──────────
;; ╔════════════════════════════════════════════════════════════════════════╗
;; ║                               Variables                                ║
;; ╚════════════════════════════════════════════════════════════════════════╝
;;                                   ──────
;;                 Things change; variables stay the same...
;;                                 ──────────
;;
;; Internal: variables, getters, setters, etc.
;;
;; API: Variable Init & Friends!
;;   - `nub:vars:init'
;;   - `nub:vars:reset'
;;   - `nub:vars:terminate'
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Section Title
;;------------------------------------------------------------------------------

;; TODO: stuff here


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
;; TODO: Choose one!
(imp:provide :nub 'variables.el)
(imp:provide:with-emacs :nub 'variables.el)
(provide 'variables.el)
;;; output/nub/variables.el -*- lexical-binding: t; -*-



(imp:require :nub 'internal)
(imp:require :nub 'alist)


;;------------------------------------------------------------------------------
;; Constants
;;------------------------------------------------------------------------------

(defconst int<nub>:var:user:fallback :default
  "Nub's default/fallback user.

 Used when actual user not found so that output can still happen.")


(defconst nub:output:levels  '(:error :warning :info :debug)
  "All of nub's output levels.")


(defun int<nub>:level:exists? (caller level &optional error?)
  "Return non-nil if LEVEL is a valid `nub' output level.

CALLER (string) should be the calling function's name or calling file's path.

If ERROR? is non-nil, signals an error if things are not ok.
Else returns nil if things are not ok."
  (if (memq level nub:output:levels)
      level

    (if error?
        (int<nub>:error caller
                        "Level %S is not a valid `nub' output level!"
                        level)
      nil)))


;;------------------------------------------------------------------------------
;; Users
;;------------------------------------------------------------------------------

(defvar int<nub>:var:users nil
  "List of all users.")


(defun int<nub>:user:exists? (caller user &optional error?)
  "Return non-nil if USER is a registered `nub' user.

CALLER (string) should be the calling function's name or calling file's path.

USER should be the nub user keyword."
  (if (or (eq user int<nub>:var:user:fallback)
          (memq user int<nub>:var:users))
      t

    (if error?
        (int<nub>:error caller
                        "User `%S' is not a register `nub' user! Registered: %S"
                        user
                        int<nub>:var:users)
      nil)))


(defun int<nub>:init:user (user)
  "Add USER as a registered `nub' user."
  (unless (keywordp user)
    (int<nub>:error "int<nub>:init:user"
                    "USER should be a keyword! Got '%S': '%S'"
                    (type-of user)
                    user))

  (push user int<nub>:var:users))


(defun int<nub>:terminate:user (user)
  "Add USER as a registered `nub' user."
  (setq int<nub>:var:users (remove user int<nub>:var:users)))


;;--------------------------------------------------------------------------------
;; Paths
;;--------------------------------------------------------------------------------

(defvar int<nub>:var:path
  (list (cons int<nub>:var:user:fallback user-emacs-directory))
  "Alist of `nub' user to root path for the user.")


(defun int<nub>:var:path (user &optional no-fallback?)
  "Get USER's root path.

Return value from `int<nub>:var:user:fallback' user if USER's root path is not
found. If NO-FALLBACK? is non-nil, return `:does-not-exist' when a path is not
found."
  (let ((func/name "int<nub>:var:path")
        value)
    ;; Assert USER.
    (int<nub>:user:exists? func/name user :error)

    ;; Try to get the path for USER...
    (setq value (int<nub>:alist:get/value user
                                          int<nub>:var:path
                                          :does-not-exist))

    ;; Do we need to get the fallback/default value?
    (when (and (null no-fallback?)
               (eq value :does-not-exist))
      (setq value (int<nub>:alist:get/value int<nub>:var:user:fallback
                                            int<nub>:var:path
                                            :does-not-exist))
      ;; If it's still not found then we should error, probably?
      (when (eq value :does-not-exist)
        (int<nub>:error func/name
                        "User `%S' has no root path and neither does fallback/default user `%S'?!"
                        user
                        int<nub>:var:user:fallback)))

    ;; Done; return the value.
    value))
;; (int<nub>:var:path :default)
;; (int<nub>:var:path :secret)
;; (int<nub>:var:path :dne :no-fallback)


(defun int<nub>:init:path (user path:root)
  "Add PATH:ROOT as USER's root path.

PATH:ROOT should be one of:
  - string - An absolute path string to an existing directory.
  - nil    - \"Figure out what directory we're in now and use that, please.\"

Used mainly for creating relative paths to files if no CALLER param is supplied."
  ;;------------------------------
  ;; Default Path?
  ;;------------------------------
  (when (null path:root)
    (setq path:root (int<nub>:path:current:dir)))

  ;;------------------------------
  ;; Error Checking
  ;;------------------------------
  (unless (stringp path:root)
    (int<nub>:error "int<nub>:init:path"
                    "PATH:ROOT should be a string! Got '%S': '%S'"
                    (type-of path:root)
                    path:root))
  (unless (file-name-absolute-p path:root)
    (int<nub>:error "int<nub>:init:path:root"
                    "PATH:ROOT should be an absolute path! Got: '%S'"
                    path:root))
  (unless (file-directory-p path:root)
    (int<nub>:error "int<nub>:init:path:root"
                    "PATH:ROOT should be an existing directory! Doesn't exist or not a directory: '%S'"
                    path:root))

  ;;------------------------------
  ;; Save PATH:ROOT
  ;;------------------------------
  (int<nub>:alist:update user
                         ;; Canonicalize to abbreviated absolute directory path ("~/foo/bar/baz.qux/").
                         (abbreviate-file-name (file-name-as-directory path:root))
                         int<nub>:var:path))
;; (int<nub>:init:path :test (path:current:dir))
;; (int<nub>:init:path :test nil)
;; int<nub>:var:path


;;------------------------------------------------------------------------------
;; Get from Alist for User at Level
;;------------------------------------------------------------------------------

(defun int<nub>:var:assert-user-level (caller user level error?)
  "Assert that USER is registered and LEVEL is a valid `nub' output level.

CALLER (string) should be the calling function's name or calling file's path.

If ERROR? is non-nil, signals an error if things are not ok.
Else returns nil if things are not ok."
  (and (int<nub>:user:exists?  caller user  error?)
       (int<nub>:level:exists? caller level error?)))


(defun int<nub>:var:user-at-level (user level alist &optional no-fallback?)
  "Get value from ALIST for USER at output LEVEL.

Return value from `int<nub>:var:user:fallback' user if USER is not found.
  - If NO-FALLBACK? is non-nil, just return `:does-not-exist' when USER not found."
  (let ((func/name "int<nub>:var:user-at-level")
        value)
    ;; Ensure USER and LEVEL are ok.
    (int<nub>:var:assert-user-level func/name user level :error)

    ;; Assert ALIST.
    (if (not (int<nub>:alist:alist? alist))
        (int<nub>:error func/name
                        "ALIST must be an alist! Got: %S"
                        alist)

      ;; Try to get the value for USER at LEVEL - we'll fall back if they've requested it.
      (setq value (int<nub>:alist:get/value level
                                            (int<nub>:alist:get/value user alist)
                                            :does-not-exist))

      ;; Do we need to get the fallback/default value?
      (when (and (null no-fallback?)
                 (eq value :does-not-exist))
        (setq value (int<nub>:alist:get/value level
                                              (int<nub>:alist:get/value int<nub>:var:user:fallback alist)
                                              :does-not-exist))
        ;; If it's still not found then we should error on the level, probably?
        (when (eq value :does-not-exist)
          (int<nub>:error func/name
                          "User %S at level %S had no value and neither does fallback/default user %S. Invalid level?"
                          user level int<nub>:var:user:fallback)))

      ;; Done; return the value.
      value)))
;; (int<nub>:var:user-at-level :jeff :dne nil :no-fallback)
;; (int<nub>:var:user-at-level :jeff :dne nil :no-fallback)


;;------------------------------------------------------------------------------
;; Verbosity Settings
;;------------------------------------------------------------------------------
;; This allows our tests to take over output easier.
;;------------------------------

;;------------------------------
;; Output Message Prefixes
;;------------------------------

(defvar int<nub>:var:prefix:backup
  (list (cons int<nub>:var:user:fallback '((:error   . "[ERROR   ]: ")
                                           (:warning . "[WARNING ]: ")
                                           (:info    . "[INFO    ]: ")
                                           ;; Noticibly different so when debugging any error/warning messages stand out if all sent to the same buffer?
                                           (:debug   . "[   debug]: "))))
  "Prefix strings for the verbosity levels.

alist of USER keyword
  -> alist of verbosity level
     -> prefix strings for output messages")


(defvar int<nub>:var:prefix
  (int<nub>:alist:copy/shallow int<nub>:var:prefix:backup)
  "Prefix strings for the verbosity levels.

alist of USER keyword
  -> alist of verbosity level
     -> prefix strings for output messages")


(defun int<nub>:var:prefix (user level)
  "Get message prefix for USER at output LEVEL."
  ;; Ensure USER and LEVEL are ok.
  (int<nub>:var:assert-user-level "int<nub>:var:prefix" user level :error)

  (int<nub>:var:user-at-level user level int<nub>:var:prefix))
;; (int<nub>:var:prefix :fallback :debug)


(defun int<nub>:init:prefix (user alist)
  "Set output message prefix string ALIST for USER.

ALIST should have all output levels in it.

Sets both current and backup values (backups generally only used for tests)."
  ;; Ensure USER is ok.
  (int<nub>:user:exists? "int<nub>:init:prefix" user :error)

  ;; Set in the backup variable.
  ;; Use a copy so it doesn't get changed out from under us.
  (let ((alist/copy (int<nub>:alist:copy/shallow alist)))
    (int<nub>:alist:update
     user
     alist/copy
     int<nub>:var:prefix:backup))

  ;; Set in the actual variable.
  (int<nub>:alist:update
   user
   alist
   int<nub>:var:prefix))
;; (int<nub>:init:prefix :test '((:error . "erm... uh-oh:")))


;;------------------------------
;; Verbosity Enabled? (Per-Level)
;;------------------------------

(defvar int<nub>:var:enabled?:backup
  (list (cons int<nub>:var:user:fallback (list (cons :error   t)
                                               (cons :warning t)
                                               (cons :info    t)
                                               ;; Init debug to env/Emacs vars.
                                               (cons :debug   (int<nub>:debug:init)))))
  "Alist of USER keyword to verbosity of various log levels for the user.

Valid values:
  non-nil     - output level enabled
  nil         - output level disabled
  <predicate> - Call <predicate> (w/ level keyword) to get nil/non-nil value.

Predicate's signature should be:
  (defun enabled-predicate-for-my-user (level) ...)
    - It should return nil/non-nil.")


(defvar int<nub>:var:enabled?
  (int<nub>:alist:copy/shallow int<nub>:var:enabled?:backup)
  "Default alist of USER keyword to verbosity of various log levels for the user.

Valid values:
  t   - output level enabled
  nil - output level disabled
  <predicate> - Call <predicate> (w/ level keyword) to get nil/non-nil value.")


;; TODO: verify function for `int<nub>:var:enabled?'?


(defun int<nub>:init:enabled? (user alist)
  "Set 'enabled?' ALIST for USER.

ALIST should have all output levels in it.

Sets both current and backup values (backups generally only used for tests)."
  ;; Ensure USER is ok.
  (int<nub>:user:exists? "int<nub>:init:enabled?" user :error)

  ;; Set in the backup variable.
  ;; Use a copy so it doesn't get changed out from under us.
  (let ((alist/copy (int<nub>:alist:copy/shallow alist)))
    (int<nub>:alist:update
     user
     alist/copy
     int<nub>:var:enabled?:backup))

  ;; Set in the actual variable.
  (int<nub>:alist:update
   user
   alist
   int<nub>:var:enabled?))


(defun int<nub>:var:enabled? (user level &optional no-fallback?)
  "Return non-nil if output is enabled for USER at output LEVEL.

If USER at LEVEL is not found, return nil/non-nil for fallback/default user at
LEVEL.
  - If NO-FALLBACK? is non-nil, skip the fallback/default user check."
  ;; Ensure USER and LEVEL are ok.
  (int<nub>:var:assert-user-level "int<nub>:var:enabled?" user level :error)

  (let ((enabled? (int<nub>:var:user-at-level user
                                              level
                                              int<nub>:var:enabled?
                                              no-fallback?)))

    ;; Found a function - call w/ level to get enabled value.
    (cond ((functionp enabled?)
           (funcall enabled? level))

          ;; Default: Just have something to return.
          (t
           enabled?))))
;; (int<nub>:var:enabled? :default :error)
;; (int<nub>:var:enabled? :test<nub/utils>::int<nub>:var:enabled? :info)


(defun int<nub>:var:enabled?:set (user level value)
  "Set enabled flag for USER at output LEVEL (or togggle the flag).

If VALUE is `:toggle', this will toggle the flag. Otherwise it will set/unset
the flag based on truthiness of VALUE."
  ;; Ensure USER and LEVEL are ok.
  (int<nub>:var:assert-user-level "int<nub>:var:enabled?:set" user level :error)

  (let* ((enabled?/value (cond ((eq :toggle value)
                                (not (int<nub>:var:enabled? user level)))
                               (value
                                t)
                               (t
                                nil)))
         (alist/user (int<nub>:alist:get/value user int<nub>:var:enabled?))
         (alist/updated (int<nub>:alist:update level
                                               enabled?/value
                                               alist/user)))
    (int<nub>:alist:update user
                           alist/updated
                           int<nub>:var:enabled?)))
;; (nub:vars:init :test)
;; (int<nub>:var:enabled? :test :debug)
;; (int<nub>:var:enabled? :test :debug :no-fallback)
;; (int<nub>:var:enabled?:set :test :debug 'foo)
;; (int<nub>:var:enabled?:set :test :debug :toggle)
;; (int<nub>:var:enabled?:set :test :warning 'bar)


;;------------------------------
;; Message Sinks (Per-Level)
;;------------------------------

(defvar int<nub>:var:sink:backup
  (list (cons int<nub>:var:user:fallback '((:error   . error)
                                           (:warning . warn)
                                           (:info    . message)
                                           (:debug   . message))))
  "Alist of USER keyword to an alist of output level keyword to sink function.")


(defvar int<nub>:var:sink
  (int<nub>:alist:copy/shallow int<nub>:var:sink:backup)
  "Default alist of user keyword to an alist of output level keyword to sink value.

Valid sinks:
  `int<nub>:var:user:fallback' (aka `:default')
  t (alias for `int<nub>:var:user:fallback' / `:default')
    - output normally (use default/fallback output sink)
  nil
    - do not output
  <function>
    - Use <function> to output instead.
    - Used by unit testing.
  <list>
    - Use all sinks in the list of sinks.
    - Valid list members are the values above.")


(defvar int<nub>:var:inhibit-message
  (list (cons int<nub>:var:user:fallback '((:error   . nil)
                                           (:warning . nil)
                                           (:info    . nil)
                                           (:debug   . t))))
  "`inhibit-message' setting per user per output level.

Alist of USER keyword to an alist of output level keyword to `inhibit-message'
value.

Setting `inhibit-message' vaule to non-nil will prevent `message' from
displaying to the minibuffer, if `message' happens to be the sink.")


(defun int<nub>:var:sink:verify (caller sink &optional error? context list-invalid?)
  "Return non-nil if SINK is valid.

CALLER should be calling function's name.

If ERROR? is non-nil, signals an error if SINK is invalid.
Else, returns nil if SINK is invalid.

If CONTEXT is non-nil, it will be added to error messages.

If LIST-INVALID? is non-nil, a list is not valid. Used for recursion."
  ;;------------------------------
  ;; Always valid: `int<nub>:var:user:fallback', `t', `nil', functions.
  ;;------------------------------
  (cond ((eq sink int<nub>:var:user:fallback))

        ;; Alias for `int<nub>:var:user:fallback'.
        ((eq sink t))

        ;; Must check for `nil' before any checks for lists since `nil' is a list
        ;; but we do not want to treat `nil' as a list for validity's sake.
        ((eq sink nil))

        ((functionp sink))

        ;;------------------------------
        ;; Maybe valid, maybe not: list
        ;;------------------------------
        ;; Always invalid if LIST-INVALID? is non-nil.
        ((and (listp sink)
              list-invalid?)
         (if error?
             (int<nub>:error caller
                             "Sink cannot be a list. %sGot: %S"
                             (if context
                                 (format "Context: %s " context)
                               "")
                             sink)
           nil))

        ;; LIST-INVALID? is nil once we get here, so check each member.
        ((listp sink)
         (let ((valid t))
           ;; Check each list member for validity of entire list as a sink.
           (dolist (entry sink)
             (setq valid (and valid
                              (int<nub>:var:sink:verify caller
                                                        entry
                                                        error?
                                                        context
                                                        ;; List is only valid at top level.
                                                        t))))
           ;; Return whether valid or not.
           valid))

        ;;------------------------------
        ;; Always an error: Everything else.
        ;;------------------------------
        (t
         (if error?
             (int<nub>:error caller
                             (if list-invalid?
                                 "Sink must be `t', `nil', or a function. %sGot: %S"
                               "Sink must be a list, `t', `nil', or a function. %sGot: %S")
                             (if context
                                 (format "Context: %s " context)
                               "")
                             sink)
           nil))))
;; Valid:
;;   (int<nub>:var:sink:verify "test" t :error)
;;   (int<nub>:var:sink:verify "test" nil :error)
;;   (int<nub>:var:sink:verify "test" #'ignore :error)
;;   (int<nub>:var:sink:verify "test" '(t nil ignore) :error)
;; Invalid:
;;   (int<nub>:var:sink:verify "test" "hello" :error)
;;   (int<nub>:var:sink:verify "test" '("hello") :error)
;;   (int<nub>:var:sink:verify "test" '(t nil (t nil)) :error "Invalid test for list-in-list.")


(defun int<nub>:init:sink (user sink-alist &optional inhibit-message-alist)
  "Set SINK-ALIST for USER, and optionally INHIBIT-MESSAGE-ALIST.

SINK-ALIST and INHIBIT-MESSAGE-ALIST should have all output levels in it.

Set both current and backup values (backups generally only used for tests)."
  ;; Ensure USER is ok.
  (int<nub>:user:exists? "int<nub>:init:sink" user :error)

  ;;------------------------------
  ;; Set SINK-ALIST.
  ;;------------------------------
  ;; Ensure the sinks are ok.
  (if (int<nub>:alist:alist? sink-alist)
      (dolist (entry sink-alist)
        (int<nub>:var:sink:verify "int<nub>:init:sink"
                                  (cdr entry)
                                  :error
                                  ;; Full entry for context in case of error.
                                  entry))
    (int<nub>:error "int<nub>:init:sink"
                    "SINK-ALIST must be an alist. Got: %S"
                    sink-alist))

  ;; Set in the backup variable.
  ;; Use a copy so it doesn't get changed out from under us.
  (let ((sink-alist/copy (int<nub>:alist:copy/shallow sink-alist)))
    (int<nub>:alist:update
     user
     sink-alist/copy
     int<nub>:var:sink:backup))

  ;; Set in the actual variable.
  (int<nub>:alist:update
   user
   sink-alist
   int<nub>:var:sink)

  ;;------------------------------
  ;; Set INHIBIT-MESSAGE-ALIST.
  ;;------------------------------
  (when inhibit-message-alist
    ;; No backup; just set in the actual variable.
    (int<nub>:alist:update
     user
     inhibit-message-alist
     int<nub>:var:inhibit-message)))
;; (int<nub>:init:sink :test '((:error . message)) '((:message . t)))


(defun int<nub>:var:sink (user level &optional no-fallback?)
  "Return output sink for USER at output LEVEL.

If USER at LEVEL is not found, return value for fallback/default user at LEVEL.
  - If NO-FALLBACK? is non-nil, skip the fallback/default user check."
  ;; Ensure USER and LEVEL are ok.
  (int<nub>:var:assert-user-level "int<nub>:var:sink" user level :error)

  (int<nub>:var:user-at-level user
                              level
                              int<nub>:var:sink
                              no-fallback?))
;; (int<nub>:var:sink :test :error)
;; (int<nub>:var:sink :test<nub/debug>::nub:debug :debug)


(defun int<nub>:var:sink:set (user level sink)
  "Set the SINK for USER at output LEVEL."
  ;; Ensure USER and LEVEL are ok.
  (int<nub>:var:assert-user-level "int<nub>:var:sink:set" user level :error)
  ;; Ensure the sink is ok.
  (int<nub>:var:sink:verify "int<nub>:init:sink"
                            (cdr entry)
                            :error)

  (let* ((alist/user (int<nub>:alist:get/sink user int<nub>:var:sink))
         (alist/updated (int<nub>:alist:update level
                                               sink
                                               alist/user)))
    (int<nub>:alist:update user
                           alist/updated
                           int<nub>:var:sink)))
;; (nub:vars:init :test)
;; (int<nub>:var:sink:set :test :debug 'ignore)
;; (int<nub>:var:sink:set :test :warning 'warning)


(defun int<nub>:var:inhibit-message? (user level &optional no-fallback?)
  "Return `inhibit-message' boolean value for USER at output LEVEL.

If USER at LEVEL is not found, return value for fallback/default user at LEVEL.
  - If NO-FALLBACK? is non-nil, skip the fallback/default user check."
  ;; Ensure USER and LEVEL are ok.
  (int<nub>:var:assert-user-level "int<nub>:var:sink" user level :error)

  (int<nub>:var:user-at-level user
                              level
                              int<nub>:var:inhibit-message
                              no-fallback?))
;; (nub:vars:init :test)
;; (int<nub>:var:inhibit-message? :test :error)
;; (int<nub>:var:inhibit-message? :test :debug)
;; (int<nub>:var:inhibit-message? :test :debug :no-fallback)
;; (int<nub>:var:inhibit-message? :test<nub/debug>::nub:debug :debug :no-fallback)


;;------------------------------------------------------------------------------
;; Debugging
;;------------------------------------------------------------------------------

;; NOTE: For "is debugging active?", check `int<nub>:var:enabled?' for USER at `:debug' level.
;; Example for default user:
;;   (int<nub>:var:enabled? :default :debug)


(defvar int<nub>:var:debug:tags nil
  "Alist of USER keyword to list of active debugging keyword tags.

Any keyword matched in the list will be printed out when debugging is active.

If there are no tags in the list, or the list is nil, everything
will be printed.")


(defun int<nub>:var:debug:tags (user)
  "Get current debug tags flag for USER.

Return nil if not found."
  ;; Ensure USER is ok.
  (int<nub>:user:exists? "int<nub>:var:debug:tags" user :error)

  (int<nub>:alist:get/value user
                            int<nub>:var:debug:tags))
;; (int<nub>:var:debug:tags :test)


(defun int<nub>:var:debug:tags:set (user tags)
  "Set list of all active TAGS for USER."
  ;; Ensure USER is ok.
  (int<nub>:user:exists? "int<nub>:var:debug:tags:set" user :error)

  (unless (listp tags)
    (int<nub>:error "int<nub>:var:debug:tags"
                    "Tags must be a list or nil; got: %S"
                    tags))

  (if (null tags)
      (int<nub>:alist:delete user
                             int<nub>:var:debug:tags)
    (int<nub>:alist:update user
                           tags
                           int<nub>:var:debug:tags)))
;; (int<nub>:var:debug:tags :test)


(defun int<nub>:var:debug:tag:active? (user tag)
  "Return whether TAG is active for USER."
  ;; Ensure USER is ok.
  (int<nub>:user:exists? "int<nub>:var:debug:tag:active?" user :error)

  (not (null (memq tag (int<nub>:var:debug:tags user)))))
;; (int<nub>:var:debug:tags :test)


(defun int<nub>:var:debug:tag:set (user tag value)
  "Set USER's debug TAG to VALUE (or togggles the tag).

If VALUE is `:toggle', this will toggle TAG. Otherwise it will set/unset
TAG based on truthiness of VALUE."
  ;; Ensure USER is ok.
  (int<nub>:user:exists? "int<nub>:var:debug:tag:set" user :error)

  (let ((action (if value
                    :add
                  :delete)))

    ;; Did they ask us to toggle it?
    (when (eq value :toggle)
      ;; Toggling, so get the value first.
      (let ((current (int<nub>:var:debug:tag:active? user tag)))
        ;; Update `value' so we can pretend they asked for the 'correct' value to do a toggle.
        ;;   - If it's currently set, delete it & vice versa.
        (setq action (if current
                         :delete
                       :add))))

    ;; Now we can just set to what `action' says.
    ;;   - If truthy, that means ensure tag is in list.
    ;;   - If falsy, that means delete tag from list.
    (cond ((eq action :add)
           (let ((tags (int<nub>:alist:get/value user
                                                 int<nub>:var:debug:tags)))
             (push tag tags)
             (int<nub>:alist:update user
                                    tags
                                    int<nub>:var:debug:tags))
           ;; Return truthy to indicate it was turned on.
           t)

          ((eq action :delete)
           (let* ((tags (int<nub>:alist:get/value user
                                                  int<nub>:var:debug:tags))
                  (tags:updated (remove tag tags)))
             (int<nub>:alist:update user
                                    tags:updated
                                    int<nub>:var:debug:tags))
           ;; Return falsy to indicate it was turned off.
           nil)

          (t
           (int<nub>:error "int<nub>:var:debug:tag:set"
                           "Don't know what to do? Action is: %S"
                           action)))))
;; (int<nub>:var:debug:tags :test)


(defvar int<nub>:var:debug:tags/common nil
  "Alist of USER keyword to list of common debugging keyword tags.

Used to prompt end-user in interactive commands.

Any keyword can be used regardless of this list - these will be provided to
`nub:debug/toggle-tag' as potential candidates to toggle.")


(defun int<nub>:var:debug:tags/common (user)
  "Get common debug tags flag for USER.

Return nil if not found."
  ;; Ensure USER is ok.
  (int<nub>:user:exists? "int<nub>:var:debug:tags/common" user :error)

  (int<nub>:alist:get/value user
                            int<nub>:var:debug:tags/common))
;; (int<nub>:var:debug:tags :test)


(defun int<nub>:var:debug:tags/common:set (user tags)
  "Set USER's list of common debug TAGS."
  ;; Ensure USER is ok.
  (int<nub>:user:exists? "int<nub>:var:debug:tags/common:set" user :error)

  (int<nub>:alist:update user
                         tags
                         int<nub>:var:debug:tags/common))
;; (int<nub>:var:debug:tags/common:set :test '(:hello :there))


;;------------------------------------------------------------------------------
;; Debug Message Helpers
;;------------------------------------------------------------------------------

(defconst int<nub>:var:debug:fills
  (list (cons int<nub>:var:user:fallback '("  "
                                           "- ")))
  "Alist of users to list of strings to use for alternating fill/padding strings.")


(defun int<nub>:var:debug:fills (user)
  "Get debug fill/padding strings for USER.

Return value from `int<nub>:var:user:fallback' user if USER is not found."
  ;; Ensure USER is ok.
  (int<nub>:user:exists? "int<nub>:var:debug:fills" user :error)

  (let ((fills (int<nub>:alist:get/value user
                                         int<nub>:var:debug:fills
                                         :does-not-exist)))
    (if (eq fills :does-not-exist)
        ;; Didn't find it - get the fallback.
        (int<nub>:alist:get/value int<nub>:var:user:fallback
                                  int<nub>:var:debug:fills)
      ;; Found it.
      fills)))
;; (int<nub>:var:debug:tags :test)


(defun int<nub>:var:debug:fills:set (user fill-strings)
  "Set USER's list of FILL-STRINGS."
  ;; Ensure USER is ok.
  (int<nub>:user:exists? "int<nub>:var:debug:fills:set" user :error)

  (int<nub>:alist:update user
                         fill-strings
                         int<nub>:var:debug:fills))
;; (int<nub>:var:debug:fills:set :test '("-.-" "^.^"))


(defvar int<nub>:var:debug:fills/index
  (list (cons int<nub>:var:user:fallback 0))
  "Alist of USER to next fill/padding (index).

Fill/padding index used for USER's `int<nub>:var:debug:fills' alist entry.")


(defun int<nub>:var:debug:fills/index (user)
  "Get current index into debug fill/padding strings for USER."
  ;; Ensure USER is ok.
  (int<nub>:user:exists? "int<nub>:var:debug:fills/index" user :error)

  (let ((fills/index (int<nub>:alist:get/value user
                                               int<nub>:var:debug:fills/index
                                               :does-not-exist)))
    (if (eq fills/index :does-not-exist)
        ;; Didn't find it - get the fallback.
        (int<nub>:alist:get/value int<nub>:var:user:fallback
                                  int<nub>:var:debug:fills/index)
      ;; Found it.
      fills/index)))
;; (int<nub>:var:debug:tags :test)


(defun int<nub>:var:debug:fills/index:set (user index &optional create)
  "Set USER's index into the list of fill/padding strings.

If CREATE is nil and USER doesn't exist in index alist, sets the
default user instead."
  ;; Ensure USER is ok.
  (int<nub>:user:exists? "int<nub>:var:debug:fills/index:set" user :error)

  (if (and (null create)
           (not (int<nub>:alist:get/pair user int<nub>:var:debug:fills/index)))
      ;; Updated default index instead.
      (int<nub>:alist:update int<nub>:var:user:fallback
                             index
                             int<nub>:var:debug:fills/index)

    ;; Set/create user's index.
    (int<nub>:alist:update user
                           index
                           int<nub>:var:debug:fills/index)))
;; (int<nub>:var:debug:fills/index:set :test 42)


;;------------------------------------------------------------------------------
;; Init/Reset all user's vars.
;;------------------------------------------------------------------------------

;; TODO: A `nub:register' function? Maybe just rename this?
(cl-defun nub:vars:init (user &key path:root list:debug:tags/common alist:prefixes alist:enabled? alist:sinks)
  "Register USER at PATH:ROOT and set their default settings for output levels.

USER should be a keyword.

Optional Keyword Params:
  - PATH:ROOT - Root directory for the USER's code or whatever.
    - Default to `user-emacs-directory' if nil.

  - LIST:DEBUG:TAGS/COMMON - a list of debugging keyword tags.
    - Used for prompting for what debug tags to toggle.

  - ALIST:PREFIXES - an alist of verbosity level to strings.

  - ALIST:ENABLED? - an alist of verbosity level to t/nil.

  - ALIST:SINKS - an alist of verbosity level to:
                  t, nil, a function, or a list-of-functions.

Alists should have all output levels in them; for valid levels, see
`nub:output:levels'.
If an alist is nil, the default/fallback will be used instead.

Sets both current and backup values (backups generally only used for tests)."
  (int<nub>:init:user user)
  (int<nub>:init:path user path:root)

  (when alist:enabled?
    (int<nub>:init:enabled? user alist:enabled?))

  (when alist:sinks
    (int<nub>:init:sink user alist:sinks))

  (when alist:prefixes
    (int<nub>:init:prefix user alist:prefixes))

  (when list:debug:tags/common
    (int<nub>:var:debug:tags/common:set user list:debug:tags/common)))


(defun nub:vars:reset (user)
  "Reset output vars for USER to their initialized/backup values."
  (int<nub>:user:exists? "int<nub>:var:reset" user :error)

  (let ((default:enabled? (int<nub>:alist:get/value user int<nub>:var:enabled?:backup))
        (default:sink     (int<nub>:alist:get/value user int<nub>:var:sink:backup))
        (default:prefix   (int<nub>:alist:get/value user int<nub>:var:prefix:backup)))
    (if default:enabled?
        (int<nub>:alist:update user
                               default:enabled?
                               int<nub>:var:enabled?)
      (int<nub>:alist:delete user
                             int<nub>:var:enabled?))

    (if default:sink
        (int<nub>:alist:update user
                               default:sink
                               int<nub>:var:sink)
      (int<nub>:alist:delete user
                             int<nub>:var:sink))

    (if default:prefix
        (int<nub>:alist:update user
                               default:prefix
                               int<nub>:var:prefix)
      (int<nub>:alist:delete user
                             int<nub>:var:prefix))

    nil))
;; (setq int<nub>:var:enabled? nil)
;; (nub:vars:reset :test)


(defun nub:vars:terminate (user)
  "Remove USER and their settings from nub (initialized/backup values too)."
  ;;---
  ;; Allow multiple calls or calls for non-existant users.
  ;;---
  ;; (int<nub>:user:exists? "int<nub>:var:reset" user :error)

  ;;---
  ;; Delete user's output settings.
  ;;---
  (int<nub>:alist:delete user
                         int<nub>:var:enabled?)
  (int<nub>:alist:delete user
                         int<nub>:var:enabled?:backup)

  (int<nub>:alist:delete user
                         int<nub>:var:sink)
  (int<nub>:alist:delete user
                         int<nub>:var:sink:backup)

  (int<nub>:alist:delete user
                         int<nub>:var:prefix)
  (int<nub>:alist:delete user
                         int<nub>:var:prefix:backup)

  ;;---
  ;; Delete user's debug settings.
  ;;---
  (int<nub>:alist:delete user
                         int<nub>:var:debug:tags)

  (int<nub>:alist:delete user
                         int<nub>:var:debug:tags/common)

  ;;---
  ;; Final step: Delete the actual user.
  ;;---
  (int<nub>:alist:delete user
                         int<nub>:var:path)
  (int<nub>:terminate:user user)

  nil)


(defun int<nub>:vars:nuke/tactical ()
  "Delete all of everything (except `int<nub>:var:user:fallback'; they're cool)."
  ;; Delete all users except `int<nub>:var:user:fallback'.

  (message "\nNuking `nub' variables...")

  ;;---
  ;; Delete output settings.
  ;;---
  (message "  `int<nub>:var:enabled?'")
  (dolist (user-assoc int<nub>:var:enabled?)
    (let ((user (car user-assoc)))
      (unless (eq user int<nub>:var:user:fallback)
        (message "    - %S" user)
        (int<nub>:alist:delete user
                               int<nub>:var:enabled?))))

  (message "  `int<nub>:var:enabled?:backup'")
  (dolist (user-assoc int<nub>:var:enabled?:backup)
    (let ((user (car user-assoc)))
      (unless (eq user int<nub>:var:user:fallback)
        (message "    - %S" user)
        (int<nub>:alist:delete user
                               int<nub>:var:enabled?:backup))))

  (message "  `int<nub>:var:sink'")
  (dolist (user-assoc int<nub>:var:sink)
    (let ((user (car user-assoc)))
      (unless (eq user int<nub>:var:user:fallback)
        (message "    - %S" user)
        (int<nub>:alist:delete user
                         int<nub>:var:sink))))

  (message "  `int<nub>:var:sink:backup'")
  (dolist (user-assoc int<nub>:var:sink:backup)
    (let ((user (car user-assoc)))
      (unless (eq user int<nub>:var:user:fallback)
        (message "    - %S" user)
        (int<nub>:alist:delete user
                               int<nub>:var:sink:backup))))

  (message "  `int<nub>:var:prefix'")
  (dolist (user-assoc int<nub>:var:prefix)
    (let ((user (car user-assoc)))
      (unless (eq user int<nub>:var:user:fallback)
        (message "    - %S" user)
        (int<nub>:alist:delete user
                               int<nub>:var:prefix))))

  (message "  `int<nub>:var:prefix:backup'")
  (dolist (user-assoc int<nub>:var:prefix:backup)
    (let ((user (car user-assoc)))
      (unless (eq user int<nub>:var:user:fallback)
        (message "    - %S" user)
        (int<nub>:alist:delete user
                               int<nub>:var:prefix:backup))))

  ;;---
  ;; Delete debug settings.
  ;;---
  (message "  `int<nub>:var:debug:tags'")
  (dolist (user-assoc int<nub>:var:debug:tags)
    (let ((user (car user-assoc)))
      (unless (eq user int<nub>:var:user:fallback)
        (message "    - %S" user)
        (int<nub>:alist:delete user
                               int<nub>:var:debug:tags))))

  (message "  `int<nub>:var:debug:tags/common'")
  (dolist (user-assoc int<nub>:var:debug:tags/common)
    (let ((user (car user-assoc)))
      (unless (eq user int<nub>:var:user:fallback)
        (message "    - %S" user)
        (int<nub>:alist:delete user
                               int<nub>:var:debug:tags/common))))

  ;;---
  ;; Final step: Delete the actual users.
  ;;---
  (message "  `int<nub>:var:path'")
  (dolist (user-assoc int<nub>:var:path)
    (let ((user (car user-assoc))
          (path (cdr user-assoc)))
      (unless (eq user int<nub>:var:user:fallback)
        (message "    - %S @ '%S'" user path)
        (int<nub>:alist:delete user
                               int<nub>:var:path))))

  (message "  `int<nub>:var:users'")
  (dolist (user int<nub>:var:users)
    (message "    - %S" user))
  (setq int<nub>:var:users nil)

  (message "\nDone.")
  nil)
;; (int<nub>:vars:nuke/tactical)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :nub 'variables)
