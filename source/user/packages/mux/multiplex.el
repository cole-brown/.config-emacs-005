;;; mux/multiplex.el --- Knowledge About All Systems -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2020-10-28
;; Timestamp:  2025-12-02
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Define all systems so that we can then know which system this system is.
;;
;; Use `mux-define' to define your systems.
;;
;; Then you can use `mux-set' and `mux-get' to set
;; up in a system-agnostic(ish) manner.
;;
;;; Code:


(require 'cl-lib)

(imp-require 'str)
(imp-require 'path)
(imp-require 'buffer)


;;------------------------------------------------------------------------------
;; Customs, Constants, Etc.
;;------------------------------------------------------------------------------

;; TODO(mux): set this somehow. Make a func?
(defvar mux-system-id nil
  "ID for \this\ current system.")


;; (setq mux-systems nil mux-system-id nil)
(defcustom mux-systems '()
  "Alist of all known system ids.

Each entry has the format (HASH ID DOCSTR (PLIST))

HASH   - keyword (see func `mux-hash')
ID     - keyword (see func `mux-id')
DOCSTR - string or nil
PLIST  - property list or nil"
  :group 'mux)


(defconst _mux-rx-hash (rx-to-string `(seq string-start
                                           (optional ":") ; keyword?
                                           (+ hex)
                                           ,str:hash:join/slices
                                           (+ hex)
                                           string-end)
                                     :no-group))


(defconst _mux-rx-id (rx-to-string `(seq string-start
                                         (optional ":") ; keyword?
                                         ;; id strings
                                         (zero-or-more
                                          (one-or-more alphanumeric)
                                          (optional ,str:hash:join/prefixes))
                                         ;; strings/hash separator
                                         ,str:hash:join/prepend
                                         ;; hash
                                         (+ hex)
                                         ,str:hash:join/slices
                                         (+ hex)
                                         string-end)
                                   :no-group))


;;------------------------------------------------------------------------------
;; Getter/Setter
;;------------------------------------------------------------------------------

(defun _mux-plist-delete (plist property)
  "Delete PROPERTY from PLIST."
  (let (p)
    (while plist
      (unless (eq property (car plist))
          (setq p (plist-put p (car plist) (nth 1 plist))))
      (setq plist (cddr plist)))
    p))


(cl-defun mux-set (&rest plist &key hash (id nil) (docstr nil) &allow-other-keys)
  "For setting a system's multiplex settings.

- REQUIRED key:
  + HASH (`:hash') - keyword or symbol (see `mux-hash')
     - `:this' or `this' for \this\ system's hash
- OPTIONAL key:
  + ID (`:id') - keyword or symbol (see `mux-id') (default nil)
    - nil, `:this', or `this' for \this\ system's id
  + DOCSTR (`:docstr') - documentation string (default nil)

The rest of the keyword/value pairs in PLIST will be stored as HASH's
property list."
  ;; Delete known keys from plist; rest should be key/value pairs to store.
  (setq plist (_mux-plist-delete plist :hash))
  (setq plist (_mux-plist-delete plist :id))
  (setq plist (_mux-plist-delete plist :docstr))

  (let ((funcname 'mux-set))
    (_mux-debug funcname
      '("args:\n"
        "  HASH:   %S\n"
        "  ID:     %S\n"
        "  DOCSTR: %S\n"
        "  PLIST:  %S")
      hash
      id
      docstr
      plist)

    ;;------------------------------
    ;; Validate.
    ;;------------------------------
    (when (and (symbolp hash)
               (memq hash '(:this this)))
      (setq hash (mux-hash)))

    (when (and (symbolp id)
               (memq id '(nil :this this)))
      (setq id (mux-id)))

    (unless (or (keywordp hash)
                (symbolp hash))
      (_mux-error funcname
        "`:hash' key parameter must have a (string) value; got: %S"
        hash))

    (unless (or (null docstr)
                (stringp docstr))
      (_mux-error funcname
        "`:docstr' key parameter must have a string or nil value; got: %S"
        docstr))

    (when plist
      (let (plist-verify (seq-copy plist))
        (while (> (length plist-verify) 2)
          (let ((key   (pop plist-verify))
                (value (pop plist-verify)))
            (unless (and key
                         (keywordp key))
              (_mux-error funcname
                "key parameter must be a keyword; got: %S"
                key))
            ;; ignore value. It can be anything; we don't care.
            ))
        ;; Shouldn't have any leftovers.
        (unless plist
          (_mux-error funcname
            '("PLIST has extras after verifying. PLIST must be `:KEYWORD VALUE' pairs. "
              "PLIST: %S "
              "extra: %S")
            plist
            plist-verify))))

    ;;------------------------------
    ;; Save.
    ;;------------------------------

    ;; Delete previous entry if it exists.
    (when (and mux-systems
               (alist-get hash mux-systems))
      (setf (alist-get hash
                       mux-systems
                       nil
                       'remove)
            nil))

    ;; Add this entry.
    (let ((entry (list hash docstr plist)))
      (if (null mux-systems)
          (setq mux-systems (list entry))
        (push entry mux-systems))

      (_mux-debug funcname
        '("Added entry to `mux-systems':\n"
          "  entry: %S\n"
          "  mux-systems: %S\n")
        entry
        mux-systems)

      ;; return this entry?
      entry)))
;; (setq mux-systems nil)
;; mux-systems
;; (mux-set :hash :test :foo 'bar :baz "qux")
;; (mux-set :hash :test :docstr "hi" :foo 'bar :baz 'qux)


(cl-defun mux-get (&key id key)
  "For getting a system's multiplex settings.

ID should be:
  - keyword or symbol of the system's id used when saving the setting
    - `this' or `:this' for \"this current system's id\"

KEY should be the keyword that the setting was saved under
(which, see function `mux-set')."
  (let ((funcname 'mux-get))
    ;;------------------------------
    ;; Validate Inputs
    ;;------------------------------
    ;; TODO(mux): setq _mux-verify-id
    ;; TODO(mux): get hash from id

    (cond ((not (keywordp key))
           (_mux-error funcname
             "KEY must be a keyword; got %S: %S"
             (type-of key)
             key))

          ((not (or (keywordp id)
                    (symbolp id)))
           (_mux-error funcname
             "ID must be a symbol or keyword (`this'/`:this' for current system); got: %S"
             id))

          ;; Special Case: `this' system's ID.
          ((or (eq id 'this)
               (eq id :this))
           ;; TODO(mux): error if `mux-system-id' is unset.
           (setq id mux-system-id))

          (t
           nil))

    ;;------------------------------
    ;; Get Multiplexer Value
    ;;------------------------------
    (if-let ((plist (nth 2 (assoc id mux-systems))))
        (plist-get plist key)
      nil)))
;; (setq mux-systems nil)
;; mux-systems
;; (mux-set :id :test :foo 'bar :baz "qux")
;;
;; (setq mux-system-id :test)
;; (mux-get :id 'this :key :path/secret/emacs)
;; (mux-get :id :test :key :foo)
;; (mux-get :id :test :key 'foo)


;;------------------------------------------------------------------------------
;; Helpers
;;------------------------------------------------------------------------------

(defun _mux-str-to-keyword (str)
  (if (keywordp str)
      str

    (unless (stringp str)
      (_mux-error '_mux-str-to-keyword
        "STR must be a string; got %S: %S"
        (type-of str)
        str))

    (intern
     (if (string-prefix-p ":" str)
         str
       (concat ":" str)))))
;; (_mux-str-to-keyword "foo")
;; (_mux-str-to-keyword :foo)


(defun _mux-this? (hash/id)
  (cond ((or (eq hash/id 'this)
             (eq hash/id :this))
         (mux-hash))
        ((_mux= hash/id (mux-hash))
         (mux-hash))
        (_
         nil)))


(defun _mux-verify-hash (hash)
  (let ((funcname '_mux-verify-hash))
    (when (or (eq hash 'this)
              (eq hash :this))
      (setq hash (mux-hash)))

    (unless (keywordp hash)
      (_mux-error funcname
        "HASH must be a keyword (`this'/`:this' for current system); got: %S"
        hash))

    (unless (string-match-p _mux-rx-hash (symbol-name hash))
      (_mux-error funcname
        "HASH does not match regex (%S); got: %S"
        _mux-rx-hash
        (symbol-name hash)))

    hash))


(defun _mux-verify-id (id)
  (let ((funcname '_mux-verify-id))
    (when (or (eq id 'this)
              (eq id :this))
      ;; TODO(mux): error if `mux-system-id' is unset.
      (setq id mux-system-id))

    (unless (keywordp id)
      (_mux-error funcname
        "ID must be a keyword (`this'/`:this' for current system); got: %S"
        id))

    (unless (string-match-p _mux-rx-id (symbol-name id))
      (_mux-error funcname
        "ID does not match regex (%S); got: %S"
        _mux-rx-id
        (symbol-name id)))

    id))


(defun _mux-id-to-hash (id)
  "Return hash from ID."
  (setq id (_mux-verify-id id))

  (_mux-str-to-keyword
   (nth 1 (string-split (symbol-name id) str:hash:join/prepend))))
;; (mux-id-to-hash (mux-id))


(defun _mux-hash-to-id (hash)
  "Return ID from hash or nil."
  (setq hash (_mux-verify-hash hash))

  (nth 1 (assoc hash mux-systems)))
;; (mux-id-to-hash (mux-id))


(defun _mux-type-of (hash/id)
  "Return `:hash', `:id' or nil."
  (let ((str (if (stringp hash/id)
                 hash/id
               (symbol-name hash/id))))
    (cond ((string-match-p _mux-rx-hash str)
           :hash)
          ((string-match-p _mux-rx-id str)
           :id)
          (t
           nil))))
;; (_mux-type-of (mux-hash))
;; (_mux-type-of (mux-id))
;; (_mux-type-of (mux-id :foo "bar" 'etc))
;; (_mux-type-of "jeff")


(defun _mux-to-hash (hash/id)
  (pcase (_mux-type-of hash/id)
    (:hash hash/id)
    (:id   (_mux-id-to-hash hash/id))
    (_     nil)))
;; (_mux-to-hash (mux-id))
;; (_mux-to-hash (mux-id :foo "bar" 'etc))
;; (_mux-to-hash (mux-hash))


(defun _mux= (left right)
  "Compare hashes and/or IDs.

LEFT and RIGHT must be keywords and can be:
  - hashes (see `mux-hash')
  - IDs (see `mux-id')

Return non-nil if the hashes are equal."
  (and (keywordp left)
       (keywordp right)
       (eq (_mux-to-hash left)
           (_mux-to-hash right))))
;; (_mux= (mux-hash) (mux-id))


;;------------------------------------------------------------------------------
;; System Hash & ID
;;------------------------------------------------------------------------------

(defun mux-hash ()
  "Return /this/ system's short hash as keyword."
  (_mux-str-to-keyword
   (str:hash:short (list (system-name) system-type))))
;; (mux-hash)
;; (str:hash:short (list (system-name) system-type))


(defun mux-id (&rest prefixes)
  "Generate a unique system ID from /this/ system's hash and PREFIXES.

PREFIXES should be 0-3 strings or symbol.
For example: I use a domain, date, and type for the system.
  - DOMAIN: Where the system is or what its for.
    - \"work\", \"home\", ...
  - DATE: When I got the system.
      -  \"2020\", \"2022\", etc.
  - TYPE:
    - \"desk\", \"lap\", etc...

Return an ID keyword with prefixes and hash.
For example:
  (mux-id 'work '2025 'lap)
    => :work/2025/lap::c3cdd4-955447"
  (_mux-str-to-keyword
   (str:hash:pretty prefixes
                    (list (system-name) system-type))))
;; (mux-id 'work '2025 'lap)
;; (mux-id "jeff" "2020" "compy")
;; (mux-id)


;; TODO(path): didn't I have a path safing func in imp or ns:path?
(defun _mux-to-string-path-safe (&optional arg)
  ;; Just delete anything else that isn't safe.
  (replace-regexp-in-string
   "[<>:\"/\\|? *]" ""
   ;; Replace expected things from `mux-id' with chars that make sense.
   (replace-regexp-in-string
    "::" "_"
    (replace-regexp-in-string
     "/" "-"
     ;; Get rid of keyword's leading colon.
     (string-remove-prefix
      ":"
      ;; Make ARG a string.
      (cond ((memq arg '(nil this :this))
             (symbol-name (mux-hash)))
            ((or (symbolp arg)
                 (keywordp arg))
             (symbol-name arg))
            ((stringp id)
             id)
            (t
             (format "%S" arg))))))))
;; (_mux-to-string-path-safe (mux-hash))
;; (_mux-to-string-path-safe :this)
;; (_mux-to-string-path-safe (mux-id 'work '2025 'lap))
;; (_mux-to-string-path-safe "hello there")


(defun mux-path-safe-dir (root id-or-hash)
  "Convert system's ID-OR-HASH to a safe (absolute) directory path.

ROOT must be an absolute path string.

If ID-OR-HASH is `:this' or `this', use \this\ system's hash.
If UNIQUE-ID is nil, use this system's ID."
  (path:abs:dir root
                (_mux-to-string-path-safe id-or-hash)))
;; (mux-path-safe-dir "c:/foo" ":bar")
;; (mux-path-safe-dir "/foo" ":bar")
;; (mux-path-safe-dir "/foo" :this)


;;------------------------------------------------------------------------------
;; Commands & Info
;;------------------------------------------------------------------------------

(defun mux-bootstrap (minimal? &rest prefixes)
  "Return a `mux-set' sexpr to use for this system.

If MINIMAL? is non-nil, `mux-set' will only include hash & id.
Else, an example docstr and custom keys will be included.

PREFIXES should be 0-3 strings or symbol.
See func `
For example: I use a domain, date, and type for the system.
  - DOMAIN: Where the system is or what its for.
    - \"work\", \"home\", ...
  - DATE: When I got the system.
      -  \"2020\", \"2022\", etc.
  - TYPE:
    - \"desk\", \"lap\", etc...

For example, IDs with and without prefixes:
  (mux-id 'work '2025 'lap)
    => :work/2025/lap::c3cdd4-955447
  (mux-id)
    => ::c3cdd4-955447"
  (if minimal?
      `(mux-set :hash   ,(mux-hash)
                :id     ,(apply #'mux-id prefixes))
    `(mux-set :hash   ,(mux-hash)
              :id     ,(apply #'mux-id prefixes)
              :docstr "TODO: ExampleCo work laptop 2025"
              ::TODO:: "Replace this and the rest with your key/value pairs for this system"
              ;; :key value
              :dir-sys-init (mux-path-safe-dir "~/path/to/secret/mux/" ,(apply #'mux-id prefixes))
              :dir-org   "TODO: ~/path/to/org/files/"
              :dir-repos "TODO: ~/path/to/repositories"
              :foo 'bar
              :etc "...")))
;; (mux-bootstrap nil 'work '2025 'lap)
;; (mux-bootstrap t   'work '2025 'lap)


(defun mux-system-info ()
  "Print out this system's info.

If this system is not in `mux-systems', print out code for adding it."
  (if-let ((system (assoc (mux-hash) mux-systems)))
      (_mux-show (nth 1 system) "*Messages*")
    ;; else, print out info and the `mux-set' sexpr to get them started
    (display-warning
     'mux
     (mapconcat (lambda (x) (format (if (stringp x) "%s" "%S") x))
                `("Unknown system."
                  "  Init system with something like:"
                  ,(mux-bootstrap nil)
                  "  For a more user-friendly ID, call `mux-bootstrap' with PREFIXES."
                  "  Example: (mux-bootstrap nil 'work '2025 'laptop)")
                "\n"))))
;; (mux-system-info)


(defun _mux-show (id buffer)
  "Display system multiplexer info for system identified by ID in BUFFER.

ID should be a keyword from e.g. `mux-id'.

BUFFER should be a buffer or buffer name string."
  (with-current-buffer (get-buffer-create buffer)
    (goto-char (point-max))
    (let* ((assoc (assoc hash mux-systems))
           (plist (nth 3 assoc))
           (plist-str (cond ((null plist)
                             "(empty)")
                            ((not (proper-list-p plist))
                             "(invalid)")
                            (t
                             (let (plist-str)
                               ;; TODO(mux): this doesn't delete shit from `mux-systems', does it?
                               (while (> (length plist) 2)
                                 (setq plist-str (concat plist-str
                                                         "\n"
                                                         (format "      %S: %S"
                                                                 (pop plist)
                                                                 (pop plist)))))
                               plist-str)))))

      (insert
       (format (mapconcat #'identity
                          '("System %s:%s"
                            "  ---> THIS SYSTEM! <---"
                            "    -> hash:          %s"
                            "    -> id:            %s"
                            "    -> description:   %s"
                            "    -> plist:         %s")
                          "\n")
               id
               (if (_mux-this? id)
                   "\n  ---> THIS SYSTEM! <---"
                 "")
               (_mux-to-hash id)
               (nth 2 assoc)
               plist-str)))))


(defun mux-show (&optional id)
  "Display system info for system identified by ID.

If ID is nil, displays all systems' infos."
  (interactive)
  ;; Error checking
  (unless (or (null id)
              (stringp id)
              (keywordp id))
    (_mux-error 'mux-show
      "ID must be a string, keyword, or nil. Got type %S: %S"
      (type-of id)
      id))

  ;; Show the matching system, or all of 'em.
  (let ((buffer "*mux: systems*"))
    (if id
        (_mux-show id buffer)

      (dolist (system mux-systems)
        (_mux-show (nth 1 system) buffer)))))
;; (mux-show)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide mux multiplex)
