;;; namespaced/path/regex.el --- Regexes & Globs -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2023-06-22
;; Timestamp:  2025-11-04
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Regexes: Now you have 2 problems.
;; Globs:   Globs and regexes are the same thing, right?
;;
;;; Code:


(imp-require alist:/type/string)


;;------------------------------------------------------------------------------
;; Regex: Constants & Variables
;;------------------------------------------------------------------------------

;; TODO: filesystem ignore case?
;;   - Is that defined elsewhere in path?
(defconst _:path:rx:case-sensitive?
  (pcase system-type
    ('windows-nt nil)
    ('gnu/linux t)
    ;; Other systems as needed:
    ;;   gnu, gnu/kfreebsd, darwin, ms-dos, cygwin
    )
  "Any valid directory/path separator for the operating system type.

NOTE: This is an `rx' forms.")


;;------------------------------------------------------------------------------
;; Regex: API
;;------------------------------------------------------------------------------

(defun path:rx (root regex &optional absolute-regex absolute-paths)
  "Regex searches for paths matching the given REGEX starting at ROOT dir.

If ABSOLUTE-REGEX is non-nil, absolute paths will be matched against REGEX.
Otherwise paths relative to ROOT will be matched against REGEX.

Returns a list of path strings or nil. If ABSOLUTE-PATHS is non-nil, returned
paths will be absolute. Else returned paths will be relative to ROOT."
  (let (matches)
    ;; Walk directory tree at root, looking for matches.
    (path:walk root
               (lambda (root dir child)
                 "Check path against REGEX, save to `matches' if matched."
                 (when (string-match-p regex
                                       ;; What check depends on what kind of regex was supplied.
                                       (if absolute-regex
                                           (path:join root dir child)
                                         (path:join dir child)))
                   ;; What we depends on what caller wants.
                   (push (if absolute-paths
                             (path:join root dir child)
                           (path:join dir child))
                         matches))
                 ;; Always return "continue walking tree".
                 t))

    ;; Return whatever was found.
    matches))
;; (path:rx (path:current:dir) (rx string-start (one-or-more printing) ".el" string-end) t)
;; (path:rx (path:parent (path:current:dir)) (rx string-start (one-or-more printing) ".el" string-end) t)


;;------------------------------------------------------------------------------
;; Globs -> Regex: Constants & Variables
;;------------------------------------------------------------------------------

(defconst _:path:rx:glob/chars
  (rx-to-string '(or "*" "?" "[") :no-group)
  "Regex string for characters that start a glob pattern.

There are other characters that belong to globs, like \"!\" and \"]\", but they
never begin a pattern.")


(defconst _:path:rx:separator:dirs
  (pcase system-type
    ('windows-nt '(or "/" "\\"))
    ('gnu/linux "/")
    ;; Other systems as needed:
    ;;   gnu, gnu/kfreebsd, darwin, ms-dos, cygwin
    )
  "Any valid directory/path separator for the operating system type.

NOTE: This is an `rx' forms.")


(defconst _:path:rx:separator:paths
  (pcase system-type
    ('windows-nt ";")
    ('gnu/linux  ":")
    ;; Other systems as needed:
    ;;   gnu, gnu/kfreebsd, darwin, ms-dos, cygwin
    )
  "Any valid directory/path separator for the operating system type.

NOTE: This is an `rx' forms.")


(defconst _:path:rx:glob:any/name
  '(or alphanumeric " ")
  "Any valid directory or file name character.
AKA not a directory or path separator.

NOTE: This is an `rx' forms.")


(defconst _:path:rx:glob:any/path
  (list 'or
        _:path:rx:glob:any/name
        _:path:rx:separator:dirs)
  "Any valid path character, including directory separators.

NOTE: This is an `rx' forms.")
;; (rx-to-string (list 'one-or-more _:path:rx:glob:any/path) :no-group)


(defconst _:path:rx:glob:range
  '(group (not "-") "-" (not "-"))
  "A glob range (from inside a glob character class).

Example:
  Glob \"[A-Z]\" class and \"[!A-Z]\" compliment class both have range \"A-Z\".

NOTE: This is an `rx' forms.")


(defconst _:path:glob->rx:alist
  ;; "**" - Recursive Match: matches zero or more directories (and any file if used at end of regex).
  (list (cons "**" (rx-to-string (list 'zero-or-more _:path:rx:glob:any/path)
                                 :no-group))
        ;; "*" - Name Match: matches zero or one directory/file name characters.
        (cons "*" (rx-to-string (list 'zero-or-one _:path:rx:glob:any/name)
                                :no-group))
        ;; "?" - Name Match: matches exactly one directory/file name character.
        (cons "?" (rx-to-string _:path:rx:glob:any/name
                                :no-group))
        ;; "[" & "[!" - Character Class: matches whatever's before the closing "]" (must match at least one char).
        (cons "[" (rx-to-string _:path:rx:glob:range
                                :no-group)))
  "Glob string to replacement regex string.")


;;------------------------------------------------------------------------------
;; Globs -> Regex: Helper Functions
;;------------------------------------------------------------------------------

(defun _:path:rx:match (regex string)
  "Runs a `string-match' on STRING with case sensitivity set correctly.

NOTE: Does not wrap in `save-match-data' macro - caller must do that.

Returns `string-match' results."
  ;; Set case sensitivity flag.
  (let* ((case-fold-search (not _:path:rx:case-sensitive?)))
    ;; Run search, return nil/integer result.
    (string-match regex string)))


(defun _:path:glob->rx:find (string:glob string:rx)
  "Find the start of the next glob in STRING:GLOB.

Move characters before that start to the end of STRING:RX. If no glob-start
characters found, moves all of STRING:GLOB to the end of STRING:RX.

Returns a plist of the updated STRING:GLOB and STRING:RX with keys:
  - `:glob'
  - `:rx'"
  (save-match-data
    (let ((matched:at (_:path:rx:match _:path:rx:glob/chars string:glob)))
      ;; Are there any glob characters?
      (if (not matched:at)
          ;;---
          ;; No glob chars, so just add the whole glob string to the regex string.
          ;;---
          (setq string:rx (concat string:rx
                                  string:glob)
                string:glob nil)

        ;;---
        ;; Found a glob; save everything before this glob.
        ;;---
        (setq string:rx (concat string:rx
                                (substring string:glob 0 matched:at))
              ;; We're now dealing with whatever we just matched and the rest after it.
              string:glob (substring string:glob matched:at))))

    ;;---
    ;; Return updated glob and rx strings.
    ;;---
    (list :glob string:glob
          :rx   string:rx)))


(defun _:path:glob->rx:build:character-class (string:glob &optional compliment?)
  "Convert STRING:GLOB to a regex string.

STRING:GLOB (normal) should be \"[\"STRING:GLOB\"]\".
STRING:GLOB (compliment) should be \"[!\"STRING:GLOB\"]\".

Deals correctly with ranges."
  (let (regexes)
    (save-match-data
      ;;------------------------------
      ;; Find & extract ranges ("a-z").
      ;;------------------------------
      (while (_:path:rx:match (alist:string:get/value "[" _:path:glob->rx:alist)
                              string:glob)
        (let ((string:match (match-string 1 string:glob))
              (match:start  (nth 2 (match-data :integers)))
              (match:end    (nth 3 (match-data :integers)))
              (string:start 0)
              (string:end   (length string:glob)))
          (push string:match regexes)
          (setq string:glob (concat (substring string:glob string:start match:start)
                                    (substring string:glob match:end    string:end)))))

      ;;------------------------------
      ;; Add the non-ranges to the regexes.
      ;;------------------------------
      (dolist (char (split-string string:glob "" :no-nulls))
        (push char regexes))

      ;;------------------------------
      ;; Create a regex string from the list of ranges, chars.
      ;;------------------------------
      ;; `rx' form `any' deals with range strings correctly, so just stuff it
      ;; into the front of the list and use the list to create the regex.
      (push 'any regexes)

      ;; Invert (Compliment) the regex?
      (when compliment?
        (setq regexes (list 'not regexes)))

      ;; Compile regex.
      (rx-to-string regexes :no-group))))
;; (_:path:glob->rx:build:character-class "A-Za-z-")
;; (_:path:glob->rx:build:character-class "A-Za-z-" t)


(defun _:path:glob->rx:convert (string:glob string:rx)
  "Convert the glob at the start of STRING:GLOB into a regex.

Takes glob pattern out of STRING:GLOB, converts to a regex string, and adds to
the end of STRING:RX.

Returns a plist of the updated STRING:GLOB and STRING:RX with keys:
  - `:glob'
  - `:rx'"
  (save-match-data
    ;;------------------------------
    ;; Glob patterns -> rx patterns.
    ;;------------------------------
    ;; NOTE: Order Matters! Need to find/convert "**" before "*", for example.
    ;; NOTE: All matches should be done at `string-start'.

    ;; "[!"<something>"]" - Compliment Character Class/Range: Any name character that is not these characters.
    ;;    NOTE: <something> follows regex character class rules.
    (cond ((_:path:rx:match (rx string-start "[!" (group (one-or-more printing)) "]") string:glob)
           (setq string:rx (concat string:rx
                                   (_:path:glob->rx:build:character-class (match-string 1 string:glob)
                                                                          :compliment))
                 string:glob (substring string:glob (nth 1 (match-data :integers)))))

          ;; "["<something>"]" - Character Class/Range: Any name character that is not these characters.
          ;;    NOTE: <something> follows regex character class rules, except that leading "^" is not a negation/compliment.
          ((_:path:rx:match (rx string-start "[" (group (one-or-more printing)) "]") string:glob)
           (setq string:rx (concat string:rx
                                   (_:path:glob->rx:build:character-class (match-string 1 string:glob)))
                 string:glob (substring string:glob (nth 1 (match-data :integers)))))

          ;; "**" - Recursive Match: matches zero or more directories (and any file if used at end of regex).
          ((_:path:rx:match (rx string-start "**") string:glob)
           (setq string:rx (concat string:rx
                                   (alist:string:get/value "**" _:path:glob->rx:alist))
                 string:glob (substring string:glob (nth 1 (match-data :integers)))))

          ;; "*" - Name Match: matches zero or more directory/file name characters.
          ((_:path:rx:match (rx string-start "*") string:glob)
           (setq string:rx (concat string:rx
                                   (alist:string:get/value "*" _:path:glob->rx:alist))
                 string:glob (substring string:glob (nth 1 (match-data :integers)))))

          ;; "?" - Name Match: matches exactly one directory/file name character.
          ((_:path:rx:match (rx string-start "?") string:glob)
           (setq string:rx (concat string:rx
                                   (alist:string:get/value "?" _:path:glob->rx:alist))
                 string:glob (substring string:glob (nth 1 (match-data :integers))))))

    ;;------------------------------
    ;; Return updated glob and rx strings.
    ;;------------------------------
    (list :glob string:glob
          :rx   string:rx)))
;; (_:path:glob->rx:convert "[!A-Za-z-]" "")
;; (_:path:glob->rx:convert "[A-Za-z-]" "")
;; (_:path:glob->rx:convert "**" "")
;; (_:path:glob->rx:convert "*" "")
;; (_:path:glob->rx:convert "?" "")
;; (_:path:glob->rx:convert "[a-z]*/**" "")


;;------------------------------------------------------------------------------
;; Globs -> Regex: API
;;------------------------------------------------------------------------------

(defun path:glob->rx (glob)
  "Convert GLOB string into a regex string.

Replace glob strings in GLOB so that the returned regex string is equivalent.
Correctly deals with e.g. \"**\" and \"*\" (that is, replaces things in the correct order.

Raises an error signal if GLOB is not a string."
  ;;------------------------------
  ;; Errors?
  ;;------------------------------
  (cond ((not (stringp glob))
         (error "_:path:glob/rx:alist: GLOB needs to be a string: %S"
                glob))

        ((= 0 (length glob))
         (error "_:path:glob/rx:alist: GLOB needs to be a string of length > 0: %S"
                glob))

        ;;------------------------------
        ;; Replace Globs.
        ;;------------------------------
        (t
         (let ((string:glob glob)
               (string:rx "")
               plist)

           ;;---
           ;; Replace globs until done processing input string.
           ;;---
           ;; `_:path:glob->rx:find' will move any non-glob substrings from
           ;; `string:glob' to `string:rx', so when `string:glob' is empty we
           ;; are done processing.
           (while string:glob
             ;; Search for start of next glob pattern...
             (setq plist (_:path:glob->rx:find string:glob string:rx))

             ;; Update our strings.
             (setq string:glob (plist-get plist :glob)
                   string:rx   (plist-get plist :rx))

             ;; Process a glob pattern or loop.
             (when string:glob
               (setq plist (_:path:glob->rx:convert string:glob string:rx))
               ;; Update our strings and loop.
               (setq string:glob (plist-get plist :glob)
                     string:rx   (plist-get plist :rx))))

           ;; Return compiled regex.
           string:rx))))
;; (path:glob->rx "/foo/bar*baz")


(defun path:glob (root glob &optional absolute-glob absolute-paths)
  "Searches for paths matching the given GLOB starting at ROOT dir.

If ABSOLUTE-GLOB is non-nil, absolute paths will be matched against GLOB.
Otherwise paths relative to ROOT will be matched against GLOB.

Returns a list of path strings or nil. If ABSOLUTE-PATHS is non-nil, returned
paths will be absolute. Else returned paths will be relative to ROOT."

  (path:rx root
           (path:glob->rx glob)
           absolute-glob
           absolute-paths))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide path regex)
