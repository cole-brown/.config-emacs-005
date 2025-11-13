;;; core/modules/emacs/dlv/+display.el --- Display DLV info. -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2023-06-21
;; Timestamp:  2023-07-18
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Display info for Directory Local Variables.
;;
;; Write info to '*Messages*' buffer.
;;
;;; Code:


(imp:require :dlv 'dlv)


;;------------------------------------------------------------------------------
;; Check value at path.
;;------------------------------------------------------------------------------

(defun dlv:check (filepath symbol expected)
  "Get SYMBOL's value at FILEPATH in a few different ways.

EXPECTED is output with actual value, and also used to check DLV safeness via
`safe-local-variable-p'.

Output to '*Messages*' buffer."
  (let ((func/name "dlv:check")
        ;; (func/tags '(:info))
        (file/existed? (file-exists-p filepath))
        (buffer/existed? (get-file-buffer filepath))
        buffer/local
        value/ffns
        value/blv
        value/slvp)

    ;; These don't work if the file doesn't exist.
    (unwind-protect
        (progn
          (setq buffer/local (get-buffer-create filepath))
          (setq value/blv (buffer-local-value symbol buffer/local))
          (with-current-buffer buffer/local
            (setq value/slvp (safe-local-variable-p symbol expected))))
      (unless buffer/existed?
        (kill-buffer buffer/local))
      (setq buffer/local nil))

    ;; This should always work.
    (unwind-protect
        (progn
          (setq buffer/local (find-file-noselect filepath))
          (with-current-buffer buffer/local
            (setq value/ffns (symbol-value symbol))))
      (unless buffer/existed?
        (kill-buffer buffer/local))
      (unless file/existed?
        (delete-file filepath))
      (setq buffer/local nil))

    ;; Output results.
    (message (concat "%s:\n"
                     "  inputs:\n"
                     "    filepath:                              %s\n"
                     "    symbol: - - - - - - - - - - - - - - -  %S\n"
                     "    EXPECTED: ---------------------------> %S\n"
                     "  values:\n"
                     "    `find-file-noselect':                  %S\n"
                     "    `buffer-local-value': - - - - - - - -  %S\n"
                     "    `safe-local-variable-p':               %S\n"
                     "  info:\n"
                     "    `enable-local-variables': - - - - - -  %S\n"
                     "    symbol property `safe-local-variable': %S\n"
                     "    assoc `safe-local-variable-values':    %S")
             func/name
             ;; inputs
             filepath
             symbol
             expected
             ;; values
             value/ffns
             value/blv
             value/slvp
             ;; infos
             enable-local-variables
             (get symbol 'safe-local-variable)
             (assoc symbol safe-local-variable-values))))
;; (let* ((dir "~/dlv-test/")
;;        (filename (format "locals-%S.txt" (datetime:format 'iso-8601 'file)))
;;        (filepath (concat dir filename))
;;        (class 'int<dlv>:test:class)
;;        (mode nil)
;;        (tuple '(int<dlv>:test:variable :test/local int<dlv>:test:safe-p)))
;;   (dlv:set class dir mode tuple)
;;   (dlv:check filepath 'int<dlv>:test:variable :test/local))


;;------------------------------------------------------------------------------
;; Show Local Variable Lists
;;------------------------------------------------------------------------------

(defun dlv:cmd:buffer-locals:show/all ()
  "Show all the buffer-local variables and values for the buffer.

NOTE: Probably a huge alist!

Output to '*Messages*' buffer."
  (interactive)
  (let ((func/name "dlv:cmd:buffer-locals:show/all")
        ;; (func/tags '(:cmd :info))
        )
    (int<dlv>:message:boxed.xml :start func/name
                                (cons "buffer" (buffer-name))
                                (cons "path" (buffer-file-name)))
    (message "")

    (pp (buffer-local-variables))

    (message "")
    (int<dlv>:message:boxed.xml :end func/name)))


(defun dlv:cmd:buffer-locals:show/dlvs ()
  "Show dir-locals(/file-locals) from the buffer-locals alist.

A very much reduced list from `dlv:buffer-locals:show/all'.

Output to '*Messages*' buffer."
  (interactive)
  (let ((func/name "dlv:cmd:buffer-locals:show/dlvs")
        ;; (func/tags '(:cmd :info))
        )
    (int<dlv>:message:boxed.xml :start func/name
                                (cons "buffer" (buffer-name))
                                (cons "path" (buffer-file-name)))
    (message "")

    (let* ((all-locals (buffer-local-variables))
           (dir-locals (alist-get 'dir-local-variables-alist
                                  all-locals))
           (file-locals (alist-get 'file-local-variables-alist
                                   all-locals)))

      (int<dlv>:message:line ?─)
      (message "dir-local-variables-alist:")
      (pp dir-locals)

      (message "")
      (int<dlv>:message:line ?─)
      (message "file-local-variables-alist:")
      (pp file-locals))

    (message "")
    (int<dlv>:message:boxed.xml :end func/name)))


(defun dlv:cmd:dir-locals/show/all (filepath)
  "Show all DLVs for FILEPATH.

Output to '*Messages*' buffer."
  (interactive (list (read-directory-name "Path: "
                                          buffer-file-name)))
  (let ((func/name "dlv:cmd:buffer-locals:show/all")
        ;; (func/tags '(:cmd :info))
        dlv.classes
        dlv.dir-cache
        dlv.class-alist
        dlv.safe-local-vars)
    (pp (dir-locals-find-file filepath)
        (lambda (char) (setq dlv.classes (cons char dlv.classes))))
    (pp dir-locals-directory-cache
        (lambda (char) (setq dlv.dir-cache (cons char dlv.dir-cache))))
    (pp dir-locals-class-alist
        (lambda (char) (setq dlv.class-alist (cons char dlv.class-alist))))
    (pp safe-local-variable-values
        (lambda (char) (setq dlv.safe-local-vars (cons char dlv.safe-local-vars))))

    (let ((char-to-str (lambda (chars)
                         (concat (nreverse chars))))
          (indent (lambda (str &optional indent)
                    (let* ((indent-amt (or indent 2))
                           (indent-fmt (format "%%%ds" indent-amt))
                           indented)
                      (dolist (line (split-string str "\n") indented)
                        (setq indented (concat indented
                                               (format indent-fmt line)
                                               "\n")))))))
      (setq dlv.classes (funcall indent (funcall char-to-str dlv.classes)))
      (setq dlv.dir-cache (funcall indent (funcall char-to-str dlv.dir-cache)))
      (setq dlv.class-alist (funcall indent (funcall char-to-str dlv.class-alist)))
      (setq dlv.safe-local-vars (funcall indent (funcall char-to-str dlv.safe-local-vars))))

    (int<dlv>:message:boxed.xml :start func/name (cons "path" filepath))
    (message "")

    (message "`enable-local-variables': %S"
             enable-local-variables)

    (int<dlv>:message:line ?─)
    (message "dir local classes:\n%s"
             dlv.classes)

    (int<dlv>:message:line ?─)
    (message "`dir-locals-directory-cache':\n%s"
             dlv.dir-cache)

    (int<dlv>:message:line ?─)
    (message "`dir-locals-class-alist':\n%s"
             dlv.class-alist)

    (int<dlv>:message:line ?─)
    (message "`safe-local-variable-values':\n%s"
             dlv.safe-local-vars)
    (int<dlv>:message:boxed.xml :end func/name)))


;;------------------------------------------------------------------------------
;; Messaging Helpers
;;------------------------------------------------------------------------------
;; TODO: move all of these to mis/nub/whatever?
;;---

(defmacro int<dlv>:message:stream.chars (stream)
  "Return a lambda that will store the supplied char into the STREAM variable."
  `(let ((int<dlv>:message:stream.chars:stream ,stream))
     (lambda (char) (setq int<dlv>:message:stream.chars:stream
                          (cons char int<dlv>:message:stream.chars:stream)))))


(defun int<dlv>:message:stream->str (stream)
  "Convert the character STREAM into a string destructively.

Uses `nreverse' on STREAM."
  (concat (nreverse stream)))


;; TODO: Should this trim (just) before returing? Won't that mess up the first indent?
(defun int<dlv>:message:indent (str &optional indent)
  "Indent each line in STR by INDENT amount (default 2).

Trim string of leading/trailing whitespace before returning."
  (let* ((indent.amt (or indent 2))
         (indent.fmt (concat "%" (number-to-string indent.amt) "s"))
         string.indented)
    (string-trim
     (dolist (line (split-string str "\n") string.indented)
       (setq string.indented (concat string.indented
                                     (format indent.fmt line)
                                     "\n"))))))


(defun int<dlv>:message:line (char &optional width)
  "Print a line of CHAR to the '*Messages*' buffer.

If WIDTH is a positive integer, output line will be WIDTH wide.
The default, otherwise, is 80.

(int<dlv>:message:line ?─ 10)
  -> \"\n──────────\n\"
(int<dlv>:message:line ?- 10)
  -> \"\n----------\n\"
(int<dlv>:message:line ?x 10)
  -> \"\nxxxxxxxxxx\n\""
  ;; Figure out what width to use.
  (let ((width.usable (if (and (integerp width) (> width 0))
                          width
                       80)))
    (message "\n%s\n"
             (make-string width.usable char))))


(defun int<dlv>:message:boxed.xml (start? title &rest kvp)
  "Print a message boxed in an ASCII double-line box.

Vaguely XML-themed format.

If START? is non-nil, it will be a start tag.
  \"<title\" [...]
If START? is nil or `:end', it will be an end tag.
  \"</title\" [...]

TITLE will be the XML 'tag name'.

KVP, if not nil, should be 2-tuples (cons) of field-name and field-value
strings.

Example:
\(int<dlv>:message:boxed.xml t \"testing\"
                            (cons \"greeting\" \"hello\")
                            (cons \"subject\" \"world\"))
╔══════════════════════════════════════════════════════════════════════════════╗
║ <testing                                                                     ║
║   greeting=\"hello\"                                                           ║
║   subject=\"world\">                                                           ║
╚══════════════════════════════════════════════════════════════════════════════╝"
  (let* ((width.total 80)
         (width.sides 2)
         (width.padding 2)
         (indent.fields 2)
         (width.usable (- 80 width.sides width.padding)) ;; 'Usable' has to account for sides of box and padding.
         (line.width.middle (make-string (- width.total width.sides) ?═))
         (format.width.title (concat "%-" (int-to-string width.usable) "s"))
         (format.width.fields (concat "%-" (int-to-string (- width.usable indent.fields)) "s")))

    ;;------------------------------
    ;; Top of box.
    ;;------------------------------
    (message "%s%s%s"
             "╔"
             line.width.middle
             "╗")

    ;;------------------------------
    ;; Box's message lines.
    ;;------------------------------
    (if (null kvp)
        ;; Just the title.
        (message "%s %s %s"
                 "║"
                 (format format.width.title
                         (concat "<"
                                 (if (memq start? '(nil :end)) "/" "")
                                 title
                                 ">"))
                 "║")

      ;; Title on first line...
      (message "%s %s %s"
               "║"
               (format format.width.title
                       (concat "<"
                               (if (memq start? '(nil :end)) "/" "")
                               title
                               ;; Don't close the tag.
                               " "))
               "║")

      ;; KVPs each on separate line.
      (let ((length.kvp (length kvp)))
        (dotimes (index length.kvp)
          (let ((key (car (elt kvp index)))
                (value (cdr (elt kvp index)))
                (final (= index (1- length.kvp))))
            (message "%s   %s %s"
                     "║"
                     (format format.width.fields (concat key
                                                         "=\""
                                                         value
                                                         "\""
                                                         (if final ">" " ")))
                     "║")))))

    ;;------------------------------
    ;; Bottom of box.
    ;;------------------------------
    (message "%s%s%s"
             "╚"
             line.width.middle
             "╝")
    nil))
;; (int<dlv>:message:boxed.xml nil "testing")
;; (int<dlv>:message:boxed.xml nil "testing" (cons "hello" "there"))
;; (int<dlv>:message:boxed.xml t "testing" (cons "greeting" "hello") (cons "subject" "world"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :dlv 'display)
