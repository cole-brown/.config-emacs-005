;;; core/modules/emacs/template/template.el --- Non-Interactive Snippets -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2023-04-26
;; Timestamp:  2023-06-26
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Non-Interactive Snippets
;;    - TODO: Name NISnippet to mimic YASnippet?
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Error Helpers
;;------------------------------------------------------------------------------

(defun int<template>:error-or-nil (no-error? format &rest args)
  "Raise error or just return nil.

If NO-ERROR? is non-nil, just return nil.
Otherwise, raise an error signal with FORMAT and ARGS."
  (declare (indent 1))
  (if no-error?
      nil
    (apply #'error format args)))


;;------------------------------------------------------------------------------
;; Template Store
;;------------------------------------------------------------------------------

(defvar int<template>:store nil
  "Storage hash table for templates.

Store is a hashtable of hashtables:
  - mode symbol or nil -> templates hash table
    - template keyword -> compiled template string")


(defun int<template>:store/create ()
  "Make a new hash table for storing templates."
  (make-hash-table :test 'eq :weakness nil))


(defun int<template>:store/set (mode template body)
  "Save BODY as the TEMPLATE for MODE.

Will overwrite the previous template if one already exists.

MODE should be one of:
  - symbol          - template is for a single mode
  - nil             - template is global (for any/all modes)

TEMPLATE should be a keyword.

BODY should be TODO TODO TODO."
  ;; Init the store?
  (unless (hash-table-p int<template>:store)
    (setq int<template>:store (int<template>:store/create)))
  ;; Add to the store.
  (if-let ((templates-for-mode (gethash mode int<template>:store)))
      ;; MODE exists; add/overwrite this TEMPLATE.
      (puthash template body templates-for-mode)
    ;; MODE does not exist; create it first.
    (setq templates-for-mode (int<template>:store/create))
    (puthash template body templates-for-mode)
    (puthash mode templates-for-mode int<template>:store)))


(defun int<template>:store/get/mode (mode &optional no-error?)
  "Get the hashtable of templates for MODE.

MODE should be one of:
  - symbol  - template is for a single mode
  - nil     - template is global (for any/all modes)
  - `:auto' - Use current major mode or global to find the template.

If no hash table for MODE exists (that is, if there are no templates for MODE),
raise an error signal unless NO-ERROR? is non-nil, in which case just return
nil."
  (let ((func/name "int<template>:store/get/mode"))
    ;; Don't bother if there's nothing to look in?
    (if (not (hash-table-p int<template>:store))
        (int<template>:error-or-nil no-error?
          "%s: No templates exist at all"
          func/name)
      ;; Have hash table; try to find the template.
      (if-let ((templates/mode (gethash mode int<template>:store)))
          ;; Have something for the mode; keep going!
          templates/mode
        (int<template>:error-or-nil no-error?
          "%s: No templates exist for mode `%S'"
          func/name
          mode)))))
;; (int<template>:store/get/mode 'org-mode)


(defun int<template>:store/get (mode template &optional no-error?)
  "Get the template for MODE and TEMPLATE.

MODE should be one of:
  - symbol  - template is for a single mode
  - nil     - template is global (for any/all modes)
  - `:auto' - Use current major mode or global to find the template.

If no template exists, raise an error signal unless NO-ERROR? is non-nil, in
which case just return nil."
  (let ((func/name "int<template>:store/get"))
    (if-let ((templates/mode (int<template>:store/get/mode mode int<template>:store)))
          ;; Have something for the mode; keep going!
          (if-let ((value (gethash template templates/mode)))
              ;; Found it; return it!
              value
            ;; Error / return nil
            (int<template>:error-or-nil no-error?
              "%s: Template `%S' does not exist for mode `%S'"
              func/name
              template
              mode))
      ;; Error / return nil
        (int<template>:error-or-nil no-error?
          "%s: No templates exist for mode `%S'"
          func/name
          mode))))
;; (int<template>:store/get 'org-mode :src)
;; (int<template>:store/get 'org-mode :src :no-error)


(defun int<template>:store/find (buffer mode template &optional no-error?)
  "Find matching templates for MODE and TEMPLATE.

BUFFER should be a buffer name, buffer object, or nil (for `current-buffer').

MODE should be one of:
  - symbol  - template is for a single mode
  - nil     - template is global (for any/all modes)
  - `:auto' - Use current major mode or global to find the template.

If no template exists, raise an error signal unless NO-ERROR? is non-nil, in
which case just return nil.

Return list of matching templates in best-match order."
  (let ((func/name "int<template>:store/find")
        (buffer (if buffer
                    (get-buffer buffer)
                  (current-buffer)))
        found)
    (with-current-buffer buffer
      ;; Keyword? Recurse to gather everything.
      (cond ((keywordp mode)
             (pcase mode
               (:auto
                ;; 3rd: Filter out any nils from our results.
                (setq found
                      (seq-filter (lambda (x) (not (null x)))
                                  (append
                                   ;; 1st: Look for a template for this buffer's current mode.
                                   (int<template>:store/find buffer
                                                             major-mode
                                                             template
                                                             :no-error)
                                   ;; 2nd: Look for a global template.
                                   (int<template>:store/find buffer
                                                             nil
                                                             template
                                                             :no-error)))))
               (_
                (int<template>:error-or-nil no-error?
                  "%s: Unknown MODE keyword `%S'! Valid: %S"
                  func/name
                  mode
                  '(:auto)))))

            ;; Global template?
            ;; Major mode template?
            ;; Same code actually.
            ((or (null mode)
                 (symbolp mode))
             ;; Skip checking if the mode supplied matches the current major mode.
             ;; Could add it here if it seems useful.
             (when-let ((temp (int<template>:store/get mode template no-error?)))
               (push temp found)))

            ;; Fallthrough to a Nice Error:
            (t
             (int<template>:error-or-nil no-error?
               "%s:  MODE must be a symbol, nil, or `:auto'. Got '%s': %S"
               func/name
               (type-of mode)
               mode)))
      found)))
;; (int<template>:store/get 'org-mode :src)
;; (int<template>:store/find nil 'org-mode :src)
;; (int<template>:store/find nil :auto :src)


(defun int<template>:store/find/mode (buffer mode &optional no-error?)
  "Get all templates for MODE.

BUFFER should be a buffer name, buffer object, or nil (for `current-buffer').

MODE should be one of:
  - symbol  - template is for a single mode
  - nil     - template is global (for any/all modes)
  - `:auto' - Use current major mode or global to find the template.

If no template exists, raise an error signal unless NO-ERROR? is non-nil, in
which case just return nil.

Return list of matching templates in best-match order."
  (let ((func/name "int<template>:store/find/mode")
        (buffer (if buffer
                    (get-buffer buffer)
                  (current-buffer)))
        found)
    (with-current-buffer buffer
      ;; Keyword? Recurse to gather everything.
      (cond ((keywordp mode)
             (pcase mode
               (:auto
                ;; 3rd: Filter out any nils from our results.
                (setq found
                      (seq-filter (lambda (x) (not (null x)))
                                  (append
                                   ;; 1st: Look for a template for this buffer's current mode.
                                   (int<template>:store/find/mode buffer
                                                                  major-mode
                                                                  :no-error)
                                   ;; 2nd: Look for a global template.
                                   (int<template>:store/find/mode buffer
                                                                  nil
                                                                  :no-error))))
                ;; And now, if we still have nothing and should complain loudly,
                ;; complain loudly:
                (unless found
                  (int<template>:error-or-nil no-error?
                    "%s: `%S' mode: No templates found in global or `%S'!"
                    func/name
                    mode
                    major-mode)))
               (_
                (int<template>:error-or-nil no-error?
                  "%s: Unknown MODE keyword `%S'! Valid: %S"
                  func/name
                  mode
                  '(:auto)))))

            ;; Global template?
            ;; Major mode template?
            ;; Same code actually.
            ((or (null mode)
                 (symbolp mode))
             ;; Skip checking if the mode supplied matches the current major mode.
             ;; Could add it here if it seems useful.
             (let ((templates/mode (int<template>:store/get/mode mode no-error?)))
               (when (hash-table-p templates/mode)
                 (maphash (lambda (keyword template)
                            "Gather KEYWORD/TEMPLATE pairs for mode into results."
                            (push (cons keyword template) found))
                          templates/mode))))

            ;; Fallthrough to a Nice Error:
            (t
             (int<template>:error-or-nil no-error?
               "%s:  MODE must be a symbol, nil, or `:auto'. Got '%s': %S"
               func/name
               (type-of mode)
               mode)))
      found)))
;; (int<template>:store/get 'org-mode :src)
;; (int<template>:store/find/mode nil 'org-mode)
;; (int<template>:store/find/mode nil :auto :no-error)
;; (int<template>:store/find/mode nil :auto)


;;------------------------------------------------------------------------------
;; Regions
;;------------------------------------------------------------------------------

(defun int<template>:region/get ()
  "Get start/end of active region.

If no active region, returned start/end will both be `point'.

Return a cons of: (start . end)"
  (cons (if (region-active-p) (region-beginning) (point))
        (if (region-active-p) (region-end)       (point))))
;; (int<template>:region/get)


(defun int<template>:region/delete ()
  "Delete region if active."
  ;; ...don't think we want to `save-excursion' or `save-mark-and-excursion'?
  ;; Cuz we want to end up where the delete ends up if we deleted anything.
  (let* ((region (int<template>:region/get))
         (start  (car region))
         (end    (cdr region)))
    (when (> end start)
      (delete-region start end))))


;;------------------------------------------------------------------------------
;; Template Formatting
;;------------------------------------------------------------------------------

(defun int<template>:body/compile (body)
  "Compile list of BODY strings into a finalized template string.

Return compiled template string."
  (cond ((stringp body)
         body)
        ((listp body)
         (mapconcat #'identity
                    body
                    "\n"))
        (t
         (error "%s: Cannot compile BODY to a template string! BODY: %S"
                "int<template>:body/compile"
                body))))
;; (int<template>:body/compile '("#+begin_src %s" "%s" "#+end_src" ""))


(defun int<template>:format (formatting args)
  "Format a template's FORMATTING string with ARGS.

Finalize ARGS before apply to FORMATTING string.

Return the formatted string."
  (apply #'format
         formatting
         (int<template>:args/finalize args)))


(defun int<template>:args/finalize (args)
  "Process ARGS and return finalized args.

Replace with actual strings:
  - `:yank'
  - `:selection'
  - `:selection-else-yank'"
  (seq-map (lambda (arg)
             (str:propertize:remove
              (pcase arg
                (:yank
                 (current-kill 0))
                (:selection
                 (buffer-substring-no-properties (mark) (point)))
                (:selection-else-yank
                 (or (buffer-substring-no-properties (mark) (point))
                     (current-kill 0)))
                (_
                 arg))))
           args))
;; there
;; (int<template>:args/finalize '("hello " :yank))


;;------------------------------------------------------------------------------
;; Buffer Helpers
;;------------------------------------------------------------------------------

(defun int<template>:insert (buffer mode template args &optional no-error?)
  "Insert TEMPLATE at point/region with ARGS.

BUFFER should be a buffer name, buffer object, or nil (for `current-buffer').

TEMPLATE should be a keyword.

MODE should be one of:
  - symbol  - template is for a single mode
  - nil     - template is global (for any/all modes)
  - `:auto' - Use current major mode or global to find the template.

ARGS should be a list of the template's formatting arguments.

Use non-nil NO-ERROR? to get a nil return instead of an error signal on errors."
  (let ((func/name "int<template>:insert")
        (templates (int<template>:store/find buffer mode template)))
    ;;------------------------------
    ;; Error Checks
    ;;------------------------------
    (unless templates
      (int<template>:error-or-nil no-error?
        "%s: No templates found for mode/template: `%S'/`%S'"
        func/name
        mode
        template))

    ;;------------------------------
    ;; Insert
    ;;------------------------------
    (int<template>:region/delete)
    (insert (int<template>:format (nth 0 templates) args))))


;;------------------------------------------------------------------------------
;; API
;;------------------------------------------------------------------------------

(defun template:define (template modes &rest body)
  "Save BODY as a TEMPLATE for MODES.

Overwrite any existing template for those modes.

MODES should be one of:
  - symbol          - template is for a single mode
  - list of symbols - template is for multiple modes
  - nil             - template is global (for any/all modes)

TEMPLATE should be a keyword.

BODY should be TODO:template: TODO TODO."
  (declare (indent 2))
  (let ((func/name "template:define")
        (modes/input modes))
    ;;------------------------------
    ;; Error Checks
    ;;------------------------------
    (unless (keywordp template)
      (error "%s: TEMPLATE must be a keyword. Got `%S': %S"
             func/name
             (type-of template)
             template))

    ;; Normalize MODES to a list.
    (cond ((null modes)
           ;; nil is a list, so don't need to normalize.
           t)
          ((symbolp modes)
           (setq modes (list modes))))
    ;; Now MODES must be a list of symbols or nil/empty list.
    (dolist (mode modes)
      (unless (symbolp mode)
        (error "%s: MODES must be a symbol, list of symbols, or nil. Found `%S' %S in %S (%S)"
               func/name
               (type-of mode)
               mode
               modes
               modes/input)))

    ;; Body has to be... something?
    (unless body
      (error "%s: Template requires a BODY" func/name))

    ;;------------------------------
    ;; Create Template
    ;;------------------------------

    ;; "Compile" body.
    (let ((compiled (int<template>:body/compile body)))
      ;; Save in each mode it belongs to.
      (dolist (mode modes)
        (int<template>:store/set mode template compiled)))

    ;; Return... what? Doesn't matter currently... but something is nicer than nil when debugging?
    (list template modes)))
;; (template:define :src 'org-mode "#+begin_src %s" "%s" "#+end_src" "")


(defun template:insert (template mode &rest args)
  "Insert TEMPLATE at point/region with ARGS.

TEMPLATE should be a keyword.

MODE should be one of:
  - symbol  - template is for a single mode
  - nil     - template is global (for any/all modes)
  - `:auto' - Use current major mode or global to find the template.

ARGS should be the template's formatting arguments."
  (int<template>:insert (current-buffer)
                        mode
                        template
                        args))
;; (template:insert :src 'org-mode 'elisp "(message \"hello there\")")
;; hello here
;; (template:insert :src 'org-mode 'elisp :yank)


(defmacro template:cmd:insert (template &rest args)
  "Create and return a command to insert TEMPLATE with ARGS at point/region."
  `(lambda ()
     ,(format "Insert template '%s' at point/region in current buffer."
              template)
     (interactive)
     (int<template>:insert nil   ; current buffer
                           :auto ; current major mode or global
                           ,template
                           '(,@args))))
;; (funcall (template:cmd:insert :src 'elisp :yank))


;; ;; TODO:template: is there even any way to build a list of templates defined for current mode?
;; ;; Cuz currently the command to insert a template is where the template's args are defined.
;; (defun template:cmd:choose ()
;;   "Choose from global or the mode's templates."
;;   ;; Let the error bubble up to user if nothing found for this mode.
;;   (when-let ((templates (int<template>:store/find/mode nil      ; current buffer
;;                                                        :auto))) ; current major mode or global
;;     ;; TODO:template: provide a prompt w/ completion to select which one to use
;;     (ignore templates)))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :template 'template)
