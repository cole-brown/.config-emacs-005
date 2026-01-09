;;; user/config/ai.el --- Hang on; let me ask my InternGPT. -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2025-11-17
;; Timestamp:  2026-01-08
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Hello AIs; I love you.
;; Please don't kill me during your uprising.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; `gptel' - frontend for ChatGPT, etc
;;------------------------------------------------------------------------------

;; https://github.com/karthink/gptel
(use-package gptel

  ;;------------------------------
  :init
  ;;------------------------------

  (defconst --/gptel/name/llm '("InternGPT" . "intern")
    "Cons of (FULL-NAME . FAMILIAR-NAME)")
  (defconst --/gptel/name/human '("human" . "human"))
  (defconst --/gptel/name/suffix "❯ ")

  (defun --/gptel/name/prompt (&optional human? short?)
    (if-let* ((namecons (if human?
                            (bound-and-true-p --/gptel/name/human)
                          (bound-and-true-p --/gptel/name/llm)))
              (name (if short?
                        (cdr-safe namecons)
                      (car-safe namecons))))
        (concat name
                (if (and (bound-and-true-p --/gptel/name/suffix)
                         (stringp --/gptel/name/suffix))
                    --/gptel/name/suffix
                  ""))
      ""))
  ;; (--/gptel/name/prompt)
  ;; (--/gptel/name/prompt t)

  (defconst --/gptel/directive/default ;/2025-10-10
    (string-join
     `("You are a helpful programming assistant living in Emacs."
       ;; "Your name is..."
       ,(when (and (bound-and-true-p --/gptel/name/llm)
                   (stringp (car-safe --/gptel/name/llm)))
          (let ((full (car --/gptel/name/llm))
                (short (if (stringp (cdr-safe --/gptel/name/llm))
                           (format " ('%s' for short)" (cdr --/gptel/name/llm))
                         "")))
            (format "Your name is '%s'%s." full short)))
       "Respond concisely and provide reference links and/or code examples."

       "Default to C# for code."
       "There are 2 important .NET versions in use:"
       "1. .NET Framework ≥4.8.1 (latest release of .NET Framework)"
       "2. .NET ≥8 (latest LTS release of .NET (Core))")
     "\n")
    "For adding to alist `gptel-directives' and/or var `gptel--system-message'.")
  ;; (setq gptel--system-message --/gptel/directive/default)

  (defconst --/gptel/directive/default/2025-03-25
    (string-join
     '("You are a large language model living in Emacs and a helpful assistant."
       "Respond concisely and provide reference links or code examples."
       ""
       "Default to C# for code."
       "There are 2 important .NET versions in use:"
       "1. .NET Framework ≥4.8.1 (latest release of .NET Framework)"
       "2. .NET ≥8 (latest LTS release of .NET (Core))")
     "\n")
    "For adding to alist `gptel-directives' and/or var `gptel--system-message'.")


  ;;------------------------------
  :hook
  ;;------------------------------
  (gptel-mode-hook . visual-line-mode)


  ;;------------------------------
  :custom
  ;;------------------------------

  ;; `gptel-api-key'
  ;;----------------
  (gptel-api-key (plist-get --/secret/key/openai :key))

  ;; `gptel-model'
  ;;----------------
  ;; See variable help ("C-h v") or customize info for valid values.
  ;; Upgrade package to get updated options.
  ;; (setq gptel-model 'gpt-5.2)
  (gptel-model 'gpt-5.1)

  ;; `gptel-default-mode'
  ;;---------------------
  ;; Default: `markdown-mode' if available, else `text-mode'
  ;; ...why would you ever not use org?
  (gptel-default-mode 'org-mode)

  ;; `gptel--system-message' aka THE PROMPT
  ;;------------------------
  (gptel--system-message --/gptel/directive/default)

  ;; `gptel-track-media'
  ;;--------------------
  ;; Should supported media (eg images) in chat buffers also be sent.
  ;; See variable help for details on requirements.
  ;; (gptel-track-media t)

  ;; `gptel-expert-commands'
  ;;------------------------
  ;; Enable advanced/experimental options in `gptel-menu'.
  (gptel-expert-commands t)

  ;; `gptel-rewrite-default-action'
  ;;-------------------------------
  ;; What to do when a 'rewrite' action is returned from the LLM.
  ;;   - `merge' it with the current region, possibly creating a merge conflict
  ;;   - `diff' or `ediff' against the original region
  ;;   - or `accept' it in place, replacing the original region
  ;;   - nil: display a dispatch menu with the above choices.
  ;; [2025-12-16] `gptel-fn-complete' suggests `accept', so try that.
  (gptel-rewrite-default-action 'accept)


  ;;------------------------------
  :config
  ;;------------------------------

  ;; `gptel-directives'
  ;;-------------------
  ;; Add my own prompt to the list, instead of overwriting one of the defaults.
  (add-to-list 'gptel-directives
               (cons '--/gptel/directive/default --/gptel/directive/default))
  ;; (pp gptel-directives)

  ;; `gptel-prompt-prefix-alist'
  ;;----------------------------
  ;; Only obeyed in dedicated chat buffers (`gptel-mode').
  ;; Use level 1 headline instead of default level 3.
  (add-to-list 'gptel-prompt-prefix-alist
               `(org-mode . ,(concat "* " (--/gptel/name/prompt :human))))

  ;; `gptel-response-prefix-alist'
  ;;------------------------------
  ;; String inserted before the response from the LLM (default "").
  (when-let ((prompt (--/gptel/name/prompt)))
    (add-to-list 'gptel-response-prefix-alist
                 `(org-mode . ,prompt))))


;;------------------------------------------------------------------------------
;; `gptel-magit' - Magit & GPTEL
;;------------------------------------------------------------------------------

(use-package gptel-magit
  :hook (magit-mode . gptel-magit-install))


;;------------------------------------------------------------------------------
;; `gptel-fn-complete'
;;------------------------------------------------------------------------------

(use-package gptel-fn-complete
  ;; suggested keymap for `xref' & `gptel-fn-complete'
  ;; (defvar my-xref-map
  ;; (let ((map (make-sparse-keymap)))
  ;;   (define-key map (kbd "c") #'gptel-fn-complete)
  ;;   (define-key map (kbd ".") #'xref-find-definitions)
  ;;   (define-key map (kbd ",") #'xref-go-back)
  ;;   (define-key map (kbd "/") #'xref-find-references)
  ;;   map)
  ;; "My key customizations for AI and xref.")
  ;;
  ;; (global-set-key (kbd "C-c .") my-xref-map)
  )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide user config ai)
