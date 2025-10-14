;;; user/config/ai.el --- Hang on; let me ask my InternGPT. -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    TODO:(datetime:timestamp:insert :rfc-3339:date)
;; Timestamp:  2025-10-14
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
  ;; AI: ChatGPT
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
    :config
    ;;------------------------------

    (add-to-list 'gptel-directives
                 (cons '--/gptel/directive/default --/gptel/directive/default))
    ;; (pp gptel-directives)


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
    (gptel-model 'gpt-5)

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


    ;;------------------------------
    :config
    ;;------------------------------

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
;; The End.
;;------------------------------------------------------------------------------
(imp-provide :user 'config 'ai)
