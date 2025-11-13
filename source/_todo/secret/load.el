;;; core/modules/system/secret/load.el --- Load Secrets -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2023-06-26
;; Timestamp:  2023-06-26
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Load the Secret
;;
;;; Code:


(imp:require :nub)
(imp:require :jerky)
(imp:require :path)


;;------------------------------------------------------------------------------
;; Helper Functions for Loading Files
;;------------------------------------------------------------------------------

;; TODO: Move to nub or mis or str or plist/collection?
(defun int<system/secret>:plist:pretty-string (plist)
  "Pretty print PLIST to string and return the string."
  (let ((plist-too (copy-sequence plist))
        (width/key 0)
        output)

    ;; Figure out format width.
    (while plist
      (let ((key (pop plist))
            (_ (pop plist))) ;; Don't care about value (yet).
        (setq width/key (max width/key
                             (length (symbol-name key))))))

    ;; Format each line.
    (let ((fmt/line (concat "  " ;; leading indent
                            "%" (number-to-string width/key) "S"
                            " "
                            "%S")))
      (while plist-too
        (let ((key (pop plist-too))
              (value (pop plist-too)))
          (push (format fmt/line key value) output))))

    ;; Combine lines and output.
    (mapconcat #'identity
               (nreverse output)
               "\n")))
;; (system:secret:validate :load "init")
;; (int<system/secret>:plist:pretty-string (system:secret:validate :load "init"))


(defun int<system/secret>:validate-and-load (caller feature file)
  "Load FEATURE from FILE for CALLER function.

FEATURE should be list of keyword/symbols for `imp:load'.

FILE should be filepath relative to secret's load directory.

CALLER should be a string of calling function's name.

Returns nil/non-nil for loading success.
Outputs warning to 'nub' warning buffer if secret fail validation."
  (let* ((plist (system:secret:validate :load file)))
    (if-let* ((plist plist)
              (success   (plist-get plist :success))
              (path/load (plist-get plist :path/file/load)))
        ;; Validated successfully and have load path; try to load.
        ;; Success/Failure Return Value: `imp:load' return value.
        (imp:load :feature  feature
                  :path     path/load)

      ;; Failure Message.
      (nub:warning
          :system/secret
          caller
        '(:line:each
          "%s: Cannot load secret '%s'; invalid system secrets."
          "Validation Result:\n%s")
        "[SKIP]"
        file
        (int<system/secret>:plist:pretty-string plist))
      ;; Failure Return Value
      nil)))

;;------------------------------------------------------------------------------
;; Initialization
;;------------------------------------------------------------------------------

(defun system:secret:init (&rest feature)
  "Load secret's root init.el.

FEATURE must be the imp feature name (keyword and symbols)."
  (int<system/secret>:validate-and-load "system:secret:init"
                                        feature
                                        "init"))
;; (system:secret:init)


;;------------------------------------------------------------------------------
;; Configuration
;;------------------------------------------------------------------------------

(defun system:secret:config (&rest feature)
  "Configure this system's secrets.

FEATURE must be the imp feature name (keyword and symbols)."
  (int<system/secret>:validate-and-load "system:secret:config"
                                        feature
                                        "config"))
;; (system:secret:config)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :system 'secret 'load)
