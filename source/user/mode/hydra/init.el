;;; user/mode/hydra/nesting.el --- Hydra Inception -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-07-13
;; Timestamp:  2025-11-17
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Hydra Inception:
;;
;; 1. Call one hydra from another hydra.
;; 2. Nest hydras inside of each other.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Nesting Hydra Helpers
;;------------------------------------------------------------------------------
;; https://github.com/abo-abo/hydra/wiki/Nesting-Hydras

;; (defhydra test-a (:post (message "post a"))
;;   "a"
;;   ("q" test-b/body "to b" :exit t))
;;
;; (defhydra test-b (:body-pre (message "pre b"))
;;   "b"
;;   ("q" nil "exit" :exit t))


(defun hydra:nest (hydra)
  "Call entry function for defined HYDRA.

For us in:
  - Entering a hydra from another hydra.
  - Entering a hydra from keybinds.
  - Not needing to remember the name of your HYDRA's entry function.

NOTE: Use `:exit t' or appropriate color for exiting the caller hydra!"
  (funcall (intern-soft (concat (symbol-name hydra) "/body"))))
;; (defun test/body () (message "hello there"))
;; (hydra:nest 'test)


(defalias 'hydra:call 'hydra:nest)


;;------------------------------------------------------------------------------
;; Hydra Stacks
;;------------------------------------------------------------------------------
;; https://github.com/abo-abo/hydra/wiki/Nesting-Hydras

(defvar _:hydra:stack nil
  "A list of the stack of hydras currently in use.")


(defun _:hydra:push (func)
  "Add the hydra FUNC to the stack."
  (push func _:hydra:stack))


(defun hydra:push (hydra)
  "Add the HYDRA to the stack and enter its `<hydra>/body'.

Example:
  With a hydra named `s:hydra:qux':
    (hydra:push 's:hydra:qux)
      - Will push to the stack: '(s:hydra:qux/body)"
  ;; Create function call to `hydra' body function.
  (let ((hydra/body (intern-soft (concat (symbol-name hydra) "/body"))))
    ;; Call the hydra, then push to stack to save state.
    (funcall 'hydra/body)
    (push hydra/body _:hydra:stack)))


(defun hydra:pop ()
  "Pop a hydra body off of `_:hydra:stack' if one exists and call it."
  (interactive)
  (let ((hydra/func/body (pop _:hydra:stack)))
    (when hydra/func/body
      (funcall hydra/func/body))))


;;------------------------------
;; Example:
;;------------------------------
;; teal: default to exit on hydra heads
;;   - Think just used to make example more compact?
;;
;; (defhydra hydra-a (:color teal)
;;   "a"
;;   ("b" (progn
;;          (hydra-b/body)
;;          (hydra-push '(hydra-a/body)))
;;        "visit hydra-b")
;;   ("c" (progn
;;          (hydra-c/body)
;;          (hydra-push '(hydra-a/body)))
;;        "visit hydra-c")
;;   ("i" (message "I am a") :exit nil)
;;   ("q" hydra-pop "exit"))
;;
;; (defhydra hydra-b (:color teal)
;;   "b"
;;   ("a" (progn
;;          (hydra-a/body)
;;          (hydra-push '(hydra-b/body)))
;;        "visit hydra-a")
;;   ("c" (progn
;;          (hydra-c/body)
;;          (hydra-push '(hydra-b/body)))
;;        "visit hydra-c")
;;   ("i" (message "I am b") :exit nil)
;;   ("q" hydra-pop "exit"))
;;
;; (defhydra hydra-c (:color teal)
;;   "c"
;;   ("a" (progn
;;          (hydra-a/body)
;;          (hydra-push '(hydra-c/body)))
;;        "visit hydra-a")
;;   ("b" (progn
;;          (hydra-b/body)
;;          (hydra-push '(hydra-c/body)))
;;        "visit hydra-b")
;;   ("i" (message "I am c") :exit nil)
;;   ("q" hydra-pop "exit"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp-provide user mode hydra init) ; nesting)
