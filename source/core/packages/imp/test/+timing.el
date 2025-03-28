;;; core/modules/emacs/imp/test/+timing.el --- Tests for "+timing.el" -*- no-byte-compile: t; lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-01-25
;; Timestamp:  2023-06-22
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Tests for "+timing.el"
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Test Requirements
;;------------------------------------------------------------------------------

(test<imp>:init:load "base.el")

(test<imp>:init:load "../feature.el")
(test<imp>:init:load "../alist.el")
(test<imp>:init:load "../tree.el")
(test<imp>:init:load "../path.el")
(test<imp>:init:load "../+timing.el")
(test<imp>:init:load "../provide")
(test<imp>:init:load "../load.el")
;; Require up to load so we can actually time something.


;;------------------------------------------------------------------------------
;; Variables
;;------------------------------------------------------------------------------

(defconst test<imp/timing>:buffer:name "test<imp/timing>:output"
  "Special name for our timing tests' output buffer.")


(defvar test<imp/timing>:buffer:name:backup nil
  "Backup `imp/timing-enabled?' so we can test it and then restore it.")


(defvar test<imp/timing>:enabled?:backup nil
  "Backup `imp/timing-enabled?' so we can test it and then restore it.")


(defvar test<imp/timing>:enabled?:test nil
  "Save `imp/timing-enabled?' after a test so we can check it for debugging.")


(defvar test<imp/timing>:feature?:backup nil
  "Backup `imp/timing-feature?' so we can test it and then restore it.")


(defvar test<imp/timing>:feature?:test nil
  "Save `imp/timing-feature?' after a test so we can check it for debugging.")


;;------------------------------------------------------------------------------
;; Timing Test Helpers
;;------------------------------------------------------------------------------

(defun test<imp/timing>:buffer ()
  "Returns the timing buffer if it exists, else nil."
  (get-buffer imp/timing-buffer-name))


(defun test<imp/timing>:buffer:kill ()
  "Kill the timing buffer if it exists."
  (when-let ((buffer (test<imp/timing>:buffer)))
    (kill-buffer buffer)))
;; (get-buffer-create imp/timing-buffer-name)
;; (test<imp/timing>:buffer:kill)


(defun test<imp/timing>:buffer:has (string)
  "Returns non-nil if timing buffer exists and contains STRING."
  (when-let ((buffer (test<imp/timing>:buffer)))
    (with-current-buffer buffer
      (goto-char (point-min))
      (search-forward string nil t))))


(defun test<imp/timing>:enable (&rest plist)
  "Enable/disable timing by feature flag and/or setting variable.

To modify feature flag, use plist keyword `:feature'.
To modify setting variable, use plist keyword `:setting'.

Use a boolean (nil/non-nil) for the values - it will be set as-is."
  (when (plist-member plist :feature)
    (setq imp/timing-feature? (plist-get plist :feature)))

  (when (plist-member plist :setting)
    (setq imp/timing-enabled? (plist-get plist :setting))))


;;------------------------------------------------------------------------------
;; Set-Up / Tear-Down
;;------------------------------------------------------------------------------

(defun test<imp/timing>:buffer:mark (name begin?)
  "If timing buffer is the *Messages* buffer, inserts a begin or end marker.

Else kills timing buffer if BEGIN? is non-nil (test is just starting)."
  (if (imp--timing-buffer-messages?)
      ;;------------------------------
      ;; *Messages* buffer.
      ;;------------------------------
      ;; If it's the *Messages* buffer, just insert some
      ;; "test is starting/ending" marker.
      (with-current-buffer "*Messages*"
        (let* ((string:left        (if begin?
                                       "╠═begin═╣ "
                                     "╠═end═══╣ "))
               (string:right/start " ╠")
               (char:right/pad     ?═)
               (string:right/stop   "╣")
               (padding (- (window-total-width)
                           ;; Be smaller than absolute max width...
                           10
                           ;; Subtract out the test name string.
                           (length name)
                           ;; Subtract out all the planned chars
                           (length string:left)
                           (length string:right/start)
                           (length string:right/stop))))
          ;; Make and insert the marker.
          (message (concat "\n"
                          string:left
                          name
                          string:right/start
                          (make-string padding char:right/pad :multibyte)
                          string:right/stop
                          "\n"))))

    ;;------------------------------
    ;; Non-*Messages* buffer.
    ;;------------------------------
    ;; It's a special imp-timing-only buffer.
    ;; Kill it if a new test is starting so the test has a clean slate.
    ;; Leave it alone at end of test so we can view it.
    (when begin?
      (imp/cmd-timing-buffer-kill t))))


(defun test<imp/timing>:setup (name)
  "Set-up for timing tests."
  (setq test<imp/timing>:enabled?:backup    imp/timing-enabled?
        test<imp/timing>:feature?:backup    imp/timing-feature?
        test<imp/timing>:buffer:name:backup imp/timing-buffer-name
        imp/timing-enabled?                 nil
        imp/timing-feature?                 nil
        imp/timing-buffer-name              test<imp/timing>:buffer:name
        test<imp/timing>:enabled?:test      nil
        test<imp/timing>:feature?:test      nil)

  ;; Deal with timing buffer.
  (test<imp/timing>:buffer:mark name t))


(defun test<imp/timing>:teardown (name)
  "Clean up vars from timing tests."
  ;; Deal with timing buffer.
  (test<imp/timing>:buffer:mark name nil)

  (setq test<imp/timing>:enabled?:test      imp/timing-enabled?
        test<imp/timing>:feature?:test      imp/timing-feature?
        imp/timing-enabled?                 test<imp/timing>:enabled?:backup
        imp/timing-feature?                 test<imp/timing>:feature?:backup
        imp/timing-buffer-name              test<imp/timing>:buffer:name:backup
        test<imp/timing>:enabled?:backup    nil
        test<imp/timing>:feature?:backup    nil
        test<imp/timing>:buffer:name:backup nil))


;; ╔═════════════════════════════╤═══════════╤═════════════════════════════════╗
;; ╟─────────────────────────────┤ ERT TESTS ├─────────────────────────────────╢
;; ╠═════════════════════╤═══════╧═══════════╧═══════╤═════════════════════════╣
;; ╟─────────────────────┤ You're taking too long... ├─────────────────────────╢
;; ╚═════════════════════╧═══════════════════════════╧═════════════════════════╝


;;------------------------------------------------------------------------------
;; Tests: Imp Timing Functions
;;------------------------------------------------------------------------------

;;------------------------------
;; imp/timing-enabled?
;;------------------------------

(ert-deftest test<imp/timing>::imp/timing-enabled? ()
  "Test that `imp/timing-enabled?' behaves appropriately."
  (test<imp>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<imp/timing>::imp/timing-enabled?"
      #'test<imp/timing>:setup
      #'test<imp/timing>:teardown

    ;;===
    ;; Run the test.
    ;;===

    (test<imp/timing>:enable :feature nil :setting nil)
    (should-not (imp/timing-enabled?))

    (test<imp/timing>:enable :feature t :setting nil)
    (should (imp/timing-enabled?))

    (test<imp/timing>:enable :feature nil :setting t)
    (should (imp/timing-enabled?))

    (test<imp/timing>:enable :feature t :setting t)
    (should (imp/timing-enabled?))

    (test<imp/timing>:enable :feature :foo :setting :bar)
    (should (imp/timing-enabled?))))


;;------------------------------
;; imp--timing-tree-type
;;------------------------------

(ert-deftest test<imp/timing>::imp--timing-tree-type ()
  "Test that `imp--timing-tree-type' behaves appropriately."
  (test<imp>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<imp/timing>::imp--timing-tree-type"
      #'test<imp/timing>:setup
      #'test<imp/timing>:teardown

    ;;===
    ;; Run the test.
    ;;===
    (should (string= (imp--timing-tree-type :root 0)
                     ""))

    (should (string= (imp--timing-tree-type :root 1)
                     "├─"))

    (should (string= (imp--timing-tree-type :root 2)
                     "│ "))

    (should (string= (imp--timing-tree-type :leaf 0)
                     "└─"))

    (should (string= (imp--timing-tree-type :leaf 1)
                     "│ "))

    (should (string= (imp--timing-tree-type :leaf 2)
                     "│ "))))


;;------------------------------
;; imp--timing-tree-string
;;------------------------------

(ert-deftest test<imp/timing>::imp--timing-tree-string ()
  "Test that `imp--timing-tree-string' behaves appropriately."
  (test<imp>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<imp/timing>::imp--timing-tree-string"
      #'test<imp/timing>:setup
      #'test<imp/timing>:teardown

    ;;===
    ;; Run the test.
    ;;===
    (let ((imp--timing-indent 0))
      (should (string= (imp--timing-tree-string :root)
                       ""))
      (should (string= (imp--timing-tree-string :leaf)
                     "└─")))

    (let ((imp--timing-indent 1))
      (should (string= (imp--timing-tree-string :root)
                       "├─"))
      (should (string= (imp--timing-tree-string :leaf)
                       "│ └─")))

    (let ((imp--timing-indent 2))
      (should (string= (imp--timing-tree-string :root)
                       "│ ├─"))
      (should (string= (imp--timing-tree-string :leaf)
                       "│ │ └─")))))


;;------------------------------
;; imp--timing-buffer-insert
;;------------------------------

(ert-deftest test<imp/timing>::imp--timing-buffer-insert ()
  "Test that `imp--timing-buffer-insert' behaves appropriately."
  (test<imp>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<imp/timing>::imp--timing-buffer-insert"
      #'test<imp/timing>:setup
      #'test<imp/timing>:teardown

    ;;===
    ;; Run the test.
    ;;===

    ;;------------------------------
    ;; With Timing Disabled
    ;;------------------------------
    (should-not (test<imp/timing>:buffer))
    (test<imp/timing>:enable :feature nil :setting nil)

    (let ((test-string "Hello there."))
      (imp--timing-buffer-insert test-string)
      ;; Buffer should still not exist since timing is disabled.
      (should-not (test<imp/timing>:buffer)))

    ;;------------------------------
    ;; With Timing Enable
    ;;------------------------------
    (test<imp/timing>:enable :feature nil :setting t)

    (let ((test-string "Hello there."))
      (imp--timing-buffer-insert test-string)
      (should (test<imp/timing>:buffer))
      (test<imp/timing>:buffer:has test-string))))


;;------------------------------
;; imp/timing
;;------------------------------

(ert-deftest test<imp/timing>::imp/timing ()
  "Test that `imp/timing' behaves appropriately."
  (test<imp>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<imp/timing>::imp/timing"
      #'test<imp/timing>:setup
      #'test<imp/timing>:teardown

    ;;===
    ;; Run the test.
    ;;===

    (let* ((body-counter 0)
           (timed-body-fn (lambda ()
                            (setq body-counter (1+ body-counter))
                            ;; Delay a tiny bit so we can check the timing output?
                            (sleep-for 0 10))))

      ;;------------------------------
      ;; With Timing Disabled
      ;;------------------------------
      (should-not (test<imp/timing>:buffer))
      (should (= body-counter 0))

      (test<imp/timing>:enable :feature nil :setting nil)
      (imp/timing
          ;; These don't matter; just for output messages.
          test<imp>:feature:loading:doesnt-exist
          test<imp>:file:loading:doesnt-exist
          test<imp>:path:root:loading
        ;; Run the body.
        (funcall timed-body-fn))

      ;; Nothing output, because no timing enabled.
      (should-not (test<imp/timing>:buffer))
      ;; Body still ran.
      (should (= body-counter 1))

      ;;------------------------------
      ;; With Timing Enabled
      ;;------------------------------
      (test<imp/timing>:enable :setting t)
      (imp/timing
          ;; These don't matter; just for output messages.
          test<imp>:feature:loading:doesnt-exist
          test<imp>:file:loading:doesnt-exist
          test<imp>:path:root:loading
        ;; Run the body.
        (funcall timed-body-fn))

      ;; Something output this time.
      (should (test<imp/timing>:buffer))
      ;; Has loading message pieces.
      (test<imp/timing>:buffer:has "loading")
      (test<imp/timing>:buffer:has
       (format "%S"
               (imp--feature-normalize-display test<imp>:feature:loading:doesnt-exist)))
      ;; Has timing/loaded message pieces.
      (test<imp/timing>:buffer:has "└─00.0") ;; Should take ~0.01x seconds but don't care much.
      (test<imp/timing>:buffer:has "seconds")

      ;; Body ran again.
      (should (= body-counter 2)))))
