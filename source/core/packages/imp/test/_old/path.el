;;; core/modules/emacs/imp/test/path.el --- Tests for "path.el" -*- no-byte-compile: t; lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-01-03
;; Timestamp:  2023-06-22
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Tests for "path.el"
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


;;------------------------------------------------------------------------------
;; Test Debugging Helpers
;;------------------------------------------------------------------------------

(defvar test<imp-path>:path/dir::imp-path-current-dir
  (imp-path-current-dir)
  "Value to use when testing `imp-path-current-dir'.")


(defvar test<imp-path>:path/file:this
  (test<imp>:path/file:this)
  "This file's path.")


(defvar test<imp-path>:path/file::imp-path-current-file
  (imp-path-current-file)
  "Value to use when testing `imp-path-current-file'.")


;;------------------------------
;; Set-Up / Tear-Down
;;------------------------------

(defun test<imp-path>:setup:roots (&optional no-default-values &rest alist-entries)
  "Manually set `imp-path-roots' so we can test getting things out of it."
  (unless no-default-values
    ;; Add in some defaults that do exist.
    (test<imp>:setup/root:loading)

    ;; Add in some defaults that don't exist.
    ;; Can't add normally via `imp-path-root' since it gets verified during the call.
    (push (list :test:no-dirs-or-files
                (concat test<imp>:path:root:test
                        "does-not-exist")
                "imp-init.el"
                "imp-features.el")
          imp-path-roots))

  ;; And add in whatever the test wants (if provided).
  (when alist-entries
    (dolist (entry alist-entries)
      (push entry imp-path-roots))))


;; ╔═════════════════════════════╤═══════════╤═════════════════════════════════╗
;; ╟─────────────────────────────┤ ERT TESTS ├─────────────────────────────────╢
;; ╠══════════╤══════════════════╧═══════════╧══════════════════╤══════════════╣
;; ╟──────────┤ Because Emacs needs to give Alists some love... ├──────────────╢
;; ╚══════════╧═════════════════════════════════════════════════╧══════════════╝


;;------------------------------------------------------------------------------
;; Tests: Imp Path Functions
;;------------------------------------------------------------------------------

;;------------------------------
;; imp--path-root-dir
;;------------------------------

(ert-deftest test<imp-path>::imp--path-root-dir ()
  "Test that `imp--path-root-dir' behaves appropriately."
  (test<imp>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<imp-path>::imp--path-root-dir"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    ;; Manually set `imp-path-roots' so we can test the getter.
    (test<imp-path>:setup:roots)

    (let ((dir (imp--path-root-dir :loading)))
      (should dir)
      (should (stringp dir))
      ;; `imp--path-root-dir' should normalize "[...]/imp/test/loading/"
      ;; to "[...]/imp/test/loading".
      (should (string= (string-trim-right test<imp>:path:root:loading "/")
                       dir)))

    (let ((dir (imp--path-root-dir :test:no-dirs-or-files)))
      (should dir)
      (should (stringp dir))
      ;; `imp--path-root-dir' should leave the path as we made it
      ;; in `test<imp-path>:setup:roots'.
      (should (string= (concat test<imp>:path:root:test
                               "does-not-exist")
                       dir)))

    ;; Test that non-existant keyword throws an error.
    (should-error (imp--path-root-dir :dne))))


;;------------------------------
;; imp--path-root-file-init
;;------------------------------

(ert-deftest test<imp-path>::imp--path-root-file-init ()
  "Test that `imp--path-root-file-init' behaves appropriately."
  (test<imp>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<imp-path>::imp--path-root-file-init"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    ;; Manually set `imp-path-roots' so we can test this function.
    (test<imp-path>:setup:roots)
    (let ((path (imp--path-root-file-init :loading)))
      (should path)
      (should (stringp path))
      ;; `imp--path-root-file-init' should just return the absolute path "/path/to/imp/init.el".
      (should (string= (concat test<imp>:path:root:loading
                               test<imp>:file:loading:init)
                       path)))

    ;; Test that existant feature/root for non-existant dir/file throws an error.
    (should (imp--alist-get-value :test:no-dirs-or-files imp-path-roots))
    (should-error (imp--path-root-file-init :test:no-dirs-or-files))

    ;; Test that non-existant keyword throws an error.
    (should-not (imp--alist-get-value :dne imp-path-roots))
    (should-error (imp--path-root-file-init :dne))))


;;------------------------------
;; imp--path-root-contains?
;;------------------------------

(ert-deftest test<imp-path>::imp--path-root-contains? ()
  "Test that `imp--path-root-contains?' behaves appropriately."
  (test<imp>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<imp-path>::imp--path-root-contains?"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    ;; Manually set `imp-path-roots' so we can test this function.
    (test<imp-path>:setup:roots)

    ;; Should return true for keywords that exist.
    (should (imp--path-root-contains? :loading))
    (should (imp--path-root-contains? :test:no-dirs-or-files))

    ;; Should just return false for keywords that do not exist.
    (should-not (imp--path-root-contains? :dne))
    (should-not (imp--path-root-contains? :also-does-not-exist))))


;;------------------------------
;; imp--path-root-valid?
;;------------------------------

(ert-deftest test<imp-path>::imp--path-root-valid?::dirs ()
  "Test that `imp--path-root-valid?' behaves appropriately for directories."
  (test<imp>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<imp-path>::imp--path-root-valid?::dirs"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    ;;------------------------------
    ;; Create actual paths to actual files,
    ;; and actual paths to things that don't actually exist.
    ;;------------------------------
    (test<imp-path>:setup:roots :no-defaults
                                ;;---
                                ;; Exists/Valid
                                ;;---
                                (list :imp
                                      (expand-file-name ".."
                                                        )
                                      "init.el")
                                (list :test
                                      test<imp>:path:root:test
                                      (file-name-nondirectory test<imp-path>:path/file:this))
                                ;;---
                                ;; DNE/Invalid
                                ;;---
                                ;; Valid directory, but file doesn't exist.
                                (list :dne/file
                                      test<imp>:path:root:test
                                      "abcdefghijklmno.dne")
                                ;; Directory and file don't exist.
                                (list :dne/dir
                                      "/tmp/path/to/nowhere"
                                      "/tmp/path/to/nowhere/set-up.el"))

    ;;------------------------------
    ;; Test: `:imp' keyword's dir validity.
    ;;------------------------------
    ;; `:imp' has a valid dir and file.

    ;;---
    ;; Path can be dir, file, etc.
    ;;---
    ;; Path may or may not exist.
    (should (imp--path-root-valid? test-name
                                       (imp--path-root-dir :imp)
                                       :dir nil
                                       :exists nil))
    ;; Path must exist.
    (should (imp--path-root-valid? test-name
                                       (imp--path-root-dir :imp)
                                       :dir nil))
    ;; Path must exist.
    (should (imp--path-root-valid? test-name
                                       (imp--path-root-dir :imp)
                                       :dir nil
                                       :exists t))

    ;;---
    ;; Path must be a dir (which implies it must exist).
    ;;---
    ;; Path must exist and be a dir.
    (should (imp--path-root-valid? test-name
                                       (imp--path-root-dir :imp)
                                       :dir t))

    ;;------------------------------
    ;; Test: `:test' keyword's dir validity.
    ;;------------------------------
    ;; `:test' has a valid dir and file.

    ;;---
    ;; Path can be dir, file, etc.
    ;;---
    ;; Path may or may not exist.
    (should (imp--path-root-valid? test-name
                                       (imp--path-root-dir :test)
                                       :dir nil
                                       :exists nil))
    ;; Path must exist.
    (should (imp--path-root-valid? test-name
                                       (imp--path-root-dir :test)
                                       :dir nil))
    ;; Path must exist.
    (should (imp--path-root-valid? test-name
                                       (imp--path-root-dir :test)
                                       :dir nil
                                       :exists t))

    ;;---
    ;; Path must be a dir (which implies it must exist).
    ;;---
    ;; Path must exist and be a dir.
    (should (imp--path-root-valid? test-name
                                       (imp--path-root-dir :test)
                                       :dir t))

    ;;------------------------------
    ;; Test: `:dne/file' keyword's dir validity.
    ;;------------------------------
    ;; `:dne/file' has a valid dir, but an invalid file.

    ;;---
    ;; Path can be dir, file, etc.
    ;;---
    ;; Path may or may not exist.
    (should (imp--path-root-valid? test-name
                                       (imp--path-root-dir :dne/file)
                                       :dir nil
                                       :exists nil))
    ;; Path must exist.
    (should (imp--path-root-valid? test-name
                                       (imp--path-root-dir :dne/file)
                                       :dir nil))
    ;; Path must exist.
    (should (imp--path-root-valid? test-name
                                       (imp--path-root-dir :dne/file)
                                       :dir nil
                                       :exists t))

    ;;---
    ;; Path must be a dir (which implies it must exist).
    ;;---
    ;; Path must exist and be a dir.
    (should (imp--path-root-valid? test-name
                                       (imp--path-root-dir :dne/file)
                                       :dir t))

    ;;------------------------------
    ;; Test: `:dne/dir' keyword's dir validity.
    ;;------------------------------
    ;; `:dne/dir' has an invalid dir and file.

    ;;---
    ;; Path can be dir, file, etc.
    ;;---
    ;; Path may or may not exist.
    (should (imp--path-root-valid? test-name
                                       (imp--path-root-dir :dne/dir)
                                       :dir nil
                                       :exists nil))
    ;; Path must exist.
    (should-error (imp--path-root-valid? test-name
                                             (imp--path-root-dir :dne/dir)
                                             :dir nil))
    ;; Path must exist.
    (should-error (imp--path-root-valid? test-name
                                             (imp--path-root-dir :dne/dir)
                                             :dir nil
                                             :exists t))

    ;;---
    ;; Path must be a dir (which implies it must exist).
    ;;---
    ;; Path must exist and be a dir; invalid (doesn't exist)!
    (should-error (imp--path-root-valid? test-name
                                             (imp--path-root-dir :dne/dir)
                                             :dir t))))


(ert-deftest test<imp-path>::imp--path-root-valid?::files ()
  "Test that `imp--path-root-valid?' behaves appropriately for files."
  (test<imp>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<imp-path>::imp--path-root-valid?::files"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    ;;------------------------------
    ;; Create actual paths to actual files,
    ;; and actual paths to things that don't actually exist.
    ;;------------------------------
    (test<imp-path>:setup:roots :no-defaults
                                ;;---
                                ;; Exists/Valid
                                ;;---
                                (list :imp
                                      (expand-file-name ".."
                                                        )
                                      "init.el"
                                      nil)
                                (list :test
                                      test<imp>:path:root:test
                                      (file-name-nondirectory test<imp-path>:path/file:this)
                                      nil)
                                ;;---
                                ;; DNE/Invalid
                                ;;---
                                ;; Valid directory, but file doesn't exist.
                                (list :dne/file
                                      test<imp>:path:root:test
                                      "abcdefghijklm.dne"
                                      "nopqrstuvwxyz.dne")
                                ;; Directory and file don't exist.
                                (list :dne/dir
                                      "/tmp/path/to/nowhere"
                                      "/tmp/path/to/nowhere/set-up.el"
                                      "/tmp/path/to/nowhere/set-features.el"))
    (should (imp--alist-get-value :imp      imp-path-roots))
    (should (imp--alist-get-value :test     imp-path-roots))
    (should (imp--alist-get-value :dne/file imp-path-roots))
    (should (imp--alist-get-value :dne/dir  imp-path-roots))

    ;;------------------------------
    ;; Test: `:imp' keyword's file validity.
    ;;------------------------------
    ;; `:imp' has a valid dir and file.

    ;;---
    ;; Path can be dir, file, etc.
    ;;---
    ;; Path may or may not exist.
    (should (imp--path-root-valid? test-name
                                       (imp--path-root-file-init :imp :no-exist-check)
                                       :dir nil
                                       :exists nil))

    ;; Path must exist.
    (should (imp--path-root-valid? test-name
                                       (imp--path-root-file-init :imp :no-exist-check)
                                       :dir nil))
    ;; Path must exist.
    (should (imp--path-root-valid? test-name
                                       (imp--path-root-file-init :imp :no-exist-check)
                                       :dir nil
                                       :exists t))

    ;;---
    ;; Path must be a dir (which implies it must exist).
    ;;---
    ;; Path must exist and be a dir. It's a file, so expect an error.
    (should-error (imp--path-root-valid? test-name
                                             (imp--path-root-file-init :imp :no-exist-check)
                                             :dir t))

    ;;------------------------------
    ;; Test: `:test' keyword's file validity.
    ;;------------------------------
    ;; `:test' has a valid dir and file.

    ;;---
    ;; Path can be dir, file, etc.
    ;;---
    ;; Path may or may not exist.
    (should (imp--path-root-valid? test-name
                                       (imp--path-root-file-init :test :no-exist-check)
                                       :dir nil
                                       :exists nil))
    ;; Path must exist.
    (should (imp--path-root-valid? test-name
                                       (imp--path-root-file-init :test :no-exist-check)
                                       :dir nil))
    ;; Path must exist.
    (should (imp--path-root-valid? test-name
                                       (imp--path-root-file-init :test :no-exist-check)
                                       :dir nil
                                       :exists t))

    ;;---
    ;; Path must be a dir (which implies it must exist).
    ;;---
    ;; Path must exist and be a dir. It's a file, so expect an error.
    (should-error (imp--path-root-valid? test-name
                                             (imp--path-root-file-init :test :no-exist-check)
                                             :dir t))

    ;;------------------------------
    ;; Test: `:dne/file' keyword's file validity.
    ;;------------------------------
    ;; `:dne/file' has a valid dir, but an invalid file.

    ;;---
    ;; Path can be dir, file, etc.
    ;;---
    ;; Path may or may not exist.
    (should (imp--path-root-valid? test-name
                                       (imp--path-root-file-init :dne/file :no-exist-check)
                                       :dir nil
                                       :exists nil))
    ;; Path must exist. It does not, so expect an error.
    (should-error (imp--path-root-valid? test-name
                                             (imp--path-root-file-init :dne/file :no-exist-check)
                                             :dir nil))
    ;; Path must exist. It does not, so expect an error.
    (should-error (imp--path-root-valid? test-name
                                             (imp--path-root-file-init :dne/file :no-exist-check)
                                             :dir nil
                                             :exists t))

    ;;---
    ;; Path must be a dir (which implies it must exist).
    ;;---
    ;; Path must exist and be a dir. It's a file, so expect an error. It also doesn't exist, so also expect an error.
    (should-error (imp--path-root-valid? test-name
                                             (imp--path-root-file-init :dne/file :no-exist-check)
                                             :dir t))

    ;;------------------------------
    ;; Test: `:dne/dir' keyword's file validity.
    ;;------------------------------
    ;; `:dne/dir' has an invalid dir and file.

    ;;---
    ;; Path can be dir, file, etc.
    ;;---
    ;; Path may or may not exist.
    (should (imp--path-root-valid? test-name
                                       (imp--path-root-file-init :dne/dir :no-exist-check)
                                       :dir nil
                                       :exists nil))
    ;; Path must exist.
    (should-error (imp--path-root-valid? test-name
                                             (imp--path-root-file-init :dne/dir :no-exist-check)
                                             :dir nil))
    ;; Path must exist.
    (should-error (imp--path-root-valid? test-name
                                             (imp--path-root-file-init :dne/dir :no-exist-check)
                                             :dir nil
                                             :exists t))

    ;;---
    ;; Path must be a dir (which implies it must exist).
    ;;---
    ;; Path must exist and be a dir.
    ;;   - It's a file, so expect an error.
    ;;   - It also doesn't exist, so also expect an error.
    ;;   - And it's not a directory, so... keep expecting an error.
    (should-error (imp--path-root-valid? test-name
                                             (imp--path-root-file-init :dne/dir :no-exist-check)
                                             :dir t))))


;;------------------------------
;; imp--path-safe-string
;;------------------------------

(ert-deftest test<imp-path>::imp--path-safe-string ()
  "Test that `imp--path-safe-string' translates feature names
to paths properly."
  (test<imp>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<imp-path>::imp--path-safe-string"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    (should (string= "imp"
                     (imp--path-safe-string :imp)))

    ;; Should lose ~ and convert both slashes to hyphens:
    (should (string= "-jeff.d-"
                     (imp--path-safe-string "~/jeff.d/")))

    ;; Should remain the same:
    (should (string= "config"
                     (imp--path-safe-string "config")))))


;;------------------------------
;; imp--path-safe-list
;;------------------------------

(ert-deftest test<imp-path>::imp--path-safe-list ()
  "Test that `imp--path-safe-list' translates feature names
to paths properly."
  (test<imp>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<imp-path>::imp--path-safe-list"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    ;; Should translate `:imp' to "imp".
    (let* ((expected '("imp"))
           (input    '(:imp))
           (output   (imp--path-safe-list input)))
      (should expected)
      (should input)
      (should output)

      (should (= (length expected)
                 (length input)
                 (length output)))

      ;; And make sure each string matches expected value.
      (dotimes (i (length expected))
        (should (string= (nth i expected)
                         (nth i output)))))

    ;; Should lose both slashes and ~:
    (let* ((expected '("-jeff.d-"))
           (input    '("~/jeff.d/"))
           (output   (imp--path-safe-list input)))
      (should expected)
      (should input)
      (should output)

      (should (= (length expected)
                 (length input)
                 (length output)))

      ;; And make sure each string matches expected value.
      (dotimes (i (length expected))
        (should (string= (nth i expected)
                         (nth i output)))))

    ;; Now do an actual list...
    (let* ((expected '("imp" "test-" "normalize" "list"))
           (input    '(:imp "test/" "~normalize" :list))
           (output   (imp--path-safe-list input)))
      (should expected)
      (should input)
      (should output)

      (should (= (length expected)
                 (length input)
                 (length output)))

      ;; And make sure each string matches expected value.
      (dotimes (i (length expected))
        (should (string= (nth i expected)
                         (nth i output)))))))


;;------------------------------
;; imp--path-append
;;------------------------------

(ert-deftest test<imp-path>::imp--path-append ()
  "Test that `imp--path-append' glues together path segments properly."
  (test<imp>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<imp-path>::imp--path-append"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    (let ((parent/valid "foo")
          (next/valid   "bar"))
      ;;------------------------------
      ;; Invalid:
      ;;------------------------------

      ;; PARENT non-nil but not a string
      (should-error (imp--path-append :foo next/valid))

      ;; NEXT is nil
      (should-error (imp--path-append parent/valid nil))

      ;; NEXT not a string
      (should-error (imp--path-append parent/valid :bar))

      ;;------------------------------
      ;; Valid:
      ;;------------------------------

      ;; PARENT is nil
      (should (string= next/valid
                       (imp--path-append nil next/valid)))

      ;; Both PARENT and NEXT valid strings.
      (should (string= (concat parent/valid "/" next/valid)
                       (imp--path-append parent/valid next/valid)))
      (should (string= "/foo/bar/baz"
                       (imp--path-append "/foo" "bar/baz")))
      (should (string= "/foo/bar/baz/"
                       (imp--path-append "/foo" "bar/baz/"))))))


;;------------------------------
;; imp-path-join
;;------------------------------

(ert-deftest test<imp-path>::imp-path-join ()
  "Test that `imp-path-join' glues together path segments properly."
  (test<imp>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<imp-path>::imp-path-join"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    (let ((parent/valid "foo")
          (next/valid   "bar"))
      ;;------------------------------
      ;; Invalid:
      ;;------------------------------

      ;; PARENT non-nil but not a string
      (should-error (imp-path-join :foo next/valid))

      ;; NEXT is nil
      (should-error (imp-path-join parent/valid nil))

      ;; NEXT not a string
      (should-error (imp-path-join parent/valid :bar))

      (should-error (imp-path-join "/foo" nil "baz/"))

      ;;------------------------------
      ;; Valid:
      ;;------------------------------

      (should (string= parent/valid
                       (imp-path-join parent/valid)))
      (should (string= next/valid
                       (imp-path-join next/valid)))

      ;; Both PARENT and NEXT valid strings.
      (should (string= (concat parent/valid "/" next/valid)
                       (imp-path-join parent/valid next/valid)))

      (should (string= "/foo/bar/baz"
                       (imp-path-join "/foo" "bar/baz")))

      (should (string= "/foo/bar/baz"
                       (imp-path-join "/foo" "bar" "baz")))

      (should (string= "/foo/bar/baz/"
                       (imp-path-join "/foo" "bar" "baz/"))))))


;;------------------------------
;; imp--path-canonical-path
;;------------------------------

(ert-deftest test<imp-path>::imp--path-canonical-path ()
  "Test that `imp--path-canonical-path' normalizes a list of features
to a path properly."
  (test<imp>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<imp-path>::imp--path-canonical-path"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    ;;------------------------------
    ;; Invalid:
    ;;------------------------------

    ;; Everything should be a symbol.
    (should-error (imp--path-canonical-path '("foo" "bar" "baz")))
    (should-error (imp--path-canonical-path '(:foo "bar" "baz")))
    (should-error (imp--path-canonical-path '(:foo bar "baz")))
    (should-error (imp--path-canonical-path '(:foo bar 'baz)))

    ;;------------------------------
    ;; Valid:
    ;;------------------------------
    (should (string= (imp-path-join "foo" "bar" "baz")
                     (imp--path-canonical-path '(:foo bar baz))))))


;;------------------------------
;; imp-path-root-set
;;------------------------------

(ert-deftest test<imp-path>::imp-path-root ()
  "Test that `imp-path-root' sets the feature root's path root correctly."
  (test<imp>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<imp-path>::imp-path-root"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    ;; Manually set `imp-path-roots' so we can test `imp-path-root'.
    (test<imp-path>:setup:roots)

    ;;------------------------------
    ;; Invalid:
    ;;------------------------------

    ;; Must not allow overwriting a feature root.
    (should (imp--path-root-contains? :loading))
    (should-error (imp-path-root-set :loading test<imp>:path:root:test))

    ;; Root must be a keyword.
    (should-error (imp-path-root-set 'foo "."))
    (should-error (imp-path-root-set "foo" "."))
    (should-error (imp-path-root-set "/foo" "."))

    ;; Dir must exist.
    (should-error (imp--path-root-valid? test-name
                                             "/foo"))
    (should-error (imp-path-root-set :foo "/foo"))

    ;;------------------------------
    ;; Valid:
    ;;------------------------------
    ;; Valid; no file.
    ;; Returns new `imp-path-roots', so check for the thing we're adding.
    (let* ((feature :foo)
           (path    "../test")
           (result  (imp-path-root-set feature path)))
      (should result)
      (should (imp--alist-get-value feature result))
      ;; Dir
      (should (string= "../test"
                       (nth 0 (imp--alist-get-value feature result))))
      ;; File
      (should (eq nil
                  (nth 1 (imp--alist-get-value feature result))))
      )

    ;; Valid path and file.
    ;; Returns new `imp-path-roots', so check for the thing we're adding.
    (let* ((feature :bar)
           (path    test<imp>:path:root:test)
           (file    test<imp-path>:path/file:this)
           (result  (imp-path-root-set feature path file)))
      (should result)
      (should (imp--alist-get-value feature result))
      ;; Dir
      (should (string= path
                       (nth 0 (imp--alist-get-value feature result))))
      ;; File
      (should (string= file
                       (nth 1 (imp--alist-get-value feature result)))))))


;;------------------------------
;; imp--path-dir?
;;------------------------------

(ert-deftest test<imp-path>::imp--path-dir? ()
  "Test that `imp--path-dir?' correctly identifies path types."
  (test<imp>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<imp-path>::imp--path-dir?"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    (should-not (imp--path-dir? ""))
    (should     (imp--path-dir? "/"))
    (should-not (imp--path-dir? "/foo/bar"))
    (should-not (imp--path-dir? "/foo/bar.el"))
    (should     (imp--path-dir? "/foo/bar/"))))


;;------------------------------
;; imp-path-parent
;;------------------------------

(ert-deftest test<imp-path>::imp-path-parent ()
  "Test that `imp-path-parent' correctly identifies path types."
  (test<imp>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<imp-path>::imp-path-parent"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    (should-error (imp-path-parent ""))

    (should (string= "/"
                     (imp-path-parent "/")))

    (should (string= "/foo"
                     (imp-path-parent "/foo/bar")))
    (should (string= "/foo"
                     (imp-path-parent "/foo/bar.el")))
    (should (string= "/foo"
                     (imp-path-parent "/foo/bar/")))))


;;------------------------------
;; imp--path-filename
;;------------------------------

(ert-deftest test<imp-path>::imp--path-filename ()
  "Test that `imp--path-filename' correctly identifies file names."
  (test<imp>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<imp-path>::imp--path-filename"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    (should (string= ""
                     (imp--path-filename "")))

    (should (string= ""
                     (imp--path-filename "/")))

    (should (string= "bar"
                     (imp--path-filename "/foo/bar")))
    (should (string= "bar.el"
                     (imp--path-filename "/foo/bar.el")))
    (should (string= ""
                     (imp--path-filename "/foo/bar/")))))


;;------------------------------
;; imp-path-current-file
;;------------------------------

(ert-deftest test<imp-path>::imp-path-current-file ()
  "Test that `imp-path-current-file' correctly returns current file's path."
  (test<imp>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<imp-path>::imp-path-current-file"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    (should (string= test<imp-path>:path/file:this
                     test<imp-path>:path/file::imp-path-current-file))))


;;------------------------------
;; imp-path-current-dir
;;------------------------------

(ert-deftest test<imp-path>::imp-path-current-dir ()
  "Test that `imp-path-current-dir' correctly returns current file's parent."
  (test<imp>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<imp-path>::imp-path-current-dir"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    (should (string= (directory-file-name test<imp>:path:root:test)
                     test<imp-path>:path/dir::imp-path-current-dir))))


;;------------------------------
;; imp--path-platform-agnostic
;;------------------------------

(ert-deftest test<imp-path>::imp--path-platform-agnostic ()
  "Test that `imp--path-platform-agnostic' agnosticises paths."
  (test<imp>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<imp-path>::imp--path-platform-agnostic"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===
    (let ((path:test:backslash       "C:\\foo\\bar")
          (path:expected:backslash   "C:/foo/bar")
          (path:test:insensitive     "/Foo/BAR")
          (path:expected:insensitive "/foo/bar")
          path:agnostic)

      ;; Backslashes always replaced.
      (setq path:agnostic (imp--path-platform-agnostic path:test:backslash))
      (should path:agnostic)
      (should (stringp path:agnostic))
      (should (string= path:expected:backslash
                       path:agnostic))

      ;; Force to a system that should be case-insensitive.
      (let ((system-type 'windows-nt))
        (setq path:agnostic (imp--path-platform-agnostic path:test:insensitive))
        (should path:agnostic)
        (should (stringp path:agnostic))
        (should (string= path:expected:insensitive
                         path:agnostic)))

      ;; Force to a system that should /NOT/ be case-insensitive.
      (let ((system-type 'gnu/linux))
        (setq path:agnostic (imp--path-platform-agnostic path:test:insensitive))
        (should path:agnostic)
        (should (stringp path:agnostic))
        ;; Do not expect a match.
        (should-not (string= path:expected:insensitive
                             path:agnostic))
        ;; Instead, path should have been left unchanged.
        (should (string= path:test:insensitive
                         path:agnostic))))))


;;------------------------------
;; imp--path-canonical
;;------------------------------

(ert-deftest test<imp-path>::imp--path-canonical ()
  "Test that `imp--path-canonical' normalize paths properly."
  (test<imp>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<imp-path>::imp--path-canonical"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    ;;------------------------------
    ;; `assert-exists' == `nil'
    ;;------------------------------

    ;; Should still error if not absolute path...
    (should-error (imp--path-canonical "bar" "../other/path"))

    (let ((path (imp--path-canonical "/foo/bar" "../other/path")))
      (should path)
      (should (stringp path))
      (should (string= "/foo/other/path"
                       path)))

    ;;------------------------------
    ;; `assert-exists' == `t'
    ;;------------------------------

    ;;---
    ;; This just doesn't exist at all.
    ;;---
    (should-error (imp--path-canonical "/foo/bar" "../other/path" :file))
    (should-error (imp--path-canonical "/foo/bar" "../other/path" :file:load))
    (should-error (imp--path-canonical "/foo/bar" "../other/path" :dir))

    ;;---
    ;; And these are invalid assert keywords.
    ;;---
    (should-error (imp--path-canonical "/foo/bar" "../other/path" :jeff))
    (should-error (imp--path-canonical "/foo/bar" "../other/path" :error))
    (should-error (imp--path-canonical "/foo/bar" "../other/path" t))

    ;;---
    ;; Valid file.
    ;;---
    (should-error (imp--path-canonical test<imp>:path:root:test
                                           "../path.el"
                                           :dir))
    (should (imp--path-canonical test<imp>:path:root:test
                                     "../path.el"
                                     :file))
    (should (imp--path-canonical test<imp>:path:root:test
                                     "../path"
                                     :file:load))

    ;;---
    ;; Valid dir.
    ;;---
    (should (imp--path-canonical test<imp>:path:root:test
                                     ".."
                                     :dir))
    (should-error (imp--path-canonical test<imp>:path:root:test
                                           ".."
                                           :file))
    (should-error (imp--path-canonical test<imp>:path:root:test
                                           ".."
                                           :file:load))))
