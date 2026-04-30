;;; path.el --- Tests for imp/path.el -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:

;; Focused ERT coverage for `source/core/packages/imp/path.el`.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)

(defvar imp-roots nil)
(defvar imp-features nil)

(defconst imp-path-test--dir
  (file-name-directory (or load-file-name buffer-file-name)))

(defconst imp-path-test--imp-dir
  (expand-file-name ".." imp-path-test--dir))

(defun imp-path-test--load (name)
  "Load imp helper file NAME from the imp package directory."
  (load (expand-file-name name imp-path-test--imp-dir) nil 'nomessage))

(imp-path-test--load "settings")
(imp-path-test--load "fundamental")
(imp-path-test--load "output")
(imp-path-test--load "error")
(imp-path-test--load "debug")
(imp-path-test--load "list")
(imp-path-test--load "feature")
(imp-path-test--load "path")

(defmacro imp-path-test--with-temp-dir (dir &rest body)
  "Bind DIR to a fresh temporary directory for BODY."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,dir (make-temp-file "imp-path-test-" t)))
     (unwind-protect
         (progn ,@body)
       (delete-directory ,dir t))))

(defun imp-path-test--write-file (path &optional contents)
  "Create PATH and write CONTENTS to it."
  (make-directory (file-name-directory path) t)
  (with-temp-file path
    (insert (or contents "")))
  path)

(defmacro imp-path-test--with-current-dir (dir &rest body)
  "Evaluate BODY with `imp-path-current-dir' bound to DIR."
  (declare (indent 1) (debug (symbolp body)))
  `(cl-letf (((symbol-function 'imp-path-current-dir)
              (lambda () ,dir)))
     ,@body))

;;------------------------------------------------------------------------------
;; TESTS
;;------------------------------------------------------------------------------

(ert-deftest imp-path-test:/join/flattens-and-normalizes-symbols ()
  (should (equal (imp-path-join "/tmp" '("alpha" (:beta gamma)) "delta.el")
                 "/tmp/alpha/beta/gamma/delta.el")))

(ert-deftest imp-path-test:/join/simple-cases ()
  (should (string= (imp-path-join "/foo" "bar.el")
                   "/foo/bar.el"))
  (should (string= (imp-path-join '("/foo" ("bar.el")))
                   "/foo/bar.el"))
  (should (string= (imp-path-join "foo" "bar.el")
                   "foo/bar.el"))
  (should (string= (imp-path-join "foo")
                   "foo"))

  (should-error (imp-path-join nil nil))
  (should (eq (let (imp-path-error?) (imp-path-join nil))
              nil)))

(ert-deftest imp-path-test:/split/simple-cases ()
  (should (equal (imp-path-split "/path/to/some/where.txt")
                 '("path" "to" "some" "where.txt")))

  (let ((system-type 'windows-nt))
    (should (string= (imp-path-split "C:\\path\\to\\some\\where.txt")
                     '("C:" "path" "to" "some" "where.txt"))))

  (should-error (imp-path-split :foo))
  (should (eq (let (imp-path-error?) (imp-path-split :foo))
              nil)))

;; todo here


(ert-deftest imp-path-test-normalizes-relative-paths-from-current-dir ()
  (imp-path-test--with-temp-dir root
    (let ((expected (expand-file-name "alpha/beta.el" root)))
      (imp-path-test--with-current-dir root
        (should (equal (imp-path "alpha" "beta.el") expected)))
      (should (equal (imp-path-abbreviate expected)
                     (abbreviate-file-name expected))))))

(ert-deftest imp-path-test-canonical-resolves-dotdot-and-trailing-slash ()
  (should (equal (imp-path-canonical "../gamma/" "/tmp/alpha/beta/")
                 "/tmp/alpha/gamma")))

(ert-deftest imp-path-test-relative-uses-string-root-feature-root-and-user-emacs-directory ()
  (imp-path-test--with-temp-dir root
    (let* ((project (expand-file-name "project" root))
           (child (expand-file-name "nested/file.el" project))
           (outside (expand-file-name "outside/file.el" root))
           (user-emacs-directory project)
           (imp-roots (list (list 'project-root project))))
      (imp-path-test--with-current-dir project
        (should (equal (imp--path-relative project child) "nested/file.el"))
        (should (equal (imp--path-relative 'project-root child) "nested/file.el"))
        (should (equal (imp--path-relative nil child) "nested/file.el"))
        (should (equal (imp--path-relative project outside) outside))
        (should-error (imp--path-relative project outside :error))))))

(ert-deftest imp-path-test-relative-rejects-invalid-root-and-path-when-errors-enabled ()
  (should-error (imp--path-relative "relative/root" "/tmp/example.el" :error))
  (should-error (imp--path-relative nil :not-a-string :error)))

(ert-deftest imp-path-test-relative-joins-and-canonicalizes-before-comparing ()
  (imp-path-test--with-temp-dir root
    (let ((project (expand-file-name "project" root)))
      (let ((default-directory (file-name-as-directory project)))
        (imp-path-test--with-current-dir project
          (should (equal (imp-path-relative project "module" ".." "module" "file.el")
                         "module/file.el")))))))

(ert-deftest imp-path-test-root-valid-checks-existence-and-directory-shape ()
  (imp-path-test--with-temp-dir root
    (let ((dir (expand-file-name "dir" root))
          (file (expand-file-name "dir/file.el" root))
          (missing (expand-file-name "missing" root)))
      (make-directory dir)
      (imp-path-test--write-file file ";; test\n")
      (should (imp--path-root-valid? "test" dir))
      (should (imp--path-root-valid? "test" file :dir nil))
      (should (imp--path-root-valid? "test" missing :exists nil :dir nil))
      (should-error (imp--path-root-valid? "test" missing))
      (should-error (imp--path-root-valid? "test" file)))))

(ert-deftest imp-path-test-parent-and-filename-handle-files-and-directories ()
  (should (equal (imp-path-parent "/tmp/alpha/beta.el") "/tmp/alpha"))
  (should (equal (imp-path-parent "/tmp/alpha/beta/") "/tmp/alpha"))
  (should (equal (imp--path-filename "/tmp/alpha/beta.el") "beta.el"))
  (should (equal (imp--path-filename "/tmp/alpha/beta/") "")))

(ert-deftest imp-path-test-current-file-prefers-byte-compile-load-and-current-load-list ()
  (let ((byte-compile-current-file "/tmp/byte-compile.el")
        (load-file-name nil)
        (current-load-list nil))
    (should (equal (imp-path-current-file) "/tmp/byte-compile.el")))
  (let ((byte-compile-current-file nil)
        (load-file-name "/tmp/load-file.el")
        (current-load-list nil))
    (should (equal (imp-path-current-file) "/tmp/load-file.el")))
  (let ((byte-compile-current-file nil)
        (load-file-name nil)
        (current-load-list '("/tmp/current-load.el")))
    (should (equal (imp-path-current-file) "/tmp/current-load.el"))))

(ert-deftest imp-path-test-current-file-falls-back-to-buffer-file-name ()
  (with-temp-buffer
    (setq-local buffer-file-name "/tmp/from-buffer.el")
    (let ((byte-compile-current-file nil)
          (load-file-name nil)
          (current-load-list nil))
      (should (equal (imp-path-current-file) "/tmp/from-buffer.el"))
      (should (equal (imp-file-current) "from-buffer.el"))
      (should (equal (imp-file-current t) "from-buffer"))
      (should (equal (imp-path-current-dir) "/tmp")))))

(ert-deftest imp-path-test-current-file-signals-when-no-source-is-available ()
  (with-temp-buffer
    (let ((byte-compile-current-file nil)
          (load-file-name nil)
          (current-load-list nil))
      (should-error (imp-path-current-file)))))

(ert-deftest imp-path-test-platform-agnostic-normalizes-separators-and-case-on-windows ()
  (let ((system-type 'windows-nt))
    (should (equal (imp--path-platform-agnostic "C:\\Foo\\BAR")
                   "c:/foo/bar")))
  (let ((system-type 'gnu/linux))
    (should (equal (imp--path-platform-agnostic "C:\\Foo\\BAR")
                   "C:/Foo/BAR"))))

(ert-deftest imp-path-test-converts-and-appends-path-segments ()
  (should (equal (imp--path-to-str :alpha) "alpha"))
  (should (equal (imp--path-to-str 'beta) "beta"))
  (should (equal (imp--path-to-str "/tmp/gamma") "/tmp/gamma"))
  (should-error (imp--path-to-str 42))
  (should (equal (imp--path-append nil "alpha") "alpha"))
  (should (equal (imp--path-append "/tmp" "alpha/beta") "/tmp/alpha/beta"))
  (should-error (imp--path-append "/tmp" nil)))

(ert-deftest imp-path-test-split-sans-extension-and-with-extension-handle-common-cases ()
  (should (equal (imp-path-split "/tmp/alpha/beta.el")
                 '("tmp" "alpha" "beta.el")))
  (let ((system-type 'windows-nt))
    (should (equal (imp-path-split "C:\\tmp\\alpha\\beta.el")
                   '("C:" "tmp" "alpha" "beta.el"))))
  (should (equal (imp-path-sans-extension "/tmp" "alpha.el")
                 "/tmp/alpha"))
  (should (equal (imp-path-with-extension "/tmp/alpha" "el")
                 "/tmp/alpha.el"))
  (should (equal (imp-path-with-extension "/tmp/alpha.el" ".el")
                 "/tmp/alpha.el")))

(ert-deftest imp-path-test-load-path-helpers-detect-and-locate-loadable-files ()
  (imp-path-test--with-temp-dir root
    (let* ((file (imp-path-test--write-file (expand-file-name "feature.el" root)
                                            "(provide 'feature)\n"))
           (base (file-name-sans-extension file)))
      (should (imp-path-has-load-extension file))
      (should (equal (imp-path-load-file file) file))
      (should (equal (imp-path-load-file base) file))
      (should-not (imp-path-load-file (expand-file-name "missing" root))))))

(ert-deftest imp-path-test-root-set-get-and-delete-manage-imp-roots ()
  (imp-path-test--with-temp-dir root
    (let ((imp-roots nil)
          (feature 'test-root))
      (imp-path-test--with-current-dir root
        (should (equal (imp-path-root-set feature root)
                       (list (list feature root))))
        (should (equal (imp-path-root-get feature) root))
        (should-not (imp-path-root-set feature root))
        (should-error (imp-path-root-set feature (expand-file-name "other" root)))
        (should (equal (imp-path-root-delete feature) nil))
        (should-not imp-roots)
        (should-error (imp-path-root-get feature))
        (should-not (imp-path-root-get feature :no-error))))))

(provide 'imp-path-test)

;;; path.el ends here
