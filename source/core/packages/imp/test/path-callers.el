;;; path-callers.el --- Tests for migrated path callers -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:

;; Completed paths use `imp-path'; unrooted feature paths remain relative.

;;; Code:

(load (expand-file-name "path.el" (file-name-directory (or load-file-name buffer-file-name)))
      nil 'nomessage)

(let ((imp-features nil)
      (features features))
  (imp-path-test--load "tree.el")
  (imp-path-test--load "provide.el")
  (imp-path-test--load "load.el"))

(ert-deftest imp-path-test:/migration/rooted-and-unrooted-features ()
  (let ((imp-roots '((test "/tmp/imp-path-root/"))))
    (should (equal (imp-path-of-feature 'test:/alpha/beta)
                   "/tmp/imp-path-root/alpha/beta"))
    (should (equal (imp-path-of-feature 'test:)
                   "/tmp/imp-path-root"))
    (should (equal (imp-path-of-feature 'alpha/beta)
                   "alpha/beta"))))

(ert-deftest imp-path-test:/migration/current-file-relative-feature ()
  (let (imp-roots)
    (imp-path-test--with-current-dir "/tmp/imp-path-source/"
      (should (equal (imp-path-of-feature './alpha/beta)
                     "/tmp/imp-path-source/alpha/beta")))))

(ert-deftest imp-path-test:/migration/parser-path-prefixes ()
  (imp-path-test--with-current-dir "/tmp/imp-path-source/"
    (should (equal (imp-parser-normalize-path-string 'test :path "./alpha/../beta/")
                   "/tmp/imp-path-source/beta"))
    (should (equal (imp-parser-normalize-path-string 'test :path "./")
                   "/tmp/imp-path-source")))
  (cl-letf (((symbol-function 'imp-path-root-get)
             (lambda (_feature &optional _no-error) "/tmp/imp-path-root/")))
    (should (equal (imp-parser-normalize-path-string 'test :path ":/alpha/../beta/")
                   "/tmp/imp-path-root/beta"))
    (should (equal (imp-parser-normalize-path-string 'test :path ":/")
                   "/tmp/imp-path-root")))
  (should (equal (imp-parser-normalize-path-string 'test :path "alpha/beta/")
                 "alpha/beta/")))

(ert-deftest imp-path-test:/migration/parser-appends-feature-segments ()
  (let ((imp-roots '((test "/tmp/imp-path-root/"))))
    (cl-letf (((symbol-function 'message) #'ignore))
      ;; An explicit base uses strings; a feature root supplies symbols.
      (should (equal (imp-parser-normalize-path-feature 'alpha/beta :path
                                                      "/tmp/imp-path-base/")
                     "/tmp/imp-path-base/alpha/beta"))
      (should (equal (imp-parser-normalize-path-feature 'test:/alpha/beta :path nil)
                     "/tmp/imp-path-root/alpha/beta"))
      ;; A bare root supplies an empty suffix.
      (should (equal (imp-parser-normalize-path-feature 'test: :path nil)
                     "/tmp/imp-path-root")))))

(ert-deftest imp-path-test:/migration/parser-preserves-loadable-file ()
  (imp-path-test--with-temp-dir root
    (let ((file (imp-path-test--write-file (expand-file-name "existing.el" root))))
      (should (equal (imp-parser-normalize-path-feature 'alpha/beta :path file)
                     file)))))

(provide 'imp-path-callers-test)
;;; path-callers.el ends here
