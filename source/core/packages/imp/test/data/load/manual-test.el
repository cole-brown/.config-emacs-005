;; -*- no-byte-compile: t; -*-

;; (makunbound 'imp--test-manual)
(setq imp--test-manual (1+ (or (bound-and-true-p imp--test-manual) -1)))

(message "Loading 'manual-test.el' for %s time..."
         imp--test-manual)

;; (imp imp:/test/data/load/manual-test)
;; (imp manual-test :path (imp-path-current-dir))
