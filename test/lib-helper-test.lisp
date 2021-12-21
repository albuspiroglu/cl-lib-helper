;; lib-helper/test/lib-helper-test.lisp

(defpackage "LIB-HELPER/TEST"
  (:use #:cl #:fiveam))

(in-package "LIB-HELPER/TEST")

(def-suite :lib-helper)

(in-suite :lib-helper)

(test loaded-both-hierarchies
  (is (= 2 (length lib~::*hierarchies*))))

(test std-hierarchy-count-is-more-than-500
  (is (> (lib~::symbol-count lib~::*std-package-tree*)
         500)))

(test lib-hierarchy-count-is-more-than-5000
  (is (> (lib~::symbol-count lib~::*lib-package-tree*)
         5000)))

(test find-syms-returns-results
  (is (> (length (lib:find-syms "test" nil))
         1)))
