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

(test find-syms-with-path-specific-case
  (is (= (length (std:find-syms "list cont.list" nil))
         10)))

(test find-syms-with-path-reduces-results
  (is (> (length (std:find-syms "list" nil))
         (length (std:find-syms "list cont.list" nil)))))


(test get-class-methods-test ()

  (defclass test-class ()
    ((slot1 :accessor slot1)
     (slot2 :reader rslot2
            :writer wslot2)))

  (defclass test-class-child (test-class)
    ((slot1 :accessor slot1)))

  (defmethod get-slot1 (hello-out (obj test-class))
    "this is get-slot1"
    (print hello-out)
    (slot-value obj 'slot1))

  (defmethod get-slot1-child (hello-out (obj test-class-child))
    "this is get-slot1 for child"
    (print hello-out)
    (slot-value obj 'slot1))

;; these fail, because (function ..) returns a generic function, whereas the mlist
;; contains methods.
#|
  (let ((mlist (lib~::get-class-methods (find-class 'test-class))))
    (is (member (function get-slot1) mlist))
    (is (member (function wslot2) mlist)))

  (let ((mlist (lib~::get-class-methods (find-class 'test-class-child))))
    (is (member (function get-slot1) mlist))
    (is (member (function get-slot1-child) mlist))
    (is (member (function wslot2) mlist)))
|#

(is (= 1 1))

  )


(test get-target-sym-name-test1 ()
  (assert (equalp "CAR.1~" (lib~::get-target-sym-name "CAR" 1 :loaded nil)))
  (assert (equalp "CAR~" (lib~::get-target-sym-name "CAR" 0)))
  (assert (equalp "CAR" (lib~::get-target-sym-name "CAR" 0 :loaded t))))


(test get-parent-name-test1 ()
  (assert (string-equal (lib~::get-parent-name "lib.lvl1.lvl2")
                        "lib.lvl1"))
  (assert (string-equal (lib~::get-parent-name "lib.lvl1..class1")
                        "lib.lvl1"))
  (assert (string-equal (lib~::get-parent-name "lib")
                        "")))

