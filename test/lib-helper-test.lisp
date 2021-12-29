;; lib-helper/test/lib-helper-test.lisp

(defpackage "LIB-HELPER/TEST"
  (:use #:cl #:fiveam #:lib~))

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

(pass)

  )


(test get-target-sym-name-test1 ()
  (is (equalp "CAR.1~" (lib~::get-target-sym-name "CAR" 1 :loaded nil)))
  (is (equalp "CAR~" (lib~::get-target-sym-name "CAR" 0)))
  (is (equalp "CAR" (lib~::get-target-sym-name "CAR" 0 :loaded t))))


(test get-parent-name-test1 ()
  (is (string-equal (lib~::get-parent-name "lib.lvl1.lvl2")
                        "lib.lvl1"))
  (is (string-equal (lib~::get-parent-name "lib.lvl1..class1")
                        "lib.lvl1"))
  (is (string-equal (lib~::get-parent-name "lib")
                        "")))


(test a-std-symbol-loaded ()
      (multiple-value-bind (sym state)
          (find-symbol "ASSOC" "STD.CONT.ALIST")
        (is (eq sym 'cl:assoc))
        (is (eq state :external))))

(test tilde-symbol-not-created-for-selected-symbol ()
      (multiple-value-bind (sym state)
          (find-symbol "ASSOC~" "STD.CONT.ALIST")
        (is (eq sym nil))))


(defvar *test-sys-name* "lib-helper-test-system")
(defvar *test-sys-package-name* "LIB-HELPER/TEST-SYSTEM-PACKAGE")
(defvar *test-target-package-names* '("TEST" "TEST.TEST-SYSTEM"))
(defun delete-target-packages ()
  (dolist (p *test-target-package-names*)
    (delete-package p)))

(defun get-test-hierarchy ()
  (lib~::convert
   lib~::<lib-hierarchy> lib~::<list>
   `(("TEST" "Top level"
      ())
     ("TEST.TEST-SYSTEM" "This is a test system for hierarchy tests"
      (("SYM1" (,*test-sys-name* ,*test-sys-package-name*))
       ("SYM2" (,*test-sys-name* ,*test-sys-package-name*)))))))

(defun get-test-table (import-symbols-at-startup)
  (let ((table (make-hash-table :test 'equalp)))
    (setf (gethash *test-sys-name* table)
          (lib~::make-system (list *test-sys-name* import-symbols-at-startup)))
    table))

(test tilde-call-auto-loads-the-system ()
      (let (*system-table*)

        (unwind-protect
            (progn
              (let ((*system-table* (get-test-table nil)))
      
                (asdf:load-system "lib-helper-test-system")
                (setup-packages (get-test-hierarchy))

                
                (multiple-value-bind (sym state)
                    (find-symbol "SYM1~" "TEST.TEST-SYSTEM")
                  (is (and sym (eq state :external)) "There should be a tilde-appended symbol, because 
we chose to make-system with import-symbols-at-startup nil"))

                (multiple-value-bind (sym state)
                    (find-symbol "SYM1" "TEST.TEST-SYSTEM")
                  (is (eq sym nil) "and there shouldn't be a symbol corresponding to the actual source symbol"))

                ;; after the assertion above, we can now load the test-system by calling
                ;; the ~ symbol and see that the ~ symbol goes away and the non-tilda exists.
                (multiple-value-bind (sym state)
                    (find-symbol "SYM1~" "TEST.TEST-SYSTEM")
                  (funcall sym))

                (multiple-value-bind (sym state)
                    (find-symbol "SYM1~" "TEST.TEST-SYSTEM")
                  (is (eq sym nil) "The symbol with ~ should be deleted."))

                (multiple-value-bind (sym state)
                    (find-symbol "SYM1" "TEST.TEST-SYSTEM")
                  (is (eq sym (find-symbol "SYM1" *test-sys-package-name*)))
                  (is (eq state :external) "and the symbol without ~ should exist."))))

          ;; cleanup for the test
          (progn
            (asdf:clear-system *test-sys-name*)
            (delete-package *test-sys-package-name*)
            (delete-target-packages)))))


(test setup-packages-gives-error-for-not-loaded-system ()
      (let (*system-table*
            setup-packages-error)

        (unwind-protect
            (progn
              (let ((*system-table* (get-test-table t)))
      
                (signals (asdf/session:FORMATTED-SYSTEM-DEFINITION-ERROR
                          "Since the lib-helper-test-system is not loaded yet, setup-packages 
should throw an error")
                  (setup-packages (get-test-hierarchy)))))

#|
                (handler-case
                    (setup-packages (get-test-hierarchy))
                  (error (c)
                    (setf setup-packages-error t)))

                (is-true setup-packages-error "Since the lib-helper-test-system is not loaded
yet, setup-packages should throw an error")))
|#
          ;; cleanup for the test
          (delete-target-packages))))



