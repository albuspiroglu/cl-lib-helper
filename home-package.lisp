
(defpackage "LIB~"
  (:use :cl)
  (:export "*PACKAGE-LISTS*"
           "GET-PACKAGE-NAMES-AUX"
           "SETUP-PACKAGES"
           "*CLHS-CHAPTERS*"
           "DELETE-SYSTEM-AUX"
           "*STD-PACKAGE-TREE*"
           "*LIB-PACKAGE-TREE*"
           "PACKAGES-AUX"
           "*SYSTEM-TABLE*"
           "SYMBOL-COUNT"
           "GENERATE-PACKAGE-SYMBOLS"
           "APROPOS-LIB"
           "FIND-SYMS"
           "DEFUN-IN-PACKAGE"))

(in-package "LIB~")

(eval-when (:compile-toplevel :execute :load-toplevel)

  (in-package "LIB~")

  (defparameter *package-lists* nil "list of used package-details")

  (defvar *system-table* (make-hash-table :test 'equalp)
    "A hash table of {key:string val:(bool bool)},
       with meanings:
         key: system-name,
         val: (import-symbols-at-startup loaded)")
  
  (defmacro defun-in-package ((pkg from-fn to-fn) &body call-pattern)
    "Define a function named to-fn in package pkg, with the body call-pattern, 
copying the documentation of from-fn over to to-fn.

Call-pattern must be a lambda expression.

e.g. call:
(defun-in-package (\"LIB\" 
                   'lib~:delete-system-aux 
                   \"DELETE-THIS-SYSTEM\")
  (lambda ()
    (lib~:delete-system-aux)))

will create a lib:delete-this-system function with the lambda definition
and documentation of 'lib~:delete-system-aux.

Why did I use this macro? Because I couldn't find a way to define this function in
lib (or std) package during compile-toplevel time, as I am creating lib package during
compilation at a different lexical scope - I think.
"
    (let ((pkg-name (gensym))
          (to-fn-sym (gensym)))
      `(let ((,pkg-name (find-package ,pkg)))
         (let ((,to-fn-sym (intern ,to-fn ,pkg-name)))
           (setf (symbol-function ,to-fn-sym)
                 ,@call-pattern
                 
                 (documentation ,to-fn-sym 'function)
                 (documentation ,from-fn 'function))))))
)

