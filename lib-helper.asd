
(defpackage lib-helper-asd
  (:use :cl :asdf))

(in-package :lib-helper-asd)

(defsystem "lib-helper"
  :description "Reorganise existing symbols in standard and third party libs to common hierarchical packages."
  :version "1.10.1"
  :author "Albus M Piroglu <mattapiroglu@gmail.com>"
  :maintainer "Albus M Piroglu <mattapiroglu@gmail.com>"
  :licence "MIT"
  :depends-on ("cl-ppcre"
               "closer-mop")

  :components
  ((:file "package")

   (:module generics
    :depends-on ("package")
    :components ((:file "convert"
                  :description "Generic function for conversion between types"))

    :description "Generics are where the defgeneric forms reside. Then the specialisations,
                  i.e. defmethods are defined for each related type, under types module.")

   (:module types
    :depends-on ("generics")
    :components ((:file "system"
                  :description "Defines load behaviour for systems.")

                 (:file "methods"
                  :description "Find generic functions and their methods for organising classes.")

                 (:file "origin-package"
                  :depends-on ("system")
                  :description "The package where a symbol originated from")

                 (:file "lib-hierarchy"
                  :description "Types and functions pertaining the lib-hierarchy")

                 (:file "lib-symbol"
                  :description "A symbol that can exist in multiple source packages, mapped
                                to multiple symbols under one lib-hierarchy-branch")

                 (:file "converters"
                  :depends-on ("lib-hierarchy")
                  :description "Converter methods definitions."))

    :description "Classes, structs, deftypes and their methods.")

   (:file "known-libs" 
    :depends-on ("package")
    :description "A collection of know systems and their load-at-startup status.")

   (:file "utils" :depends-on ("package"))

   (:file "packages-common" 
    :depends-on ("package" "utils")
    :description "Shared functionality between packages-*.lisp below")

   (:file "system-helpers"
    :depends-on ("package" "utils")
    :description "Helper functions to generate hierarchies for systems")

   (:module std-lib
    :pathname ""
    :depends-on (packages-common)
    :components
    ((:file "std-defs")
     (:file "packages-std" :depends-on ("std-defs")))
    :perform (load-op :after (op std-lib)
                      (lib-helper-asd::prepare-std-libs)))

   (:module libs
    :pathname ""
    :depends-on (packages-common)
    :components
    ((:file "lib-defs")
     (:file "packages-lib" :depends-on ("lib-defs")))
    :perform (load-op :after (op libs)
                      (lib-helper-asd::prepare-libs))))

  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))

  :in-order-to ((test-op (test-op :lib-helper/test)))
  )

(defsystem "lib-helper/test"
  :description "lib-helper tests."
  :version "0.1.0"
  :author "Albus M Piroglu <mattapiroglu@gmail.com>"
  :maintainer "Albus M Piroglu <mattapiroglu@gmail.com>"
  :licence "MIT"
  :depends-on ("lib-helper"
               "fiveam")
  :components ((:module "test"
                :components ((:file "lib-helper-test"))))
  :perform (test-op (o c) (uiop/package:symbol-call :5am :run! :lib-helper)))


(defun prepare-std-libs ()
  (let* ((plib (find-package "LIB~"))
         (setup-packages (symbol-function (find-symbol "SETUP-PACKAGES" plib)))
         (std-package-tree (symbol-value (find-symbol "*STD-PACKAGE-TREE*" plib)))
         (symbol-count (symbol-function (find-symbol "SYMBOL-COUNT" plib)))
         (add-to-package-lists (symbol-function (find-symbol "ADD-TO-PACKAGE-LISTS" plib))))
    (funcall setup-packages std-package-tree)
    (format t "~&====Loaded std:*, total ~a symbols.====~%"
            (funcall symbol-count std-package-tree))
    (funcall add-to-package-lists std-package-tree)))

(defun prepare-libs ()
  (let* ((plib (find-package "LIB~"))
         (setup-packages (symbol-function (find-symbol "SETUP-PACKAGES" plib)))
         (lib-package-tree (symbol-value (find-symbol "*LIB-PACKAGE-TREE*" plib)))
         (symbol-count (symbol-function (find-symbol "SYMBOL-COUNT" plib)))
         (add-to-package-lists (symbol-function (find-symbol "ADD-TO-PACKAGE-LISTS" plib))))
    (funcall setup-packages lib-package-tree)
    (format t "~&=====Loaded lib:*, total ~a symbols.=====~%"
            (funcall symbol-count lib-package-tree))
    (funcall add-to-package-lists lib-package-tree)))

#|
would writing a macro to allow the following syntax be worthwhile?
PS. note that there's uiop:symbol-call for a similar purpose.

(defun prepare-libs-2 ()
  (with-package ("LIB~")
     (using-objects (setup-packages
                     lib-package-tree
                     symbol-count
                     add-to-package)
        (funcall setup-packages lib-package-tree)
        (format t "~&=====Loaded lib:*, total ~a symbols.=====~%"
            (funcall symbol-count lib-package-tree))
        (funcall add-to-package-lists lib-package-tree))))

or this is easier to implement:
------------------
(defun prepare-libs ()
  (let* ((#PFlib~:setup-packages) ; --> expands to:
            ; (setup-packages (symbol-function (find-symbol "SETUP-PACKAGERS" (find-package "LIB~"))))
         (#PVlib~:lib-package-tree)
         (#PFlib~:symbol-count)
         (#PFlib~:add-to-package-lists))
    (funcall setup-packages lib-package-tree)
    (format t "~&=====Loaded lib:*, total ~a symbols.=====~%"
            (funcall symbol-count lib-package-tree))
    (funcall add-to-package-lists lib-package-tree)))

|#
