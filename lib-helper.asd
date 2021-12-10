
(defpackage lib-helper-asd
  (:use :cl :asdf))

(in-package :lib-helper-asd)
 
(defsystem "lib-helper"
  :description "Reorganise existing symbols in standard and third party libs to common hierarchical packages."
  :version "0.0.1"
  :author "Albus M Piroglu <mattapiroglu@gmail.com>"
  :maintainer "Albus M Piroglu <mattapiroglu@gmail.com>"
  :licence "MIT"
  :depends-on ("cl-ppcre"
               "closer-mop")
  :components ((:file "home-package")
               (:file "known-libs" :depends-on ("home-package"))
               (:file "package-helpers" :depends-on ("home-package"))

               (:module std-lib
                :pathname ""
                :depends-on (package-helpers)
                :components
                ((:file "std-defs")
                 (:file "packages-std" :depends-on ("std-defs")))
                :perform (load-op :after (op std-lib)
                                  (lib-helper-asd::prepare-std-libs)))

               (:module libs
                :pathname ""
                :depends-on (package-helpers)
                :components
                ((:file "lib-defs")
                 (:file "packages-lib" :depends-on ("lib-defs")))
                :perform (load-op :after (op libs)
                                  (lib-helper-asd::prepare-libs))))
               
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
)

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

