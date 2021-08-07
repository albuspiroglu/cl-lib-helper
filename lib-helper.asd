 
(defsystem "lib-helper"
  :description "Reorganise existing symbols in standard and third party libs to common hierarchical packages."
  :version "0.0.1"
  :author "Albus M Piroglu <mattapiroglu@gmail.com>"
  :maintainer "Albus M Piroglu <mattapiroglu@gmail.com>"
  :licence "MIT"
  :depends-on ("cl-ppcre")
  :components ((:file "home-package")
               (:file "known-libs" :depends-on ("home-package"))
               (:file "package-helpers" :depends-on ("home-package"))

               (:module std-lib
                :pathname ""
                :depends-on (package-helpers)
                :components
                ((:file "std-defs")
                 (:file "packages-std" :depends-on ("std-defs"))))

               (:module libs
                :pathname ""
                :depends-on (package-helpers)
                :components
                ((:file "lib-defs")
                 (:file "packages-lib" :depends-on ("lib-defs")))))
               
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
)

