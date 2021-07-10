
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
           "*SYSTEM-TABLE*"))

(in-package "LIB~")

(defvar *package-lists* nil "list of used package-details")

(eval-when (:compile-toplevel :execute :load-toplevel)
  (setf *package-lists* nil))

