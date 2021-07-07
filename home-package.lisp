
(defpackage "LIB~"
  (:use :cl)
  (:export "*PACKAGE-LISTS*"
           "GET-PACKAGE-NAMES-AUX"
           "SETUP-PACKAGES"
           "*CLHS-CHAPTERS*"
           "DELETE-SYSTEM-AUX"
           "*STD-PACKAGE-DETAILS*"
           "*LIB-PACKAGE-DETAILS*"
           "PACKAGES-AUX"))

(in-package "LIB~")

(defvar *package-lists* nil "list of used package-details")

(eval-when (:compile-toplevel :execute :load-toplevel)
  (setf *package-lists* nil))

