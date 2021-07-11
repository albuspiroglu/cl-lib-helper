
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
           "SYMBOL-COUNT"))

(in-package "LIB~")


(eval-when (:compile-toplevel :execute :load-toplevel)
  (defparameter *package-lists* nil "list of used package-details")
  (defvar *system-table* (make-hash-table :test 'equalp)))

