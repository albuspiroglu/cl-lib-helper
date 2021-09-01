
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
           "FIND-SYMS"))

(in-package "LIB~")


(eval-when (:compile-toplevel :execute :load-toplevel)

  (defparameter *package-lists* nil "list of used package-details")

  (defvar *system-table* (make-hash-table :test 'equalp)
    "A hash table of {key:string val:(bool bool)},
       with meanings:
         key: system-name,
         val: (import-symbols-at-startup loaded)")
  )

