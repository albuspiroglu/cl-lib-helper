

(defpackage "STD"
  (:use :cl)
  (:export 
   "DELETE-THIS-SYSTEM"
   "GET-PACKAGE-NAMES"
   "PACKAGES"
   "SYMBOL-COUNT"
   "APROPOS-LIB"
   "FIND-SYMS")
  (:import-from "LIB~"
   "DELETE-SYSTEM-AUX"
   "GET-PACKAGE-NAMES-AUX"
   "PACKAGES-AUX"
   "*STD-PACKAGE-TREE*"
   "*PACKAGE-LISTS*"))

(in-package "STD")

(defun delete-this-system ()
  (delete-system-aux))

(defun get-package-names ()
  (get-package-names-aux *std-package-tree*))

(defun packages (&key (stream *standard-input*))
  (packages-aux *std-package-tree* :stream stream))

(defun symbol-count ()
  (lib~:symbol-count *std-package-tree*))

(defun find-syms (phrase &optional (print-results t))
  (lib~:find-syms phrase *std-package-tree* print-results))

(defun apropos-lib (sub-str &optional (print-results t))
  (lib~:apropos-lib sub-str *std-package-tree* print-results))




