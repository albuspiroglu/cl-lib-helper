

(defpackage "STD"
  (:use :cl)
  (:export 
   "DELETE-THIS-SYSTEM"
   "DELETE-THIS-HIERARCHY"
   "GET-PACKAGE-NAMES"
   "PACKAGES"
   "SYMBOL-COUNT"
   "APROPOS-LIB"
   "FIND-SYMS")
  (:import-from "LIB~"
   "DELETE-SYSTEM-AUX" "DELETE-HIERARCHY"
   "GET-PACKAGE-NAMES-AUX"
   "PACKAGES-AUX"
   "*STD-PACKAGE-TREE*"))

(in-package "STD")

(defun delete-this-system ()
  "Delete all packages defined by system lib-helper, and remove it from asdf buffer."
  (delete-system-aux))

(defun delete-this-hierarchy ()
  "Delete all packages and symbols defined by hierarchy std:"
  (delete-hierarchy *std-package-tree*))

(defun get-package-names ()
  (get-package-names-aux *std-package-tree*))

(defun packages (&key (stream *standard-input*))
  (packages-aux *std-package-tree* :stream stream))

(defun symbol-count ()
  (lib~:symbol-count *std-package-tree*))

(defun find-syms (phrase &optional (print-results t))
  (lib~:find-syms phrase *std-package-tree* print-results))

(eval-when (:load-toplevel)
  (setf (documentation 'find-syms 'function)
        (documentation 'lib~:find-syms 'function)))

(defun apropos-lib (sub-str &optional (print-results t))
  (lib~:apropos-lib sub-str *std-package-tree* print-results))




