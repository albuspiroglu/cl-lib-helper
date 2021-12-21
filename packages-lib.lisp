
(defpackage "LIB"
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
   "*LIB-PACKAGE-TREE*"))

(in-package "LIB")

(defun delete-this-system ()
  "Delete all packages defined by system lib-helper, and remove it from asdf buffer."
  (delete-system-aux))

(defun delete-this-hierarchy ()
  (delete-hierarchy *lib-package-tree*))

(defun get-package-names ()
  (get-package-names-aux *lib-package-tree*))

(defun packages (&key (stream *standard-input*))
  (packages-aux *lib-package-tree* :stream stream))

(defun symbol-count ()
  (lib~:symbol-count *lib-package-tree*))

(defun find-syms (phrase &optional (print-results t))
  (lib~:find-syms phrase *lib-package-tree* print-results))

(defun apropos-lib (sub-str &optional (print-results t))
  (lib~:apropos-lib sub-str *lib-package-tree* print-results))


