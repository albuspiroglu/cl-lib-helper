
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
   "*LIB-PACKAGE-TREE*"
   "*USER-PACKAGE-TREE*"))

(in-package "LIB")

(defun delete-this-system ()
  "Delete all packages defined by system lib-helper, and remove it from asdf buffer."
  (delete-system-aux))

(defun delete-this-hierarchy ()
  "Delete all packages and symbols defined by hierarchy lib:"
  (delete-hierarchy *lib-package-tree*)
  (delete-hierarchy *user-package-tree*))

(defun get-package-names ()
  (append (get-package-names-aux *user-package-tree*)
          (get-package-names-aux *lib-package-tree*)))

(defun packages (&key (stream *standard-input*))
  (packages-aux *lib-package-tree* :stream stream)
  (packages-aux *user-package-tree* :stream stream))

(defun symbol-count ()
  (+ (lib~:symbol-count *lib-package-tree*)
     (lib~:symbol-count *user-package-tree*)))

(defun find-syms (phrase &optional (print-results t))
  (append (lib~:find-syms phrase *lib-package-tree* print-results)
          (lib~:find-syms phrase *user-package-tree* print-results)))

(eval-when (:load-toplevel)
  (setf (documentation 'find-syms 'function)
        (documentation 'lib~:find-syms 'function)))

(defun apropos-lib (sub-str &optional (print-results t))
  (append (lib~:apropos-lib sub-str *lib-package-tree* print-results)
          (lib~:apropos-lib sub-str *user-package-tree* print-results)))


