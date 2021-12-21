;; lib-helper/types/origin-package.lisp

(in-package "LIB~")

(defclass origin-package ()
  ((pkg-name :initarg :pkg-name
             :accessor pkg-name)

   (system :initarg :system
           :accessor containing-system
           :documentation "The system that contains this package"))

  (:documentation "Defines where a package comes from. Used by new symbols interned
in the hierarchy so that we can point to the actual place."))


(defun make-origin-package (system-and-package)
  (make-instance 'origin-package
                 :pkg-name (second system-and-package)
                 :system (gethash (first system-and-package) 
                                  *system-table*)))


(defun origin-packagep (obj) (typep obj 'origin-package))

(defmethod print-object ((obj origin-package) stream)
  (format stream "#<~S ~A>"
          (type-of obj)
          (concatenate 'string
                       (containing-system obj)
                       "-"
                       (pkg-name obj)))
  obj)