;; lib-helper/types/lib-symbol.lisp"

(in-package "LIB~")

(defclass method-detail ()
  ((method-obj :initarg :method-obj
               :accessor method-obj
               :type method
               :documentation "The method object.")

   (specializers :initarg :specializers
                :accessor specializers
                :type list
                :documentation "Method lambda list."))

  (:documentation "Details of a method."))

(defvar <method-detail> (make-instance 'method-detail)
  "An empty object to be passed to generic functions as an interface.")

(defmethod print-object ((obj method-detail) stream)
  (print-unreadable-object (obj stream :type nil :identity t)
    (format stream "MD: ~a" (c2mop:generic-function-name
                             (c2mop:method-generic-function (method-obj obj))))))

(defun method-detailp (obj) (typep obj 'method-detail))

(defun all-elements-are-method-details (lst) (every #'method-detailp lst))

(deftype list-of-method-details ()
  `(and list (satisfies all-elements-are-method-details)))

(defclass gf-tree ()
  ((gf :initarg :gf
       :accessor gf
       :documentation "The generic function object")

   (gmethods :initarg :gmethods
             :accessor gmethods
             :type list-of-method-details
             :documentation "Methods and lambda lists of the gm."))

  (:documentation "A tree for a generic function including its methods and their
                   lambda lists."))

(defvar <gf-tree> (make-instance 'gf-tree)
  "An empty object to be passed to generic functions as an interface.")

(defvar <symbol> (make-symbol "<SYMBOL>")
  "An empty object to be passed to generic functions as an interface.")

(defmethod print-object ((obj gf-tree) stream)
  (print-unreadable-object (obj stream :type nil :identity t)
    (format stream "GT: ~a" (c2mop:generic-function-name (gf obj)))))

