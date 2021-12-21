
(in-package "LIB~")

(defclass lib-hierarchy ()
  ((branches :initarg :branches
             :accessor branches
             :type list)

   (name :initarg :name
         :accessor get-name))

  (:documentation "A library hierarchy (a.k.a. package-tree) is defined with this type.
                   It is essentially a list."))

 
(defmethod print-object ((obj lib-hierarchy) stream)
  (format stream "#<~S ~A>"
          (type-of obj)
          (get-name obj))
  obj)

;; sbcl barfs at this: (defvar <lib-hierarchy> (c2mop:class-prototype 'lib-hierarchy))
;; that's why I'm creating a direct instance of the lib-hierarchy below
(defvar <lib-hierarchy>
  (make-instance 'lib-hierarchy)
  "An empty object to be passed to generic functions as an interface.
Idea partially from lil library.")

(defvar <list> nil
  "An empty object to be passed to generic functions as an interface.")

(defclass lib-hierarchy-branch ()
  ((path :initarg :path
         :accessor path
         :type string
         :documentation "Path name for the branch. This corresponds to a package, or
                         rather a package is created that corresponds to every branch.")

   (parent :initarg :parent
           :accessor parent
           :type lib-hierarchy
           :documentation "A link to the package-tree of this branch (this will be
                           either std-tree or lib-tree).")

   (path-desc :initarg :path-desc
              :accessor path-desc
              :type string
              :documentation "Description of the path or package.")

   (lib-symbols :initarg :lib-symbols
                :accessor lib-symbols
                :type list-of-lib-symbols))

  (:documentation "A branch of a lib-hierarchy / package tree.
A branch is e.g.:
(\"LIB.CONT.LIST.CREATE\" \"List creation\"
     ((\"CIRCULAR-LIST\" (\"alexandria\" \"ALEXANDRIA\")) ; symbols list start here
      (\"CONS\" (NIL \"CL\"))
      (\"COPY-LIST\" (NIL \"CL\"))
      ...
      "))

(defmethod print-object ((obj lib-hierarchy-branch) stream)
  (format stream "#<~S ~A>"
          (type-of obj)
          (path obj))
  obj)

(defvar <lib-hierarchy-branch> (make-instance 'lib-hierarchy-branch)
  "An empty object to be passed to generic functions as an interface.")

(defun get-package-names-aux (hierarchy)
  "Return a list of package names in the package-tree."
  (let (names)
    (dolist (p (branches hierarchy) names)
      (push (path p) names)))) 


(defclass symbol-and-desc ()
  ((sym :initarg :sym
        :reader sym
        :type symbol
        :documentation "The symbol")

   (hierarchy-name :initarg :hierarchy-name
                   :reader hierarchy-name
                   :type string
                   :documentation "The lib-tree hierarchy path.")

   (full-desc :initarg :full-desc
              :accessor full-desc
              :type string
              :documentation "Hierarchy path + all namespace descriptions of the
                              symbol combined."))

  (:documentation "A type that contains the symbol, the hierarchy in tree and full description
                   of the symbol. Used by find-sym and match functions."))

(defvar <symbol-and-desc> (make-instance 'symbol-and-desc)
  "An empty object to be passed to generic functions as an interface.")

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
  `(and (satisfies listp)
        (satisfies all-elements-are-method-details)))

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

