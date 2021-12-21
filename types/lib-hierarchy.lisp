
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
