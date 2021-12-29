
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

 
(defun get-parent-name (pkg-name)
  "Given lib.lvl1.lvl2 shaped package name, return lib.lvl1; or
lib.lvl1.lvl2..class1, return lib.lvl1.lvl2"
  (let ((dot-pos
         (let ((test-pos (search ".." pkg-name :from-end t)))
           (if test-pos
               test-pos
             (search "." pkg-name :from-end t)))))
    (if dot-pos
        (subseq pkg-name 0 dot-pos)
        "")))


(defun get-sub-packages (pkg-name package-tree)
  "pkg-name: string
Returns a list of paths that are sub packages of pkg-name."
  (let (subs)
    (dolist (p (branches package-tree) subs)
      (if (string-equal (get-parent-name (path p))
                        pkg-name)
          (push (path p) subs)))))


(defun add-sub-packages (h-branch parent-pkg)
  "Find package names that are sub-packages of h-branch, and make them
symbols of .package-name, i.e. a dot prepended, and intern in the parent package.

e.g.
a package path1.path2 can have symbols interned for it such as:
path1.path2:.path2.1
path1.path2:.path2.2
path1.path2:.path2.3
"
  (dolist (s (get-sub-packages (path h-branch) (parent h-branch)))
    (let ((sym (intern (subseq s (length (path h-branch)))
                       parent-pkg)))
      (setf (symbol-value sym) (find-package s)) ;; prob. the best way to keep a reference to the package
      (export sym parent-pkg))))


(defun define-sub-package-syms (p)
  "Define all symbols of the existing target branch package p,
including the listed symbols & symbols for each .sub-package

INPUTS:
  p: lib-hierarchy-branch

OUTPUTS: nil
"

  (let (syms
        (to-pkg (find-package (path p))))

    (dolist (s (lib-symbols p))
      (setf (syms s) (import-and-get-symbols s to-pkg))
      (setf syms (append syms (syms s)))
      (set-full-desc s))
    (export syms to-pkg)
    (add-sub-packages p to-pkg)))


(defun setup-packages (package-tree)
  "Creates and defines packages in package-tree."
  (flet ((%create-packages ()
           "Create each package, without any detail such as import, use (except use :cl) etc."

           (let (failed-packages)

             (dolist (branch (branches package-tree))

               (handler-case
                   (let ((pkg (make-package (path branch) :use '("COMMON-LISP"))))
                     (setf (documentation pkg t)
                           (path-desc branch)))
                 (error (c)
                   (declare (ignore c))
                   (push (path branch) failed-packages)))

               ;; We're manually creating the top-level package, that's why skip one error.
               (when (> (length failed-packages) 1)
                 (format t "Failed to create packages: ~{~a, ~}.~%" failed-packages))))))

    (%create-packages)

    (dolist (branch (branches package-tree))
      (define-sub-package-syms branch))))
