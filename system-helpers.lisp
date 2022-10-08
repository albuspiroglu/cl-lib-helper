;; lib-helper/system-helpers.lisp

(in-package "LIB~")

#|
reference:

(defun %get-generic-functions ()
-  "Return all generic functions defined in the current lisp image.
-
-(gm1 gm2 ...):
-gm: (gf-sym (gm1 lambda-list1 ..) ..)
-"
-  (let (result)
-    (do-all-symbols (sym (mapcar
-                          (lambda (g)
-                            (append (list g)
-                                    (mapcar
-                                     (lambda (gm)
-                                       (list gm (closer-mop:method-lambda-list gm)))
-                                     (closer-mop:generic-function-methods (symbol-function g)))))
-                          (remove-duplicates
-                           ;; remove any setf ones:
-                           (remove-if (lambda (s) (consp
-                                                   (closer-mop:generic-function-name
-                                                    (symbol-function s))))
-                                      result))))
-      (if (and (fboundp sym)
-               (typep (symbol-function sym) 'generic-function))
-          (push sym result)))))


(defun %remove-setf-functions (lst extract-function)
  "From a list of symbols of generic functions, remove the setf ones.
extract-function: lambda to get the symbol-function from an element."
  (remove-if (lambda (s) (consp
                          (closer-mop:generic-function-name
                           (funcall extract-function s))))
             lst))


 |#
(defun get-generic-functions ()
  "Return all generic functions defined in the current lisp image as
list of gf-tree.
"
  (let (result)
    (do-all-symbols (sym (mapcar (lambda (g) (convert <gf-tree> <symbol> g))
                                 (remove-duplicates result)))
;                                  (%remove-setf-functions result))))
      (when (and (fboundp sym)
                 (typep (symbol-function sym) 'generic-function))
        (push sym result)))))


(defun filter-external-syms (names pkg)
  (let (result)
    (dolist (sym names result)
      (multiple-value-bind (s status) (intern sym pkg)
        (declare (ignore s))
        (when (eq status :external)
          (push sym result))))))

#|
(defun %get-class-externals (class pkg-name generic-functions)
  "Return a list of symbols related to the class."
  (let ((slots (closer-mop:class-direct-slots class))
        names methods)
    (dolist (slot slots)
      (setf names (append (mapcar (lambda (s) (symbol-name s))
                                   (closer-mop:slot-definition-readers slot))
                          names))
      (setf names (append (mapcar (lambda (s) (symbol-name (if (symbolp s) s (second s))))
                                   (closer-mop:slot-definition-writers slot))
                          names)))
    (dolist (gm generic-functions) ; gm: (gf-sym (gm1 lambda-list1 ..) ..)
      (when (some (lambda (ml)
                    (some (lambda (ll) (eq (find-class ll nil) class))
                          (first (rest ml))))
                  (rest gm))
        (push (symbol-name (first gm)) methods)))

    (remove-duplicates
     (append (filter-external-syms names (find-package pkg-name))
             methods)
     :test 'equalp)))

(defun %compute-applicable-methods-using-classes (gf required-classes)
  (remove-if-not (lambda (method)
                   (every #'c2mop:subclassp
                          required-classes
                          (c2mop:method-specializers method)))
                 (c2mop:generic-function-methods gf)))
|#


(defun get-package-external-symbols (package)
  "Return the symbols that are exported from the package, i.e. external."
  (unless (packagep package)
    (setf package (find-package package)))
  (let (result)
    (with-package-iterator (sym package :external)
      (loop (multiple-value-bind (more? symbol access pkg) (sym)
              (declare (ignore access pkg))
              (unless more? (return))
              (push symbol result))))
    result))


(defun get-struct-externals (struct-name package-name)
  "Given a struct name, check for each struct name and return the
package-external ones as a list."
  (flet ((%get-struct-slot-names (struct-name package-name)
           "Return the list of names of the structure's slots.
FIXME: if a conc-name is used, we would miss it, but there are ways to find it,
such as checking each symbol in the package and finding the ones ending with the
slot names, then a reduction in this set will give us a confident result."
           (let ((struct-form
                  (with-output-to-string (s)
                    (princ (closer-mop:class-prototype
                            (find-class
                             (intern (string-upcase struct-name) (find-package package-name))))
                           s))))
             (mapcar (lambda (w) (mkstr struct-name "-" (subseq w 1 (1- (length w)))))
                     (cl-ppcre:all-matches-as-strings ":[^:].+? " struct-form)))))
    (let* ((struct-name (string-upcase struct-name))
           (names
            (append
             (list (mkstr struct-name "-P")
                   (mkstr "MAKE-" struct-name)
                   (mkstr "COPY-" struct-name)) ; slot names couldn't be retrieved portably yet
             (%get-struct-slot-names struct-name package-name)))
           (pkg (find-package package-name)))
      (filter-external-syms names pkg))))


(defvar *skipped-classes-in-check*
  (list (find-class 'standard-object)
        (find-class t)
        #+lispworks (find-class 'clos::dependee-mixin)
        #+lispworks (find-class 'clos::specializer)
        #+lispworks (find-class 'harlequin-common-lisp:metaobject)

        ))

(defun get-class-methods (class &optional (generic-functions nil))
  "Return all methods in the current lisp image which act on class type or its parent types, i.e.
have parameters of type class or any of class' parents."
  (labels ((%in-skipped-classes (c)
             (member c *skipped-classes-in-check*))

           (%subclassp (sub parent-tree)
             (find sub
                   (remove-if #'%in-skipped-classes parent-tree))))

    (let (methods
          (class-tree (c2mop:class-precedence-list class)))
      (unless generic-functions (setf generic-functions (get-generic-functions)))
      (dolist (gt generic-functions methods)
        (dolist (gm (gmethods gt))
          ;; (when (some (lambda (arg) (eq arg class))
          (when (some (lambda (arg) (%subclassp arg class-tree))
                      (specializers gm))
            (push gm methods)))))))


(defun get-class-methods-unique-names (class &optional (generic-functions nil))
  (remove-duplicates
   (mapcar (lambda (m)
            (c2mop:generic-function-name
             (c2mop:method-generic-function (method-obj m))))
          (get-class-methods class generic-functions))
   :test 'equalp))

(defun format-package-tree-syms (tree stream)

  (flet ((%format-package-tree-branch-syms (tree stream)
           (format stream "(\"~a\" \"~a\"~%(" (first tree) (second tree))
           (dolist (s (third tree))
             (format stream "(~s (~s ~s))~%"
                     (first s) (caadr s) (cadadr s)))
           (format stream "))~%")))

    (dolist (branch tree)
      (%format-package-tree-branch-syms branch stream))))


(defun get-symbol-node (sym sys-name package-name)

  (flet ((%symbol-full-string (sym)
           (with-output-to-string (s)
             (format s "~s" sym))))

    (list (if (and (consp sym) (eql (first sym) 'SETF))
              (concatenate 'string "SETF-" (%symbol-full-string (second sym)))
            (%symbol-full-string sym))
          (list sys-name package-name))))


(defun generate-system-symbols (sys-name prefix packages
                                         &optional (stream *standard-output*))
  "function maturity: 1
Generate a package tree corresponding to the system. Then the result can be
manually pasted into the lib-defs.lisp

Also creates sections for classes and their implemented methods.

sys-name: string, name of the asdf system
prefix  : the package-tree section prefix for packages of the system to be imported from.
          e.g. for containers package to be under lib.cont pass \"LIB.CONT\".
packages: list of packages to import from and to (\"package-from\" \"package-to\".
          A system may define many packages, but the
          the user should manually choose a subset that the symbols will be imported
          from, and pass them here.

example calls:

  (with-open-file (f \"temp-defs.lisp~\" :direction :output :if-exists :supersede)
    (generate-system-symbols \"lil\" \"LIB.CONT\"
                             '((\"LIL/CORE/ALL\" \"LIL.CORE\")
                               (\"LIL/INTERFACE/ALL\" \"LIL.INTERFACE\"))
                             f))

  (with-open-file (f \"temp-defs.lisp~\" :direction :output :if-exists :supersede)
    (generate-system-symbols \"cl-ppcre\" \"LIB.STR\"
                             '((\"CL-PPCRE\" \"ppcre\"))
                             f))

an example return output:

(\"LIB.CONT.LIL/PURE/HASH-TABLE\" \"Package: LIL/PURE/HASH-TABLE\"
     ((\"<HASH-TABLE>\" (\"lil\" \"LIL/PURE/HASH-TABLE\"))
      (\"HASHMAP-INTERFACE\" (\"lil\" \"LIL/PURE/HASH-TABLE\"))
      (\"BUCKETMAP-INTERFACE\" (\"lil\" \"LIL/PURE/HASH-TABLE\"))
      ...
      ))
(\"LIB.CONT.LIL.INTERFACE.ORDER\" \"Package: LIL/INTERFACE/ORDER\"
     ((\"<ORDER>\" (\"lil\" \"LIL/INTERFACE/ORDER\"))
      (\"<ORDER-FROM-LESSP>\" (\"lil\" \"LIL/INTERFACE/ORDER\"))
      (\"<LESSP>\" (\"lil\" \"LIL/INTERFACE/ORDER\"))
      ...
      ))
"
  (labels ((get-header (package-name to-package-name &optional (desc "Package: "))
             (list (concatenate 'string prefix "." to-package-name)
                   (concatenate 'string desc package-name)))

           (get-package-symbols (package-name sys generic-functions)
             "Return three values:
              val1: a list of package-syms
              val2: a tree of classes and their symbols as:
                    ((class1-obj class1 sym1.1 sym1.2 ...)
                     (class2-obj class2 sym2.1 sym2.2 ...))
              val3: a tree of structs and their symbols as:
                    ((struct1 syms1.1 syms1.2 ...)
                     (..))"
             (let (sym-list
                   class-syms struct-syms
                   (external-syms (get-package-external-symbols package-name)))

               (dolist (sym external-syms)
                 (push (get-symbol-node sym sys package-name) sym-list)
                 (when (typep (find-class sym nil) 'structure-class)
                   (push sym struct-syms))
                 (when (typep (find-class sym nil) 'standard-class)
                   (push (list (find-class sym) sym) class-syms)))

               (dolist (st struct-syms)
                 (nconc st
                        (sort
                         (mapcar (lambda (s) (get-symbol-node s sys package-name))
                                 (get-struct-externals st package-name))
                         #'string-lessp :key #'car)))

               (dolist (cs class-syms)
                 (nconc cs
                        (sort
                         (mapcar (lambda (s) (get-symbol-node s sys package-name))
                                 (get-class-methods-unique-names (first cs) generic-functions))
                         #'string-lessp :key #'car)))
               (values
                (sort sym-list #'string-lessp :key #'car)
                (remove-if-not #'third class-syms)
                struct-syms))))

    (let (tree
          package-name
          (generic-functions (get-generic-functions)))
      (dolist (p packages (format-package-tree-syms (nreverse tree)
                                                     stream))
        (setf package-name (package-name (first p)))
        (push (get-header package-name (second p)) tree)
        (multiple-value-bind (package-symbols class-syms struct-syms)
            (get-package-symbols package-name sys-name generic-functions)
          (nconc (first tree) (list package-symbols))
          (dolist (c class-syms)
            (when (third c)
              (push (get-header (mkstr package-name ".." (second c))
                                (mkstr (second p) ".." (second c))
                                "Class: ")
                    tree)
              (nconc (first tree) (list (nthcdr 2 c)))))
          (dolist (s struct-syms)
            (when (second s)
              (push (get-header (mkstr package-name ".." (first s))
                                (mkstr (second p) ".." (second s))
                                "Struct: ")
                    tree)
              (nconc (first tree) (list (cdr s))))))))))