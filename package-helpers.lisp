
(in-package "LIB~")

(defun %append-not-loaded-suffix (sym-name)
  (concatenate 'string sym-name "~"))

(defun %get-target-sym-name (sym-name index &key (loaded nil))
  "Name of the symbol to create depends on how many systems / packages are
exporting the symbol. If more than one, than the first one is the sym-name,
and subsequent ones are appended a dot + number starting from 1. If the system
is not loaded, then a tilde will be appended to the name.
index: 0 based, which index system is the symbol imported from in a symbol list
       of (sym-name (sys0 pkg0) (sys1 pkg1) ..)

e.g. from a list of: (\"CAR\" (NIL \"CL\") (\"my-system\" \"MY-PACKAGE\"))
     we want the symbol for my-system, thus call
       (%get-target-sym-name \"CAR\" 1 :loaded nil)
     to get \"CAR.1~\"
"
  (let ((new-sym-name
         (if (zerop index)
             sym-name
           (concatenate 'string sym-name "." (write-to-string index)))))
    (if loaded
        new-sym-name
      (%append-not-loaded-suffix new-sym-name))))

(defun %intern-now (sym-name sym to-pkg)
  (if (equalp sym-name (symbol-name sym))
      (progn
        (shadowing-import sym to-pkg)
        sym)
    (let ((new-sym (intern sym-name to-pkg)))
      (setf (symbol-value new-sym) sym)
      new-sym)))

(defun %rename-import-syms (orig-pkg branch from-pkg)
  "For the symbols in branch that belong to orig-pkg,
unintern the symbols corresponding to that package, which will be named
as a-symbol{.N}~, and shadowing-import every the a-symbol name from from-pkg
to to-pkg in branch.

orig-pkg: is of type origin-package
branch: a lib-hierarchy-branch
from-pkg: a common-lisp package designator
"
  (flet ((belongs-to-sys (lib-sym)
           (search (list (containing-system orig-pkg))
                   (origin-packages lib-sym)
                   :test (lambda (a b) (equalp a (first b))))))

    (let ((to-pkg (find-package (path branch))))
      (dolist (lib-sym (lib-symbols branch))
        (let ((i (belongs-to-sys lib-sym)))
          (when i
            (let ((sym (find-symbol (sym-name lib-sym) from-pkg)))
              (shadowing-import sym to-pkg)
              (unintern (find-symbol (%get-target-sym-name (sym-name lib-sym) i)
                                     to-pkg)
                        to-pkg)
              (export sym to-pkg))))))))

(defun %intern-later (sym-name orig-pkg to-pkg pkg-tree)
  "Create a symbol with a ~ appended to end, bound to a function to do:
load the associated system (via asdf - not quicklisp, so everything is offline),
create the expected symbol without the ~ this time, pointing to the actual object of concern and
delete the symbol with the ~ at the end.
"
  (let* ((new-sym-name (concatenate 'string sym-name "~"))
         (new-sym (intern new-sym-name to-pkg)))
    (setf (symbol-function new-sym)
          (lambda () (activate-system orig-pkg pkg-tree)))
    new-sym))

(defun %get-parent-name (pkg-name)
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

(defun %get-parent-name.test1 ()
  (assert (string-equal (%get-parent-name "lib.lvl1.lvl2")
                        "lib.lvl1"))
  (assert (string-equal (%get-parent-name "lib.lvl1..class1")
                        "lib.lvl1"))
  (assert (string-equal (%get-parent-name "lib")
                        "")))

(defun %get-sub-packages (pkg-name package-tree)
  "pkg-name: string
Returns a list of paths that are sub packages of pkg-name."
  (let (subs)
    (dolist (p (branches package-tree) subs)
      (if (string-equal (%get-parent-name (path p))
                        pkg-name)
          (push (path p) subs)))))

(defun %add-sub-packages (h-branch parent-pkg)
  (dolist (s (%get-sub-packages (path h-branch) (parent h-branch)))
    (let ((sym (intern (subseq s (length (path h-branch)))
                       parent-pkg)))
      (setf (symbol-value sym) (find-package s)) ;; prob. the best way to keep a reference to the package
      (export sym parent-pkg))))

(defun %mkstr (&rest args)
  "Useful utility from PGraham."
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defvar %doc-sys%
  (list
   (list #'fboundp 'function)
   (list (lambda (v) (typep v 'method-combination)) 'method-combination)
   (list #'compiler-macro-function 'compiler-macro)
   (list (lambda (v) (typep (find-class v nil) 'structure-class)) 'structure-class)
   (list (lambda (v) (typep (find-class v nil) 'standard-class)) 'type))
  "Doc system functions and corresponding types for documentation
lookup in multiple namespaces of a symbol.")

(defun %get-doc-str (sym doc-sys)
  "Given a symbol, and a doc-sys in the form of '(predicate type), which is
an item in %doc-sys%, return the documentation if any."
  (if (funcall (first doc-sys) sym)
      (or (documentation sym (second doc-sys)) "")
    ""))

(defun %get-sym-desc (sym)
  "Given a symbol, return its corresponding description. If there are descriptions in
more than one namespace (function, variable, class, etc.), combine the results."
  (apply #'%mkstr
         (mapcar (lambda (doc-sys) (%get-doc-str sym doc-sys))
                 %doc-sys%)))

(defun %set-full-desc (s)
  "s: lib-symbol"
  (setf (full-desc s)
        (concatenate 'string (path (parent s))
                     (%get-sym-desc (first (syms s))))))

(defun setup-packages (package-tree)
  "Creates and defines packages in package-tree."
  (flet ((%create-packages (package-tree)
           "Create each package, without any detail such as import, use etc."
           (let (failed-packages)
             (dolist (branch (branches package-tree))
               (handler-case
                   (let ((pkg (make-package (path branch) :use '("COMMON-LISP"))))
                     (setf (documentation pkg t)
                           (path-desc branch)))
                 (error (c)
                   (declare (ignore c))
                   (push (path branch) failed-packages)))
               (when (> (length failed-packages) 1)
                 (format t "Failed to create packages: ~{~a, ~}.~%" failed-packages)))))

         (%link-subpackages (package-tree)
           "For each package, create symbols of \".sub-package-name\" that refers
            to the sub package object."
           (flet ((%define-sub-package-syms (p)
                    "p: lib-hierarchy-branch"
                    (flet ((%import-and-get-symbols (lib-sym to-pkg)
                             "For one target symbol, return a list of symbols which are either from a
            system-package, or a list of sym-nameN{~}* where ~ is optional. See
            Lazy interning in the top comment of *lib-package-tree* for details.

            lib-sym: lib-symbol
            sym-name: string
            systems: list of (sys-name package-name)
            "
                             (flet ((%lazy-intern (lib-sym sym-cnt to-pkg)
                                      "sys: (sys-name package-name)

See Lazy interning in the
top comment of *lib-package-tree* for details.

Intern a symbol, and return that symbol name (package relative).
"
                                      (let* ((new-sym-name (%get-target-sym-name (sym-name lib-sym) sym-cnt :loaded t))
                                             (orig-pkg (nth sym-cnt (origin-packages lib-sym)))
                                             (from-package (find-package (pkg-name orig-pkg))))
                                        (unless from-package
                                          (error "lazy-intern: package ~a not found.~%" (pkg-name orig-pkg)))
                                        (if (system-loaded (containing-system orig-pkg))
                                            (%intern-now new-sym-name
                                                         (find-symbol (sym-name lib-sym) from-package)
                                                         to-pkg)
                                          (%intern-later new-sym-name orig-pkg to-pkg (parent (parent lib-sym)))))))

                               (let (syms
                                     (last-added-sym 0))
                                 (dolist (orig-pkg (origin-packages lib-sym) syms)
                                   (maybe-load-system-at-startup orig-pkg)
                                   (push (%lazy-intern lib-sym last-added-sym to-pkg)
                                         syms)
                                   (incf last-added-sym))))))

                      (let (syms
                            (to-pkg (find-package (path p))))
                        (dolist (s (lib-symbols p))
                          (setf (syms s) (%import-and-get-symbols s to-pkg))
                          (setf syms (append syms (syms s)))
                          (%set-full-desc s))
                        (export syms to-pkg)
                        (%add-sub-packages p to-pkg)))))

             (dolist (branch (branches package-tree))
               (%define-sub-package-syms branch)))))

    (%create-packages package-tree)
    (%link-subpackages package-tree)))

(defun delete-system-aux ()
  (dolist (pd lib~:*hierarchies*)
    (delete-hierarchy pd))
  (delete-package "LIB~")
  (asdf:clear-system :lib-helper))

(defun delete-hierarchy (hierarchy)
  (let (pkgs-error)
    (dolist (p (branches hierarchy))
      (handler-case
          (delete-package (path p))
        (error (c)
          (declare (ignore c))
          (push (path p) pkgs-error))))
    (when pkgs-error
      (format t "For lib hierarchy ~A:~%  Error deleting packages ~{~a, ~}.~%" 
              hierarchy 
              pkgs-error))))

(defun %get-target-sym-name.test1 ()
  (assert (equalp "CAR.1~" (%get-target-sym-name "CAR" 1 :loaded nil)))
  (assert (equalp "CAR~" (%get-target-sym-name "CAR" 0)))
  (assert (equalp "CAR" (%get-target-sym-name "CAR" 0 :loaded t))))

(defun packages-aux (package-tree &key (stream *standard-output*))
  "Print package names in first hierachical categorization."
  (dolist (p (branches package-tree))
    (let* ((p-subs (%get-sub-packages (path p) package-tree))
           (line-length 90)
           (separation 27)
           (cursor separation))
      (when p-subs
        (format stream "~a~25,0t: ~a "
                (path-desc p)
                (path p))
        (dolist (p1 p-subs)
          (when (> cursor (- line-length separation))
            (setf cursor separation)
            (format stream "~%~25,0t  "))
          (format stream "~a " p1)
          (incf cursor (1+ (length p1))))
        (format stream "~%")
        (terpri stream)))))

(defun symbol-count (package-tree)
  "Return the count of unique symbols within the tree."
  (let (flat-syms)
    (dolist (p (branches package-tree))
      (setq flat-syms (append flat-syms (lib-symbols p))))
    (length (delete-duplicates flat-syms :test #'equalp))))

(defun %remove-setf-functions (lst extract-function)
  "From a list of symbols of generic functions, remove the setf ones.
extract-function: lambda to get the symbol-function from an element."
  (remove-if (lambda (s) (consp
                          (closer-mop:generic-function-name
                           (funcall extract-function s))))
             lst))

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

 |#
(defun %get-generic-functions ()
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

(defun %format-package-tree-branch-syms (tree stream)
  (format stream "(\"~a\" \"~a\"~%(" (first tree) (second tree))
  (dolist (s (third tree))
    (format stream "(~s (~s ~s))~%"
            (first s) (caadr s) (cadadr s)))
  (format stream "))~%"))

(defun %format-package-tree-syms (tree stream)
  (dolist (branch tree)
    (%format-package-tree-branch-syms branch stream)))

(defun %symbol-full-string (sym)
  (with-output-to-string (s)
    (format s "~s" sym)))

(defun %get-symbol-node (sym sys-name package-name)
  (list (if (and (consp sym) (eql (first sym) 'SETF))
            (concatenate 'string "SETF-" (%symbol-full-string (second sym)))
          (%symbol-full-string sym))
        (list sys-name package-name)))


(defun %filter-external-syms (names pkg)
  (let (result)
    (dolist (sym names result)
      (multiple-value-bind (s status) (intern sym pkg)
        (declare (ignore s))
        (when (eq status :external)
          (push sym result))))))

(defun %get-struct-slot-names (struct-name package-name)
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
    (mapcar (lambda (w) (%mkstr struct-name "-" (subseq w 1 (1- (length w)))))
            (cl-ppcre:all-matches-as-strings ":[^:].+? " struct-form))))

(defun %get-struct-externals (struct-name package-name)
  "Given a struct name, check for each struct name and return the
package-external ones as a list."
  (let* ((struct-name (string-upcase struct-name))
         (names
          (append
           (list (%mkstr struct-name "-P")
                 (%mkstr "MAKE-" struct-name)
                 (%mkstr "COPY-" struct-name)) ; slot names couldn't be retrieved portably yet
           (%get-struct-slot-names struct-name package-name)))
         (pkg (find-package package-name)))
    (%filter-external-syms names pkg)))

(defun %compute-applicable-methods-using-classes (gf required-classes)
  (remove-if-not (lambda (method)
                   (every #'c2mop:subclassp
                          required-classes
                          (c2mop:method-specializers method)))
                 (c2mop:generic-function-methods gf)))

(defvar *skipped-classes-in-check*
  (list (find-class 'standard-object)
        (find-class t)
        #+lispworks (find-class 'clos::dependee-mixin)
        #+lispworks (find-class 'clos::specializer)
        #+lispworks (find-class 'harlequin-common-lisp:metaobject)

        ))

(defun %in-skipped-classes (c)
  (member c *skipped-classes-in-check*))

(defun %subclassp (sub parent-tree)
  (find sub
        (remove-if #'%in-skipped-classes parent-tree)))

(defun %get-class-methods (class &optional (generic-functions nil))
  (let (methods
        (class-tree (c2mop:class-precedence-list class)))
    (unless generic-functions (setf generic-functions (%get-generic-functions)))
    (dolist (gt generic-functions methods)
      (dolist (gm (gmethods gt))
        ;; (when (some (lambda (arg) (eq arg class))
        (when (some (lambda (arg) (%subclassp arg class-tree))
                    (specializers gm))
          (push gm methods))))))

(defun %get-class-methods-unique-names (class &optional (generic-functions nil))
  (remove-duplicates
   (mapcar (lambda (m)
            (c2mop:generic-function-name
             (c2mop:method-generic-function (method-obj m))))
          (%get-class-methods class generic-functions))
   :test 'equalp))

(defun %get-class-methods.test1 ()

  (defclass test-class ()
    ((slot1 :accessor slot1)
     (slot2 :reader rslot2
            :writer wslot2)))

  (defclass test-class-child (test-class)
    ((slot1 :accessor slot1)))

  (defmethod get-slot1 (hello-out (obj test-class))
    "this is get-slot1"
    (print hello-out)
    (slot-value obj 'slot1))

  (defmethod get-slot1-child (hello-out (obj test-class-child))
    "this is get-slot1 for child"
    (print hello-out)
    (slot-value obj 'slot1))

  (let ((mlist (%get-class-methods (find-class 'test-class))))
    (assert (member (function get-slot1) mlist))
    (assert (member (function wslot2) mlist)))

  (let ((mlist (%get-class-methods (find-class 'test-class-child))))
    (assert (member (function get-slot1) mlist))
    (assert (member (function get-slot1-child) mlist))
    (assert (member (function wslot2) mlist)))

  )

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
     (append (%filter-external-syms names (find-package pkg-name))
             methods)
     :test 'equalp)))

(defun %get-package-external-symbols (package)
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

(defun %get-package-symbols (package-name sys generic-functions)
  "Return two values:
              val1: a list of package-syms
              val2: a tree of classes and their symbols as:
                    ((class1-obj class1 sym1.1 sym1.2 ...)
                     (class2-obj class2 sym2.1 sym2.2 ...))
              val3: a tree of structs and their symbols as:
                    ((struct1 syms1.1 syms1.2 ...)
                     (..))"
  (let (sym-list
        class-syms struct-syms
        (external-syms (%get-package-external-symbols package-name)))

    (dolist (sym external-syms)
      (push (%get-symbol-node sym sys package-name) sym-list)
      (when (typep (find-class sym nil) 'structure-class)
        (push sym struct-syms))
      (when (typep (find-class sym nil) 'standard-class)
        (push (list (find-class sym) sym) class-syms)))

    (dolist (st struct-syms)
      (nconc st
             (sort
              (mapcar (lambda (s) (%get-symbol-node s sys package-name))
                      (%get-struct-externals st package-name))
              #'string-lessp :key #'car)))

    (dolist (cs class-syms)
      (nconc cs
             (sort
              (mapcar (lambda (s) (%get-symbol-node s sys package-name))
                      (%get-class-methods-unique-names (first cs) generic-functions))
              #'string-lessp :key #'car)))
    (values
     (sort sym-list #'string-lessp :key #'car)
     (remove-if-not #'third class-syms)
     struct-syms)))

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

e.g. given call: (generate-system-symbols \"lil\" \"LIB.CONT\"
                                          \"(\"LIL/PURE/HASH-TABLE\"
                                            \"LIL/INTERFACE/ORDER\")
this function returns:

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
                   (concatenate 'string desc package-name))))

    (let (tree
          package-name
          (generic-functions (%get-generic-functions)))
      (dolist (p packages (%format-package-tree-syms (nreverse tree)
                                                     stream))
        (setf package-name (package-name (first p)))
        (push (get-header package-name (second p)) tree)
        (multiple-value-bind (package-symbols class-syms struct-syms)
            (%get-package-symbols package-name sys-name generic-functions)
          (nconc (first tree) (list package-symbols))
          (dolist (c class-syms)
            (when (third c)
              (push (get-header (%mkstr package-name ".." (second c))
                                (%mkstr (second p) ".." (second c))
                                "Class: ")
                    tree)
              (nconc (first tree) (list (nthcdr 2 c)))))
          (dolist (s struct-syms)
            (when (second s)
              (push (get-header (%mkstr package-name ".." (first s))
                                (%mkstr (second p) ".." (second s))
                                "Struct: ")
                    tree)
              (nconc (first tree) (list (cdr s))))))))))

(defun generate-system-symbols.example1 ()
  (generate-system-symbols "lil" "LIB.CONT"
                             '(("LIL/CORE/ALL" "LIL.CORE")
                               ("LIL/INTERFACE/ALL" "LIL.INTERFACE")
                               ("LIL/PURE/ALL" "LIL.PURE")
                               ("LIL/STATEFUL/ALL" "LIL.STATEFUL")
                               ("LIL/TRANSFORM/CLASSY" "LIL.CLASSY")
                               ("LIL/TRANSFORM/POSH" "LIL.POSH"))))

(defun %get-package-owned-external-symbols (package)
  "Return the symbols that are exported from the package, i.e. external."
  (unless (packagep package)
    (setf package (find-package package)))
  (let (result)
    (with-package-iterator (sym package :external)
      (loop (multiple-value-bind (more? symbol access pkg) (sym)
              (declare (ignore access pkg))
              (unless more? (return))
              (when (eql (symbol-package symbol) package)
                (push symbol result)))))
    result))


(defun %find-lib-aux (search-closure package-tree &optional (print-results t))
  "Search in package-tree for symbols meeting search-closure predicate.
     print-results: if t, print results, don't return, otherwise return a list of results.
"
  (let (result)
    (dolist (branch (branches package-tree))
      (dolist (sym-list (lib-symbols branch))
        (when (funcall search-closure sym-list)
          (push (concatenate 'string
                             (path (parent sym-list))
                             ":"
                             (sym-name sym-list))
                result))))
    (if print-results
        (format t "~{~a~%~}" result)
      result)))

(defun apropos-lib (sub-str package-tree &optional (print-results t))
  "Look for symbols containing sub-str in the lib hierarchy and
print the matching branches.

print-results: if t (default), print the results instead of returning a list.
"
  (%find-lib-aux (lambda (sym-list)
                   (search sub-str (sym-name sym-list)
                           :test 'equalp))
                package-tree print-results))

(defun %match-with-symbol (phrase-regexes sd-obj)
  "Return true if any expression in phrase-regexes matches the symbol name AND
all the rest in phrase-regexes match the symbol's full-description (which
includes hierarchy path along with all namespace descriptions)."
  (let* ((sym-name (symbol-name (sym sd-obj)))
         (desc (full-desc sd-obj)))
    (and
     (cl-ppcre:scan (first phrase-regexes) sym-name)
     (every (lambda (s) (cl-ppcre:scan s desc)) (rest phrase-regexes)))))

(defun find-syms (phrase package-tree &optional (print-results t))
  "Given a number of words or re-patterns in the phrase (first word for the symbol, others for
description and package path, find the closest matches within the lib hierarchy.

re-patterns use cl-ppcre.

print-results: if t (default), print the results instead of returning a list.
"
  (let ((phrase-regexes (mapcar (lambda (w) (cl-ppcre:create-scanner w
                                                                     :case-insensitive-mode t))
                                (typecase phrase
                                  (cons phrase)
                                  (string
                                   (mapcar (lambda (w) (concatenate 'string
                                                                    ".*"
                                                                    w
                                                                    ".*"))
                                           (cl-ppcre:split "\\s+" phrase)))))))
    (%find-lib-aux (lambda (sym-list)
                     (%match-with-symbol phrase-regexes
                                         (convert <symbol-and-desc> <lib-symbol> sym-list)))
                   package-tree print-results)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some dev helpers:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-lib.v1 (lib &optional (output *standard-output*))
  (format output "(\"~a\" \"~a\"~%(" (first lib) (second lib))
  (dolist (sym (third lib))
    (format output "(\"~a\" (NIL \"~a\"))~%"
            (first sym)
            (if (atom sym)
                "CL"
              (second sym))))
  (format output "))~%"))

(defun print-libs (&optional (output *standard-output*)
                             (package-tree '*lib-package-tree*))
  (format output "(defvar *lib-package-tree*~%'(")
  (dolist (lib (symbol-value package-tree))
    (print-lib.v1 lib output))
  (format output ")~%\"~a\")"
          (documentation package-tree 'variable)))

(defun save-libs-to-file (&optional (file "lib-defs-auto.lisp"))
  (with-open-file (s file :direction :output
                     :if-exists :supersede)
    (print-libs s)))

(defun print-lib (lib &optional (output *standard-output*))
  (format output "(\"~a\" \"~a\"~%(" (first lib) (second lib))
  (dolist (sym (third lib))
    (format output "(\"~a\" \"~a\")~%"
            (if (atom sym)
                sym
              (first sym))
            (if (atom sym)
                "CL"
              (second sym))))
  (format output "))~%"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; package-tree generation examples for some systems

;; lil:
(lambda ()
  (with-open-file (f "temp-defs.lisp~" :direction :output :if-exists :supersede)
    (generate-system-symbols "lil" "LIB.CONT"
                             '(("LIL/CORE/ALL" "LIL.CORE")
                               ("LIL/INTERFACE/ALL" "LIL.INTERFACE")
                               ("LIL/PURE/ALL" "LIL.PURE")
                               ("LIL/STATEFUL/ALL" "LIL.STATEFUL")
                               ("LIL/TRANSFORM/CLASSY" "LIL.CLASSY")
                               ("LIL/TRANSFORM/POSH" "LIL.POSH"))
                             f))

  (with-open-file (f "temp-defs.lisp~" :direction :output :if-exists :supersede)
    (generate-system-symbols "cl-ppcre" "LIB.STR"
                             '(("CL-PPCRE" "ppcre"))
                             f)))

(defun %create-hierarchy.test ()
  (let ((pkg-tree
         (convert
          <lib-hierarchy> <list>
          '(("STD" "Top level"
             ())
            ("STD.FUN" "Functions"
             (("DEFUN" (NIL "CL"))
              ("VALUES" (NIL "CL"))
              ("MULTIPLE-VALUE-BIND" (NIL "CL"))
              ("LAMBDA" (NIL "CL"))
              ("FUNCALL" (NIL "CL"))
              ("APPLY" (NIL "CL"))
              ("DEFSETF" (NIL "CL"))
              ("COMPILED-FUNCTION" (NIL "CL"))
              ("FUNCTION" (NIL "CL"))
              ("FUNCTIONP" (NIL "CL"))
              ("DEFMACRO" (NIL "CL"))
              ("COMPLEMENT" (NIL "CL"))
              ("IDENTITY" (NIL "CL"))
              ("FBOUNDP" (NIL "CL"))
              ("CONSTANTLY" (NIL "CL"))
              ))
            ("STD.CONT" "Containers"
             (
              ))
            ("STD.CONT.SEQ" "Sequences"
             (
              ))))))
    pkg-tree))

(defun %packages-aux.test ()
  (packages-aux (%create-hierarchy.test)))



#|
(defmacro defun-in-pkg (name args (&key package) &body body)
  "Define a function in a package.

**e.g.**
    (defun-in-pkg fn (arg)
        (:package (find-package \"LIB.STR\")
      (print arg)))
"
  `(let ((f (intern ,(string-upcase (quote name)) ,package)))
      (setf (symbol-function f)
            (lambda ,args
              ,@body))))

(defmacro mtest (name args (&key package) &body body)
  `(let ((f ,(intern (string-upcase (string name))
                     package)))
     (setf (symbol-function (quote ,f))
           (lambda ,args
             ,@body))))

(defmacro mtest (name args (&key package) &body body)
  (let ((f (intern (string-upcase (string name))
                   package)))
     `(setf (symbol-function ,f)
            (lambda ,args
              ,@body))))


(defun defun-in-pkg.test ()
  (assert (equal
           '(SYSTEM::SET-SYMBOL-FUNCTION LIB::TEST-FN (LAMBDA (ARG) (PRINT ARG)))
           (let ((pkg (find-package "LIB")))
             (macroexpand (list 'lib~::mtest 'test-fn '(arg)
                                `(:package ,pkg)
                                '(print arg)))))))


|#
