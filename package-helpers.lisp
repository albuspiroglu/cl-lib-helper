
(in-package "LIB~")

(defclass lib-hierarchy ()
  ((branches :initarg :branches
             :accessor branches
             :type list))

  (:documentation "The list that defines the library hierarchy."))

(defvar <lib-hierarchy> (make-instance 'lib-hierarchy))
;; sbcl barfs at this: (defvar <lib-hierarchy> (c2mop:class-prototype 'lib-hierarchy))
(defvar <list> nil)

(defclass lib-hierarchy-branch ()
  ((path :initarg :path
         :accessor path
         :type string
         :documentation "Path name for the branch. This corresponds to a package, or
                         rather a package is created that corresponds to every branch.")

   (parent :initarg :parent
           :accessor parent
           :type lib-hierarchy
           :documentation "A link to the package-tree of this branch (either std-tree or lib-tree).")
   
   (path-desc :initarg :path-desc
              :accessor path-desc
              :type string
              :documentation "Description of the path or package.")
   
   (lib-symbols :initarg :lib-symbols
                :accessor lib-symbols
                :type list-of-lib-symbols))

  (:documentation "A package tree branch.
A branch is e.g.:
(\"LIB.CONT.LIST.CREATE\" \"List creation\" 
     ((\"CIRCULAR-LIST\" (\"alexandria\" \"ALEXANDRIA\")) ; symbols list start here
      (\"CONS\" (NIL \"CL\"))
      (\"COPY-LIST\" (NIL \"CL\"))
      ...
      "))


(defvar <lib-hierarchy-branch> (make-instance 'lib-hierarchy-branch))

(defclass lib-symbol ()
  ((sym-name :initarg :sym-name
             :accessor sym-name
             :type string
             :documentation "The symbol")

   (sys-pkgs :initarg :sys-pkgs
             :accessor sys-pkgs
             :type list
             :documentation "Systems and packages for this symbol. Each item of
                             the list contains two items, first one name of the system,
                             second one package name.")

   (parent :initarg :parent
           :accessor parent
           :type lib-hierarchy-branch)
   
   (full-desc :initarg :full-desc
              :accessor full-desc
              :type string
              :documentation "Hierarchy path + all namespace descriptions of the
                              symbol combined.")

   (syms :accessor syms
         :type list-of-symbols
         :documentation "The actual symbols, corresponding to sym-name, one for each sys-pkgs.
                         First one is named the same as sym-name, subsequent ones appended
                         an increasing number, from 1. If lazy-interned, then ~ or {.N}~, n
                         being the same number."))
                             
  (:documentation "When there are multiple sys-pkg, multiple symbols will be created in the branch,
      first one with the symbol's name, subsequent ones having a {.N}~, N in (1,2..).
      Also there's a lazy interning process. If a system is not loaded, then its symbols
      will not be imported, but rather a symbol of the same name + ~ appended, and tied
      to a closure that'll load the system then import all symbols from the system to
      their branches."))

(defvar <lib-symbol> (make-instance 'lib-symbol))

(defun lib-symbolp (obj) (typep obj 'lib-symbol))

(defun all-elements-are-lib-symbols (lst) (every #'lib-symbolp lst))

(deftype list-of-lib-symbols ()
  `(and (satisfies listp)
        (satisfies lib-symbolp)))

(defun all-elements-are-symbols (lst) (every #'symbolp lst))

(deftype list-of-symbols ()
  `(and (satisties listp)
        (satisfies all-elements-are-symbols)))

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

(defvar <symbol-and-desc> (make-instance 'symbol-and-desc))

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

(defvar <method-detail> (make-instance 'method-detail))

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

(defvar <gf-tree> (make-instance 'gf-tree))
(defvar <symbol> (make-symbol "<SYMBOL>"))

(defmethod print-object ((obj gf-tree) stream)
  (print-unreadable-object (obj stream :type nil :identity t)
    (format stream "GT: ~a" (c2mop:generic-function-name (gf obj)))))

(defgeneric convert (destination origin obj &key &allow-other-keys)
  (:documentation "Convert obj from type origin to type destination.")
  
  (:method ((destination symbol-and-desc) (origin lib-symbol) obj &key &allow-other-keys)
   (let ((path-name (path (parent obj))))
     (make-instance
      'symbol-and-desc
      :sym (intern (sym-name obj) (find-package path-name))
      :hierarchy-name path-name
      :full-desc (full-desc obj))))

  (:method ((destination lib-hierarchy) (origin list) obj &key &allow-other-keys)
   (let ((this (make-instance 'lib-hierarchy))
         res)
     (setf (slot-value this 'branches)
           (dolist (b obj res)
             (push (convert <lib-hierarchy-branch> <list> b :parent this) res)))
     this))

  (:method ((destination lib-hierarchy-branch) (origin list) obj &key parent &allow-other-keys)
   (let ((this (make-instance 'lib-hierarchy-branch
                              :path (first obj)
                              :parent parent
                              :path-desc (second obj)))
         res)
     (setf (slot-value this 'lib-symbols)
           (dolist (s (third obj) res)
             (push (convert <lib-symbol> <list> s :parent this) res)))
     this))

  (:method ((destination lib-symbol) (origin list) obj &key parent &allow-other-keys)
   (make-instance
    'lib-symbol
    :sym-name (first obj)
    :sys-pkgs (rest obj)
    :parent parent))

  (:method ((destination gf-tree) (origin symbol) g &key &allow-other-keys)
   (let ((sg (symbol-function g)))
     (make-instance
      'gf-tree
      :gf sg
      :gmethods (mapcar
                 (lambda (gm)
                   (make-instance
                    'method-detail
                    :method-obj gm
                    :specializers (c2mop:method-specializers gm)))
                 (c2mop:generic-function-methods sg))))))

(defun get-package-names-aux (package-tree)
  "Return a list of package names in the package-tree."
  (let (names)
    (dolist (p (branches package-tree) names)
      (push (path p) names))))

(defun %create-packages (package-tree)
  "Create each package, without any detail such as import, use etc."
  (dolist (branch (branches package-tree))
    (setf (documentation (make-package (path branch) :use '("COMMON-LISP")) t)
          (path-desc branch))))

(defmacro %with-system ((sys-var sys-name) &body body)
  (let ((name (gensym)))
    `(let* ((,name ,sys-name)
            (,sys-var (gethash ,name *system-table*)))
       (if ,sys-var
           (progn
             ,@body)
         (error "System name ~a not found in *system-table*, consider adding it?~%"
                ,name)))))

(defun %set-loaded (sys-name)
  (%with-system (system sys-name)
    (setf (second system) t)))

(defun %loaded? (sys-name)
  (if sys-name
      (%with-system (system sys-name)
        (second system))
    t))
 
(defun %should-load-at-startup (sys-name)
  "Return t if sys-name should be loaded. This depends on load-at-startup and (already)
loaded values."
  (if sys-name
      (%with-system (system sys-name)
        (and (first system) (not (second system))))
    ;; nil sys-name means cl std pkg, no loading
    nil))

(defun %asdf-system-loaded (sys-name)
  (find sys-name (asdf:already-loaded-systems) :test #'equalp))
  
(defun %maybe-load-at-startup (system)
  "asdf load the system if necessary.
system: '(sys-name from-package-name)"
  (if (%should-load-at-startup (first system))
      (progn
        ;; asdf isn't happy about loading other systems during a load operation
        ;; and since we're currently loading lib-helper, don't asdf:load
        ;; the system and just tell the user what to do & give up.
        (unless (or (%asdf-system-loaded (first system))
                    (find-package (second system)))
          (error "=========A symbol is exported from system ~a, but it is currently
not loaded. Either load the system before lib-helper, or remove the flag to
import-symbols-at-startup in known-libs.lisp.~%" (first system)))
        (%set-loaded (first system))
        t)
    nil))

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

(defun %maybe-load (sys-name)
  "asdf load the system if necessary."
  (if (%loaded? sys-name)
      nil
    (progn
      (asdf:load-system sys-name)
      (%set-loaded sys-name)
      t)))

(defun %rename-import-syms (sys branch from-pkg)
  "For the symbols in branch that belong to sys,
unintern the symbols corresponding to that package, which will be named
as a-symbol{.N}~, and shadowing-import every the a-symbol name from from-pkg
to to-pkg in branch.

sys: (list sys-name package-name)
"
  (flet ((belongs-to-sys (lib-sym)
           (search (list (first sys))
                   (sys-pkgs lib-sym)
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
  
(defun %activate-system (sys pkg-tree)
  "asdf:load the system and for every symbol of it
import-export them in the tree."
  (if (%maybe-load (first sys))
      (let ((from-pkg (find-package (second sys))))
        (dolist (branch (branches pkg-tree))
          (%rename-import-syms sys branch from-pkg))
        (format t "All symbols of system ~a imported.~%" (first sys)))
    (format t "System ~a already activated. Nothing to do.~%"
            (first sys))))

(defun %intern-later (sym-name sys to-pkg pkg-tree)
  "Create a symbol with a ~ appended to end, bound to a function to do:
load the associated system (via asdf - not quicklisp, so everything is offline),
create the expected symbol without the ~ this time, pointing to the actual object of concern and
delete the symbol with the ~ at the end.
"
  (let* ((new-sym-name (concatenate 'string sym-name "~"))
         (new-sym (intern new-sym-name to-pkg)))
    (setf (symbol-function new-sym)
          (lambda () (%activate-system sys pkg-tree)))
    new-sym))
   
(defun %lazy-intern (lib-sym sym-cnt to-pkg)
  "sys: (sys-name package-name)

See Lazy interning in the
top comment of *lib-package-tree* for details.

Intern a symbol, and return that symbol name (package relative).
"
  (let* ((new-sym-name (%get-target-sym-name (sym-name lib-sym) sym-cnt :loaded t))
         (sys (nth sym-cnt (sys-pkgs lib-sym)))
         (from-package (find-package (second sys))))
    (unless from-package
      (error "lazy-intern: package ~a not found.~%" (second sys)))
    (if (%loaded? (first sys))
        (%intern-now new-sym-name
                     (find-symbol (sym-name lib-sym) from-package)
                     to-pkg)
      (%intern-later new-sym-name sys to-pkg (parent (parent lib-sym))))))
  
(defun %import-and-get-symbols (lib-sym to-pkg)
  "For one target symbol, return a list of symbols which are either from a
system-package, or a list of sym-nameN{~}* where ~ is optional. See
Lazy interning in the top comment of *lib-package-tree* for details.

  lib-sym: lib-symbol
  sym-name: string
  systems: list of (sys-name package-name)
"
  (let (syms
        (last-added-sym 0))
    (dolist (sys (sys-pkgs lib-sym) syms)
      (%maybe-load-at-startup sys)
      (push (%lazy-intern lib-sym last-added-sym to-pkg)
            syms)
      (incf last-added-sym))))

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

(defun %define-sub-package-syms (p)
  "p: lib-hierarchy-branch"
  (let (syms
        (to-pkg (find-package (path p))))
    (dolist (s (lib-symbols p))
      (setf (syms s) (%import-and-get-symbols s to-pkg))
      (setf syms (append syms (syms s)))
      (%set-full-desc s))
    (export syms to-pkg)
    (%add-sub-packages p to-pkg)))
  
(defun %define-subpackages (package-tree)
  (dolist (branch (branches package-tree))
    (%define-sub-package-syms branch)))

(defun setup-packages (package-tree)
  "Creates and defines packages in package-tree."
  (%create-packages package-tree)
  (%define-subpackages package-tree))

(defun delete-system-aux ()
  (let (pkgs-error)
    (dolist (pd lib~:*package-lists*)
      (dolist (p (branches (symbol-value pd)))
        (handler-case
            (delete-package (path p))
          (error (c)
            (declare (ignore c))
            (push (path p) pkgs-error)))))
    (when pkgs-error
      (format t "Error deleting packages ~{~a, ~}.~%" pkgs-error)))
  (delete-package "LIB~")
  (asdf:clear-system :lib-helper))

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
      (if (and (fboundp sym)
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
  (with-open-file (f "temp-defs.lisp" :direction :output :if-exists :supersede)
    (generate-system-symbols "lil" "LIB.CONT"
                             '(("LIL/CORE/ALL" "LIL.CORE")
                               ("LIL/INTERFACE/ALL" "LIL.INTERFACE")
                               ("LIL/PURE/ALL" "LIL.PURE")
                               ("LIL/STATEFUL/ALL" "LIL.STATEFUL")
                               ("LIL/TRANSFORM/CLASSY" "LIL.CLASSY")
                               ("LIL/TRANSFORM/POSH" "LIL.POSH"))
                             f))

  (with-open-file (f "temp-defs.lisp" :direction :output :if-exists :supersede)
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
