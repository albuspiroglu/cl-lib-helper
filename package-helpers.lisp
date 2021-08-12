
(in-package "LIB~")


(defun %package-tree-branch-symbol (sym-list)
  "Given a sym-list from the package-tree, return the symbol string.
A sym-list is of shape: (\"CIRCULAR-LIST\" (\"alexandria\" \"ALEXANDRIA\"))
"
  (first sym-list))

(defun %package-tree-branch-full-desc (sym-list)
  "Given a sym-list from the package-tree, return the full description.
A sym-list is of shape: (\"CIRCULAR-LIST\" (\"alexandria\" \"ALEXANDRIA\") \"full-description\")
"
  (third sym-list))
  
(defun %package-tree-branch-systems (sym-list)
  "Given a sym-list from the package-tree, return the systems list.
A sym-list is of shape: (\"CIRCULAR-LIST\" (\"alexandria\" \"ALEXANDRIA\"))
and the systems list is (sys-name pkg-name) (sys2-name pkg2-name) ..
"
  (rest sym-list))
  
(defun %package-tree-symbols (branch)
  "Return the symbols list of a package-tree branch.
A branch is e.g.:
(\"LIB.CONT.LIST.CREATE\" \"List creation\" 
     ((\"CIRCULAR-LIST\" (\"alexandria\" \"ALEXANDRIA\")) ; symbols list start here
      (\"CONS\" (NIL \"CL\"))
      (\"COPY-LIST\" (NIL \"CL\"))
      ...
"
  (third branch))

(defun %package-tree-symbols-cdr (branch)
  (nthcdr 1 branch))

(defun %package-tree-push-sym-list (branch sym-list)
  "Insert a sym-list, which is of shape: (sym (sys package))
into branch."
  (if (third branch)
      (push sym-list (third branch))
    (nconc branch (list sym-list))))

(defun %package-tree-branch-path (branch)
  "Return the path for the package-tree-branch.
A branch is e.g.:
(\"LIB.CONT.LIST.CREATE\" \"List creation\" 
     ((\"CIRCULAR-LIST\" (\"alexandria\" \"ALEXANDRIA\"))
      (\"CONS\" (NIL \"CL\"))
      (\"COPY-LIST\" (NIL \"CL\"))
      ...
"
  (first branch))


(defun %package-tree-branch-doc (branch)
  "Return the documentation for the package-tree-branch.
A branch is e.g.:
(\"LIB.CONT.LIST.CREATE\" \"List creation\" 
     ((\"CIRCULAR-LIST\" (\"alexandria\" \"ALEXANDRIA\"))
      (\"CONS\" (NIL \"CL\"))
      (\"COPY-LIST\" (NIL \"CL\"))
      ...
"
  (second branch))

(defun get-package-names-aux (package-tree)
  "Return a list of package names in the package-tree."
  (let (names)
    (dolist (p package-tree names)
      (push (%package-tree-branch-path p) names))))

(defun setup-packages (package-tree)
  "Creates and defines packages in package-tree."
  (%create-packages package-tree)
  (%define-subpackages package-tree))

(defun %create-packages (package-tree)
  "Create each package, without any detail such as import, use etc."
  (dolist (p package-tree)
    (setf (documentation (make-package (%package-tree-branch-path p) :use '("COMMON-LISP")) t)
          (%package-tree-branch-doc p))))

(defun %define-subpackages (package-tree)
  (dolist (p package-tree)
    (%define-sub-package-syms p package-tree)))

(defun delete-system-aux ()
  (dolist (pd lib~:*package-lists*)
    (dolist (p (symbol-value pd))
      (handler-case
          (delete-package (first p))
        (error (c)
          (format t "Error deleting package ~a.~%" (first p))))))
  (delete-package "LIB~")
  (asdf:clear-system :lib-helper))

(defun %define-sub-package-syms (p package-tree)
  "p: list of (pkg-name description (syms*))"
  (let (syms
        (to-pkg (find-package (%package-tree-branch-path p))))
    (dolist (s (%package-tree-symbols p))
      ;; s: (sym-name (from-system1 from-pkg1) (from-system2 from-pkg2) ..)
      (let ((s-syms (%import-and-get-symbols (%package-tree-branch-symbol s)
                                             (%package-tree-branch-systems s)
                                             to-pkg
                                             package-tree)))
        (setf syms (append syms s-syms))
        (%add-desc-to-sym-branch s (first s-syms) (%package-tree-branch-path p))))
    (export syms to-pkg)
    (%add-sub-packages (first p) to-pkg package-tree)))

(defun %add-desc-to-sym-branch (s sym path)
  "s being: (sym-name (from-system1 from-pkg1) (from-system2 from-pkg2) ..),
append the full description to the end."
  (nconc s (cons (concatenate 'string path " : "
                              (%get-sym-desc sym)) nil)))      
  
(defun %import-and-get-symbols (sym-name systems to-pkg pkg-tree)
  "For one target symbol, return a list of symbols which are either from a
system-package, or a list of sym-nameN{~}* where ~ is optional. See
Lazy interning in the top comment of *lib-package-tree* for details.

  sym-name: string
  systems: list of (sys-name package-name)
"
  (let (syms
        (last-added-sym 0))
    (dolist (sys systems syms)
      (%maybe-load-at-startup sys)
      (push (%lazy-intern sym-name sys last-added-sym to-pkg pkg-tree)
            syms)
      (incf last-added-sym))))

(defun %maybe-load-at-startup (system)
  "asdf load the system if necessary.
system: (sys-name from-package-name)"
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

(defun %asdf-system-loaded (sys-name)
  (find sys-name (asdf:already-loaded-systems) :test #'equalp))

(defun %maybe-load (sys-name)
  "asdf load the system if necessary."
  (if (%loaded? sys-name)
      nil
    (progn
      (asdf:load-system sys-name)
      (%set-loaded sys-name)
      t)))

(defun %lazy-intern (sym-name sys sym-cnt to-pkg pkg-tree)
  "sys: (sys-name package-name)
See Lazy interning in the
top comment of *lib-package-tree* for details.

Intern a symbol, and return that symbol name (package relative).
"
  (let ((new-sym-name (%get-target-sym-name sym-name sym-cnt :loaded t))
        (from-package (find-package (second sys))))
    (unless from-package
      (error "lazy-inter: package ~a not found.~%" (second sys)))
    (if (%loaded? (first sys))
        (%intern-now new-sym-name
                     (find-symbol sym-name from-package)
                     to-pkg)
      (%intern-later new-sym-name sys to-pkg pkg-tree))))

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
      (%append-unloaded-suffix new-sym-name))))

(defun %append-unloaded-suffix (sym-name)
  (concatenate 'string sym-name "~"))

(defun %get-target-sym-name.test1 ()
  (assert (equalp "CAR.1~" (%get-target-sym-name "CAR" 1 :loaded nil)))
  (assert (equalp "CAR~" (%get-target-sym-name "CAR" 0)))
  (assert (equalp "CAR" (%get-target-sym-name "CAR" 0 :loaded t))))

(defmacro %with-system ((sys-var sys-name) &body body)
  (let ((name (gensym)))
    `(let* ((,name ,sys-name)
            (,sys-var (gethash ,name *system-table*)))
       (if ,sys-var
           (progn
             ,@body)
         (error "System name ~a not found in *system-table*, consider adding it?~%"
                ,name)))))

(defun %should-load-at-startup (sys-name)
  "Return t if sys-name should be loaded. This depends on load-at-startup and (already)
loaded values."
  (if sys-name
      (%with-system (system sys-name)
        (and (first system) (not (second system))))
    ;; nil sys-name means cl std pkg, no loading
    nil))

(defun %set-loaded (sys-name)
  (%with-system (system sys-name)
    (setf (second system) t)))

(defun %loaded? (sys-name)
  (if sys-name
      (%with-system (system sys-name)
        (second system))
    t))

(defun %intern-now (sym-name sym to-pkg)
  (if (equalp sym-name (symbol-name sym))
      (progn
        (shadowing-import sym to-pkg)
        sym)
    (let ((new-sym (intern sym-name to-pkg)))
      (setf (symbol-value new-sym) sym)
      new-sym)))

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

(defun %activate-system (sys pkg-tree)
  "asdf:load the system and for every symbol of it
import-export them in the tree."
  (if (%maybe-load (first sys))
      (let ((from-pkg (find-package (second sys))))
        (dolist (to-pkg-details pkg-tree)
          (%rename-import-syms sys to-pkg-details from-pkg))
        (format t "All symbols of system ~a imported.~%" (first sys)))
    (format t "System ~a already activated. Nothing to do.~%"
            (first sys))))

(defun %rename-import-syms (sys to-pkg-details from-pkg)
  "For the symbols in to-pkg-details that belong to sys,
unintern the symbols corresponding to that package, which will be named
as a-symbol{.N}~, and shadowing-import every the a-symbol name from from-pkg
to to-pkg in to-pkg-details.
sys: (list sys-name package-name)"
  (flet ((belongs-to-sys (pkg-sym)
           (search (list (first sys))
                   (cdr pkg-sym)
                   :test (lambda (a b) (equalp a (first b))))))
    (let ((to-pkg (find-package (first to-pkg-details))))
      (dolist (s (%package-tree-symbols to-pkg-details))
        (let ((i (belongs-to-sys s)))
          (when i
            (let ((sym (find-symbol (%package-tree-branch-symbol s) from-pkg)))
              (shadowing-import sym to-pkg)
              (unintern (find-symbol (%get-target-sym-name (%package-tree-branch-symbol s) i)
                                     to-pkg)
                        to-pkg)
              (export sym to-pkg))))))))
  
(defun %add-sub-packages (p-name parent-pkg package-tree)
  (dolist (s (%get-sub-packages p-name package-tree))
    (let ((sym (intern (subseq s (length p-name))
                       parent-pkg)))
      (setf (symbol-value sym) (find-package s))
      (export sym parent-pkg))))
  
(defun %get-sub-packages (pkg-name package-tree)
  "pkg-name: string"
  (let (subs)
    (dolist (p package-tree subs)
      (if (string-equal (%get-parent-name (first p))
                        pkg-name)
          (push (first p) subs)))))

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
  (assert (string-equal (%get-parent-name "lib")
                        "")))

(defun packages-aux (package-tree &key (stream *standard-output*))
  "Print package names in first hierachical categorization."
  (dolist (p package-tree)
    (let* ((p-subs (%get-sub-packages (first p) package-tree))
           (line-length 90)
           (separation 27)
           (cursor separation))
      (when p-subs
        (format stream "~a~25,0t: ~a "
                (%package-tree-branch-doc p)
                (%package-tree-branch-path p))
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
    (dolist (p package-tree)
      (setq flat-syms (append flat-syms (%package-tree-symbols p))))
    (delete-duplicates flat-syms :test #'equalp)
    (length flat-syms)))

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
                   (concatenate 'string desc package-name)))
                   
           )

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

(defun %get-generic-functions ()
  "Return all generic functions defined in the current lisp image.

(gm1 gm2 ...):
gm: (gf-sym (gm1 lambda-list1 ..) ..)
"
  (let (result)
    (do-all-symbols (sym (mapcar
                          (lambda (g)
                            (append (list g)
                                    (mapcar
                                     (lambda (gm)
                                       (list gm (closer-mop:method-lambda-list gm)))
                                     (closer-mop:generic-function-methods (symbol-function g)))))
                          (remove-duplicates
                           ;; remove any setf ones:
                           (remove-if (lambda (s) (consp
                                                   (closer-mop:generic-function-name
                                                    (symbol-function s))))
                                      result))))
      (if (and (fboundp sym)
               (typep (symbol-function sym) 'generic-function))
          (push sym result)))))

(defun %get-symbol-node (sym sys-name package-name)
  (list (string sym)
        (list sys-name package-name)))

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
                      (%get-class-externals (first cs)
                                            package-name
                                            generic-functions))
              #'string-lessp :key #'car)))
    (values
     (sort sym-list #'string-lessp :key #'car)
     (remove-if-not #'third class-syms)
     struct-syms)))

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

(defun generate-system-symbols.test1 ()
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

(defun %format-package-tree-syms (tree stream)
  (dolist (branch tree)
    (%format-package-tree-branch-syms branch stream)))

(defun %format-package-tree-branch-syms (tree stream)
  (format stream "(\"~a\" \"~a\"~%(" (first tree) (second tree))
  (dolist (s (%package-tree-symbols tree))
    (format stream "(~s (~s ~s))~%"
            (first s) (caadr s) (cadadr s)))
  (format stream "))~%"))

(defun %find-lib-aux (search-closure package-tree &optional (print-results t))
  "Search in package-tree for symbols meeting search-closure predicate.
     print-results: if t, print results, don't return, otherwise return a list of results.
"
  (let (result)
    (dolist (branch package-tree)
      (dolist (sym-list (%package-tree-symbols branch))
        (when (funcall search-closure sym-list branch)
          (push (concatenate 'string
                             (%package-tree-branch-path branch)
                             ":"
                             (%package-tree-branch-symbol sym-list))
                result))))
    (if print-results
        (format t "~{~a~%~}" result)
      result)))

(defun apropos-lib (sub-str package-tree &optional (print-results t))
  "Look for symbols containing sub-str in the lib hierarchy and
print the matching branches.

print-results: if t (default), print the results instead of returning a list.
"
  (%find-lib-aux (lambda (sym-list branch)
                   (declare (ignore branch))
                   (search sub-str (%package-tree-branch-symbol sym-list)
                           :test 'equalp))
                package-tree print-results))

(defun find-syms (phrase package-tree &optional (print-results t))
  "Given a number words in the phrase (first word for the symbol, others for
description and package path, find the closest matches within the lib hierarchy.

print-results: if t (default), print the results instead of returning a list.
"
  (%find-lib-aux (lambda (sym-list branch)
                   (%match-with-symbol phrase (%package-tree-branch-symbol-fullpath
                                               sym-list
                                               (%package-tree-branch-path branch))))
                 package-tree print-results))

(defun %package-tree-branch-symbol-fullpath (sym-list branch-path)
  "Given a package-tree's sym-list, return the symbol with package-path.
e.g. given:
         sym-list    : ('<HASH-TABLE>' ('lil' 'LIL/PURE/HASH-TABLE') \"full-description\")
         branch-path : 'LIB.CONT.LIL.PURE.HASH-TABLE'
     return the list:
        (--some-original-package-name--:<HASH-TABLE> \"LIB.CONT.LIL.PURE.HASH-TABLE\" \"full-description\")
"
  (list (intern (%package-tree-branch-symbol sym-list) (find-package branch-path))
        branch-path
        (%package-tree-branch-full-desc sym-list)))
         
(defun %match-with-symbol (phrase full-path-symbol)
  "Return true if any word in phrase matches the symbol name AND
all the words in the phrase (except the symbol name) matches the
symbol's description."
  (let* ((phrase-words (cl-ppcre:split "\\s+" phrase))
         (sym-name (symbol-name (first full-path-symbol)))
         (phrase-words-except-sym (rest phrase-words))
         (desc (third full-path-symbol)))
    (and
     (search (first phrase-words) sym-name :test 'equalp)
     (every (lambda (s) (search s desc :test 'equalp)) phrase-words-except-sym))))

(defvar %doc-sys%
  (list
   (list #'fboundp 'function)
   (list (lambda (v) (typep v 'method-combination)) 'method-combination)
   (list #'compiler-macro-function 'compiler-macro)
   (list (lambda (v) (typep (find-class v nil) 'structure-class)) 'structure-class)
   (list (lambda (v) (typep (find-class v nil) 'standard-class)) 'type))
  "Doc system functions and corresponding types for documentation lookup.")

(defun %get-sym-desc (sym)
  "Given a symbol, return its corresponding description. If there are descriptions in
more than one namespace (function, variable, class, etc.), combine the results."
  (apply #'%mkstr
         (mapcar (lambda (doc-sys) (%get-doc-str sym doc-sys))
                 %doc-sys%)))

(defun %get-doc-str (sym doc-sys)
  "Given a symbol, and a doc-sys in the form of '(predicate type), which is
an item in %doc-sys%, return the documentation if any."
  (if (funcall (first doc-sys) sym)
      (or (documentation sym (second doc-sys)) "")
    ""))

(defun %mkstr (&rest args)
  "Useful utility from PGraham."
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some dev helpers:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-libs (&optional (output *standard-output*)
                             (package-tree '*lib-package-tree*))
  (format output "(defvar *lib-package-tree*~%'(")
  (dolist (lib (symbol-value package-tree))
    (print-lib.v1 lib output))
  (format output ")~%\"~a\")"
          (documentation package-tree 'variable)))

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

(defun print-lib.v1 (lib &optional (output *standard-output*))
  (format output "(\"~a\" \"~a\"~%(" (first lib) (second lib))
  (dolist (sym (third lib))
    (format output "(\"~a\" (NIL \"~a\"))~%"
            (first sym)
            (if (atom sym)
                "CL"
              (second sym))))
  (format output "))~%"))

(defun save-libs-to-file (&optional (file "lib-defs-auto.lisp"))
  (with-open-file (s file :direction :output
                     :if-exists :supersede)
    (print-libs s)))

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
                             f)))
  
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
