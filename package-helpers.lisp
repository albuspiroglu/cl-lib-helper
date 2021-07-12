
(in-package "LIB~")

(defun get-package-names-aux (package-tree)
  "Return a list of package names in the package-tree."
  (let (names)
    (dolist (p package-tree names)
      (push (first p) names))))
    
(defun setup-packages (package-tree)
  "Creates and defines packages in package-tree."
  (%create-packages package-tree)
  (%define-subpackages package-tree))

(defun %create-packages (package-tree)
  "Create each package, without any detail such as import, use etc."
  (dolist (p package-tree)
    (setf (documentation (make-package (first p) :use '("COMMON-LISP")) t)
          (second p))))

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
        (pkg (find-package (first p))))
    (dolist (s (third p))
      (setf syms (append syms
                         (%import-and-get-symbols (first s)
                                                  (cdr s)
                                                  pkg
                                                  package-tree))))
    (export syms pkg)
    (%add-sub-packages (first p) pkg package-tree)))

(defun %import-and-get-symbols (sym-name systems to-pkg pkg-tree)
  "Return a list of symbols which are either from a system-package, or
a list of sym-nameN{~}* where ~ is optional. See Lazy interning in the
top comment of *lib-package-tree* for details.

  sym-name: string
  systems: list of (sys-name package-name)
"
  (let (syms
        (last-added-sym 0))
    (dolist (sys systems syms)
      (%maybe-load-at-startup (first sys))
      (push (%lazy-intern sym-name sys last-added-sym to-pkg pkg-tree)
            syms)
      (incf last-added-sym))))

(defun %maybe-load-at-startup (sys-name)
  "asdf load the system if necessary."
  (if (%should-load-at-startup sys-name)
      (progn
        (asdf:load-system sys-name)
        (%set-loaded sys-name)
        t)
    nil))

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
  (let ((new-sym-name (%get-target-sym-name sym-name sym-cnt :loaded t)))
    (if (%loaded? (first sys))
        (%intern-now new-sym-name
                     (find-symbol sym-name (find-package (second sys)))
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

(defun %already-loaded.1 (sys-name)
  (if sys-name
      (let ((system (gethash sys-name *system-table*)))
        (if system
            (second system)
          (%raise-sys-not-found sys-name)))
    t))

(defun %should-load-at-startup (sys-name)
  "Return t if sys-name should be loaded. This depends on load-at-startup and (already)
loaded values."
  (if sys-name
      (%with-system (system sys-name)
        (and (first system) (not (second system))))
    ;; nil sys-name means cl std pkg, no loading
    nil))

(defun %should-load-at-startup.1 (sys-name)
  "Return t if sys-name should be loaded. This depends on load-at-startup and (already)
loaded values."
  (if sys-name
      (let ((load-val (gethash sys-name *system-table*)))
        (if load-val
            (and (first load-val) (not (second load-val)))
          (%raise-sys-not-found sys-name)))
    ;; nil sys-name means cl std pkg, no loading
    nil))

(defun %set-loaded (sys-name)
  (%with-system (system sys-name)
    (setf (second system) t)))

(defun %set-loaded.1 (sys-name)
  (let ((system (gethash sys-name *system-table*)))
    (if system
        (setf (second system) t)
      (%raise-sys-not-found sys-name))))

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
      (dolist (s (third to-pkg-details))
        (let ((i (belongs-to-sys s)))
          (when i
            (let ((sym (find-symbol (first s) from-pkg)))
              (shadowing-import sym to-pkg)
              (unintern (find-symbol (%get-target-sym-name (first s) i)
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
  "Given lib.lvl1.lvl2 shaped package name, return lib.lvl1"
  (let ((dot-pos (search "." pkg-name :from-end t)))
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
        (format stream "~a~25,0t: ~a " (second p) (first p))
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
      (setq flat-syms (append flat-syms (third p))))
    (delete-duplicates flat-syms :test #'equalp)
    (length flat-syms)))

  
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
