
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
    (setf (documentation (make-package (first p)) t)
          (second p))))

(defun %define-subpackages (package-tree)
  (dolist (p package-tree)
    (%define-sub-package-syms p package-tree)))

(defun delete-system-aux ()
  (dolist (pd lib~:*package-lists*)
    (dolist (p (symbol-value pd))
      (delete-package (first p))))
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
                                                  pkg))))
    (export syms pkg)
    (%add-sub-packages (first p) pkg package-tree)))

(defun %import-and-get-symbols (sym-name systems to-pkg)
  "Return a list of symbols which are either from a system-package, or
a list of sym-nameN{~}* where ~ is optional. See Lazy interning in the
top comment of *lib-package-tree* for details.

  sym-name: string
  systems: list of (sys-name package-name)
"
  (let (syms
        (last-added-sym 0))
    (dolist (sys systems syms)
      (%maybe-load (first sys))
      (push (%lazy-intern sym-name sys last-added-sym to-pkg)
            syms)
      (incf last-added-sym))))

(defun %maybe-load (sys-name)
  "asdf load the system if necessary."
  (when (%should-load sys-name)
    (asdf:load-system sys-name)
    (%set-loaded sys-name)))

(defun %lazy-intern (sym-name sys sym-cnt to-pkg)
  "sys: (sys-name package-name)
See Lazy interning in the
top comment of *lib-package-tree* for details.

Intern a symbol, and return that symbol name (package relative).
"
  (let ((new-sym-name (if (zerop sym-cnt)
                          sym-name
                        (concatenate 'string sym-name (write-to-string sym-cnt)))))
    (if (%loaded? (first sys))
        (%intern-now new-sym-name
                     (find-symbol sym-name (find-package (second sys)))
                     to-pkg)
      (%intern-later new-sym-name sys to-pkg))))
  
(defun %should-load (sys-name)
  "Return t if sys-name should be loaded. This depends on load-at-startup and (already) loaded values."
  (if sys-name
      (let ((load-val (gethash sys-name *system-table*)))
        (and (first load-val) (not (second load-val))))
    nil))
        
(defun %set-loaded (sys-name)
  (setf (second (gethash sys-name *system-table*)) t))

(defun %loaded? (sys-name)
  (if sys-name
      (second (gethash sys-name *system-table*))
    t))

(defun %intern-now (sym-name sym to-pkg)
  (if (equalp sym-name (symbol-name sym))
      (progn
        (import sym to-pkg)
        sym)
    (let ((new-sym (intern sym-name to-pkg)))
      (setf (symbol-value new-sym) sym)
      new-sym)))

(defun %intern-later (sym-name sys to-pkg)
  "Create a symbol with a ~ appended to end, bound to a function to do:
load the associated system (via asdf - not quicklisp, so everything is offline),
create the expected symbol without the ~ this time, pointing to the actual object of concern and
delete the symbol with the ~ at the end.
"
  (let* ((new-sym-name (concatenate 'string sym-name "~"))
         (new-sym (intern new-sym-name to-pkg)))
    (setf (symbol-function new-sym)
          (lambda ()
            (asdf:load-system (first sys))
            (let ((sym (find-symbol sym-name (find-package (second sys)))))
              (%intern-now sym-name sym to-pkg)
              (unintern new-sym to-pkg)
              (export sym to-pkg)
              (format t "New symbol ~a interned and ~a removed.~%"
                      sym-name new-sym-name))))
    new-sym))

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
