
(in-package "LIB~")

(defun get-package-names-aux (package-details)
  (let (names)
    (dolist (p package-details names)
      (push (first p) names))))
    
(defun create-packages (package-details)
  (dolist (p package-details)
    (setf (documentation (make-package (first p)) t)
          (second p))))

(defun define-subpackages (package-details)
  (dolist (p package-details)
    (define-sub-package-syms p package-details)))

(defun setup-packages (package-details)
  (create-packages package-details)
  (define-subpackages package-details))

(defun delete-system-aux ()
  (dolist (pd lib~:*package-lists*)
    (dolist (p (symbol-value pd))
      (delete-package (first p))))
  (delete-package "LIB~")
  (asdf:clear-system :lib-helper))

(defun define-sub-package-syms (p package-details)
  "p: list of (pkg-name description (syms*))"
  (let (syms
        (pkg (find-package (first p))))
    (dolist (s (third p))
      (push (find-symbol (first s) (find-package (second s)))
            syms))
    (import syms pkg)
    (export syms pkg)
    (add-sub-packages (first p) pkg package-details)))

(defun add-sub-packages (p-name parent-pkg package-details)
  (dolist (s (get-sub-packages p-name package-details))
    (let ((sym (intern (subseq s (length p-name))
                       parent-pkg)))
      (setf (symbol-value sym) (find-package s))
      (export sym parent-pkg))))
  
(defun get-sub-packages (pkg-name package-details)
  "pkg-name: string"
  (let (subs)
    (dolist (p package-details subs)
      (if (string-equal (get-parent-name (first p))
                        pkg-name)
          (push (first p) subs)))))

(defun get-parent-name (pkg-name)
  "Given lib.lvl1.lvl2 shaped package name, return lib.lvl1"
  (let ((dot-pos (search "." pkg-name :from-end t)))
    (if dot-pos
        (subseq pkg-name 0 dot-pos)
        "")))

(defun get-parent-name.test1 ()
  (assert (string-equal (get-parent-name "lib.lvl1.lvl2")
                        "lib.lvl1"))
  (assert (string-equal (get-parent-name "lib")
                        "")))


;; some helper code:
(defun print-libs (&optional (output *standard-output*)
                             (package-details *lib-package-details*))
  (format output "(defvar *lib-package-details*~%'(")
  (dolist (lib package-details)
    (print-lib lib output))
  (format output ")~%\"~a\")"
          (documentation 'package-details 'variable)))

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

(defun save-libs-to-file (&optional (file "lib-defs-auto.lisp"))
  (with-open-file (s file :direction :output
                     :if-exists :overwrite)
    (print-libs s)))

(defun packages-aux (package-tree &key (stream *standard-output*))
  "Print std package names in first hierachical categorization."
  (dolist (p package-tree)
    (format stream "~a~20,0t: " (first p))
    (dolist (p1 (get-sub-packages (first p) package-tree))
      (format stream "~a " p1))
    (format stream "~%")))

#|
I may need to improve the packages-aux function above, to have
a print similar to below.

sequences: LIB-LIST LIB-LIST-ACCESS LIB-ALIST LIB-ARRAY LIB-SEQUENCE LIB-SETS
strings  : LIB-STR-COMP LIB-STR
language : LIB-LANG LIB-LANG-COMPILE LIB-LANG-BLOCKS LIB-LANG-DEFN
functions: LIB-FUNCTIONS LIB-PATTERNS
datetime : LIB-DATETIME
packages : LIB-PACKAGES
io, os   : LIB-IO LIB-OS LIB-REPL
math     : LIB-NUM LIB-MATH-LOGICAL LIB-MATH
types    : LIB-CLOS LIB-TYPES LIB-CONDITION-TYPES LIB-CONDITIONS

 |#
