;; lib-helper/test/devhelp.lisp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some dev helpers:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "LIB~")

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


(defun get-package-owned-external-symbols (package)
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
