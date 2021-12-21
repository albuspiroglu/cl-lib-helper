;; lib-helper/utils.lisp

(in-package "LIB~")

(defun mkstr (&rest args)
  "Useful utility from PGraham."
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

