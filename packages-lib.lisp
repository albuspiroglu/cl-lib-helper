

#|
We're using make-package to create the packages in the hierarchy.
Since we need to make the packages available before using the STD
package and defining some utility functions in it, we'll do
eval during compile or load or execute time.
|#
(eval-when (:compile-toplevel :execute :load-toplevel)

  (in-package "LIB~")

  (unless (find-package "LIB")
    (lib~:setup-packages lib~::*lib-package-tree*)
    (format t "=====Loaded lib:*, total ~a symbols.=====~%"
            (lib~:symbol-count lib~:*lib-package-tree*))
    (use-package :cl (find-package "LIB")))
    
  (in-package "LIB")

  (let ((pkg (find-package "LIB")))
    (let ((f (intern "DELETE-THIS-SYSTEM" pkg)))
      (setf (symbol-function f)
            (lambda ()
              (lib~:delete-system-aux))))
    (let ((f (intern "GET-PACKAGE-NAMES" pkg)))
      (setf (symbol-function f)
            (lambda ()
              (lib~:get-package-names-aux lib~:*lib-package-tree*))))
    (let ((f (intern "PACKAGES" pkg)))
      (setf (symbol-function f)
            (lambda (&key (stream *standard-output*))
              (lib~:packages-aux lib~:*lib-package-tree*
                                 :stream stream))))
    (let ((f (intern "SYMBOL-COUNT" pkg)))
      (setf (symbol-function f)
            (lambda ()
              (lib~:symbol-count lib~:*lib-package-tree*))))
    (let ((f (intern "APROPOS-LIB" pkg)))
      (setf (symbol-function f)
            (lambda (sub-str &optional (print-results t))
              (lib~:apropos-lib sub-str lib~:*lib-package-tree* print-results))))
    (let ((f (intern "FIND-SYMS" pkg)))
      (setf (symbol-function f)
            (lambda (phrase &optional (print-results t))
              (lib~:find-syms phrase lib~:*lib-package-tree* print-results))))
    )
  )

(eval-when (:execute :load-toplevel)
  (pushnew 'lib~:*lib-package-tree* lib~:*package-lists*))

(in-package "LIB")
(export '(DELETE-THIS-SYSTEM
          GET-PACKAGE-NAMES
          PACKAGES
          SYMBOL-COUNT
          APROPOS-LIB
          FIND-SYMS))

