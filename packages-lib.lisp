

#|
We're using make-package to create the packages in the hierarchy.
Since we need to make the packages available before using the STD
package and defining some utility functions in it, we'll do
eval during compile or load or execute time.
|#
(eval-when (:compile-toplevel :execute :load-toplevel)

  (in-package "LIB~")

  (unless (find-package "LIB")
    (lib~:setup-packages lib~:*lib-package-tree*)
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
                                 :stream stream)))))

  )

(eval-when (:execute :load-toplevel)
  (push 'lib~:*lib-package-tree* lib~:*package-lists*))

(in-package "LIB")
(export '(DELETE-THIS-SYSTEM GET-PACKAGE-NAMES PACKAGES) (find-package "LIB"))

