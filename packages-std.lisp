

#|
We're using make-package to create the packages in the hierarchy.
Since we need to make the packages available before using the STD
package and defining some utility functions in it, we'll do
eval during compile or load or execute time.
|#
(eval-when (:compile-toplevel :execute :load-toplevel)

  (in-package "LIB~")

  (unless (find-package "STD")
    (lib~:setup-packages lib~:*std-package-tree*)
    (use-package :cl (find-package "STD")))

  (in-package "STD")

  (let ((pkg (find-package "STD")))
    (let ((f (intern "DELETE-THIS-SYSTEM" pkg)))
      (setf (symbol-function f)
            (lambda ()
              (lib~:delete-system-aux))))
    (let ((f (intern "GET-PACKAGE-NAMES" pkg)))
      (setf (symbol-function f)
            (lambda ()
              (lib~:get-package-names-aux lib~:*std-package-tree*))))
    (let ((f (intern "PACKAGES" pkg)))
      (setf (symbol-function f)
            (lambda (&key (stream *standard-output*))
              (lib~:packages-aux lib~:*std-package-tree*
                                 :stream stream)))))

  )

(eval-when (:execute :load-toplevel)
  (push 'lib~::*std-package-tree* lib~:*package-lists*))

(in-package "STD")
(export '(DELETE-THIS-SYSTEM GET-PACKAGE-NAMES PACKAGES) (find-package "STD"))

