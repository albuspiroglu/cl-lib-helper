

#|
We're using make-package to create the packages in the hierarchy.
Since we need to make the packages available before using the STD
package and defining some utility functions in it, we'll do
eval during compile or load or execute time.
|#
(eval-when (:compile-toplevel :execute :load-toplevel)

;;  (in-package "LIB~")

  (unless (find-package "STD")
    (lib~:setup-packages lib~:*std-package-tree*)
    (format t "====Loaded std:*, total ~a symbols.====~%"
            (lib~:symbol-count lib~:*std-package-tree*))
    (use-package :cl (find-package "STD")))


  (lib~:defun-in-package ("STD" 'lib~:delete-system-aux "DELETE-THIS-SYSTEM")
    (lambda ()
      (lib~:delete-system-aux)))

  (lib~:defun-in-package ("STD" 'lib~:get-package-names-aux "GET-PACKAGE-NAMES")
    (lambda ()
      (lib~:get-package-names-aux lib~:*std-package-tree*)))

  (lib~:defun-in-package ("STD" 'lib~:packages-aux "PACKAGES")
    (lambda (&key (stream *standard-output*))
      (lib~:packages-aux lib~:*std-package-tree*
                         :stream stream)))

  (lib~:defun-in-package ("STD" 'lib~:symbol-count "SYMBOL-COUNT")
    (lambda ()
      (lib~:symbol-count lib~:*std-package-tree*)))

  (lib~:defun-in-package ("STD" 'lib~:find-syms "FIND-SYMS")
    (lambda (phrase &optional (print-results t))
      (lib~:find-syms phrase lib~:*std-package-tree* print-results)))


  (lib~:defun-in-package ("STD" 'lib~:apropos-lib "APROPOS-LIB")
    (lambda (sub-str &optional (print-results t))
      (lib~:apropos-lib sub-str lib~:*std-package-tree* print-results)))

  )

(eval-when (:compile-toplevel :execute :load-toplevel)
  (pushnew 'lib~::*std-package-tree* lib~:*package-lists*))

(in-package "STD")
(export '(DELETE-THIS-SYSTEM
          GET-PACKAGE-NAMES
          PACKAGES
          SYMBOL-COUNT
          APROPOS-LIB
          FIND-SYMS))

