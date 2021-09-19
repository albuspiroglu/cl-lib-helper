

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
  

  (defun-in-package ("LIB" 'lib~:delete-system-aux "DELETE-THIS-SYSTEM")
    (lambda ()
      (lib~:delete-system-aux)))

  (defun-in-package ("LIB" 'lib~:get-package-names-aux "GET-PACKAGE-NAMES")
    (lambda ()
      (lib~:get-package-names-aux lib~:*lib-package-tree*)))

  (defun-in-package ("LIB" 'lib~:packages-aux "PACKAGES")
    (lambda (&key (stream *standard-output*))
      (lib~:packages-aux lib~:*lib-package-tree*
                         :stream stream)))

  (defun-in-package ("LIB" 'lib~:symbol-count "SYMBOL-COUNT")
    (lambda ()
      (lib~:symbol-count lib~:*lib-package-tree*)))

  (defun-in-package ("LIB" 'lib~:find-syms "FIND-SYMS")
    (lambda (phrase &optional (print-results t))
      (lib~:find-syms phrase lib~:*lib-package-tree* print-results)))

  (defun-in-package ("LIB" 'lib~:apropos-lib "APROPOS-LIB")
    (lambda (sub-str &optional (print-results t))
      (lib~:apropos-lib sub-str lib~:*lib-package-tree* print-results)))

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

