(in-package "LIB~")

;;; Libs/systems known to lib-helper are listed in this file.

(defparameter *hierarchies* nil "list of lib-hierarchies")

(defun add-to-package-lists (hierarchy)
  "Add the hierarchy to the *hierarchies*"
  (pushnew hierarchy *hierarchies*))


(defvar *system-table* (make-hash-table :test 'equalp)
  "A hash table of {key:string val:system},
       with meanings:
         key: system-name,
         val: system object")  

;; The libs defined below are added to the *system-table* hash-table (defined above)
;;
;; Add each system as: (string t/nil)
;;   where first item is: system-name (lower-case)
;;         second item  : import-symbols-at-startup
;;                         (t:   if system already loaded in the lisp image and
;;                                 you want to import/export its symbols,
;;                          nil: if you don't want to import the system's symbols yet,
;;                                 regardless of whether system is loaded. Each exported
;;                                 symbol of the system will have a function with same name + ~
;;                                 and when any one of the symbols is called, it will
;;                                 asdf:load-system the system and
;;                                 make all symbols of the system available.
(let ((system-table '(("alexandria" t)
                      ("asdf" t)
                      ("cl-ppcre" t)
                      ("closer-mop" t)
                      ("iterate" t)
                      ("lil" nil)
                      ("cl-containers" t))))
  (dolist (sys system-table)
    (setf (gethash (first sys) *system-table*)
          (make-system sys))))

;; Dev helper to test some internal functions, for later reference.
(defvar *lib-package-test*
  '(("LIB" "Top level"
     ())
    ("LIB.FUN" "Functions"
     (("QUIT"  ("asdf" "UIOP/IMAGE"))
      )))
  "Dev helper for testing internal functions.")
