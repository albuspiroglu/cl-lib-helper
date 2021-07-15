(in-package "LIB~")

;;; Libs known to lib-helper are listed in this file.

(eval-when (:compile-toplevel :execute :load-toplevel)
  ;; The libs defined below are added to the *system-table* hash-table.
  ;; *system-table* was created in home-package.lisp, and is populated here
  ;; from the lexical list system-table below.
  ;;
  ;; Add each system as: (string t/nil)
  ;;   where first item is: system-name (lower-case)
  ;;         second item  : import-symbols-at-startup
  ;;                         (t:   if system already loaded in the lisp image and
  ;;                                 you want to import/export its symbols,
  ;;                          nil: if you don't want to import the system's symbols yet,
  ;;                                 regardless of whether system is loaded. Each exported
  ;;                                 symbol of the system will have a function with same name + ~
  ;;                                 and when called will asdf:load-system the system and
  ;;                                 make all symbols of the system available.
  (let ((system-table '(("alexandria" t)
                        ("asdf" t)
                        ("cl-ppcre" t)
                        ("closer-mop" t)
                        ("iterate" t))))
    (dolist (sys system-table)
      (setf (gethash (first sys) *system-table*)
            ;; no system is loaded yet, put nil for second element
            (list (second sys) nil)))))

;; Dev helper to test some internal functions, for later reference.
(defvar *lib-package-test*
  '(("LIB" "Top level"
     ())
    ("LIB.FUN" "Functions"
     (("QUIT"  ("asdf" "UIOP/IMAGE"))
      )))
  "Dev helper for testing internal functions.")

