
(defpackage "LIB~"
  (:use :cl)
  (:export "*HIERARCHIES*"
           "ADD-TO-PACKAGE-LISTS"
           "GET-PACKAGE-NAMES-AUX"
           "SETUP-PACKAGES"
           "*CLHS-CHAPTERS*"
           "DELETE-SYSTEM-AUX"
           "*STD-PACKAGE-TREE*"
           "*LIB-PACKAGE-TREE*"
           "PACKAGES-AUX"
           "*SYSTEM-TABLE*"
           "SYMBOL-COUNT"
           "GENERATE-PACKAGE-SYMBOLS"
           "APROPOS-LIB"
           "FIND-SYMS"))

(in-package "LIB~")

(defparameter *hierarchies* nil "list of lib-hierarchies")

(defun add-to-package-lists (hierarchy)
  "Add the hierarchy to the *hierarchies*"
  (pushnew hierarchy *hierarchies*))

(defvar *system-table* (make-hash-table :test 'equalp)
  "A hash table of {key:string val:system},
       with meanings:
         key: system-name,
         val: system object")  

;;; section: class system
  (defclass system ()
    ((name :accessor system-name
           :initarg :name)

     (import-symbols-at-startup
      :accessor system-import-symbols-at-startup
      :initarg :import-symbols-at-startup)

     (loaded :accessor system-loaded))

    (:documentation "Objects of this class defines an asdf system and how it will
               be regarded during and after lib-helper load."))

(defmethod initialize-instance :after ((obj system) &key)
  (setf (system-loaded obj)
        (asdf:registered-system (system-name obj))))

(defgeneric make-system (sys &key &allow-other-keys)
  (:documentation "
               import-symbols-at-startup:
               (t:   if system already loaded in the lisp image and
               you want to import/export its symbols,

               nil: if you don't want to import the system's symbols yet,
               regardless of whether system is loaded. Each exported
               symbol of the system will have a function with same name + ~
               and when called will asdf:load-system the system and
               make all symbols of the system available.")

  (:method ((sys string) &key import-symbols-at-startup)
   "Given a system name and a keyword for import-symbols-at-startup,
               make an instance of system."
   (make-instance 'system
                  :name sys
                  :import-symbols-at-startup import-symbols-at-startup))

  (:method ((sys list) &key)
   "Given a list of (system-name import-symbols-at-startup),
               make an instance of system."
   (make-instance 'system
                  :name (first sys)
                  :import-symbols-at-startup (second sys))))
;;; end of section: class system
