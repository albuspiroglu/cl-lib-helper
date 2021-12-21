;; lib-helper/types/system.lisp

(in-package "LIB~")

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


(defmethod print-object ((obj system) stream)
  (format stream "#<~S ~A>"
          (type-of obj)
          (system-name obj))
  obj)


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


(defmethod system-loaded ((obj (eql nil)))
  nil)


(defmacro with-system ((sys-var sys-name) &body body)
  (let ((name (gensym)))
    `(let* ((,name ,sys-name)
            (,sys-var (gethash ,name *system-table*)))
       (if ,sys-var
           (progn
             ,@body)
         (error "System name ~a not found in *system-table*, consider adding
it in known-libs.lisp?~%"
                ,name)))))

(defun should-load-at-startup (system)
  "Return t if sys-name should be loaded. This depends on load-at-startup and (already)
loaded values."
  (if system
      (and (system-import-symbols-at-startup system)
           (not (system-loaded system)))
    ;; nil sys-name means cl std pkg, no loading
    nil))

(defun asdf-system-loaded (sys-name)
  (asdf:registered-system sys-name))

(defun maybe-load (system)
  "asdf load the system if necessary."
  (if (system-loaded system)
      nil
    (progn
      (asdf:load-system (system-name system))
      (setf (system-loaded system) t)
      t)))

(defun activate-system (orig-pkg pkg-tree)
  "asdf:load the system and for every symbol of it
import-export them in the tree.

This function is not called during lib-helper load, but is attached
to the library branches for on-demand use."
  (if (maybe-load (containing-system orig-pkg))
      (let ((from-pkg (find-package (pkg-name orig-pkg))))
        (dolist (branch (branches pkg-tree))
          (%rename-import-syms orig-pkg branch from-pkg))
        (format t "All symbols of system ~a imported.~%" (containing-system orig-pkg)))
    (format t "System ~a already activated. Nothing to do.~%"
            (containing-system orig-pkg))))

(defun maybe-load-system-at-startup (orig-pkg)
  "asdf load the system if necessary.
orig-pkg: is of type origin-package"
  (if (should-load-at-startup (containing-system orig-pkg))
      (progn
        ;; asdf isn't happy about loading other systems during a load operation
        ;; and since we're currently loading lib-helper, don't asdf:load
        ;; the system and just tell the user what to do & give up.
        (unless (or (asdf-system-loaded (containing-system orig-pkg))
                    (find-package (pkg-name orig-pkg)))
          (error "=========A symbol is exported from system ~a, but it is currently
not loaded. Either load the system before lib-helper, or remove its flag to
import-symbols-at-startup in known-libs.lisp.~%"
                 (containing-system orig-pkg)))
        (setf (system-loaded (containing-system second))
              t)
        t)
    nil))
