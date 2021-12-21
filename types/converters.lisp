;; lib-helper/types/converters.lisp

(in-package "LIB~")

(defmethod convert ((destination symbol-and-desc) (origin lib-symbol) obj &key &allow-other-keys)
  "Conversion: lib-symbol -> symbol-and-desc"
  (let ((path-name (path (parent obj))))
    (make-instance
     'symbol-and-desc
     :sym (intern (sym-name obj) (find-package path-name))
     :hierarchy-name path-name
     :full-desc (full-desc obj))))


(defmethod convert ((destination lib-hierarchy) (origin list) obj &key &allow-other-keys)
  "Conversion: list -> lib-hierarchy"
  (let ((this (make-instance 'lib-hierarchy
                             :name (let* ((hierarchy-desc (first obj))
                                          (pkg-name (first hierarchy-desc))
                                          (pkg-desc (second hierarchy-desc)))
                                     (concatenate 'string pkg-name "-" pkg-desc))))
        res)
    (setf (slot-value this 'branches)
          (dolist (b obj res)
            (push (convert <lib-hierarchy-branch> <list> b :parent this) res)))
    this))


(defmethod convert ((destination lib-hierarchy-branch) (origin list) obj &key parent &allow-other-keys)
  "Conversion: list -> lib-hierarchy-branch"
  (let ((this (make-instance 'lib-hierarchy-branch
                             :path (first obj)
                             :parent parent
                             :path-desc (second obj)))
        res)
    (setf (slot-value this 'lib-symbols)
          (dolist (s (third obj) res)
            (push (convert <lib-symbol> <list> s :parent this) res)))
    this))


(defmethod convert ((destination lib-symbol) (origin list) obj &key parent &allow-other-keys)
  "Conversion: list -> lib-symbol"
  (make-instance
   'lib-symbol
   :sym-name (first obj)
   :origin-packages
   (loop for system-and-package in (rest obj)
         collecting (make-origin-package system-and-package))
   :parent parent))


(defmethod convert ((destination gf-tree) (origin symbol) g &key &allow-other-keys)
  "Conversion: symbol -> gf-tree"
  (let ((sg (symbol-function g)))
    (make-instance
     'gf-tree
     :gf sg
     :gmethods (mapcar
                (lambda (gm)
                  (make-instance
                   'method-detail
                   :method-obj gm
                   :specializers (c2mop:method-specializers gm)))
                (c2mop:generic-function-methods sg)))))
