;; lib-helper/generics/converters.lisp

(in-package "LIB~")

(defgeneric convert (destination origin obj &key &allow-other-keys)
  (:documentation "Convert obj from type origin to type destination."))
