;; lib-helper/types/lib-symbol.lisp"

(in-package "LIB~")

(defclass lib-symbol ()
  ((sym-name :initarg :sym-name
             :accessor sym-name
             :type string
             :documentation "The symbol name of the source symbol")

   (origin-packages
    :initarg :origin-packages
    :accessor origin-packages
    :type list-of-origin-packages)

   (parent :initarg :parent
           :accessor parent
           :type lib-hierarchy-branch)

   (full-desc :initarg :full-desc
              :accessor full-desc
              :type string
              :documentation "Hierarchy path + all namespace descriptions of the
                              symbol combined.")

   (syms :accessor syms
         :type list-of-symbols
         :documentation "The actual symbols, corresponding to sym-name, one for each 
                         origin-packages.
                         First one is named the same as sym-name, subsequent ones appended
                         an increasing number, from 1. If lazy-interned, then appended ~ or 
                         {.n}~"))

  (:documentation "When there are multiple sys-pkg, multiple symbols will be created in the branch,
      first one with the symbol's name, subsequent ones having a {.N}~, N in (1,2..).
      Also there's a lazy interning process. If a system is not loaded, then its symbols
      will not be imported, but rather a symbol of the same name + ~ appended, and tied
      to a closure that'll load the system then import all symbols from the system to
      their branches."))

(defmethod print-object ((obj lib-symbol) stream)
  (format stream "#<~S ~A>"
          (type-of obj)
          (concatenate 'string
                       (path (parent obj)) "-" (sym-name obj)))
  obj)

(defvar <lib-symbol> (make-instance 'lib-symbol)
  "An empty object to be passed to generic functions as an interface.")

(defun all-elements-are-origin-packages (lst) (every #'origin-packagep lst))

(deftype list-of-origin-packages ()
  `(and (satisfies listp)
        (satisfies all-elements-are-origin-packages)))

(defun lib-symbolp (obj) (typep obj 'lib-symbol))

(defun all-elements-are-lib-symbols (lst) (every #'lib-symbolp lst))

(deftype list-of-lib-symbols ()
  `(and (satisfies listp)
        (satisfies all-elements-are-lib-symbols)))

(defun all-elements-are-symbols (lst) (every #'symbolp lst))

(deftype list-of-symbols ()
  `(and (satisties listp)
        (satisfies all-elements-are-symbols)))

(defun find-lib-aux (search-closure package-tree &optional (print-results t))
  "Search in package-tree for symbols meeting search-closure predicate.
     print-results: if t, print results, don't return, otherwise return a list of results.
"
  (let (result)
    (dolist (branch (branches package-tree))
      (dolist (lib-symbol (lib-symbols branch))
        (when (funcall search-closure lib-symbol)
          (push (concatenate 'string
                             (path (parent lib-symbol))
                             ":"
                             (sym-name lib-symbol))
                result))))
    (if print-results
        (format t "~{~a~%~}" result)
      result)))

(defun match-with-symbol (phrase-regexes lib-symbol)
  "Return true if any expression in phrase-regexes matches the symbol name AND
all the rest in phrase-regexes match the symbol's full-description (which
includes hierarchy path along with all namespace descriptions)."
  (let* ((sym-name (sym-name lib-symbol))
         (desc (full-desc lib-symbol)))
    (and
     (cl-ppcre:scan (first phrase-regexes) sym-name)
     (every (lambda (s) (cl-ppcre:scan s desc)) (rest phrase-regexes)))))


