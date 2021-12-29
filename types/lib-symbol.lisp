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


(defparameter +doc-sys+
  (list
   (list #'fboundp 'function)
   (list (lambda (v) (typep v 'method-combination)) 'method-combination)
   (list #'compiler-macro-function 'compiler-macro)
   (list (lambda (v) (typep (find-class v nil) 'structure-class)) 'structure-class)
   (list (lambda (v) (typep (find-class v nil) 'standard-class)) 'type))
  "Doc system functions and corresponding types for documentation
lookup in multiple namespaces of a symbol.")

(defun get-sym-desc (sym)
  "Given a symbol, return its corresponding description. If there are descriptions in
more than one namespace (function, variable, class, etc.), combine the results."

  (flet ((%get-doc-str (sym doc-sys)
           "Given a symbol, and a doc-sys in the form of '(predicate type), which is
            an item in %doc-sys%, return the documentation if any."
           (if (funcall (first doc-sys) sym)
               (or (documentation sym (second doc-sys)) "")
             "")))

    (apply #'mkstr
           (mapcar (lambda (doc-sys) (%get-doc-str sym doc-sys))
                   +doc-sys+))))


(defun set-full-desc (s)
  "s: lib-symbol"
  (setf (full-desc s)
        (concatenate 'string (path (parent s))
                     (get-sym-desc (first (syms s))))))


(defun get-target-sym-name (sym-name index &key (loaded nil))
  "Name of the symbol to create depends on how many systems / packages are
exporting the symbol. If more than one, than the first one is the sym-name,
and subsequent ones are appended a dot + number starting from 1. If the system
is not loaded, then a tilde will be appended to the name.
index: 0 based, which index system is the symbol imported from in a symbol list
       of (sym-name (sys0 pkg0) (sys1 pkg1) ..)

e.g. from a list of: (\"CAR\" (NIL \"CL\") (\"my-system\" \"MY-PACKAGE\"))
     we want the symbol for my-system, thus call
       (get-target-sym-name \"CAR\" 1 :loaded nil)
     to get \"CAR.1~\"
"

  (flet ((append-not-loaded-suffix (sym-name)
           (concatenate 'string sym-name "~")))

    (let ((new-sym-name
           (if (zerop index)
               sym-name
             (concatenate 'string sym-name "." (write-to-string index)))))
      (if loaded
          new-sym-name
        (append-not-loaded-suffix new-sym-name)))))


(defun intern-now (sym-name sym to-pkg)
  "Either shadowing-import or create a symbol and link to sym."
  (if (equalp sym-name (symbol-name sym))
      (progn
        (shadowing-import sym to-pkg)
        sym)
    (let ((new-sym (intern sym-name to-pkg)))
      (setf (symbol-value new-sym) sym)
      new-sym)))


(defun lazy-intern (lib-sym sym-cnt to-pkg)
  "See Lazy interning in the
top comment of *lib-package-tree* for details.

Intern a symbol, and return that symbol name (package relative).
"
  (let* ((new-sym-name (get-target-sym-name (sym-name lib-sym) sym-cnt :loaded t))
         (orig-pkg (nth sym-cnt (origin-packages lib-sym)))
         (from-package (find-package (pkg-name orig-pkg))))

    (unless from-package
      (error "lazy-intern: package ~a not found.~%" (pkg-name orig-pkg)))

    (if (system-loaded (containing-system orig-pkg))
        (intern-now new-sym-name
                    (find-symbol (sym-name lib-sym) from-package)
                    to-pkg)
      (intern-later new-sym-name orig-pkg to-pkg (parent (parent lib-sym))))))


(defun import-and-get-symbols (lib-sym to-pkg)
  "For one target symbol, return a list of symbols which are either from a
system-package, or a list of sym-nameN{~}* where ~ is optional. See
Lazy interning in the top comment of *lib-package-tree* for details."

  (let (syms
        (last-added-sym 0))

    (dolist (orig-pkg (origin-packages lib-sym) syms)
      (maybe-load-system-at-startup orig-pkg)
      (push (lazy-intern lib-sym last-added-sym to-pkg)
            syms)
      (incf last-added-sym))))


(defun rename-import-syms (orig-pkg branch from-pkg)
  "For the symbols in branch that belong to orig-pkg,
unintern the symbols corresponding to that package, which will be named
as a-symbol{.N}~, and shadowing-import every the a-symbol name from from-pkg
to to-pkg in branch.

orig-pkg: is of type origin-package
branch: a lib-hierarchy-branch
from-pkg: a common-lisp package designator
"
  (flet ((belongs-to-sys (lib-sym)
           (search (list (containing-system orig-pkg))
                   (origin-packages lib-sym)
                   :test (lambda (a b) (equalp a (containing-system b))))))

    (let ((to-pkg (find-package (path branch))))
      (dolist (lib-sym (lib-symbols branch))
        (let ((i (belongs-to-sys lib-sym)))
          (when i
            (let ((sym (find-symbol (sym-name lib-sym) from-pkg)))
              (shadowing-import sym to-pkg)
              (unintern (find-symbol (get-target-sym-name (sym-name lib-sym) i)
                                     to-pkg)
                        to-pkg)
              (export sym to-pkg))))))))


(defun intern-later (sym-name orig-pkg to-pkg pkg-tree)
  "Create a symbol with a ~ at the end, bound to a function to do:
load the associated system (via asdf - not quicklisp, so everything is offline),
create the expected symbol without the ~ this time, pointing to the actual object of concern and
delete the symbol with the ~ at the end.
"
  (let* ((new-sym-name (concatenate 'string sym-name "~"))
         (new-sym (intern new-sym-name to-pkg)))
    (setf (symbol-function new-sym)
          (lambda () (activate-system orig-pkg pkg-tree)))
    new-sym))


