
(in-package "LIB~")

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
  (if (equalp sym-name (symbol-name sym))
      (progn
        (shadowing-import sym to-pkg)
        sym)
    (let ((new-sym (intern sym-name to-pkg)))
      (setf (symbol-value new-sym) sym)
      new-sym)))

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

(defun get-parent-name (pkg-name)
  "Given lib.lvl1.lvl2 shaped package name, return lib.lvl1; or
lib.lvl1.lvl2..class1, return lib.lvl1.lvl2"
  (let ((dot-pos
         (let ((test-pos (search ".." pkg-name :from-end t)))
           (if test-pos
               test-pos
             (search "." pkg-name :from-end t)))))
    (if dot-pos
        (subseq pkg-name 0 dot-pos)
        "")))

(defun get-sub-packages (pkg-name package-tree)
  "pkg-name: string
Returns a list of paths that are sub packages of pkg-name."
  (let (subs)
    (dolist (p (branches package-tree) subs)
      (if (string-equal (get-parent-name (path p))
                        pkg-name)
          (push (path p) subs)))))

(defun add-sub-packages (h-branch parent-pkg)
  (dolist (s (get-sub-packages (path h-branch) (parent h-branch)))
    (let ((sym (intern (subseq s (length (path h-branch)))
                       parent-pkg)))
      (setf (symbol-value sym) (find-package s)) ;; prob. the best way to keep a reference to the package
      (export sym parent-pkg))))

(defconstant +doc-sys+
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

(defun setup-packages (package-tree)
  "Creates and defines packages in package-tree."
  (flet ((%create-packages ()
           "Create each package, without any detail such as import, use etc."
           (let (failed-packages)
             (dolist (branch (branches package-tree))
               (handler-case
                   (let ((pkg (make-package (path branch) :use '("COMMON-LISP"))))
                     (setf (documentation pkg t)
                           (path-desc branch)))
                 (error (c)
                   (declare (ignore c))
                   (push (path branch) failed-packages)))
               (when (> (length failed-packages) 1)
                 (format t "Failed to create packages: ~{~a, ~}.~%" failed-packages)))))

         (%link-subpackages ()
           "For each package, create symbols of \".sub-package-name\" that refers
            to the sub package object."
           (flet ((%define-sub-package-syms (p)
                    "p: lib-hierarchy-branch"
                    (flet ((%import-and-get-symbols (lib-sym to-pkg)
                             "For one target symbol, return a list of symbols which are either from a
            system-package, or a list of sym-nameN{~}* where ~ is optional. See
            Lazy interning in the top comment of *lib-package-tree* for details.

            lib-sym: lib-symbol
            sym-name: string
            systems: list of (sys-name package-name)
            "
                             (flet ((%lazy-intern (lib-sym sym-cnt to-pkg)
                                      "sys: (sys-name package-name)

See Lazy interning in the
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
                                          (intern-later new-sym-name orig-pkg to-pkg (parent (parent lib-sym)))))))

                               (let (syms
                                     (last-added-sym 0))
                                 (dolist (orig-pkg (origin-packages lib-sym) syms)
                                   (maybe-load-system-at-startup orig-pkg)
                                   (push (%lazy-intern lib-sym last-added-sym to-pkg)
                                         syms)
                                   (incf last-added-sym))))))

                      (let (syms
                            (to-pkg (find-package (path p))))
                        (dolist (s (lib-symbols p))
                          (setf (syms s) (%import-and-get-symbols s to-pkg))
                          (setf syms (append syms (syms s)))
                          (set-full-desc s))
                        (export syms to-pkg)
                        (add-sub-packages p to-pkg)))))

             (dolist (branch (branches package-tree))
               (%define-sub-package-syms branch)))))

    (%create-packages)
    (%link-subpackages)))


(defun delete-system-aux ()
  (dolist (pd lib~:*hierarchies*)
    (delete-hierarchy pd))
  (delete-package "LIB~")
  (asdf:clear-system :lib-helper))

(defun delete-hierarchy (hierarchy)
  (let (pkgs-error)
    (dolist (p (branches hierarchy))
      (handler-case
          (delete-package (path p))
        (error (c)
          (declare (ignore c))
          (push (path p) pkgs-error))))
    (when pkgs-error
      (format t "For lib hierarchy ~A:~%  Error deleting packages ~{~a, ~}.~%" 
              hierarchy 
              pkgs-error))))


(defun packages-aux (package-tree &key (stream *standard-output*))
  "Print package names in first hierachical categorization."
  (dolist (p (branches package-tree))
    (let* ((p-subs (get-sub-packages (path p) package-tree))
           (line-length 90)
           (separation 27)
           (cursor separation))
      (when p-subs
        (format stream "~a~25,0t: ~a "
                (path-desc p)
                (path p))
        (dolist (p1 p-subs)
          (when (> cursor (- line-length separation))
            (setf cursor separation)
            (format stream "~%~25,0t  "))
          (format stream "~a " p1)
          (incf cursor (1+ (length p1))))
        (format stream "~%")
        (terpri stream)))))


(defun symbol-count (package-tree)
  "Return the count of unique symbols within the tree."
  (let (flat-syms)
    (dolist (p (branches package-tree))
      (setq flat-syms (append flat-syms (lib-symbols p))))
    (length (delete-duplicates flat-syms :test #'equalp))))


(defun find-syms (phrase package-tree &optional (print-results t))
  "Given a number of words or re-patterns in the phrase (first word for the symbol, others for
description and package path, find the closest matches within the lib hierarchy.

re-patterns use cl-ppcre.

print-results: if t (default), print the results instead of returning a list.
"
  (let ((phrase-regexes (mapcar (lambda (w) (cl-ppcre:create-scanner w
                                                                     :case-insensitive-mode t))
                                (typecase phrase
                                  (cons phrase)
                                  (string
                                   (mapcar (lambda (w) (concatenate 'string
                                                                    ".*"
                                                                    w
                                                                    ".*"))
                                           (cl-ppcre:split "\\s+" phrase)))))))
    (find-lib-aux (lambda (lib-symbol)
                     (match-with-symbol phrase-regexes lib-symbol))
                   package-tree print-results)))


(defun apropos-lib (sub-str package-tree &optional (print-results t))
  "Look for symbols containing sub-str in the lib hierarchy and
print the matching branches.

print-results: if t (default), print the results instead of returning a list.
"
  (find-lib-aux (lambda (sym-list)
                  (search sub-str (sym-name sym-list)
                          :test 'equalp))
                package-tree print-results))

