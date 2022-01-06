
(in-package "LIB~")


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
  "Find closest matches within std or lib hierarchy.

INPUTS:
phrase:
  a number of words as one string (first word for the symbol, others for
  description and package path) 
OR 
  re-patterns as a list of strings in the phrase, 

print-results: if t (default), print the results instead of returning a list.

OUTPUT: nil or list of results

Note: re-patterns use cl-ppcre.

EXAMPLES:
(find-syms \"prin\")
  find symbols with prin in symbol name

(find-syms \"prin io\")
  Different to previous example, this will do a further filtering by finding symbols that have prin in symbol name, and io in either the package-path name or its documentation.

(find-syms '(\"prin\" \".*\\bio\\b.*\"))
  Further limiting to previous, filter ones with only complete word io. But one might prefer a shorter and almost equally effective:
(find-syms \"prin std.io\")
if they are trying to limit with path name.
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


(defun get-package-names-aux (hierarchy)
  "Return a list of package names in the package-tree."
  (let (names)
    (dolist (p (branches hierarchy) names)
      (push (path p) names))))

