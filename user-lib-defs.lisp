(in-package "LIB~")

;; User defined lib hierarchies can be defined here.
;;
;; To generate a list automatically for an asdf system, some examples are in test/dev-help.lisp. See
;; generate-system-symbols call for details.
;;
;; First item in the tree is skipped, and a package corresponding
;; it is expected to exist. It is used to name the hierarchy object.
;;
;; Each package is defined as:
;;   (Package-name Description Symbol-List)
;;   Package-name: string
;;   Description : string
;;
;; Symbol-List:
;;   (list Symbol-Details*)
;;
;; Symbol-Details:
;;   (Symbol-name Provider-details*)
;;   Symbol-name: string
;;   Provider-details: If there are multiple providers, Symbol-name is
;;     interned from the first provider, and Symbol-name.N (N in Z+) symbols
;;     are interned from the corresponding provider in sequence.
;;     See Notes (1) below for lazy interning.
;; Provider-details:
;;   (System-name Package-name)
;;   System-name: string (symbol NIL for CL standard)
;;   Package-name: string
;;
;; Notes:
;;  1. Lazy interning: Except a couple of systems (like alexandria) loaded by default,
;;     many symbols of other systems will be in the tree without pointing to an actual
;;     object. When this is the case, the symbol will have a ~ character postfix, and
;;     will be bound to a function/macro that when called, will load the associated
;;     system (via asdf - not quicklisp, so everything is offline), create the expected
;;     symbol without the ~ this time, pointing to the actual object of concern and
;;     delete the symbol with the ~ at the end.
;;
;;  e.g. list to start adding your own packages:
;;  '(("LIB.MY-PACKAGES" "My packages" ())
;;
;;    ("LIB.MY-PACKAGES.UTILS" "Package: MY-UTILS"
;;       ("SYMBOL1" ("my-system" "MY-UTILS"))
;;        ..
;;     ))



(defvar *user-package-tree*
  (convert
   <lib-hierarchy> <list>
   '(
     ;; keep this top level here as well, so that your sub package will be a .my-package in lib:
     ("LIB" "Top level again" ())
     ;; your list will come here
     ))
  "Names of each package in the hierarchy and their symbols.
Each package is a list, with:
first item: package name
second item: package description
third     : a list of:
  (symbol-name-to-export (original-package-name system-name) (opn2 sysname2) ..)")
