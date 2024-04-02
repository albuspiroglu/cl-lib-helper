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
   '(("LIB" "Top level again" ())
     ("LIB.AMP" "My packages" ())

     ("LIB.AMP.UTILS" "Package: SVA.UTIL"
      (("AMP-UTILS:=APPLY" ("sva-util" "SVA.UTIL"))
       ("=BIND" ("sva-util" "SVA.UTIL"))
       ("=DEFUN" ("sva-util" "SVA.UTIL"))
       ("=FUNCALL" ("sva-util" "SVA.UTIL"))
       ("=LAMBDA" ("sva-util" "SVA.UTIL"))
       ("=VALUES" ("sva-util" "SVA.UTIL"))
       ("AAND" ("sva-util" "SVA.UTIL"))
       ("ACOND" ("sva-util" "SVA.UTIL"))
       ("ACOND2" ("sva-util" "SVA.UTIL"))
       ("AFTER" ("sva-util" "SVA.UTIL"))
       ("AIF" ("sva-util" "SVA.UTIL"))
       ("AIF2" ("sva-util" "SVA.UTIL"))
       ("ALAMBDA" ("sva-util" "SVA.UTIL"))
       ("ALWAYS" ("sva-util" "SVA.UTIL"))
       ("AWHEN" ("sva-util" "SVA.UTIL"))
       ("AWHEN2" ("sva-util" "SVA.UTIL"))
       ("AWHILE" ("sva-util" "SVA.UTIL"))
       ("AWHILE2" ("sva-util" "SVA.UTIL"))
       ("BEFORE" ("sva-util" "SVA.UTIL"))
       ("BEST" ("sva-util" "SVA.UTIL"))
       ("CONJOIN" ("sva-util" "SVA.UTIL"))
       ("CONT-LEXICAL" ("sva-util" "SVA.UTIL"))
       ("CURRENT-SYMBOLS" ("sva-util" "SVA.UTIL"))
       ("CURRY" ("sva-util" "SVA.UTIL"))
       ("DEFLEXICAL" ("sva-util" "SVA.UTIL"))
       ("DEFMACRO!" ("sva-util" "SVA.UTIL"))
       ("DEFMACRO/G!" ("sva-util" "SVA.UTIL"))
       ("DEFUN-IN-PACKAGE" ("sva-util" "SVA.UTIL"))
       ("DEFUNQ" ("sva-util" "SVA.UTIL"))
       ("DISJOIN" ("sva-util" "SVA.UTIL"))
       ("DO-FILE" ("sva-util" "SVA.UTIL"))
       ("DUPLICATE" ("sva-util" "SVA.UTIL"))
       ("FACT" ("sva-util" "SVA.UTIL"))
       ("FILTER" ("sva-util" "SVA.UTIL"))
       ("FLATTEN" ("sva-util" "SVA.UTIL"))
       ("FOR" ("sva-util" "SVA.UTIL"))
       ("G!-SYMBOL-P" ("sva-util" "SVA.UTIL"))
       ("GET-NEW-ITEM-NAME" ("sva-util" "SVA.UTIL"))
       ("GET-NUMS" ("sva-util" "SVA.UTIL"))
       ("GET-STATS" ("sva-util" "SVA.UTIL"))
       ("GROUP" ("sva-util" "SVA.UTIL"))
       ("IT" ("sva-util" "SVA.UTIL"))
       ("LIST-PACKAGE-OWNED-SYMBOLS" ("sva-util" "SVA.UTIL"))
       ("LREC" ("sva-util" "SVA.UTIL"))
       ("MAP->" ("sva-util" "SVA.UTIL"))
       ("MAP-INT" ("sva-util" "SVA.UTIL"))
       ("MAP-LAZY" ("sva-util" "SVA.UTIL"))
       ("MAP0-N" ("sva-util" "SVA.UTIL"))
       ("MAP1-N" ("sva-util" "SVA.UTIL"))
       ("MAPA-B" ("sva-util" "SVA.UTIL"))
       ("MAPCARS" ("sva-util" "SVA.UTIL"))
       ("MAPPEND" ("sva-util" "SVA.UTIL"))
       ("MEMOIZE" ("sva-util" "SVA.UTIL"))
       ("MKLIST" ("sva-util" "SVA.UTIL"))
       ("MKSTR" ("sva-util" "SVA.UTIL"))
       ("MOST" ("sva-util" "SVA.UTIL"))
       ("MOSTN" ("sva-util" "SVA.UTIL"))
       ("NPUSH" ("sva-util" "SVA.UTIL"))
       ("O!-SYMBOL-P" ("sva-util" "SVA.UTIL"))
       ("PMACROEXPAND" ("sva-util" "SVA.UTIL"))
       ("PMACROEXPAND-1" ("sva-util" "SVA.UTIL"))
       ("PRINT-OBJECT-TO-STRING" ("sva-util" "SVA.UTIL"))
       ("PRUNE" ("sva-util" "SVA.UTIL"))
       ("PULL" ("sva-util" "SVA.UTIL"))
       ("RCURRY" ("sva-util" "SVA.UTIL"))
       ("READ2" ("sva-util" "SVA.UTIL"))
       ("RMAPCAR" ("sva-util" "SVA.UTIL"))
       ("SELF" ("sva-util" "SVA.UTIL"))
       ("SPLIT-IF" ("sva-util" "SVA.UTIL"))
       ("STRING-APPEND" ("sva-util" "SVA.UTIL"))
       ("SYMB" ("sva-util" "SVA.UTIL"))
       ("WHEN-NONE-OF" ("sva-util" "SVA.UTIL"))
       ("WITH-GENSYMS" ("sva-util" "SVA.UTIL"))
       ))
     ("LIB.OS.PATH" "Package: SVA.PATHNAMES"
      (("DIRECTORY-PATHNAMEP" ("sva-utils" "SVA.PATHNAMES"))
       ("DIRECTORYP" ("sva-utils" "SVA.PATHNAMES"))
       ("FILE-EXISTSP" ("sva-utils" "SVA.PATHNAMES"))
       ("FILEP" ("sva-utils" "SVA.PATHNAMES"))
       ("GET-UNIQUE-PATH" ("sva-utils" "SVA.PATHNAMES"))
       ("JOIN-PATHS" ("sva-utils" "SVA.PATHNAMES"))
       ("LIST-DIRECTORY" ("sva-utils" "SVA.PATHNAMES"))
       ("PATHNAME-AS-DIRECTORY" ("sva-utils" "SVA.PATHNAMES"))
       ("PATHNAME-AS-FILE" ("sva-utils" "SVA.PATHNAMES"))
       ("WALK-DIRECTORY" ("sva-utils" "SVA.PATHNAMES"))
       ))
     ("LIB.IO" "Package: SVA.SCRIPTS"
      (("CREATE-SYMLINKS-TO-ITEMS" ("sva-util" "SVA.SCRIPTS"))
       ))
     ("LIB.REPL.DEV" "Dev helpers" ())
     ("LIB.REPL.DEV" "Package: SVA.DEV"
      (("ADD-DATA-TO-FUNCTION" ("sva-util" "SVA.DEV"))
       ("DEF-MACRO-TEST" ("sva-util" "SVA.DEV"))
       ("DEFCLASS-TEST" ("sva-util" "SVA.DEV"))
       ))

     ))
  "Names of each package in the hierarchy and their symbols.
Each package is a list, with:
first item: package name
second item: package description
third     : a list of:
  (symbol-name-to-export (original-package-name system-name) (opn2 sysname2) ..)")
