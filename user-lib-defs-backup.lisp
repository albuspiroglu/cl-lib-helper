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

     ("LIB.AMP.UTILS" "Package: AMP-UTILS"
      (("AMP-UTILS:=APPLY" ("amp-utils" "AMP-UTILS"))
       ("=BIND" ("amp-utils" "AMP-UTILS"))
       ("=DEFUN" ("amp-utils" "AMP-UTILS"))
       ("=FUNCALL" ("amp-utils" "AMP-UTILS"))
       ("=LAMBDA" ("amp-utils" "AMP-UTILS"))
       ("=VALUES" ("amp-utils" "AMP-UTILS"))
       ("AAND" ("amp-utils" "AMP-UTILS"))
       ("ACOND" ("amp-utils" "AMP-UTILS"))
       ("ACOND2" ("amp-utils" "AMP-UTILS"))
       ("AFTER" ("amp-utils" "AMP-UTILS"))
       ("AIF" ("amp-utils" "AMP-UTILS"))
       ("AIF2" ("amp-utils" "AMP-UTILS"))
       ("ALAMBDA" ("amp-utils" "AMP-UTILS"))
       ("ALWAYS" ("amp-utils" "AMP-UTILS"))
       ("AWHEN" ("amp-utils" "AMP-UTILS"))
       ("AWHEN2" ("amp-utils" "AMP-UTILS"))
       ("AWHILE" ("amp-utils" "AMP-UTILS"))
       ("AWHILE2" ("amp-utils" "AMP-UTILS"))
       ("BEFORE" ("amp-utils" "AMP-UTILS"))
       ("BEST" ("amp-utils" "AMP-UTILS"))
       ("CONJOIN" ("amp-utils" "AMP-UTILS"))
       ("CONT-LEXICAL" ("amp-utils" "AMP-UTILS"))
       ("CURRENT-SYMBOLS" ("amp-utils" "AMP-UTILS"))
       ("CURRY" ("amp-utils" "AMP-UTILS"))
       ("DEFLEXICAL" ("amp-utils" "AMP-UTILS"))
       ("DEFMACRO!" ("amp-utils" "AMP-UTILS"))
       ("DEFMACRO/G!" ("amp-utils" "AMP-UTILS"))
       ("DEFUN-IN-PACKAGE" ("amp-utils" "AMP-UTILS"))
       ("DEFUNQ" ("amp-utils" "AMP-UTILS"))
       ("DISJOIN" ("amp-utils" "AMP-UTILS"))
       ("DO-FILE" ("amp-utils" "AMP-UTILS"))
       ("DUPLICATE" ("amp-utils" "AMP-UTILS"))
       ("FACT" ("amp-utils" "AMP-UTILS"))
       ("FILTER" ("amp-utils" "AMP-UTILS"))
       ("FLATTEN" ("amp-utils" "AMP-UTILS"))
       ("FOR" ("amp-utils" "AMP-UTILS"))
       ("G!-SYMBOL-P" ("amp-utils" "AMP-UTILS"))
       ("GET-NEW-ITEM-NAME" ("amp-utils" "AMP-UTILS"))
       ("GET-NUMS" ("amp-utils" "AMP-UTILS"))
       ("GET-STATS" ("amp-utils" "AMP-UTILS"))
       ("GROUP" ("amp-utils" "AMP-UTILS"))
       ("IT" ("amp-utils" "AMP-UTILS"))
       ("LIST-PACKAGE-OWNED-SYMBOLS" ("amp-utils" "AMP-UTILS"))
       ("LREC" ("amp-utils" "AMP-UTILS"))
       ("MAP->" ("amp-utils" "AMP-UTILS"))
       ("MAP-INT" ("amp-utils" "AMP-UTILS"))
       ("MAP-LAZY" ("amp-utils" "AMP-UTILS"))
       ("MAP0-N" ("amp-utils" "AMP-UTILS"))
       ("MAP1-N" ("amp-utils" "AMP-UTILS"))
       ("MAPA-B" ("amp-utils" "AMP-UTILS"))
       ("MAPCARS" ("amp-utils" "AMP-UTILS"))
       ("MAPPEND" ("amp-utils" "AMP-UTILS"))
       ("MEMOIZE" ("amp-utils" "AMP-UTILS"))
       ("MKLIST" ("amp-utils" "AMP-UTILS"))
       ("MKSTR" ("amp-utils" "AMP-UTILS"))
       ("MOST" ("amp-utils" "AMP-UTILS"))
       ("MOSTN" ("amp-utils" "AMP-UTILS"))
       ("NPUSH" ("amp-utils" "AMP-UTILS"))
       ("O!-SYMBOL-P" ("amp-utils" "AMP-UTILS"))
       ("PMACROEXPAND" ("amp-utils" "AMP-UTILS"))
       ("PMACROEXPAND-1" ("amp-utils" "AMP-UTILS"))
       ("PRINT-OBJECT-TO-STRING" ("amp-utils" "AMP-UTILS"))
       ("PRUNE" ("amp-utils" "AMP-UTILS"))
       ("PULL" ("amp-utils" "AMP-UTILS"))
       ("RCURRY" ("amp-utils" "AMP-UTILS"))
       ("READ2" ("amp-utils" "AMP-UTILS"))
       ("RMAPCAR" ("amp-utils" "AMP-UTILS"))
       ("SELF" ("amp-utils" "AMP-UTILS"))
       ("SPLIT-IF" ("amp-utils" "AMP-UTILS"))
       ("STRING-APPEND" ("amp-utils" "AMP-UTILS"))
       ("SYMB" ("amp-utils" "AMP-UTILS"))
       ("WHEN-NONE-OF" ("amp-utils" "AMP-UTILS"))
       ("WITH-GENSYMS" ("amp-utils" "AMP-UTILS"))
       ))
     ("LIB.OS.PATH" "Package: AMP-PATHNAMES"
      (("DIRECTORY-PATHNAMEP" ("amp-utils" "AMP-PATHNAMES"))
       ("DIRECTORYP" ("amp-utils" "AMP-PATHNAMES"))
       ("FILE-EXISTSP" ("amp-utils" "AMP-PATHNAMES"))
       ("FILEP" ("amp-utils" "AMP-PATHNAMES"))
       ("GET-UNIQUE-PATH" ("amp-utils" "AMP-PATHNAMES"))
       ("JOIN-PATHS" ("amp-utils" "AMP-PATHNAMES"))
       ("LIST-DIRECTORY" ("amp-utils" "AMP-PATHNAMES"))
       ("PATHNAME-AS-DIRECTORY" ("amp-utils" "AMP-PATHNAMES"))
       ("PATHNAME-AS-FILE" ("amp-utils" "AMP-PATHNAMES"))
       ("WALK-DIRECTORY" ("amp-utils" "AMP-PATHNAMES"))
       ))
     ("LIB.IO" "Package: AMP-SCRIPTS"
      (("CREATE-SYMLINKS-TO-ITEMS" ("amp-utils" "AMP-SCRIPTS"))
       ))
     ("LIB.REPL.DEV" "Dev helpers" ())
     ("LIB.REPL.DEV" "Package: AMP-DEV"
      (("ADD-DATA-TO-FUNCTION" ("amp-utils" "AMP-DEV"))
       ("DEF-MACRO-TEST" ("amp-utils" "AMP-DEV"))
       ("DEFCLASS-TEST" ("amp-utils" "AMP-DEV"))
       ))

     ))
  "Names of each package in the hierarchy and their symbols.
Each package is a list, with:
first item: package name
second item: package description
third     : a list of:
  (symbol-name-to-export (original-package-name system-name) (opn2 sysname2) ..)")
