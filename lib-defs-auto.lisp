(defvar *lib-package-tree*
  '(("LIB" "Top level"
     ())
    ("LIB.FUN" "Functions"
     (("DEFUN" (NIL "CL"))
      ("VALUES" (NIL "CL"))
      ("MULTIPLE-VALUE-BIND" (NIL "CL"))
      ("LAMBDA" (NIL "CL"))
      ("FUNCALL" (NIL "CL"))
      ("APPLY" (NIL "CL"))
      ("DEFSETF" (NIL "CL"))
      ("MAPC" (NIL "CL"))
      ("MAPHASH" (NIL "CL"))
      ("COMPILED-FUNCTION" (NIL "CL"))
      ("FUNCTION" (NIL "CL"))
      ("FUNCTIONP" (NIL "CL"))
      ("REDUCE" (NIL "CL"))
      ("MAPCAN" (NIL "CL"))
      ("MAP" (NIL "CL"))
      ("MAPLIST" (NIL "CL"))
      ("MAPCAR" (NIL "CL"))
      ("MAPL" (NIL "CL"))
      ("MAPCON" (NIL "CL"))
      ("MAP-INTO" (NIL "CL"))
      ("DEFMACRO" (NIL "CL"))
      ("COMPLEMENT" (NIL "CL"))
      ("IDENTITY" (NIL "CL"))
      ("FBOUNDP" (NIL "CL"))
      ("CONSTANTLY" (NIL "CL"))
      ))
    ("LIB.CONT" "Containers"
     ())
    ("LIB.CONT.LIST" "Lists"
     (("LIST*" (NIL "CL"))
      ("LIST" (NIL "CL"))
      ("CONS" (NIL "CL"))
      ("REST" (NIL "CL"))
      ("BUTLAST" (NIL "CL"))
      ("NBUTLAST" (NIL "CL"))
      ("LAST" (NIL "CL"))
      ("NREVERSE" (NIL "CL"))
      ("REVERSE" (NIL "CL"))
      ("REVAPPEND" (NIL "CL"))
      ("APPEND" (NIL "CL"))
      ("PUSHNEW" (NIL "CL"))
      ("PUSH" (NIL "CL"))
      ("POP" (NIL "CL"))
      ("NULL" (NIL "CL"))
      ("LISTP" (NIL "CL"))
      ("LDIFF" (NIL "CL"))
      ("TAILP" (NIL "CL"))
      ("MAKE-LIST" (NIL "CL"))
      ("NRECONC" (NIL "CL"))
      ("CONSP" (NIL "CL"))
      ("SUBST" (NIL "CL"))
      ("SUBLIS" (NIL "CL"))
      ("NSUBST-IF" (NIL "CL"))
      ("SUBSTITUTE" (NIL "CL"))
      ("SUBSTITUTE-IF" (NIL "CL"))
      ("SUBST-IF-NOT" (NIL "CL"))
      ("NSUBSTITUTE-IF-NOT" (NIL "CL"))
      ("SUBSTITUTE-IF-NOT" (NIL "CL"))
      ("SUBST-IF" (NIL "CL"))
      ("NSUBSTITUTE" (NIL "CL"))
      ("NSUBST-IF-NOT" (NIL "CL"))
      ("NSUBSTITUTE-IF" (NIL "CL"))
      ("NSUBST" (NIL "CL"))
      ("ATOM" (NIL "CL"))
      ("LIST-LENGTH" (NIL "CL"))
      ("COPY-TREE" (NIL "CL"))
      ("TREE-EQUAL" (NIL "CL"))
      ("CONCATENATE" (NIL "CL"))
      ("MULTIPLE-VALUE-LIST" (NIL "CL"))
      ("VALUES-LIST" (NIL "CL"))
      ("ADJOIN" (NIL "CL"))
      ("NSET-EXCLUSIVE-OR" (NIL "CL"))
      ("NCONC" (NIL "CL"))
      ("NSUBLIS" (NIL "CL"))
      ("COPY-LIST" (NIL "CL"))
      ("ENDP" (NIL "CL"))
      ("MEMBER" (NIL "CL"))
      ))
    ("LIB.CONT.LIST.ACCESS" "List access"
     (("CAR" (NIL "CL"))
      ("CDR" (NIL "CL"))
      ("CDAR" (NIL "CL"))
      ("CAAR" (NIL "CL"))
      ("CAAAR" (NIL "CL"))
      ("CAAAAR" (NIL "CL"))
      ("CDAAR" (NIL "CL"))
      ("CADR" (NIL "CL"))
      ("CDDR" (NIL "CL"))
      ("CAADR" (NIL "CL"))
      ("CADDDR" (NIL "CL"))
      ("CDADAR" (NIL "CL"))
      ("CADAR" (NIL "CL"))
      ("CDADR" (NIL "CL"))
      ("CDAADR" (NIL "CL"))
      ("CDDAR" (NIL "CL"))
      ("CDAAAR" (NIL "CL"))
      ("CAADDR" (NIL "CL"))
      ("CDDDAR" (NIL "CL"))
      ("CADDAR" (NIL "CL"))
      ("CDADDR" (NIL "CL"))
      ("CADADR" (NIL "CL"))
      ("CADAAR" (NIL "CL"))
      ("CDDDDR" (NIL "CL"))
      ("CAADAR" (NIL "CL"))
      ("CDDAAR" (NIL "CL"))
      ("CDDADR" (NIL "CL"))
      ("CADDR" (NIL "CL"))
      ("CDDDR" (NIL "CL"))
      ("CAAADR" (NIL "CL"))
      ("NTHCDR" (NIL "CL"))
      ("FIRST" (NIL "CL"))
      ("SECOND" (NIL "CL"))
      ("THIRD" (NIL "CL"))
      ("FOURTH" (NIL "CL"))
      ("FIFTH" (NIL "CL"))
      ("SIXTH" (NIL "CL"))
      ("SEVENTH" (NIL "CL"))
      ("EIGHTH" (NIL "CL"))
      ("NINTH" (NIL "CL"))
      ("TENTH" (NIL "CL"))
      ("NTH" (NIL "CL"))
      ))
    ("LIB.CONT.ITER" "Container iteration"
     ())
    ("LIB.CONT.ARRAY" "Container array"
     (("ARRAY-TOTAL-SIZE-LIMIT" (NIL "CL"))
      ("MAKE-ARRAY" (NIL "CL"))
      ("SIMPLE-ARRAY" (NIL "CL"))
      ("ADJUST-ARRAY" (NIL "CL"))
      ("ARRAY-ROW-MAJOR-INDEX" (NIL "CL"))
      ("ROW-MAJOR-AREF" (NIL "CL"))
      ("AREF" (NIL "CL"))
      ("ARRAY-DIMENSION" (NIL "CL"))
      ("ARRAY-DIMENSIONS" (NIL "CL"))
      ("ARRAY-HAS-FILL-POINTER-P" (NIL "CL"))
      ("UPGRADED-ARRAY-ELEMENT-TYPE" (NIL "CL"))
      ("ARRAY-TOTAL-SIZE" (NIL "CL"))
      ("ARRAY-RANK" (NIL "CL"))
      ("VECTOR-PUSH" (NIL "CL"))
      ("VECTOR" (NIL "CL"))
      ("VECTORP" (NIL "CL"))
      ("SIMPLE-BIT-VECTOR-P" (NIL "CL"))
      ("VECTOR-PUSH-EXTEND" (NIL "CL"))
      ("VECTOR-POP" (NIL "CL"))
      ("SIMPLE-VECTOR-P" (NIL "CL"))
      ("FILL-POINTER" (NIL "CL"))
      ("CONCATENATE" (NIL "CL"))
      ("ARRAYP" (NIL "CL"))
      ("SIMPLE-VECTOR" (NIL "CL"))
      ("ADJUSTABLE-ARRAY-P" (NIL "CL"))
      ("ARRAY-RANK-LIMIT" (NIL "CL"))
      ("ARRAY-IN-BOUNDS-P" (NIL "CL"))
      ("ARRAY-DIMENSION-LIMIT" (NIL "CL"))
      ("ARRAY-DISPLACEMENT" (NIL "CL"))
      ("SVREF" (NIL "CL"))
      ("BIT" (NIL "CL"))
      ("SBIT" (NIL "CL"))
      ))
    ("LIB.CONT.HASH" "Container hash"
     (("MAKE-HASH-TABLE" (NIL "CL"))
      ("GETHASH" (NIL "CL"))
      ("REMHASH" (NIL "CL"))
      ("MAPHASH" (NIL "CL"))
      ("WITH-HASH-TABLE-ITERATOR" (NIL "CL"))
      ("HASH-TABLE-COUNT" (NIL "CL"))
      ("HASH-TABLE" (NIL "CL"))
      ("HASH-TABLE-REHASH-THRESHOLD" (NIL "CL"))
      ("HASH-TABLE-P" (NIL "CL"))
      ("HASH-TABLE-TEST" (NIL "CL"))
      ("HASH-TABLE-WEAKNESS" (NIL "CL"))
      ("DEFINE-HASH-TABLE-TEST" (NIL "CL"))
      ("WITH-LOCKED-HASH-TABLE" (NIL "CL"))
      ("HASH-TABLE-SYNCHRONIZED-P" (NIL "CL"))
      ("HASH-TABLE-SIZE" (NIL "CL"))
      ("HASH-TABLE-REHASH-SIZE" (NIL "CL"))
      ("CLRHASH" (NIL "CL"))
      ("SXHASH" (NIL "CL"))
      ))
    ("LIB.CONT.ALIST" "Container alist"
     (("ASSOC-IF" (NIL "CL"))
      ("ASSOC-IF-NOT" (NIL "CL"))
      ("RASSOC" (NIL "CL"))
      ("RASSOC-IF" (NIL "CL"))
      ("ASSOC" (NIL "CL"))
      ("RASSOC-IF-NOT" (NIL "CL"))
      ("COPY-ALIST" (NIL "CL"))
      ))
    ("LIB.CONT.PLIST" "Container plist"
     (("PAIRLIS" (NIL "CL"))
      ("GETF" (NIL "CL"))
      ("REMPROP" (NIL "CL"))
      ("PSETF" (NIL "CL"))
      ("REMF" (NIL "CL"))
      ("SYMBOL-PLIST" (NIL "CL"))
      ("GET-PROPERTIES" (NIL "CL"))
      ("PSETQ" (NIL "CL"))
      ("GET" (NIL "CL"))
      ))
    ("LIB.CONT.SEQUENCE" "Container sequence"
     (("LENGTH" (NIL "CL"))
      ("COUNT" (NIL "CL"))
      ("POSITION" (NIL "CL"))
      ("POSITION-IF-NOT" (NIL "CL"))
      ("POSITION-IF" (NIL "CL"))
      ("SEARCH" (NIL "CL"))
      ("STABLE-SORT" (NIL "CL"))
      ("SORT" (NIL "CL"))
      ("MERGE" (NIL "CL"))
      ("REPLACE" (NIL "CL"))
      ("REMOVE" (NIL "CL"))
      ("DELETE-IF-NOT" (NIL "CL"))
      ("DELETE-IF" (NIL "CL"))
      ("REMOVE-IF" (NIL "CL"))
      ("REMOVE-DUPLICATES" (NIL "CL"))
      ("REMOVE-IF-NOT" (NIL "CL"))
      ("DELETE" (NIL "CL"))
      ("DELETE-DUPLICATES" (NIL "CL"))
      ("ELT" (NIL "CL"))
      ("FIND-IF-NOT" (NIL "CL"))
      ("FIND-IF" (NIL "CL"))
      ("FIND" (NIL "CL"))
      ("NOTEVERY" (NIL "CL"))
      ("EVERY" (NIL "CL"))
      ("SOME" (NIL "CL"))
      ("NOTANY" (NIL "CL"))
      ("MAKE-SEQUENCE" (NIL "CL"))
      ("CONCATENATE" (NIL "CL"))
      ("SUBSEQ" (NIL "CL"))
      ("MEMBER-IF-NOT" (NIL "CL"))
      ("MEMBER-IF" (NIL "CL"))
      ("MISMATCH" (NIL "CL"))
      ("COPY-SEQ" (NIL "CL"))
      ("COUNT-IF" (NIL "CL"))
      ("COUNT-IF-NOT" (NIL "CL"))
      ("FILL" (NIL "CL"))
      ))
    ("LIB.CONT.SETS" "Container sets"
     (("NINTERSECTION" (NIL "CL"))
      ("INTERSECTION" (NIL "CL"))
      ("NSET-DIFFERENCE" (NIL "CL"))
      ("SET-DIFFERENCE" (NIL "CL"))
      ("UNION" (NIL "CL"))
      ("NUNION" (NIL "CL"))
      ("SET-EXCLUSIVE-OR" (NIL "CL"))
      ("SUBSETP" (NIL "CL"))
      ))
    ("LIB.PATTERNS" "Patterns"
     (("DESTRUCTURING-BIND" (NIL "CL"))
      ))
    ("LIB.STR" "Strings"
     (("PARSE-INTEGER" (NIL "CL"))
      ("FORMAT" (NIL "CL"))
      ("PPRINT-LINEAR" (NIL "CL"))
      ("PRINT-UNREADABLE-OBJECT" (NIL "CL"))
      ("PPRINT-TAB" (NIL "CL"))
      ("PRINT" (NIL "CL"))
      ("WITH-OUTPUT-TO-STRING" (NIL "CL"))
      ("STRING-RIGHT-TRIM" (NIL "CL"))
      ("MAKE-STRING" (NIL "CL"))
      ("PRIN1-TO-STRING" (NIL "CL"))
      ("PRINC-TO-STRING" (NIL "CL"))
      ("CHAR" (NIL "CL"))
      ("SCHAR" (NIL "CL"))
      ("CONCATENATE" (NIL "CL"))
      ("STRING-TRIM" (NIL "CL"))
      ("STRING-CAPITALIZE" (NIL "CL"))
      ("STRING-UPCASE" (NIL "CL"))
      ("STRING-LEFT-TRIM" (NIL "CL"))
      ("STRING-DOWNCASE" (NIL "CL"))
      ("STRING-TO-OCTETS" (NIL "CL"))
      ("WRITE-TO-STRING" (NIL "CL"))
      ("BOTH-CASE-P" (NIL "CL"))
      ("MAKE-STRING-INPUT-STREAM" (NIL "CL"))
      ("NSTRING-UPCASE" (NIL "CL"))
      ("SIMPLE-STRING-P" (NIL "CL"))
      ("UPPER-CASE-P" (NIL "CL"))
      ("NSTRING-CAPITALIZE" (NIL "CL"))
      ("NSTRING-DOWNCASE" (NIL "CL"))
      ("LOWER-CASE-P" (NIL "CL"))
      ))
    ("LIB.STR.COMP" "String comparison"
     (("STRING<" (NIL "CL"))
      ("STRING-NOT-EQUAL" (NIL "CL"))
      ("STRING<=" (NIL "CL"))
      ("STRINGP" (NIL "CL"))
      ("STRING=" (NIL "CL"))
      ("STRING>" (NIL "CL"))
      ("STRING/=" (NIL "CL"))
      ("STRING>=" (NIL "CL"))
      ("STRING-LESSP" (NIL "CL"))
      ("STRING-NOT-LESSP" (NIL "CL"))
      ("STRING-GREATERP" (NIL "CL"))
      ("STRING-EQUAL" (NIL "CL"))
      ("STRING-NOT-GREATERP" (NIL "CL"))
      ))
    ("LIB.CHAR" "Characters"
     (("CHAR-UPCASE" (NIL "CL"))
      ("CHAR-DOWNCASE" (NIL "CL"))
      ("ALPHA-CHAR-P" (NIL "CL"))
      ("ALPHANUMERICP" (NIL "CL"))
      ("CHAR-INT" (NIL "CL"))
      ("DIGIT-CHAR" (NIL "CL"))
      ("CHAR-CODE-LIMIT" (NIL "CL"))
      ("CHARACTERP" (NIL "CL"))
      ("CODE-CHAR" (NIL "CL"))
      ("GRAPHIC-CHAR-P" (NIL "CL"))
      ))
    ("LIB.CHAR.COMP" "Character comparison"
     (("CHAR>=" (NIL "CL"))
      ("CHAR>" (NIL "CL"))
      ("CHAR=" (NIL "CL"))
      ("CHAR<=" (NIL "CL"))
      ("CHAR<" (NIL "CL"))
      ("CHAR/=" (NIL "CL"))
      ("CHAR-NOT-LESSP" (NIL "CL"))
      ("CHAR-NOT-EQUAL" (NIL "CL"))
      ("CHAR-EQUAL" (NIL "CL"))
      ("CHAR-LESSP" (NIL "CL"))
      ("CHAR-NOT-GREATERP" (NIL "CL"))
      ("CHAR-GREATERP" (NIL "CL"))
      ))
    ("LIB.PKG" "Package"
     (("PACKAGE" (NIL "CL"))
      ("DEFPACKAGE" (NIL "CL"))
      ("FIND-PACKAGE" (NIL "CL"))
      ("IMPORT" (NIL "CL"))
      ("REQUIRE" (NIL "CL"))
      ("*MODULES*" (NIL "CL"))
      ("*PACKAGE*" (NIL "CL"))
      ("PACKAGE-NAME" (NIL "CL"))
      ("PACKAGEP" (NIL "CL"))
      ("PROVIDE" (NIL "CL"))
      ("UNEXPORT" (NIL "CL"))
      ("UNUSE-PACKAGE" (NIL "CL"))
      ("USE-PACKAGE" (NIL "CL"))
      ("PACKAGE-USED-BY-LIST" (NIL "CL"))
      ("DELETE-PACKAGE" (NIL "CL"))
      ("PACKAGE-NICKNAMES" (NIL "CL"))
      ("RENAME-PACKAGE" (NIL "CL"))
      ("KEYWORD" (NIL "CL"))
      ("IN-PACKAGE" (NIL "CL"))
      ("PACKAGE-USE-LIST" (NIL "CL"))
      ("MAKE-PACKAGE" (NIL "CL"))
      ("SHADOWING-IMPORT" (NIL "CL"))
      ("SHADOW" (NIL "CL"))
      ("LIST-ALL-PACKAGES" (NIL "CL"))
      ("WITH-PACKAGE-ITERATOR" (NIL "CL"))
      ("EXPORT" (NIL "CL"))
      ))
    ("LIB.PKG.SYMS" "Symbol management"
     (("DO-ALL-SYMBOLS" (NIL "CL"))
      ("DO-SYMBOLS" (NIL "CL"))
      ("DO-EXTERNAL-SYMBOLS" (NIL "CL"))
      ("PACKAGE-SHADOWING-SYMBOLS" (NIL "CL"))
      ("FIND-ALL-SYMBOLS" (NIL "CL"))
      ("SYMBOL-PACKAGE" (NIL "CL"))
      ("MAKE-SYMBOL" (NIL "CL"))
      ("SYMBOL-VALUE" (NIL "CL"))
      ("SYMBOL-NAME" (NIL "CL"))
      ("COPY-SYMBOL" (NIL "CL"))
      ("FIND-SYMBOL" (NIL "CL"))
      ("SYMBOL" (NIL "CL"))
      ("SYMBOLP" (NIL "CL"))
      ("SYMBOL-FUNCTION" (NIL "CL"))
      ("UNINTERN" (NIL "CL"))
      ("INTERN" (NIL "CL"))
      ("MAKUNBOUND" (NIL "CL"))
      ("FBOUNDP" (NIL "CL"))
      ("BOUNDP" (NIL "CL"))
      ("FMAKUNBOUND" (NIL "CL"))
      ))
    ("LIB.LANG" "CL Language"
     ())
    ("LIB.LANG.COMPILE" "Language/compilation"
     (("COMPILATION-SPEED" (NIL "CL"))
      ("SPEED" (NIL "CL"))
      ("OPTIMIZE" (NIL "CL"))
      ("NOTINLINE" (NIL "CL"))
      ("WITH-COMPILATION-UNIT" (NIL "CL"))
      ("*COMPILE-VERBOSE*" (NIL "CL"))
      ("DECLAIM" (NIL "CL"))
      ("PROCLAIM" (NIL "CL"))
      ("LOCALLY" (NIL "CL"))
      ("INLINE" (NIL "CL"))
      ("SAFETY" (NIL "CL"))
      ("DECLARE" (NIL "CL"))
      ("SPECIAL" (NIL "CL"))
      ("DYNAMIC-EXTENT" (NIL "CL"))
      ("IGNORABLE" (NIL "CL"))
      ("IGNORE" (NIL "CL"))
      ))
    ("LIB.LANG.BLOCKS" "Code blocks"
     (("WHILE" (NIL "CL"))
      ("LOOP" (NIL "CL"))
      ("GO" (NIL "CL"))
      ("BLOCK" (NIL "CL"))
      ("RETURN-FROM" (NIL "CL"))
      ("TAGBODY" (NIL "CL"))
      ("RETURN" (NIL "CL"))
      ("LOOP-FINISH" (NIL "CL"))
      ("PROGN" (NIL "CL"))
      ("NTH-VALUE" (NIL "CL"))
      ("DO" (NIL "CL"))
      ("DOTIMES" (NIL "CL"))
      ("DOLIST" (NIL "CL"))
      ("PROGV" (NIL "CL"))
      ("PROG1" (NIL "CL"))
      ("PROG*" (NIL "CL"))
      ("PROG" (NIL "CL"))
      ("MULTIPLE-VALUE-PROG1" (NIL "CL"))
      ("PROG2" (NIL "CL"))
      ("DO*" (NIL "CL"))
      ))
    ("LIB.LANG.DEFN" "Lang definitions"
     (("LET" (NIL "CL"))
      ("LET*" (NIL "CL"))
      ("FLET" (NIL "CL"))
      ("LABELS" (NIL "CL"))
      ("MACROLET" (NIL "CL"))
      ("DEFINE-SYMBOL-MACRO" (NIL "CL"))
      ("DEFVAR" (NIL "CL"))
      ("DEFTYPE" (NIL "CL"))
      ("DEFPARAMETER" (NIL "CL"))
      ("DEFCONSTANT" (NIL "CL"))
      ("DEFINE-SETF-EXPANDER" (NIL "CL"))
      ("DEFINE-MODIFY-MACRO" (NIL "CL"))
      ("DEFINE-COMPILER-MACRO" (NIL "CL"))
      ))
    ("LIB.LANG.BASE" "Lang basic constructs"
     (("EQUAL" (NIL "CL"))
      ("COND" (NIL "CL"))
      ("SET" (NIL "CL"))
      ("OR" (NIL "CL"))
      ("WHEN" (NIL "CL"))
      ("EQ" (NIL "CL"))
      ("AND" (NIL "CL"))
      ("NOT" (NIL "CL"))
      ("T" (NIL "CL"))
      ("NIL" (NIL "CL"))
      ("EVAL" (NIL "CL"))
      ("CASE" (NIL "CL"))
      ("QUOTE" (NIL "CL"))
      ("IF" (NIL "CL"))
      ("SETQ" (NIL "CL"))
      ("MULTIPLE-VALUE-SETQ" (NIL "CL"))
      ("MULTIPLE-VALUE-CALL" (NIL "CL"))
      ("SETF" (NIL "CL"))
      ("UNWIND-PROTECT" (NIL "CL"))
      ("MULTIPLE-VALUE-LIST" (NIL "CL"))
      ("VALUES-LIST" (NIL "CL"))
      ("VALUES" (NIL "CL"))
      ("THE" (NIL "CL"))
      ("LOAD-TIME-VALUE" (NIL "CL"))
      ("EVAL-WHEN" (NIL "CL"))
      ("SET-SYNTAX-FROM-CHAR" (NIL "CL"))
      ("SPECIAL-OPERATOR-P" (NIL "CL"))
      ("LAMBDA-PARAMETERS-LIMIT" (NIL "CL"))
      ("*READ-BASE*" (NIL "CL"))
      ("KEYWORD" (NIL "CL"))
      ("GENTEMP" (NIL "CL"))
      ("KEYWORDP" (NIL "CL"))
      ("&OPTIONAL" (NIL "CL"))
      ("&ENVIRONMENT" (NIL "CL"))
      ("&REST" (NIL "CL"))
      ("&KEY" (NIL "CL"))
      ("&WHOLE" (NIL "CL"))
      ("&AUX" (NIL "CL"))
      ("LAMBDA-LIST-KEYWORDS" (NIL "CL"))
      ("OTHERWISE" (NIL "CL"))
      ("ETYPECASE" (NIL "CL"))
      ("DECLARATION" (NIL "CL"))
      ("SATISFIES" (NIL "CL"))
      ("UNLESS" (NIL "CL"))
      ("COMPILED-FUNCTION-P" (NIL "CL"))
      ("ECASE" (NIL "CL"))
      ("EQUALP" (NIL "CL"))
      ("EQL" (NIL "CL"))
      ("RPLACA" (NIL "CL"))
      ("RPLACD" (NIL "CL"))
      ("GET-SETF-EXPANSION" (NIL "CL"))
      ("CHECK-TYPE" (NIL "CL"))
      ("TYPECASE" (NIL "CL"))
      ("FDEFINITION" (NIL "CL"))
      ("SET-MACRO-CHARACTER" (NIL "CL"))
      ("CONSTANTP" (NIL "CL"))
      ("MULTIPLE-VALUES-LIMIT" (NIL "CL"))
      ("CALL-ARGUMENTS-LIMIT" (NIL "CL"))
      ))
    ("LIB.LANG.MACRO" "Macros"
     (("COMPILER-MACRO-FUNCTION" (NIL "CL"))
      ("COMPILER-MACRO" (NIL "CL"))
      ("&BODY" (NIL "CL"))
      ("MAKE-DISPATCH-MACRO-CHARACTER" (NIL "CL"))
      ("MACROEXPAND" (NIL "CL"))
      ("MACROEXPAND-1" (NIL "CL"))
      ("MACRO-FUNCTION" (NIL "CL"))
      ("SYMBOL-MACROLET" (NIL "CL"))
      ("*GENSYM-COUNTER*" (NIL "CL"))
      ("GENSYM" (NIL "CL"))
      ))
    ("LIB.DATETIME" "Datetime"
     (("GET-UNIVERSAL-TIME" (NIL "CL"))
      ("DECODE-UNIVERSAL-TIME" (NIL "CL"))
      ("GET-DECODED-TIME" (NIL "CL"))
      ("ENCODE-UNIVERSAL-TIME" (NIL "CL"))
      ("INTERNAL-TIME-UNITS-PER-SECOND" (NIL "CL"))
      ("SLEEP" (NIL "CL"))
      ("GET-INTERNAL-REAL-TIME" (NIL "CL"))
      ("GET-INTERNAL-RUN-TIME" (NIL "CL"))
      ("TIME" (NIL "CL"))
      ))
    ("LIB.REPL" "Repl and devhelp"
     (("+" (NIL "CL"))
      ("++" (NIL "CL"))
      ("+++" (NIL "CL"))
      ("APROPOS" (NIL "CL"))
      ("LOAD" (NIL "CL"))
      ("MAKE-LOAD-FORM" (NIL "CL"))
      ("*PRINT-LEVEL*" (NIL "CL"))
      ("VARIABLE" (NIL "CL"))
      ("APROPOS-LIST" (NIL "CL"))
      ("READTABLE" (NIL "CL"))
      ("*PRINT-PPRINT-DISPATCH*" (NIL "CL"))
      ("*READ-EVAL*" (NIL "CL"))
      ("UNTRACE" (NIL "CL"))
      ("*MACROEXPAND-HOOK*" (NIL "CL"))
      ("INSPECT" (NIL "CL"))
      ("DESCRIBE" (NIL "CL"))
      ("DRIBBLE" (NIL "CL"))
      ("LISP-IMPLEMENTATION-TYPE" (NIL "CL"))
      ("LISP-IMPLEMENTATION-VERSION" (NIL "CL"))
      ("*" (NIL "CL"))
      ("**" (NIL "CL"))
      ("***" (NIL "CL"))
      ("*LOAD-TRUENAME*" (NIL "CL"))
      ("*READ-BASE*" (NIL "CL"))
      ("*PRINT-ESCAPE*" (NIL "CL"))
      ("COPY-PPRINT-DISPATCH" (NIL "CL"))
      ("GET-MACRO-CHARACTER" (NIL "CL"))
      ("SHORT-SITE-NAME" (NIL "CL"))
      ("*COMPILE-FILE-PATHNAME*" (NIL "CL"))
      ("MACHINE-VERSION" (NIL "CL"))
      ("DESCRIBE-OBJECT" (NIL "CL"))
      ("FUNCTION-LAMBDA-EXPRESSION" (NIL "CL"))
      ("READTABLE-CASE" (NIL "CL"))
      ("*READTABLE*" (NIL "CL"))
      ("SET-PPRINT-DISPATCH" (NIL "CL"))
      ("*PRINT-CIRCLE*" (NIL "CL"))
      ("*PRINT-RADIX*" (NIL "CL"))
      ("CLEAR-OUTPUT" (NIL "CL"))
      ("ROOM" (NIL "CL"))
      ("*TRACE-OUTPUT*" (NIL "CL"))
      ("*PRINT-LINES*" (NIL "CL"))
      ("SOFTWARE-TYPE" (NIL "CL"))
      ("MACHINE-TYPE" (NIL "CL"))
      ("/" (NIL "CL"))
      ("//" (NIL "CL"))
      ("///" (NIL "CL"))
      ("DISASSEMBLE" (NIL "CL"))
      ("*PRINT-CASE*" (NIL "CL"))
      ("PPRINT-POP" (NIL "CL"))
      ("PPRINT-EXIT-IF-LIST-EXHAUSTED" (NIL "CL"))
      ("*PRINT-LENGTH*" (NIL "CL"))
      ("READTABLEP" (NIL "CL"))
      ("*PRINT-MISER-WIDTH*" (NIL "CL"))
      ("*LOAD-PRINT*" (NIL "CL"))
      ("MACHINE-INSTANCE" (NIL "CL"))
      ("LONG-SITE-NAME" (NIL "CL"))
      ("*TERMINAL-IO*" (NIL "CL"))
      ("*COMPILE-FILE-TRUENAME*" (NIL "CL"))
      ("COMPILE-FILE-PATHNAME" (NIL "CL"))
      ("TRANSLATE-LOGICAL-PATHNAME" (NIL "CL"))
      ("*READ-DEFAULT-FLOAT-FORMAT*" (NIL "CL"))
      ("*PRINT-READABLY*" (NIL "CL"))
      ("*COMPILE-PRINT*" (NIL "CL"))
      ("STEP" (NIL "CL"))
      ("TRACE" (NIL "CL"))
      ("*PRINT-ARRAY*" (NIL "CL"))
      ("*READ-SUPPRESS*" (NIL "CL"))
      ("GET-DISPATCH-MACRO-CHARACTER" (NIL "CL"))
      ("SOFTWARE-VERSION" (NIL "CL"))
      ("*FEATURES*" (NIL "CL"))
      ("*DEBUG-IO*" (NIL "CL"))
      ("TRANSLATE-PATHNAME" (NIL "CL"))
      ("PPRINT-TABULAR" (NIL "CL"))
      ("SET-DISPATCH-MACRO-CHARACTER" (NIL "CL"))
      ("PPRINT-LOGICAL-BLOCK" (NIL "CL"))
      ("COMPILE-FILE" (NIL "CL"))
      ("*LOAD-VERBOSE*" (NIL "CL"))
      ("COPY-READTABLE" (NIL "CL"))
      ("FORMATTER" (NIL "CL"))
      ("BREAK" (NIL "CL"))
      ("ED" (NIL "CL"))
      ("DEBUG" (NIL "CL"))
      ("DOCUMENTATION" (NIL "CL"))
      ("COMPILE" (NIL "CL"))
      ("INVOKE-DEBUGGER" (NIL "CL"))
      ("TIME" (NIL "CL"))
      ))
    ("LIB.IO" "I/O"
     (("*STANDARD-OUTPUT*" (NIL "CL"))
      ("*STANDARD-INPUT*" (NIL "CL"))
      ("*ERROR-OUTPUT*" (NIL "CL"))
      ("WRITE-LINE" (NIL "CL"))
      ("Y-OR-N-P" (NIL "CL"))
      ("YES-OR-NO-P" (NIL "CL"))
      ("WRITE-TO-STRING" (NIL "CL"))
      ("TERPRI" (NIL "CL"))
      ("READ-LINE" (NIL "CL"))
      ("FRESH-LINE" (NIL "CL"))
      ("WRITE-STRING" (NIL "CL"))
      ("*QUERY-IO*" (NIL "CL"))
      ("FILE-STRING-LENGTH" (NIL "CL"))
      ("WITH-OPEN-FILE" (NIL "CL"))
      ("PRINT-OBJECT" (NIL "CL"))
      ("PRINT-NOT-READABLE" (NIL "CL"))
      ("PRINT-NOT-READABLE-OBJECT" (NIL "CL"))
      ("PPRINT-INDENT" (NIL "CL"))
      ("UNREAD-CHAR" (NIL "CL"))
      ("READ-CHAR-NO-HANG" (NIL "CL"))
      ("READ-BYTE" (NIL "CL"))
      ("READ-CHAR" (NIL "CL"))
      ("READ-SEQUENCE" (NIL "CL"))
      ("READ-FROM-STRING" (NIL "CL"))
      ("READ-PRESERVING-WHITESPACE" (NIL "CL"))
      ("READ-DELIMITED-LIST" (NIL "CL"))
      ("FORCE-OUTPUT" (NIL "CL"))
      ("FINISH-OUTPUT" (NIL "CL"))
      ("WRITE-SEQUENCE" (NIL "CL"))
      ("LISTEN" (NIL "CL"))
      ("WRITE-BYTE" (NIL "CL"))
      ("PPRINT-FILL" (NIL "CL"))
      ("*PRINT-GENSYM*" (NIL "CL"))
      ("*PRINT-RIGHT-MARGIN*" (NIL "CL"))
      ("PPRINT-DISPATCH" (NIL "CL"))
      ("*PRINT-BASE*" (NIL "CL"))
      ("*PRINT-PRETTY*" (NIL "CL"))
      ("CLEAR-INPUT" (NIL "CL"))
      ("WITH-STANDARD-IO-SYNTAX" (NIL "CL"))
      ("PPRINT-NEWLINE" (NIL "CL"))
      ("CLOSE" (NIL "CL"))
      ("WITH-INPUT-FROM-STRING" (NIL "CL"))
      ("WRITE-CHAR" (NIL "CL"))
      ("WRITE" (NIL "CL"))
      ("OPEN" (NIL "CL"))
      ("READ" (NIL "CL"))
      ("PRINC" (NIL "CL"))
      ("PRIN1" (NIL "CL"))
      ("PPRINT" (NIL "CL"))
      ))
    ("LIB.STREAMS" "Streams"
     (("BROADCAST-STREAM" (NIL "CL"))
      ("MAKE-TWO-WAY-STREAM" (NIL "CL"))
      ("STRING-STREAM" (NIL "CL"))
      ("OUTPUT-STREAM-P" (NIL "CL"))
      ("STREAMP" (NIL "CL"))
      ("TWO-WAY-STREAM" (NIL "CL"))
      ("MAKE-BROADCAST-STREAM" (NIL "CL"))
      ("WITH-OPEN-STREAM" (NIL "CL"))
      ("GET-OUTPUT-STREAM-STRING" (NIL "CL"))
      ("STREAM-ELEMENT-TYPE" (NIL "CL"))
      ("ECHO-STREAM" (NIL "CL"))
      ("TWO-WAY-STREAM-OUTPUT-STREAM" (NIL "CL"))
      ("CONCATENATED-STREAM-STREAMS" (NIL "CL"))
      ("SYNONYM-STREAM" (NIL "CL"))
      ("MAKE-SYNONYM-STREAM" (NIL "CL"))
      ("MAKE-STRING-INPUT-STREAM" (NIL "CL"))
      ("ECHO-STREAM-OUTPUT-STREAM" (NIL "CL"))
      ("MAKE-ECHO-STREAM" (NIL "CL"))
      ("TWO-WAY-STREAM-INPUT-STREAM" (NIL "CL"))
      ("CONCATENATED-STREAM" (NIL "CL"))
      ("MAKE-STRING-OUTPUT-STREAM" (NIL "CL"))
      ("OPEN-STREAM-P" (NIL "CL"))
      ("BROADCAST-STREAM-STREAMS" (NIL "CL"))
      ("STREAM-ERROR-STREAM" (NIL "CL"))
      ("MAKE-CONCATENATED-STREAM" (NIL "CL"))
      ("INTERACTIVE-STREAM-P" (NIL "CL"))
      ("FILE-STREAM" (NIL "CL"))
      ("ECHO-STREAM-INPUT-STREAM" (NIL "CL"))
      ("STREAM-ERROR" (NIL "CL"))
      ("INPUT-STREAM-P" (NIL "CL"))
      ("STREAM-EXTERNAL-FORMAT" (NIL "CL"))
      ("SYNONYM-STREAM-SYMBOL" (NIL "CL"))
      ("STREAM" (NIL "CL"))
      ("CHAR-NAME" (NIL "CL"))
      ("CHAR-CODE" (NIL "CL"))
      ("NAME-CHAR" (NIL "CL"))
      ("CHAR-CODE-LIMIT" (NIL "CL"))
      ("CODE-CHAR" (NIL "CL"))
      ("PEEK-CHAR" (NIL "CL"))
      ("*STANDARD-INPUT*" (NIL "CL"))
      ("*ERROR-OUTPUT*" (NIL "CL"))
      ("*STANDARD-OUTPUT*" (NIL "CL"))
      ))
    ("LIB.OS" "Operating system"
     ())
    ("LIB.OS.PATH" "OS path"
     (("PATHNAME" (NIL "CL"))
      ("DIRECTORY" (NIL "CL"))
      ("DELETE-FILE" (NIL "CL"))
      ("LOGICAL-PATHNAME-TRANSLATIONS" (NIL "CL"))
      ("FILE-NAMESTRING" (NIL "CL"))
      ("PATHNAME-MATCH-P" (NIL "CL"))
      ("FILE-POSITION" (NIL "CL"))
      ("PROBE-FILE" (NIL "CL"))
      ("ENSURE-DIRECTORIES-EXIST" (NIL "CL"))
      ("MERGE-PATHNAMES" (NIL "CL"))
      ("FILE-LENGTH" (NIL "CL"))
      ("WITH-OPEN-FILE" (NIL "CL"))
      ("PATHNAMEP" (NIL "CL"))
      ("PATHNAME-HOST" (NIL "CL"))
      ("PATHNAME-DIRECTORY" (NIL "CL"))
      ("*LOAD-PATHNAME*" (NIL "CL"))
      ("PATHNAME-DEVICE" (NIL "CL"))
      ("*DEFAULT-PATHNAME-DEFAULTS*" (NIL "CL"))
      ("PATHNAME-VERSION" (NIL "CL"))
      ("PATHNAME-TYPE" (NIL "CL"))
      ("DIRECTORY-NAMESTRING" (NIL "CL"))
      ("NAMESTRING" (NIL "CL"))
      ("HOST-NAMESTRING" (NIL "CL"))
      ("ENOUGH-NAMESTRING" (NIL "CL"))
      ("FILE-NAMESTRING" (NIL "CL"))
      ("PARSE-NAMESTRING" (NIL "CL"))
      ("WILD-PATHNAME-P" (NIL "CL"))
      ("PATHNAME-NAME" (NIL "CL"))
      ("USER-HOMEDIR-PATHNAME" (NIL "CL"))
      ("LOAD-LOGICAL-PATHNAME-TRANSLATIONS" (NIL "CL"))
      ("FILE-AUTHOR" (NIL "CL"))
      ("RENAME-FILE" (NIL "CL"))
      ("FILE-WRITE-DATE" (NIL "CL"))
      ("TRUENAME" (NIL "CL"))
      ("ENOUGH-NAMESTRING" (NIL "CL"))
      ("HOST-NAMESTRING" (NIL "CL"))
      ("MAKE-PATHNAME" (NIL "CL"))
      ("LOGICAL-PATHNAME" (NIL "CL"))
      ))
    ("LIB.MATH.NUM.LIMITS" "Math numeric limits"
     (("SHORT-FLOAT-EPSILON" (NIL "CL"))
      ("SINGLE-FLOAT-NEGATIVE-EPSILON" (NIL "CL"))
      ("SINGLE-FLOAT-EPSILON" (NIL "CL"))
      ("LONG-FLOAT-NEGATIVE-EPSILON" (NIL "CL"))
      ("SHORT-FLOAT-NEGATIVE-EPSILON" (NIL "CL"))
      ("DOUBLE-FLOAT-NEGATIVE-EPSILON" (NIL "CL"))
      ("LONG-FLOAT-EPSILON" (NIL "CL"))
      ("DOUBLE-FLOAT-EPSILON" (NIL "CL"))
      ("LEAST-NEGATIVE-LONG-FLOAT" (NIL "CL"))
      ("MOST-POSITIVE-DOUBLE-FLOAT" (NIL "CL"))
      ("DOUBLE-FLOAT-POSITIVE-INFINITY" (NIL "CL"))
      ("LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT" (NIL "CL"))
      ("LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT" (NIL "CL"))
      ("LEAST-POSITIVE-SINGLE-FLOAT" (NIL "CL"))
      ("LEAST-POSITIVE-SHORT-FLOAT" (NIL "CL"))
      ("LEAST-POSITIVE-DOUBLE-FLOAT" (NIL "CL"))
      ("LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT" (NIL "CL"))
      ("LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT" (NIL "CL"))
      ("LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT" (NIL "CL"))
      ("LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT" (NIL "CL"))
      ("LEAST-NEGATIVE-DOUBLE-FLOAT" (NIL "CL"))
      ("LEAST-NEGATIVE-SHORT-FLOAT" (NIL "CL"))
      ("LEAST-POSITIVE-NORMALIZED-LONG-FLOAT" (NIL "CL"))
      ("LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT" (NIL "CL"))
      ("LEAST-POSITIVE-LONG-FLOAT" (NIL "CL"))
      ("MOST-NEGATIVE-FIXNUM" (NIL "CL"))
      ("MOST-POSITIVE-SHORT-FLOAT" (NIL "CL"))
      ("MOST-NEGATIVE-DOUBLE-FLOAT" (NIL "CL"))
      ("MOST-NEGATIVE-LONG-FLOAT" (NIL "CL"))
      ("MOST-POSITIVE-LONG-FLOAT" (NIL "CL"))
      ("MOST-NEGATIVE-SINGLE-FLOAT" (NIL "CL"))
      ("MOST-NEGATIVE-SHORT-FLOAT" (NIL "CL"))
      ("MOST-POSITIVE-FIXNUM" (NIL "CL"))
      ("LEAST-NEGATIVE-SINGLE-FLOAT" (NIL "CL"))
      ("MOST-POSITIVE-SINGLE-FLOAT" (NIL "CL"))
      ))
    ("LIB.MATH.NUM.TYPES" "Math numeric types"
     (("SHORT-FLOAT" (NIL "CL"))
      ("UPGRADED-COMPLEX-PART-TYPE" (NIL "CL"))
      ("COMPLEXP" (NIL "CL"))
      ("DOUBLE-FLOAT" (NIL "CL"))
      ("LONG" (NIL "CL"))
      ("FLOAT" (NIL "CL"))
      ("SINGLE-FLOAT" (NIL "CL"))
      ("SIGNED" (NIL "CL"))
      ("UNSIGNED-LONG" (NIL "CL"))
      ("UNSIGNED-SHORT" (NIL "CL"))
      ("SIGNED-BYTE" (NIL "CL"))
      ("INTEGERP" (NIL "CL"))
      ("NUMBERP" (NIL "CL"))
      ("INTEGER" (NIL "CL"))
      ("REAL" (NIL "CL"))
      ("RATIONAL" (NIL "CL"))
      ("REALP" (NIL "CL"))
      ("BIGNUM" (NIL "CL"))
      ("SIGNUM" (NIL "CL"))
      ("RATIONALP" (NIL "CL"))
      ("COMPLEX" (NIL "CL"))
      ("MINUSP" (NIL "CL"))
      ("EVENP" (NIL "CL"))
      ("FIXNUM" (NIL "CL"))
      ))
    ("LIB.MATH.NUM" "Math numeric"
     (("RATIONALIZE" (NIL "CL"))
      ("INCF" (NIL "CL"))
      ("INTEGER-LENGTH" (NIL "CL"))
      ("FLOAT-DIGITS" (NIL "CL"))
      ("ZEROP" (NIL "CL"))
      ("MASK-FIELD" (NIL "CL"))
      ("FLOATING-POINT-OVERFLOW" (NIL "CL"))
      ("SHIFTF" (NIL "CL"))
      ("ASH" (NIL "CL"))
      ("1+" (NIL "CL"))
      ("DECODE-FLOAT" (NIL "CL"))
      ("INTEGER-DECODE-FLOAT" (NIL "CL"))
      ("REALPART" (NIL "CL"))
      ("IMAGPART" (NIL "CL"))
      ("FLOATP" (NIL "CL"))
      ("FTRUNCATE" (NIL "CL"))
      ("TRUNCATE" (NIL "CL"))
      ("=" (NIL "CL"))
      ("<" (NIL "CL"))
      (">" (NIL "CL"))
      (">=" (NIL "CL"))
      ("<=" (NIL "CL"))
      ("/=" (NIL "CL"))
      ("RANDOM" (NIL "CL"))
      ("MAKE-RANDOM-STATE" (NIL "CL"))
      ("*RANDOM-STATE*" (NIL "CL"))
      ("RANDOM-STATE-P" (NIL "CL"))
      ("RANDOM-STATE" (NIL "CL"))
      ("ROTATEF" (NIL "CL"))
      ("FLOATING-POINT-INEXACT" (NIL "CL"))
      ("FLOATING-POINT-INVALID-OPERATION" (NIL "CL"))
      ("DEPOSIT-FIELD" (NIL "CL"))
      ("DECF" (NIL "CL"))
      ("PHASE" (NIL "CL"))
      ("FLOAT-PRECISION" (NIL "CL"))
      ("DIGIT-CHAR-P" (NIL "CL"))
      ("FLOAT-SIGN" (NIL "CL"))
      ("NUMERATOR" (NIL "CL"))
      ("ODDP" (NIL "CL"))
      ("PLUSP" (NIL "CL"))
      ("FLOAT-RADIX" (NIL "CL"))
      ("SCALE-FLOAT" (NIL "CL"))
      ))
    ("LIB.MATH.NUM.BITS" "Bitwise arithmetic"
     (("BYTE-POSITION" (NIL "CL"))
      ("BYTE-SIZE" (NIL "CL"))
      ("DPB" (NIL "CL"))
      ("LDB-TEST" (NIL "CL"))
      ("LDB" (NIL "CL"))
      ("DPB" (NIL "CL"))
      ))
    ("LIB.MATH.LOGICAL" "Boolean arithmetic"
     (("BOOLE" (NIL "CL"))
      ("BOOLE-1" (NIL "CL"))
      ("BOOLE-IOR" (NIL "CL"))
      ("BOOLE-ORC2" (NIL "CL"))
      ("BOOLE-CLR" (NIL "CL"))
      ("BOOLE-NOR" (NIL "CL"))
      ("BOOLE-ORC1" (NIL "CL"))
      ("BOOLE-NAND" (NIL "CL"))
      ("BOOLE-SET" (NIL "CL"))
      ("BOOLE-ANDC1" (NIL "CL"))
      ("BOOLE-ANDC2" (NIL "CL"))
      ("BOOLE-2" (NIL "CL"))
      ("BOOLE-AND" (NIL "CL"))
      ("BOOLE-XOR" (NIL "CL"))
      ("BOOLE-EQV" (NIL "CL"))
      ("BOOLE-C2" (NIL "CL"))
      ("BOOLE-C1" (NIL "CL"))
      ("BIT-AND" (NIL "CL"))
      ("BIT-XOR" (NIL "CL"))
      ("BIT-EQV" (NIL "CL"))
      ("BIT-ORC2" (NIL "CL"))
      ("BIT-ORC1" (NIL "CL"))
      ("BIT-ANDC2" (NIL "CL"))
      ("BIT-IOR" (NIL "CL"))
      ("BIT-ANDC1" (NIL "CL"))
      ("BIT-NOR" (NIL "CL"))
      ("BIT-NAND" (NIL "CL"))
      ("BIT-NOT" (NIL "CL"))
      ("LOGCOUNT" (NIL "CL"))
      ("LOGORC1" (NIL "CL"))
      ("LOGTEST" (NIL "CL"))
      ("LOGORC2" (NIL "CL"))
      ("LOGANDC1" (NIL "CL"))
      ("LOGNOT" (NIL "CL"))
      ("LOGXOR" (NIL "CL"))
      ("LOGNOR" (NIL "CL"))
      ("LOGANDC2" (NIL "CL"))
      ("LOGNAND" (NIL "CL"))
      ("LOGIOR" (NIL "CL"))
      ("LOGBITP" (NIL "CL"))
      ("LOGEQV" (NIL "CL"))
      ("LOGAND" (NIL "CL"))
      ))
    ("LIB.MATH" "Math"
     ())
    ("LIB.MATH.FUNCTIONS" "Math functions"
     (("SQRT" (NIL "CL"))
      ("CONJUGATE" (NIL "CL"))
      ("SIN" (NIL "CL"))
      ("GCD" (NIL "CL"))
      ("TANH" (NIL "CL"))
      ("ATAN" (NIL "CL"))
      ("CEILING" (NIL "CL"))
      ("FLOOR" (NIL "CL"))
      ("ATANH" (NIL "CL"))
      ("ASIN" (NIL "CL"))
      ("LCM" (NIL "CL"))
      ("SINH" (NIL "CL"))
      ("FROUND" (NIL "CL"))
      ("RATIO" (NIL "CL"))
      ("FFLOOR" (NIL "CL"))
      ("EXPT" (NIL "CL"))
      ("EXP" (NIL "CL"))
      ("TAN" (NIL "CL"))
      ("ISQRT" (NIL "CL"))
      ("PI" (NIL "CL"))
      ("COSH" (NIL "CL"))
      ("ROUND" (NIL "CL"))
      ("ASINH" (NIL "CL"))
      ("MIN" (NIL "CL"))
      ("ACOS" (NIL "CL"))
      ("SIGNUM" (NIL "CL"))
      ("DENOMINATOR" (NIL "CL"))
      ("CIS" (NIL "CL"))
      ("FCEILING" (NIL "CL"))
      ("ACONS" (NIL "CL"))
      ("MAX" (NIL "CL"))
      ("ACOSH" (NIL "CL"))
      ("COS" (NIL "CL"))
      ("/" (NIL "CL"))
      ("1-" (NIL "CL"))
      ("-" (NIL "CL"))
      ("REM" (NIL "CL"))
      ("MOD" (NIL "CL"))
      ("ABS" (NIL "CL"))
      ("LOG" (NIL "CL"))
      ))
    ("LIB.CONDITIONS.TYPES" "Condition types"
     (("ERROR" (NIL "CL"))
      ("SIMPLE-WARNING" (NIL "CL"))
      ("STYLE-WARNING" (NIL "CL"))
      ("SIMPLE-CONDITION-FORMAT-CONTROL" (NIL "CL"))
      ("PROGRAM-ERROR" (NIL "CL"))
      ("ARITHMETIC-ERROR-OPERANDS" (NIL "CL"))
      ("FLOATING-POINT-UNDERFLOW" (NIL "CL"))
      ("TYPE-ERROR-EXPECTED-TYPE" (NIL "CL"))
      ("TYPE-ERROR" (NIL "CL"))
      ("CONTROL-ERROR" (NIL "CL"))
      ("PACKAGE-ERROR-PACKAGE" (NIL "CL"))
      ("PACKAGE-ERROR" (NIL "CL"))
      ("SIMPLE-ERROR" (NIL "CL"))
      ("READER-ERROR" (NIL "CL"))
      ("SIMPLE-TYPE-ERROR" (NIL "CL"))
      ("INVALID-METHOD-ERROR" (NIL "CL"))
      ("FILE-ERROR" (NIL "CL"))
      ("CELL-ERROR-NAME" (NIL "CL"))
      ("PARSE-ERROR" (NIL "CL"))
      ("STORAGE-CONDITION" (NIL "CL"))
      ("SERIOUS-CONDITION" (NIL "CL"))
      ("SIMPLE-CONDITION-FORMAT-ARGUMENTS" (NIL "CL"))
      ("SIMPLE-CONDITION" (NIL "CL"))
      ("TYPE-ERROR-DATUM" (NIL "CL"))
      ("ARITHMETIC-ERROR" (NIL "CL"))
      ("ARITHMETIC-ERROR-OPERATION" (NIL "CL"))
      ("CELL-ERROR" (NIL "CL"))
      ("UNBOUND-VARIABLE" (NIL "CL"))
      ("END-OF-FILE" (NIL "CL"))
      ("FILE-ERROR-PATHNAME" (NIL "CL"))
      ("UNDEFINED-FUNCTION" (NIL "CL"))
      ("UNBOUND-SLOT-INSTANCE" (NIL "CL"))
      ("DIVISION-BY-ZERO" (NIL "CL"))
      ))
    ("LIB.CONDITIONS" "Conditions"
     (("MAKE-CONDITION" (NIL "CL"))
      ("DEFINE-CONDITION" (NIL "CL"))
      ("INVOKE-RESTART-INTERACTIVELY" (NIL "CL"))
      ("INVOKE-RESTART" (NIL "CL"))
      ("RESTART-CASE" (NIL "CL"))
      ("SIGNAL" (NIL "CL"))
      ("USE-VALUE" (NIL "CL"))
      ("HANDLER-BIND" (NIL "CL"))
      ("FIND-RESTART" (NIL "CL"))
      ("WARN" (NIL "CL"))
      ("MUFFLE-WARNING" (NIL "CL"))
      ("CERROR" (NIL "CL"))
      ("COMPUTE-RESTARTS" (NIL "CL"))
      ("CONDITION" (NIL "CL"))
      ("CCASE" (NIL "CL"))
      ("HANDLER-CASE" (NIL "CL"))
      ("THROW" (NIL "CL"))
      ("UNWIND-PROTECT" (NIL "CL"))
      ("IGNORE-ERRORS" (NIL "CL"))
      ("WITH-CONDITION-RESTARTS" (NIL "CL"))
      ("ASSERT" (NIL "CL"))
      ("RESTART-NAME" (NIL "CL"))
      ("CATCH" (NIL "CL"))
      ("CONTINUE" (NIL "CL"))
      ("WITH-SIMPLE-RESTART" (NIL "CL"))
      ("*BREAK-ON-SIGNALS*" (NIL "CL"))
      ("STORE-VALUE" (NIL "CL"))
      ("RESTART-BIND" (NIL "CL"))
      ("CTYPECASE" (NIL "CL"))
      ("ABORT" (NIL "CL"))
      ("WARNING" (NIL "CL"))
      ("RESTART" (NIL "CL"))
      ))
    ("LIB.CLOS" "CLOS"
     (("MAKE-INSTANCES-OBSOLETE" (NIL "CL"))
      ("DEFCLASS" (NIL "CL"))
      ("MAKE-INSTANCE" (NIL "CL"))
      ("NO-NEXT-METHOD" (NIL "CL"))
      ("*INVOKE-DEBUGGER-HOOK*" (NIL "CL"))
      ("*DEBUGGER-HOOK*" (NIL "CL"))
      ("CLASS-OF" (NIL "CL"))
      ("TYPE-OF" (NIL "CL"))
      ("SLOT-VALUE" (NIL "CL"))
      ("WITH-SLOTS" (NIL "CL"))
      ("WITH-ACCESSORS" (NIL "CL"))
      ("FIND-CLASS" (NIL "CL"))
      ("CLASS-NAME" (NIL "CL"))
      ("SUBTYPEP" (NIL "CL"))
      ("ADD-METHOD" (NIL "CL"))
      ("DEFGENERIC" (NIL "CL"))
      ("DEFMETHOD" (NIL "CL"))
      ("MAKE-METHOD" (NIL "CL"))
      ("CHANGE-CLASS" (NIL "CL"))
      ("REINITIALIZE-INSTANCE" (NIL "CL"))
      ("INITIALIZE-INSTANCE" (NIL "CL"))
      ("SHARED-INITIALIZE" (NIL "CL"))
      ("SLOT-UNBOUND" (NIL "CL"))
      ("SLOT-MISSING" (NIL "CL"))
      ("SLOT-EXISTS-P" (NIL "CL"))
      ("SLOT-BOUNDP" (NIL "CL"))
      ("SLOT-MAKUNBOUND" (NIL "CL"))
      ("UNBOUND-SLOT" (NIL "CL"))
      ("CALL-METHOD" (NIL "CL"))
      ("COERCE" (NIL "CL"))
      ("METHOD-COMBINATION" (NIL "CL"))
      ("NEXT-METHOD-P" (NIL "CL"))
      ("DEFINE-METHOD-COMBINATION" (NIL "CL"))
      ("&ALLOW-OTHER-KEYS" (NIL "CL"))
      ("METHOD-QUALIFIERS" (NIL "CL"))
      ("REMOVE-METHOD" (NIL "CL"))
      ("GENERIC-FUNCTION" (NIL "CL"))
      ("CALL-NEXT-METHOD" (NIL "CL"))
      ("UPDATE-INSTANCE-FOR-DIFFERENT-CLASS" (NIL "CL"))
      ("FIND-METHOD" (NIL "CL"))
      ("FUNCTION-KEYWORDS" (NIL "CL"))
      ("NO-APPLICABLE-METHOD" (NIL "CL"))
      ("ENSURE-GENERIC-FUNCTION" (NIL "CL"))
      ("ALLOCATE-INSTANCE" (NIL "CL"))
      ("UPDATE-INSTANCE-FOR-REDEFINED-CLASS" (NIL "CL"))
      ("STANDARD-CLASS" (NIL "CL"))
      ("METHOD-COMBINATION-ERROR" (NIL "CL"))
      ("STANDARD-METHOD" (NIL "CL"))
      ("COMPUTE-APPLICABLE-METHODS" (NIL "CL"))
      ("MAKE-LOAD-FORM-SAVING-SLOTS" (NIL "CL"))
      ("CLASS" (NIL "CL"))
      ("BIT-VECTOR" (NIL "CL"))
      ("ARRAY-ELEMENT-TYPE" (NIL "CL"))
      ("STRING" (NIL "CL"))
      ("SIMPLE-BIT-VECTOR" (NIL "CL"))
      ("METHOD" (NIL "CL"))
      ("STANDARD" (NIL "CL"))
      ("STRUCTURE-OBJECT" (NIL "CL"))
      ("DEFSTRUCT" (NIL "CL"))
      ("STRUCTURE" (NIL "CL"))
      ("COPY-STRUCTURE" (NIL "CL"))
      ))
    ("LIB.TYPES" "Types"
     (("COERCE" (NIL "CL"))
      ("BOOLEAN" (NIL "CL"))
      ("SIMPLE-STRING" (NIL "CL"))
      ("CONDITION" (NIL "CL"))
      ("DOUBLE-FLOAT" (NIL "CL"))
      ("LONG" (NIL "CL"))
      ("FLOAT" (NIL "CL"))
      ("SINGLE-FLOAT" (NIL "CL"))
      ("SIGNED" (NIL "CL"))
      ("SEQUENCE" (NIL "CL"))
      ("HASH-TABLE" (NIL "CL"))
      ("BASE-CHAR" (NIL "CL"))
      ("RATIONAL" (NIL "CL"))
      ("REAL" (NIL "CL"))
      ("CHARACTER" (NIL "CL"))
      ("BYTE" (NIL "CL"))
      ("NUMBER" (NIL "CL"))
      ("STRUCTURE-CLASS" (NIL "CL"))
      ("UNSIGNED-LONG" (NIL "CL"))
      ("UNSIGNED-SHORT" (NIL "CL"))
      ("STRUCT" (NIL "CL"))
      ("SIMPLE-VECTOR" (NIL "CL"))
      ("LONG-LONG" (NIL "CL"))
      ("UNSIGNED-INT" (NIL "CL"))
      ("LONG-FLOAT" (NIL "CL"))
      ("ARRAY" (NIL "CL"))
      ("UNSIGNED-CHAR" (NIL "CL"))
      ("DOUBLE" (NIL "CL"))
      ("SHORT" (NIL "CL"))
      ("VOID" (NIL "CL"))
      ("UNSIGNED-LONG-LONG" (NIL "CL"))
      ("INT" (NIL "CL"))
      ("UNSIGNED" (NIL "CL"))
      ("INTEGER" (NIL "CL"))
      ("STANDARD-OBJECT" (NIL "CL"))
      ("SIMPLE-BASE-STRING" (NIL "CL"))
      ("BUILT-IN-CLASS" (NIL "CL"))
      ("UNSIGNED-BYTE" (NIL "CL"))
      ("KEYWORDP" (NIL "CL"))
      ("STANDARD-CHAR" (NIL "CL"))
      ("BASE-STRING" (NIL "CL"))
      ("STANDARD-GENERIC-FUNCTION" (NIL "CL"))
      ("STANDARD-METHOD" (NIL "CL"))
      ("STRUCTURE-OBJECT" (NIL "CL"))
      ("STANDARD-CLASS" (NIL "CL"))
      ("EXTENDED-CHAR" (NIL "CL"))
      ("STANDARD-CHAR-P" (NIL "CL"))
      ("LOGICAL-PATHNAME" (NIL "CL"))
      ("TYPE" (NIL "CL"))
      ("BIT-VECTOR-P" (NIL "CL"))
      ("TYPEP" (NIL "CL"))
      ("FTYPE" (NIL "CL"))
      ("BIGNUM" (NIL "CL"))
      ))
    )
  "Names of each package in the hierarchy and their symbols.
Each package is a list, with:
first item: package name
second item: package description
third     : a list of:
  (symbol-name-to-export
   original-package-name)")