# LIB-HELPER

## Motive
If you have looked at your screen bewildered, trying to remember a
function or an object name in Common-Lisp standard library, then this package
may help you. There are more than 970 of those functions and objects (i.e. symbols) and
I don't think everybody can browse through them easily when they are listed
in CL package altogether.

In addition, there are thousands of third party libraries. How can one use
a number of them together without constantly checking their source code, help and
readme files all the time? There are different ways to organise knowledge and reach
it and categorising them is one of them, if not the most efficient.

Some other languages may be seen as having the advantage of organising their
standard library hierarchically, or at least with one level of grouping.

Having such a central group of packages to organise others (packages) might give us an
easy way to come up with a collection of de-facto standard library composed of
popular libraries.

## Description
This is an organisation of popular functionalities in a central, easy-to-browse
set of packages. This library by itself doesn't add any utility function to common-lisp, 
instead it just reorganises the work of others in an easier to reach structure.

The taxonomy selection is mainly based on groups from clhs sections, cl-cookbook and some other common programming languages' standard libraries.

Some symbols may be exported from multiple packages, when I think that
there's strong overlap and that one might look for the same symbol in
different packages.

The goal of this library is to create a hierarchy of packages with the sole purpose of organising existing packages and symbols independently, thus without imposing anything on them. That's why in summary all it does is import symbols from others and re-export them from suitable places in the hierarchy.

For this purpose, lib-helper creates its package hierarchy with names under LIB.* (note that these packages include CL standard symbols too).

Since CL itself puts all its standard library in a flat list, it can also benefit from such grouping. That's why I kept a pure CL hierarchy of packages under the names STD.*


## Usage
A typical usage might be with slime/sly loaded (or using your IDE's completion drop-down), start typing "(lib." on repl and hit tab to get a list of packages starting with name lib., then shortlist and find what you're looking for. Though I'm having trouble running this scenario smoothly.

Or type "(lib:" and see the list of items in that group in a dropdown.


Libraries are arranged in a hierarchy. Child level packages are created as symbols starting with a dot in the parent level. This way, after choosing the completion for a child level, one can change the dot to a colon to check the lower level's symbols, and keep going down the hierarchy until they reach a target. The way it works is:

Assume some symbols at a level is at: 

    lib.lvl1.lvl2:<symbols>

Then a dropdown at lib.lvl1: would give:
    
    lib.lvl1:
             .lvl2
             *any other symbol in lvl1*

choosing lib.lvl1:.lvl2, then editing the line to get lib.lvl1.lvl2: will give the list of \*symbols\* that we're looking for.

e.g. try to get to lib.str.comp:*

After finding what you were looking for, you can either use the symbol from the package you found, or use it directly from its original package. The primary purpose of this library is to let you find things, not to alter your coding convention (although using the package names of this library might improve readability - although this claim is not tested yet).

#### Lazy system loading
Third party systems are not listed as system dependencies in .asd file, but instead dynamically loaded as requested. For example, alexandria is not a dependency, but their symbols are in the lib-defs.lisp, and they are interned with symbol names ending with a ~.

When you want to use, e.g. alexandria:cswitch, you should first find and call:

```
(lib.lang.flow:cswitch~)
```

Which will load the corresponding system, alexandria, then intern all the symbols of alexandria to their target packages in the hierarchy. Then you can call the previous command (or any other from the same system) without ~ at the end.

Btw, system loading is done by asdf, thus you should already have the corresponding system downloaded and asdf-reachable.

#### Name non-shadowing
If a symbol is exported from multiple systems/packages, then they are added to the symbols package list (in lib-defs.lisp) and for each package a symbol.N is created (except the first one is the symbol name).

### Utilities

#### (lib:apropos), (std:apropos)
[TBD] Searches in the lib-* family.

#### (lib:packages), (std:packages)
See the list of packages, printed with some grouping.

#### (lib:get-package-names), (std:get-package-names)
Get the list of package names under lib. or std.

#### (std:clhs-sections)
[TBD] Prints a mapping from clhs sections to lib-* top level.

#### (lib:delete-this-system), (std:delete-this-system)
Deletes all the LIB.* and STD.* packages and does an asdf:clear-system :lib-helper.
If you change lib/std-defs.lisp and want to update your current lisp image, just do a:

    (lib:delete-this-system)
    (asdf:load-system :lib-helper)
    
## Implementations tested
Working for: sbcl2.1, clisp 2.49, lispworks 7.1

## Contributing
The complete hierarchy is contained in a tree in either lib-defs.lisp (std + 3rd party) or std-defs.lisp (ansi symbols only). If you want to change / add libs, modifying these lists will be enough.

## TODO

- [ ] CLHS categorisation: Using the full symbol index page (1), for each symbol, find the sections that mention this symbol in sections pages (2), and form the library packages corresponding to the sections.
- Add(ing) well known asdf libraries to the lib-* categories (~~asdf-uiop~~, ~~alexandria~~, ~~ppcre~~, containers, ~~closer-mop~~, etc.). Do this continuosly as you learn libraries.


## Improvement ideas
If you have ideas, I'll be more than
happy to collaborate as time allows. One thing worth pursuing might be
hierarchical packages (as is seen in some implementations), or using dot separation
in some hierarchical package names.

notes from my code:

	#|
	some notes on getting the diff of the :cl symbols to the symbols in buffer file:
	
	(with-open-file (out "../../mylib/cl-user-syms2.lisp" :direction :output)
	  (do-symbols (s) 
	    (unless (search (symbol-name s) f1str :test #'equalp) 
	    (format out "\"~a\"~%" s))))
	
	(with-open-file (out "../../mylib/syms-diff.lisp" :direction :output
	:if-exists :supersede)
	  (do-symbols (s) 
	    (unless (search (concatenate 'string "\"" (symbol-name s) "\"")
	                    f1str :test #'equalp) 
        (format out "\"~a\"~%" s))))
	
	(defparameter f1ls (with-open-file (in "../../mylib/cl-user-syms.lisp")
	(loop for line = (read-line in nil nil) while line
          collecting line)))
	(defparameter f1str (reduce (lambda (a b) (concatenate 'string a b)) f1ls))
	
	|#


## History

[2021-06-27]
Turn the base code into a package.

## References
1. http://www.lispworks.com/documentation/lw50/CLHS/Front/X_AllSym.htm
2. http://www.lispworks.com/documentation/lw50/CLHS/Front/Contents.htm

# License

Copyright (c) [2021] [Albus M Piroglu]

Licensed under the MIT License.
