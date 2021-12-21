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

In addition, this system gives you packages corresponding to classes and their related symbols and specialised methods under that package. This is similar to what you get with member & encapsulation based OOP-style code completion in other languages.

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

Speaking of finding, check the (lib:find-syms) function description below, it is quite powerful.

#### Classes as packages
With 2021-08-12 commit, we now have this interesting feature. If a package has a class defined in it, then there will be a corresponding package with that class name, and all the methods specialised to that class + slot accessors will be symbols under that package. This way, you can see all relevant methods in one place, instead of trying to guess or using clos functions to find the specialisations. A class symbol pointing to the sub package will be starting with double-dots ".." instead of single as is the case for sub packages.

e.g.

    (lib.cont.lil.interface:

tab completion above will list lots of classes defined in interface package, all starting with .., so:

    (lib.cont.lil.interface:..

will filter only the classes under interface. Then, choose the class \<any>, for example:

    (lib.cont.lil.interface..<any>:

and you'll see symbols specialised for that class.

Try to find your more favorite system / class as example.



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

#### (lib:apropos-lib sub-str), (std:apropos-lib sub-str)
Look for symbols containing sub-str in the lib hierarchy and
print the matching branches.

#### (lib:find-syms phrase), (std:find-syms phrase)
Given a number of words in the phrase (first word for the symbol, others for
description and package path, find the closest matches within the lib hierarchy.

phrase can be either one string with multiple words, or a list of expressions (cl-ppcre re expressions).

Match will be listed for:

    (first phrase) contained in symbol name AND
    (every (rest phrase)) contained in path or symbol description.

e.g.

    (lib:find-syms "interface lil")

will list more results then:

    (lib:find-syms "interface lil pure")

The way we extract the description of any symbol is:
"symbol-path-in-hierarchy : any description in any symbol namespace"

This means lib-helper will search for function description, variable description, class or struct or macro description all the same, and concat them to the description. This gives us a powerful way to do search (or I call it a better apropos). For example to find any symbol with the word "structure" in its descriptions:

    (lib:find-syms '(".*" "structure"))
    or
    (lib:find-syms ".* structure")


#### (lib:packages), (std:packages)
See the list of packages, printed with some grouping.

#### (lib:get-package-names), (std:get-package-names)
Get the list of package names under lib. or std.

#### (lib:delete-this-system), (std:delete-this-system)
Deletes all the LIB.* and STD.* packages and does an asdf:clear-system :lib-helper.
If you change lib-defs.lisp or std-defs.lisp and want to update your current lisp image, just do a:

    (lib:delete-this-system)
    (asdf:load-system :lib-helper)

## Implementations tested
Working for: sbcl2.1, clisp 2.49, lispworks 7.1

## Contributing
The complete hierarchy is contained in a tree in either lib-defs.lisp (std + 3rd party) or std-defs.lisp (ansi symbols only). If you want to change / add libs, modifying these lists will be enough.

## TODO

- [ ] CLHS categorisation: Using the full symbol index page (1), for each symbol, find the sections that mention this symbol in sections pages (2), and form the library packages corresponding to the sections.
- Add(ing) well known asdf libraries to the lib.* categories (~~asdf-uiop~~, ~~alexandria~~, ~~ppcre~~, ~~iterate~~, containers, ~~closer-mop~~, bordeaux-threads, ~~lil~~, lparallel, osicat, cl-opengl, etc.).
- [ ] Add cl-containers. Since I already created some container branches, I should merge cl-containers into them. But then to reduce confusion, I can add ".clc" to the end of each category to separate the cl-containers bits. This way someone looking for hash-tables can use lib.cont.hash:, or lib.cont.hash.clc: .

- [ ]
	#### [2021-08-01]
	While working on cl-containers, I started realising a problem in design. This library is trying to categorise libraries into hierarchical packages, sometimes using library author's package structure, sometimes using domain-categorised sections. Both ideas would be expected to converge but the difficulty of different authors standardising such a category is an unsolved problem. I think I'll come up with some guidelines.

	Here, the list of criteria while managing the hierarchy is roughly:

	1. Keep the tree balanced, with number of subbranches around 3-5
	2. Keep the number of symbols low, possibly < 30? Or instead minimise number of non-coherent symbols, where coherent symbols are syntactically similar (such as car, caar, cdr, cadr; or select-item, select-node, select-somethingelse).
	3. DONE: ~~Generic methods can be cloned to different branches where each branch represents a class that implements those methods. This is helpful to organise object oriented design. We will end up having packages that correspond to classes with methods and members.~~
	4. Mostly DONE: I can get symbols of a system with some hierarchy that contains class and struct separation and their methods: ** make this automatic - working on (generate-system-symbols) now.

- [ ] import module as / using namespace new_name !!!
	I just realised that I can add these functions (I feel like there are already good alternatives, starting with uiop/defpackage import-reexport facility) to easily use a library branch within a package. But the improvement here is, we'll import-reexport a whole tree of packages to a new base branch. So we can do this:

    (in-package my-dev-package)
    (import-package-as "LIB.CONT.SEQ.ACCESS" "SEQ")
    (seq:extremum '(1 2 3 4) #'<)

    (import-package "LIB.CONT.LIL.PURE") ; to import immediate syms and packages
    (queue:\<fifo-queue> ...)
    (collection:conj ...)


## Improvement ideas
If you have ideas, I'll be more than
happy to collaborate as time allows. One thing worth pursuing might be
hierarchical packages (as is seen in some implementations).

## History
### [2021-12-21]
Major code restructuring. Looking at the lib-helper.asd should make it possible to understand the complete project, or at least help with it. Classes improved & separated, with additional contextual separations.

Also tests are added.

### [2021-08-12]
Classes as packages.

### [2021-08-09]
For some packages, I had included only the symbols of a package if they originated from that package. Now the decision is to include all external symbols of a package instead. e.g. uiop:define-package's :use-reexport, or a package manually exporting its imported symbols.

### [2021-08-07]
Added find-symbols and apropos-lib. This is becoming quite useful!

### [2021-08-01]
Add cl-containers. This library proves to be quite difficult to categorise, although it is organised quite well itself.


### [2021-06-27]
Turn the base code into a package.

## References
1. http://www.lispworks.com/documentation/lw50/CLHS/Front/X_AllSym.htm
2. http://www.lispworks.com/documentation/lw50/CLHS/Front/Contents.htm

# License

Copyright (c) [2021] [Albus M Piroglu]

Licensed under the MIT License.
