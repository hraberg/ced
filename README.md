# CED - C, Clojure Edition

This project started as an [April Fool's joke](doc/README-April-1st.md), but was actually driven by the sheer coolness of projects like the [Vacietis - C to Common Lisp compiler](https://github.com/vsedach/Vacietis). Most of the references still made sense, but the discussion about them was whimsical on purpose.

So, instead of porting Ed, I will slowly implement [The C Programming Language, Second Edition](http://cm.bell-labs.com/cm/cs/cbook/) via a C to Clojure compiler. You can see the start of "Chapter 1 - A Tutorial Introduction" [here](https://github.com/hraberg/ced/tree/master/resources/k%26r). This is a side project, so don't expect to much progress.


### What Works?

Almost nothing yet. To build, run:

    ./build # Fetches musl-0.9.9.tgz, creates the C parser and bundles it all into an uberjar.

* Using [JCPP](http://www.anarres.org/projects/jcpp/) with header files from [musl](http://www.musl-libc.org/) to pre-process C and feeding it to the [Rats!](http://cs.nyu.edu/rgrimm/xtc/rats-intro.html) C parser.
* Building Clojure AST from the output.
* K&R, "Chapter 1 - A Tutorial Introduction"  works up to part "1.6, Arrays", by "cheating" and relying on Clojure implementations of `printf` `getchar` and `putchar` :

```
hello, world
[ .. lots of output from K&R Chapter 1 .. ]

```
## References

[The C Programming Language, Second Edition](http://cm.bell-labs.com/cm/cs/cbook/) Brian W. Kernighan and Dennis M. Ritchie, 1988

[C: A Reference Manual, Fifth Edition](http://careferencemanual.com/) Samuel P. Harbison III, Guy L. Steele Jr. 2002

[Expert C Programming: Deep C Secrets](http://www.amazon.com/Expert-Programming-Peter-van-Linden/dp/0131774298) Peter van der Linden, 1994

[PEG-based transformer provides front-, middleand back-end stages in a simple compiler](http://www.vpri.org/pdf/tr2010003_PEG.pdf)
Ian Piumarta, 2010

[ZETA-C-PD](http://bitsavers.informatik.uni-stuttgart.de/bits/TI/Explorer/zeta-c/) Scott L. Burson, 1987, 2003 - "Zeta-C is a C implementation for Lisp Machines that works by translating C into Lisp."

[Vacietis - C to Common Lisp compiler](https://github.com/vsedach/Vacietis) Vladimir Sedach, 2012

[musl](http://www.musl-libc.org/) Rich Felker, 2010-2013 - "musl is a new general-purpose implementation of the C library. It is lightweight, fast, simple, free, and aims to be correct in the sense of standards-conformance and safety."

[CINT](http://root.cern.ch/drupal/content/cint) Masaharu Goto, 1995-2010 "CINT is an interpreter for C and C++ code."

[Cibyl](http://code.google.com/p/cibyl/) Simon Kågström, 2007-2011 - "Cibyl is a programming environment and binary translator that allows compiled C programs to execute on J2ME-capable phones."

[NestedVM](http://nestedvm.ibex.org/) Brian Alliet and Adam Megacz, 2006-2009 - "NestedVM provides binary translation for Java Bytecode. This is done by having GCC compile to a MIPS binary which is then translated to a Java class file."

[LLJVM](http://da.vidr.cc/projects/lljvm/) David A Roberts, 2010 - "LLJVM provides a set of tools and libraries for running comparatively low level languages (such as C) on the JVM."

[µClibc](http://www.uclibc.org/about.html) Erik Andersen, 1999-2012 - "A C library for embedded Linux"

[xtc](http://cs.nyu.edu/rgrimm/xtc/) Paul Gazzillo and Robert Grimm, 2006-2012 - "The xtc (eXTensible Compiler) project is exploring novel programming languages and tools to improve the expressiveness, safety, and efficiency of complex systems."

[Rats!](http://cs.nyu.edu/rgrimm/xtc/rats-intro.html) Robert Grimm, 2004-2012 - "Grammars for Rats! build on the Parsing Expression Grammar (PEG) formalism described in Brian Ford's [Parsing Expression Grammars](http://www.brynosaurus.com/pub/lang/peg.pdf) paper.

[JCPP](http://www.anarres.org/projects/jcpp/) Shevek, 2006-2011 - "A Java C Preprocessor"

[Instaparse](https://github.com/Engelberg/instaparse) Mark Engelberg, 2013 - "What if context-free grammars were as easy to use as regular expressions?"


## License

Copyright © 2013 Håkan Råberg

Distributed under the Eclipse Public License, the same as Clojure.
