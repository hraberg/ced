# CED - Ced is the Standard Clojure EDitor

*CED is an effort to bring the power of Ed to Clojure.*


Unlike bloated shit like Emacs, [Ed](http://www.gnu.org/software/ed/) stays out of the way and doesn't burden you with an extra Lisp implementation - Clojure can be first. After having worked on [Deuce](https://github.com/hraberg/deuce) for close to 2 months now, I realized it's time to go back to basics and start from `*scratch*`.

Ed also has a serious test suite, which is something that cannot be said for Emacs. To lose no time to a future proof-ed Ed, I've decided that the easiest way to progress is to write a C to Clojure compiler - similar to Zeta-C or Vacietis for Common Lisp, together with a thin standard C library implementation on top of the JVM. The exact details are still to be hashed out. Here projects like Cibyl can serve as inspiration, which is a MIPS to JVM compiler (primarily for J2ME use).

This straight forward approach will allow for the unaltered *standard* Ed C source to be run on top of the JVM.

Extending Ed, the REPL of champions, with a powerful Lisp such as Clojure will open up unheard of possibilities in the near and not so near future - and potentially even in the past - but let's not get ahead of ourselves!

I aim to hack on this for the next 24 hours to see where it ends up, before returning to my regular scheduled Emacs development.


### What Works?

Almost nothing yet. To build, run:

    ./build # Fetches ed-1.7.tar.gz, musl-0.9.9.tgz, creates the C parser and bundles it all into an uberjar.

* Using JCPP to pre-process the full Ed 1.7 source tree and feeding it to the Rats! C parser.
* Building Clojure AST from the output.
* K&R p. 10, "The first C program" works, by cheating and relying on Clojure's `printf`:

    hello, world


### What's next?

* Walk the AST. Sprinkle a few `defmacro`s to easily compile C to Clojure.
 * Investigate an `asm.clj` like approach?
* A big decision is to figure out how pointers will work.
* Decide how to deal with `libc` - there are a few approaches:
 * Provide exact handwritten Clojure implementations for the parts that matter. This is how Vacieties and Zeta-C works.
 * Try to base the implementation on something like musl or µClibc and move down the stack - but get a lot back for free.
 * Use, or take hints from, an existing `libc` implementation for the JVM, like Cybil's.
 * Use JNA, would simplify certain things, but complicate others.
* There's luckily no need for GUI tool-kits: `*out*` and `*in*` should suffice.
* Running the *extensive* Ed test suite, which is nicely all in/out based (see above), so slotting in a script instead of `ed` starting our new Clojure port should be possible, and enable official blessing of adherence to the standard.


### Questions

* Is a C parser worth it, or is it more idiomatic to use Clojure's reader to parse the C?
* As Ed 1.7 is 2981 lines of C and 255 lines of header files, one approach could have been to just rewrite it all by hand in ~200 lines of Clojure, but I have a feeling that the standard could be compromised using this naive approach.
* If this works for Ed, should it be done for Emacs?


## References

[Ed, man! !man ed](http://www.gnu.org/fun/jokes/ed-msg.html) Patrick J. LoPresti, 1991

[The C Programming Language, Second Edition](http://cm.bell-labs.com/cm/cs/cbook/) Brian W. Kernighan and Dennis M. Ritchie, 1988

[C: A Reference Manual, Fifth Edition](http://careferencemanual.com/) Samuel P. Harbison III, Guy L. Steele Jr. 2002

[Expert C Programming: Deep C Secrets](http://www.amazon.com/Expert-Programming-Peter-van-Linden/dp/0131774298) Peter van der Linden, 1994

[xtc](http://cs.nyu.edu/rgrimm/xtc/) Paul Gazzillo and Robert Grimm, 2006-2012 -  "The xtc (eXTensible Compiler) project is exploring novel programming languages and tools to improve the expressiveness, safety, and efficiency of complex systems."

[Rats!](http://cs.nyu.edu/rgrimm/xtc/rats-intro.html) Robert Grimm, 2004-2012

[JCPP](http://www.anarres.org/projects/jcpp/) Shevek, 2006-2011 - "A Java C Preprocessor"

[ZETA-C-PD](http://bitsavers.informatik.uni-stuttgart.de/bits/TI/Explorer/zeta-c/) Scott L. Burson, 1987, 2003 - "Zeta-C is a C implementation for Lisp Machines that works by translating C into Lisp."

[Vacietis - C to Common Lisp compiler](https://github.com/vsedach/Vacietis) Vladimir Sedach, 2012

[musl](http://www.musl-libc.org/) Rich Felker, 2010-2013 - "musl is a new general-purpose implementation of the C library. It is lightweight, fast, simple, free, and aims to be correct in the sense of standards-conformance and safety."

[Cibyl](http://code.google.com/p/cibyl/) Simon Kagstrom, 2007-2011 - "Cibyl is a programming environment and binary translator that allows compiled C programs to execute on J2ME-capable phones."

[µClibc](http://www.uclibc.org/about.html) Erik Andersen, 1999-2012 - "A C library for embedded Linux"


## License

[GNU General Public License Version 3](http://www.gnu.org/licenses/gpl-3.0.html)

[GNU Ed](http://www.gnu.org/software/ed/) is Copyright (C) 1993-1994, 2006-2012 [Free Software Foundation, Inc](http://www.fsf.org/).
