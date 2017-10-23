
The Disciplined Disciple Compiler 0.4.2 (2016/04/30)
====================================================

DDC is a research compiler used to investigate program transformation in the
presence of computational effects. This is a development release. There is
enough implemented to experiment with the language, but not enough to solve
actual problems...        (unless you're looking for a compiler to hack on).

DDC compiles several related languages:

 * Disciple Tetra (.ds)

   An implicitly typed strict functional language with region and effect
   typing. Uses effect reification (`box`) and reflection (`run`) casts to
   compose computations with differing effects. Effectful computations are
   classified by the `S e a` type, for some effect `e` and return type `a`.
   Although type inference is supported, one can also write explicit type
   abstractions and applications when needed. Higher ranked types are
   supported with annotations.

 * Disciple Core Tetra (.dct)

   The desugared version of Disciple Tetra. All function application is in
   prefix form. This language also supports type inference, though the
   inferencer does not insert additional type quantifiers.

 * Disciple Core Flow (.dcf)

   Application specific language with built-in support for Series expressions
   and Data Flow Fusion. This language and its associated transforms is used by
   the repa-plugin available on Hackage.

 * Disciple Core Salt (.dcs)

   A fragment of Disciple Core that can be easily mapped onto C or LLVM code.
   The Salt language is first-order and does not support partial application.
   DDC transforms the higher level languages onto this one during code
   generation, though we can also write programs in it directly.

All core languages share the same abstract syntax tree (AST), type inferencer,
and are amenable to many of the same program transformations. They differ only
in the set of allowable language features, and which primitive types and
operators are included.


Main changes since 0.4.1
------------------------

 * Added code generation for higher order functions.

 * Added automatic insert of run and box casts.

 * Added multi-module compilation.

 * Added desugaring of guards.

 * Added primitives for dealing with arrays of boxed values and vectors
   of primitive, unboxed values.

 * Added first cut PHP code generator.

 * Added case-of-known-constructor transform.

 * Clustering and rate inference for Core Flow.

 * Source programs now accept unicode lambdas and dumps of intermediate code
   use lambdas for both term and type binders.

 * Removed the old 'Eval' and 'Lite' language fragments.


What works in this release
--------------------------

 * Compilation for the Tetra, and Salt languages.

 * Type checking and data flow fusion for the Flow language.

 * Program transformations: Anonymize (remove names), Beta (substitute),
   Bubble (move type-casts), Elaborate (add witnesses), Flatten (eliminate
   nested bindings), Forward (let-floating), Namify (add names), Prune
   (dead-code elimination), Snip (eliminate nested applications), Rewrite
   rules, cross-module inlining.


What doesn't
------------

 * No storage management.
   There is a fixed 64MB heap and when you've allocated that much space the
   runtime just calls abort().

 * No pattern matching desugaring.
   You can write case expressions with guards, but nested patterns are not
   desugared.


Previous Releases
-----------------

 * 2014/03 DDC 0.4.1: Added bi-directional type inference and region extension.
 * 2013/07 DDC 0.3.2: Added Tetra and Flow language fragments.
 * 2012/12 DDC 0.3.1: Added Lite fragment, compilation to C and LLVM.
 * 2012/02 DDC 0.2.0: Project reboot. New core language, working interpreter.
 * 2008/07 DDC 0.1.1: Alpha compiler, constructor classes, more examples.
 * 2008/03 DDC 0.1.0: Alpha compiler, used dependently kinded core language.


Immediate Plans
---------------

 1. Implement type synonyms.

 2. Implement desugaring for pattern matching.


How you can help
----------------

 1. Work through the tutorial on the web-site and send any comments to the
    mailing list.] http://disciple.ouroborus.net/wiki/Tutorial

 2. Say hello on the mailing list and we can help you get started on any of
    the main missing features. These are all interesting projects.

 3. Tell your friends.


People
------

The following people contributed to DDC since the last major release:


 * Viktar Basharymau:
   Documentation fixes.

 * Erik de Castro Lopo:
   Documentation fixes.

 * Jacob Stanley:
   Code generation and build system fixes.

 * Amos Robinson:
   PHP code generator;
   Clustering and rate inference for Core Flow.

 * Max Swadling:
   Added -run-static-link option for statically linked runtime.

 * Kyle Van Berendonck:
   Added case-of-known-constructor transform.

 * Ben Lippmeier:
   Added lambda lifter and code generation for higher order functions;
   Automatic insertion of run and box casts;
   Multi module compilation;
   Desugaring of guards.


More Information
----------------

 * See the web-site:        http://disciple.ouroborus.net

 * Getting Started Guide:   http://disciple.ouroborus.net/wiki/Tutorial/Core

 * Read the mailing list:   http://groups.google.com/group/disciple-cafe

