
The Disciplined Disciple Compiler 0.5.1 (2017/10/23) (Current Version)
======================================================================

The Disciple language is an experimental dialect of Haskell which investigates
static typing and program transformation in the presence of computational
effects. Version 0.5.1 is “working alpha” quality, meaning there is a complete
system that can be hacked around with, but it’s not yet industrial strength.


Features
--------

* Haskell-like source language, so Haskell-like programs should work with minor modifications.

* Modal region and effect system using ‘box’ and ‘run’ to suspend and force computations.

* Higher rank polymorphism with bidirectional type inference.

* Simple two space copying garbage collection.

* Default call-by-value evaluation.

* Typed external core language.


New In This Release
-------------------

* Copying garbage collection.

  We now do simple two space Cheney-scan garbage collection, and grow the heap
  as needed. We use LLVM shadow stack support to retrieve the GC roots.

* Implicit parameters.

  We now support Agda-style implicit parameters, where arguments are construct
  at use-sites using whatever bindings are currently in scope. For example,
  we can perform Haskell-style ad-hoc overloading using implicit dictionaries:

.. code-block:: none

  elem {Eq a} (k: a) (xx: List a): Bool
   = case xx of
        Nil             -> False
        Cons x xs
         | x == k       -> True
         | otherwise    -> elem k xs

* Floating point primitives.

  The addition of floating point primitives combined with proper storage
  management now lets us write more realistic example programs,
  like the [Ray Tracer](https://github.com/DDCSF/ddc/tree/master/test/ddc-demo/source/tetra/40-Graphics/10-RayTrace)

* Travis continuous integration.

  Every push to the GitHub repo now gets tested by the Travis buildbot.
  Branches can also be tested individually before being merged.

* Support for LLVM versions 3.1 - 5.0

  DDC queries the version of the installed LLVM compiler and generates
  LLVM code with the required syntax. This process now works for versions
  3.1 through to 5.0, which is the latest at the time of this release.

* New home page with the start of a language specification.

  We now have the start of a Sphinx / Restructured Text manual,
  including grammars for the source and core languages.

* Bug fixes for offside rule, compilation of nested pattern matching,
  string escapes, unsolved meta variables.

  Now with more bake.


Work In Progress
----------------

* We are moving to a new indexed binary format for interface files,
  as re-parsing interface files is currently a bottleneck.
  The file format is to be provided by the
  [Shimmer](https://github.com/DDCSF/shimmer) project, which has been
  split out into a separate repository.


People
------

The following people contributed to DDC since the last major release:

 * Chris Hall
   - Travis continuous integration.

 * Amos Robinson
   - Build system fixes, start on machine fusion transform.

 * Ben Sinclair
   - Build system fixes.

 * Jacob Stanley
   - Copying garbage collection.

 * Ben Lippmeier
   - Copying garbage collection, implicit parameters, floating point,
     new home page.


Previous Releases
-----------------

 * 2016/09 DDC 0.4.3: Nested patterns and guards, simple type synonyms.
 * 2016/04 DDC 0.4.2: Compilation of higher order functions, run and box casts.
 * 2014/03 DDC 0.4.1: Added bi-directional type inference and region extension.
 * 2013/07 DDC 0.3.2: Added Tetra and Flow language fragments.
 * 2012/12 DDC 0.3.1: Added Lite fragment, compilation to C and LLVM.
 * 2012/02 DDC 0.2.0: Project reboot. New core language, working interpreter.
 * 2008/07 DDC 0.1.1: Alpha compiler, constructor classes, more examples.
 * 2008/03 DDC 0.1.0: Alpha compiler, used dependently kinded core language.


More Information
----------------

The GitHub site:        http://github.com/DDCSF/ddc

Home Page:              http://disciple.ouroborus.net

Development Wiki:       http://trac.ouroborus.net/ddc

Development Blog:       http://disciple-devel.blogspot.com.au/

Mailing List:           http://groups.google.com/group/disciple-cafe

