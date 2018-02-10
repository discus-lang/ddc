
.. _release-notes-0.5.1:

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

 >  elem {Eq a} (k: a) (xx: List a): Bool
 >   = case xx of
 >        Nil             -> False
 >        Cons x xs
 >         | x == k       -> True
 >         | otherwise    -> elem k xs

* Floating point primitives.

  The addition of floating point primitives combined with proper storage
  management now lets us write more realistic example programs,
  like the `ray tracer demo`_, which was also described on the `blog`_.

* Travis continuous integration.

  Every push to the GitHub repo now gets tested by the `Travis buildbot`_.
  Branches can also be tested individually before being merged.

* Support for LLVM versions 3.1 - 5.0

  We query the version of the installed LLVM compiler and generate
  LLVM code with the required syntax. This process now works for versions
  3.1 through to 5.0, which is the latest at the time of this release.

* New home page with the start of a language specification.

  The `home page`_ now consists of the user manual generated from
  Sphinx / Restructured Text source. The manual includes grammars
  for source and core lanuages, as well as previous release notes.
  The bug tracker is still accessible via the `development wiki`_.

* Bug fixes for offside rule, compilation of nested pattern matching,
  string escapes, unsolved meta variables.

  Now with more bake.

.. _`ray tracer demo`:
   https://github.com/DDCSF/ddc/tree/master/test/ddc-demo/source/tetra/40-Graphics/10-RayTrace

.. _`blog`:
   http://disciple-devel.blogspot.com.au/2017/07/ray-tracer-demo.html

.. _`Travis buildbot`:
   https://travis-ci.org/DDCSF/ddc

.. _`home page`:
   http://disciple.ouroborus.net

.. _`development wiki`:
   http://trac.ouroborus.net/ddc

Work In Progress
----------------

* We are moving to a new indexed binary format for interface files,
  as re-parsing interface files is currently a bottleneck.
  The file format is to be provided by the `Shimmer project`_,
  which has been split out into a separate repository.

.. _`Shimmer project`:
   https://github.com/DDCSF/shimmer

People
------

The following people contributed to DDC since the last release:

* Chris Hall
  - Travis continuous integration.

* Amos Robinson
  - Build system fixes, start on machine fusion transform.

* Ben Sinclair
  - Build system fixes.

* Jacob Stanley
  - Copying garbage collection.

* Ben Lippmeier
  - Copying garbage collection, implicit parameters, floating point, new home page.


Previous Releases
-----------------

* 2016/09 DDC 0.4.3: Nested patterns and guards, simple type synonyms.
* 2016/04 DDC 0.4.2: Compilation of higher order functions, run and box casts.
* 2014/03 DDC 0.4.1: Added bi-directional type inference and region extension.
* 2013/07 DDC 0.3.2: Added Tetra and Flow language fragments.


More Information
----------------

Home Page:              http://disciple.ouroborus.net

The GitHub site:        http://github.com/DDCSF/ddc

Development Wiki:       http://trac.ouroborus.net/ddc

Development Blog:       http://disciple-devel.blogspot.com.au/

Mailing List:           http://groups.google.com/group/disciple-cafe

