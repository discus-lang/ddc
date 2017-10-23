
Disciple
========

The Disciple language is an experimental dialect of Haskell which investigates static typing and program transformation in the presence of computational effects. :ref:`Version 0.5.1 <release-notes-0.5.1>` is "working alpha" quality, meaning there is a complete system that can be hacked around with, but it's not yet industrial strength.

The main language features are:

* Haskell-like source language, so Haskell-like programs should work with minor modifications.
* Modal region and effect system using 'box' and 'run' to suspend and force computations.
* Higher rank polymorphism with bidirectional type inference.
* Simple two space copying garbage collection.
* Default call-by-value evaluation.
* Typed external core language.

For example programs, see the demo_ directory on github, which includes:

* `Conway's Life`_ demo which uses mutable arrays.
* `Almost Primes`_ demo which uses infinite streams.
* `Lambda`_        demo which defines a simple lambda calculus interpreter.
* `Ray Tracer`_    demo which makes nice pictures, described in this `blog post`_.

.. _demo:               https://github.com/DDCSF/ddc/tree/master/test/ddc-demo/source/tetra
.. _`Conway's Life`:    https://github.com/DDCSF/ddc/tree/master/test/ddc-demo/source/tetra/10-Defib/04-Life
.. _`Almost Primes`:    https://github.com/DDCSF/ddc/blob/master/test/ddc-demo/source/tetra/80-Rosetta/AlmostPrime/Main.ds
.. _`Lambda`:           https://github.com/DDCSF/ddc/tree/master/test/ddc-demo/source/tetra/90-Language/01-Lambda
.. _`Ray Tracer`:       https://github.com/DDCSF/ddc/tree/master/test/ddc-demo/source/tetra/40-Graphics/10-RayTrace
.. _`blog post`:        http://disciple-devel.blogspot.com.au/2017/07/ray-tracer-demo.html

.. toctree::
   :maxdepth: 1
   :caption: Contents:

   section/01-GettingStarted.rst
   section/02-Specification.rst
   section/03-Architecture.rst
   section/04-Development.rst
   section/05-Release.rst


Indices and tables
==================

* :ref:`genindex`
* :ref:`search`
