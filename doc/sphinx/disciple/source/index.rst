.. Disciple documentation master file, created by
   sphinx-quickstart on Sun Jun 25 15:26:13 2017.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Disciple 0.5.1
==============

Disciple is an experimental dialect of Haskell used to investigate static typing and program transformation in the presence of computational effects. Version 0.5.1 is "working alpha" quality, meaning there is a complete system that can be hacked around with, but it's not yet industrial strength.

For example programs, see the demo_ directory on github, which includes:

* `Almost Primes`_ demo which uses infinite streams.
* `Conway's Life`_ demo which uses mutable arrays.
* `Lambda`_        demo which defines a simple lambda calculus interpreter.

.. _demo:               https://github.com/DDCSF/ddc/tree/master/test/ddc-demo/source/tetra
.. _`Almost Primes`:    https://github.com/DDCSF/ddc/blob/master/test/ddc-demo/source/tetra/80-Rosetta/AlmostPrime/Main.ds
.. _`Conway's Life`:    https://github.com/DDCSF/ddc/tree/master/test/ddc-demo/source/tetra/10-Defib/04-Life
.. _`Lambda`:           https://github.com/DDCSF/ddc/tree/master/test/ddc-demo/source/tetra/90-Language/01-Lambda


The main language features are:

* Haskell-like source language, so Haskell-like programs should work with minor modifications.
* Modal region and effect system using 'box' and 'run' to suspend and force computations.
* Higher rank polymorphism with bidirectional type inference.
* Simple two space copying garbage collection.
* Default call-by-value evaluation.
* Typed external core language.


Getting Started
---------------

You need recent versions of GHC, Cabal and LLVM already installed:

.. code-block:: none

  $ ghc --version
  The Glorious Glasgow Haskell Compilation System, version 8.0.1

  $ cabal --version
  cabal-install version 1.24.0.0
  compiled using version 1.24.0.0 of the Cabal library

  $ llc --version
  LLVM version 3.6.2
  ...


Clone the source repository from github:

.. code-block:: none

  $ git clone git@github.com:DDCSF/ddc.git

Install dependencies into a cabal standbox.

.. code-block:: none

  $ cd ddc
  $ cabal update
  $ cabal sandbox init
  $ make setup

Build the compiler

.. code-block:: none

  $ make -j8


.. toctree::
   :maxdepth: 2
   :caption: Contents:



Indices and tables
==================

* :ref:`genindex`
* :ref:`search`
