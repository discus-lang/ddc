
Getting Started
===============


You need recent versions of GHC, Cabal and LLVM already installed:

.. code-block:: none

  $ ghc --version
  The Glorious Glasgow Haskell Compilation System, version 8.2.1

  $ cabal --version
  cabal-install version 1.24.0.2
  compiled using version 1.24.2.0 of the Cabal library

  $ llc --version
  LLVM version 5.0.0
  ...


Clone the source repository from github:

.. code-block:: none

  $ git clone git@github.com:DDCSF/ddc.git
  $ cd ddc

Install package dependencies via cabal:

.. code-block:: none

  $ make setup

Build the compiler:

.. code-block:: none

  $ make

Run the testsuite to ensure everything is ok:

.. code-block:: none

  $ make war

Build and run one of the demos:

.. code-block:: none

  $ bin/ddc -make test/ddc-demo/source/tetra/10-Defib/04-Life/Main.ds
  $ test/ddc-demo/source/tetra/10-Defib/04-Life/Main -steps 1000
