
Shimmer Inferface Files
=======================

This is the grammar for the interface files "\*.di". The files themselves are in a `binary format`_ defined by the shimmer library which is available on `Hackage`_ and `Github`_. The binary format can be converted to a human readable format with the `shimmer` command-line tool:

.. code-block:: none

  cabal update
  cabal install shimmer
  shimmer -load Path/To/Some/Module.di

Modules
-------

.. code-block:: none

  Module
   ::= Decl;+

  Decl
   ::= '+m-name'   '=' List[Text]                       (module names)

    |  '+m-ex-typ' '=' List[ExTyp]                      (exported types)
    |  '+m-ex-val' '=' List[ExVal]                      (exported values)

    |  '+m-im-typ' '=' List[ImTyp]                      (imported types)
    |  '+m-im-cap' '=' List[ImCap]                      (imported capabilities)
    |  '+m-im-val' '=' List[ImVal]                      (imported values)
    |  '+m-im-dat' '=' List[TypDat]                     (imported data types)
    |  '+m-im-syn' '=' List[TypSyn]                     (imported type synonyms)

    |  '+m-lc-dat' '=' List[TypDat]                     (local data type declarations)
    |  '+m-lc-syn' '=' List[TypSyn]                     (local type synonyms)

    |  '@d-NAME'   '=' DataDef                          (data type declaration)
    |  '@t-NAME'   '=' Type                             (type declaration)
    |  '@x-NAME'   '=' Term                             (term declaration)


Exports
-------

.. code-block:: none

 ExTyp                                                  (export type)
  ::=  '%ex-typ' Var Kind                               (export type)


 ExVal                                                  (export value)
  ::=  '%ex-val-loc' ModName NAME '@t-NAME' '@x-NAME'
                     (Nat Nat Nat)?                     (export value local)

   |   '%ex-val-sea' Name Text Type                     (export value from sea)


Imports
-------

.. code-block:: none

 ImTyp                                                  (imported type)
  ::=  '%im-typ-abs' Name Type                          (imported type abstract)
   |   '%im-typ-box' Name Type                          (imported type boxed)


 ImCap
  ::=  '%im-cap-abs' Var Type                           (imported abstract capability)


 ImVal                                                  (imported value)
  ::=  '%im-val-mod' ModName Name '@t-NAME'
                     (Nat Nat Nat)?                     (import value from module)

   |   '%im-val-sea' NAME Text '@t-NAME'                (import value from sea)


 TypDat
  ::=  '%typ-dat'    NAME '@d-NAME'                     (imported data type)


 TypSyn
  ::=  '%typ-syn'    NAME '@s-NAME'                     (imported type synonym)


Data Declarations
-----------------

 DataDef                                                (data type declarations)
  ::= ... TODO ...


Types
-----

 Type                                                   (type declarations)
  ::= ... TODO ...


Terms
-----

 Term                                                   (term declarations)
  ::= ... TODO ...


.. _`binary format`:    https://hackage.haskell.org/package/shimmer-0.1.2/docs/SMR-Core-Codec.html
.. _`Hackage`:          https://hackage.haskell.org/package/shimmer
.. _`Github`:           https://github.com/discus-lang/shimmer
