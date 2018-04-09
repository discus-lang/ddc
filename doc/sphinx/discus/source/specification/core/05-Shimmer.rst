
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

  TypDat
   ::= '%typ-dat' NAME '@d-NAME'                        (data type name)

  TypSyn
   ::= '%typ-syn' NAME '@s-NAME'                        (type synonym name)


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



Data Declarations
-----------------

.. code-block:: none

 DataDef                                                (data type declarations)
  ::= '%d-alg' Name List[Bind] Maybe[List[Ctor]]        (algebraic data type)


Types
-----

.. code-block:: none

 Type                                                   (type declarations)
  ::= Bound                                             (bound type variable)
   |  TyCon                                             (type constructor)
   |  '%tb' Bind Type                                   (type abstraction)
   |  '%ta' Type Type+                                  (type application)
   |  '%tl' Bind Type                                   (forall type)
   |  '%ts' Type Type*                                  (sum type)
   |  '%tf' TypeParam+ Type                             (flat function type)
   |  '%tu' Bound Type+                                 (flat constructor application)

 TypeParam
  ::= Type                                              (type of explicit parameter)
   | '%ni' Type                                         (type of implicit parameter)
   | '%nn' TcCon Type                                   (type of function parameter)

Terms
-----

.. code-block:: none

 Term                                                   (term declarations)
  ::= Bound                                             (bound variable)
   |  DaCon                                             (bound data constructor)
   |  VaPrim                                            (primitive value)

   |  '%xpe'                                            (primitive elaboration)
   |  '%xpp' Name                                       (primitive record projection)
   |  '%xps'                                            (primitive record shuffle)
   |  '%xpc'                                            (primitive record combine)

   |  '%xdu'                                            (primitive unit data constructor)
   |  '%xdr' Name                                       (primitive record type)
   |  '


.. _`binary format`:    https://hackage.haskell.org/package/shimmer-0.1.2/docs/SMR-Core-Codec.html
.. _`Hackage`:          https://hackage.haskell.org/package/shimmer
.. _`Github`:           https://github.com/discus-lang/shimmer
