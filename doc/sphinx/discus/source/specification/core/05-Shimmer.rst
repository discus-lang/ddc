
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

  Decl                                                  (declarations)
   ::= '+m-name'   '=' ModName                          (name of current module)
    |  '+m-deps'   '=' List[ModName]                    (names of transitively imported modules)

    |  '+m-ex-typ' '=' List[ExTyp]                      (exported types)
    |  '+m-ex-val' '=' List[ExVal]                      (exported values)

    |  '+m-im-mod' '=' List[ModName]                    (names of directly imported modules)
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

  ModName
   ::= '%module-name' Name+                             (module name)

  TypDat
   ::= '%typ-dat' ModName NAME '@d-NAME'                (data type name)

  TypSyn
   ::= '%typ-syn' NAME '@s-NAME'                        (type synonym name)


Exports
-------

.. code-block:: none

 ExTyp                                                  (export type)
  ::= '%ex-typ' Var Kind                                (export type)


 ExVal                                                  (export value)
  ::= '%ex-val-loc' ModName NAME '@t-NAME' '@x-NAME'
                   (Nat Nat Nat)?                       (export value with optional arities)

   |  '%ex-val-sea' Name Text Type                      (export value from sea land)


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

   |   '%im-val-sea' ModName Name Text '@t-NAME'        (import value from sea)


Names and Binding
-----------------

.. code-block:: none

 Bind
  ::= '%bo'  Type                                       (dummy binder)
   |  '%ba'  Type                                       (anonymous binder)
   |  '%bn'  Name Type                                  (named binder)

 Bound
  ::=  NAT                                              (anonymous bound variable)
   |   Name                                             (named bound variable)

 Name
  ::=  TEXT


Data Type Declarations
----------------------

.. code-block:: none

 DataDef                                                (data type declarations)
  ::= '%d-alg' Name List[Bind] Maybe[List[Ctor]]        (algebraic data type)


Types
-----

.. code-block:: none

 Type                                                   (type declarations)
  ::= Bound                                             (bound type variable)
   |  TypeCon                                           (type constructor)
   |  '%tb' Bind Type                                   (type abstraction)
   |  '%ta' Type Type+                                  (type application)
   |  '%tl' Bind Type                                   (forall type)
   |  '%ts' Type Type*                                  (sum type)
   |  '%tf' TypeParam+ Type                             (flat function type)
   |  '%tu' Bound Type+                                 (flat constructor application)

 TypeParam
  ::= Type                                              (type of explicit function parameter)
   |  '%ni' Type                                        (type of implicit function parameter)


Primitive Type Constructors
---------------------------

.. code-block:: none

 TypeCon
  ::= '%tsp'                                            (sort of property types)
   |  '%tsc'                                            (sort of computation types)

   |  '%tkf'                                            (arrow kind)
   |  '%tkd'                                            (kind of data types)
   |  '%tkr'                                            (kind of region types)
   |  '%tke'                                            (kind of effect types)

   |  '%tcu'                                            (type of unit values)
   |  '%tcf'                                            (type constructor for functions with explicit parameter)
   |  '%tci'                                            (type constructor for functions with implicit parameter)
   |  '%tcs'                                            (type constructor for suspended computations)
   |  '%tcr'                                            (type constructor for read effects)
   |  '%tcw'                                            (type constructor for write effects)
   |  '%tca'                                            (type constructor for alloc effects)

   |  '%dt-Tuple'                                       (tuple type constructor)
   |  '%dt-Vector'                                      (vector type constructor)

   |  '%pt-void'                                        (primitive Void type constructor)
   |  '%pt-bool'                                        (primitive Bool type constructor)
   |  '%pt-nat'                                         (primitive Nat  type constructor)
   |  '%pt-int'                                         (primitive Int  type constructor)
   |  '%pt-size'                                        (primitive Size type constructor)
   |  '%pt-addr'                                        (primitive Addr type constructor)
   |  '%pt-ptr'                                         (primitive Ptr  type constructor)
   |  '%pt-textlit'                                     (primitive TextLit type constructor)
   |  '%pt-word'  NAT                                   (primitive WordN type constructor of given width)
   |  '%pt-float' NAT                                   (primitive FloatN type constructor of given width)

Terms
-----

.. code-block:: none

 Term                                                   (term declarations)
  ::= Bound                                             (bound variable)
   |  DataCon                                           (bound data constructor)
   |  TermLit                                           (primitive value)
   |  TermOp                                            (primitive operator)

   |  '%xdu'                                            (primitive unit data constructor)

   |  '%xb'  TermParam+ Term                            (abstraction)
   |  '%xa'  Term TermArg+                              (application)

   |  '%xll' Bind Term Term                             (non-recursive let-binding)
   |  '%xlr' Pair[Bind,Term]+ Term                      (recursive let-binding)
   |  '%xlp' Bind       Maybe[Type] Bind+ Term          (private region binding)
   |  '%xlp' List[Bind] Maybe[Type] Bind+ Term          (private region bindings)

   |  '%xc'  Term Alt+                                  (case expression)

   |  '%xcw' Type Term                                  (cast to weaken effect)
   |  '%xcb' Term                                       (cast to box term)
   |  '%xcr' Term                                       (cast to run computation)

 TermParam
  ::= '%mto' Type                                       (dummy type parameter)
   |  '%mta' Type                                       (anonymous type parameter)
   |  '%mtn' Name Type                                  (named type parameter)

   |  '%mxo' Type                                       (dummy term parameter)
   |  '%mxa' Type                                       (anonymous term parameter)
   |  '%mxn' Name Type                                  (named term parameter)

   |  '%mio' Type                                       (dummy implicit term parameter)
   |  '%mia' Type                                       (anonymous implicit term parameter)
   |  '%min' Name Type                                  (named implicit term parameter)

 TermArg
  ::= Term                                              (term argument)
   |  '%rt'  Type                                       (type argument)
   |  '%ri'  Term                                       (implicit term argument)

 Alt
  ::= '%ae'  Term                                       (alternative with default pattern)
   |  '%au'  Term                                       (alternative with unit pattern)
   |  '%ap'  TermLit Bind+ Term                         (alternative with primitive literal pattern)
   |  '%ab'  DataCon Bind+ Term                         (alternative with data constructor pattern)

 DataCon
  ::= '%dcn' Maybe[ModuleName] Maybe[Type] Ref          (data constructor name)
   |  '%dd-Tuple' NAT                                   (tuple type constructor)


Primitive Term Literals
-----------------------

.. code-block:: none

 TermLit
  ::= '#true'                                           (primitive true value)
   |  '#false'                                          (primitive false value)

   |  '#nat\'NAT'                                       (primitive natural)
   |  '#int\'INT'                                       (primitive integer)

   |  '#w8\'NAT'                                        (primitive 8-bit word)
   |  '#w16\'NAT'                                       (primitive 16-bit word)
   |  '#w32\'NAT'                                       (primitive 32-bit word)
   |  '#w64\'NAT'                                       (primitive 64-bit word)

   |  '#f32\'FLOAT'                                     (primitive 32-bit float)
   |  '#f64\'FLOAT'                                     (primitive 64-bit float)

   |  '%l-s' NAT                                        (primitive size literal)
   |  '%l-c' TEXT                                       (primitive char literal)
   |  '%l-t' TEXT                                       (primitive text literal)

 TermOp
  ::= '%pa-neg-u'                                       (primitive negation)
   |  '%pa-add-u'                                       (primitive addition)
   |  '%pa-sub-u'                                       (primitive subtraction)
   |  '%pa-mul-u'                                       (primitive multiplication)
   |  '%pa-div-u'                                       (primitive division)
   |  '%pa-mod-u'                                       (primitive modulus)
   |  '%pa-rem-u'                                       (primitive remainder)
   |  '%pa-eq-u'                                        (primitive equality)
   |  '%pa-neq-u'                                       (primitive negated equality)
   |  '%pa-gt-u'                                        (primitive greater-than)
   |  '%pa-ge-u'                                        (primitive greater-than or equal)
   |  '%pa-lt-u'                                        (primitive less-than)
   |  '%pa-le-u'                                        (primitive less-than or equal)
   |  '%pa-and-u'                                       (primitive boolean and)
   |  '%pa-or-u'                                        (primitive boolean or)
   |  '%pa-shl-u'                                       (primitive shift left)
   |  '%pa-shr-u'                                       (primitive shift right)
   |  '%pa-band-u'                                      (primitive bitwise and)
   |  '%pa-bor-u'                                       (primitive bitwise or)
   |  '%pa-bxor-u'                                      (primitive bitwise exclusive or)

   |  '%pc-convert-u'                                   (primitive value conversion)
   |  '%pc-promote-u'                                   (primitive value promotion)
   |  '%pc-truncate-u'                                  (primitive value truncation)

   |  '%ov-alloc-u'                                     (primitive vector allocation)
   |  '%ov-length-u'                                    (primitive vector length)
   |  '%ov-read-u'                                      (primitive vector read)
   |  '%ov-write-u'                                     (primitive vector write)

   |  '%oe-error-u'                                     (primitive case inexhaustive error)


.. _`binary format`:    https://hackage.haskell.org/package/shimmer-0.1.2/docs/SMR-Core-Codec.html
.. _`Hackage`:          https://hackage.haskell.org/package/shimmer
.. _`Github`:           https://github.com/discus-lang/shimmer
