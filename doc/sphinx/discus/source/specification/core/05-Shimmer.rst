
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
  ::= '%data-alg' Name List[Bind] Maybe[List[Ctor]]     (algebraic data type)


Types
-----

.. code-block:: none

 Type                                                   (type declarations)
  ::= Bound                                             (bound type variable)
   |  '%ta' Type Type+                                  (type application)
   |  '%tb' Bind Type                                   (type abstraction)
   |  '%tl' Bind Type                                   (forall type)
   |  '%ts' Type Type*                                  (sum type)
   |  '%tf' TypeParam+ Type                             (function type)
   |  '%tu' Bound Type+                                 (type constructor application)
   |  TypeCon                                           (type atom)

 TypeParam
  ::= Type                                              (type of explicit function parameter)
   |  '%ni' Type                                        (type of implicit function parameter)


Primitive Type Constructors
---------------------------

.. code-block:: none

 TypeCon
  ::= '%tcn' NAME                                       (named type constructor)

   |  '%ts-prop'                                        (sort of property types)
   |  '%ts-comp'                                        (sort of computation types)

   |  '%tk-arr'                                         (arrow kind)
   |  '%tk-data'                                        (kind of data types)
   |  '%tk-region'                                      (kind of region types)
   |  '%tk-effect'                                      (kind of effect types)

   |  '%tc-void'                                        (void type)
   |  '%tc-unit'                                        (type of unit values)
   |  '%tc-fun'                                         (type constructor for functions with explicit parameter)
   |  '%tc-funi'                                        (type constructor for functions with implicit parameter)
   |  '%tc-susp'                                        (type constructor for suspended computations)
   |  '%tc-read'                                        (type constructor for read effects)
   |  '%tc-write'                                       (type constructor for write effects)
   |  '%tc-alloc'                                       (type constructor for alloc effects)

   |  '%tc-tuple'                                       (tuple type constructor)
   |  '%tc-vector'                                      (vector type constructor)

   |  '%tc-void'                                        (primitive Void type constructor)
   |  '%tc-bool'                                        (primitive Bool type constructor)
   |  '%tc-nat'                                         (primitive Nat  type constructor)
   |  '%tc-int'                                         (primitive Int  type constructor)
   |  '%tc-size'                                        (primitive Size type constructor)
   |  '%tc-addr'                                        (primitive Addr type constructor)
   |  '%tc-ptr'                                         (primitive Ptr  type constructor)
   |  '%tc-textlit'                                     (primitive TextLit type constructor)
   |  '%tc-word'  NAT                                   (primitive WordN type constructor of given width)
   |  '%tc-float' NAT                                   (primitive FloatN type constructor of given width)

Terms
-----

.. code-block:: none

 Term
  ::= Bound                                             (bound variable)
   |  '%xa'  Term TermArg+                              (application)
   |  '%xb'  TermParam+ Term                            (abstraction)
   |  '%xc'  Term Alt+                                  (case expression)
   |  '%xll' Bind Term Term                             (non-recursive let-binding)
   |  '%xlr' Pair[Bind,Term]+ Term                      (recursive let-binding)
   |  '%xlp' Bind       Maybe[Type] Bind+ Term          (private region binding)
   |  '%xlp' List[Bind] Maybe[Type] Bind+ Term          (private region bindings)
   |  '%xtw' Type Term                                  (weaken effect)
   |  '%xtb' Term                                       (box computation)
   |  '%xtr' Term                                       (run computation)
   |  TermAtom                                          (atomic term)

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

 TermAtom
  ::= DataCon                                           (bound data constructor)
   |  TermLit                                           (primitive term literal)
   |  TermOp                                            (primitive term operator)

 Alt
  ::= '%ae'  Term                                       (alternative with default pattern)
   |  '%au'  Term                                       (alternative with unit pattern)
   |  '%ap'  TermLit Bind+ Term                         (alternative with primitive literal pattern)
   |  '%ab'  DataCon Bind+ Term                         (alternative with data constructor pattern)

 DataCon
  ::= '%dcn' Maybe[ModuleName] Maybe[Type] Ref          (named data constructor)
   |  '%dc-unit'                                        (unit data constructor)
   |  '%dc-tuple' NAT                                   (tuple type constructor)


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

   |  '%lt-size' NAT                                         (primitive size literal)
   |  '%lt-char' TEXT                                        (primitive char literal)
   |  '%lt-text' TEXT                                        (primitive text literal)

 TermOp
  ::= '%op-neg'                                         (primitive negation)
   |  '%op-add'                                         (primitive addition)
   |  '%op-sub'                                         (primitive subtraction)
   |  '%op-mul'                                         (primitive multiplication)
   |  '%op-div'                                         (primitive division)
   |  '%op-mod'                                         (primitive modulus)
   |  '%op-rem'                                         (primitive remainder)
   |  '%op-eq'                                          (primitive equality)
   |  '%op-neq'                                         (primitive negated equality)
   |  '%op-gt'                                          (primitive greater-than)
   |  '%op-ge'                                          (primitive greater-than or equal)
   |  '%op-lt'                                          (primitive less-than)
   |  '%op-le'                                          (primitive less-than or equal)
   |  '%op-and'                                         (primitive boolean and)
   |  '%op-or'                                          (primitive boolean or)
   |  '%op-shl'                                         (primitive shift left)
   |  '%op-shr'                                         (primitive shift right)
   |  '%op-band'                                        (primitive bitwise and)
   |  '%op-bor'                                         (primitive bitwise or)
   |  '%op-bxor'                                        (primitive bitwise exclusive or)

   |  '%op-convert'                                     (primitive value conversion)
   |  '%op-promote'                                     (primitive value promotion)
   |  '%op-truncate'                                    (primitive value truncation)

   |  '%op-alloc'                                       (primitive vector allocation)
   |  '%op-length'                                      (primitive vector length)
   |  '%op-read'                                        (primitive vector read)
   |  '%op-write'                                       (primitive vector write)

   |  '%op-error-case'                                  (primitive case inexhaustive error)


.. _`binary format`:    https://hackage.haskell.org/package/shimmer-0.1.2/docs/SMR-Core-Codec.html
.. _`Hackage`:          https://hackage.haskell.org/package/shimmer
.. _`Github`:           https://github.com/discus-lang/shimmer
