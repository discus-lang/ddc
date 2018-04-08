
Concrete Syntax
===============

This is the concrete syntax for the DDC Core language. The definition is parameterised over a set of type and term primitives -- ``TyPrim`` and ``VaPrim`` respectively.

The Core Discus and Core Salt languages use the same base core language, but add their own sets of primitives described in:

 * :ref:`core-discus-fragment`
 * :ref:`core-salt-fragment`


Names
-----

.. code-block:: none

  Con     (c)   ⟶ (named constructors)
  QualCon (qc)  ⟶ (.. with module name qualifier)

  Var     (v)   ⟶ (named variables)
  QualVar (qv)  ⟶ (.. with module name qualifier)


Modules
-------

.. code-block:: none

  Module                                              (core module)
   ::= 'module' ModuleName
          ExportSpecs* ImportSpecs* Decl*
         'with' Exp

  ExportSpecs                                         (export specifications)
   ::= 'export'   'value'      '{' QualVarSig+; '}'   (value exports)

  ImportSpecs                                         (import specifications)
   ::= 'import'   'value'      '{' QualVarSig;+ '}'   (value  imports)
    |  'import'   'foreign'     ImportSpecsForeign    (foreign imports)
    |  'import'   DeclType                            (type synonym imports)
    |  'import'   DeclData                            (data type imports)

  ImportSpecsForeign                                  (foreign import specification)
   ::= 'boxed'    'type'       '{' ConSig;+ '}'       (foreign boxed type import)
    |  'abstract' 'type'       '{' ConSig;+ '}'       (foreign abstract type import)
    |  'abstract' 'capability' '{' VarSig;+ '}'       (foreign abstract capability import)
    |  'c'        'value'      '{' VarSig;+ '}'       (foreign c value import)

  VarSig      ::= Var     ':' Type                    (variable with type signature)
  QualVarSig  ::= QualVar ':' Type                    ( .. qualified variable)

  ConSig      ::= Con     ':' Type                    (constructor with type signature)
  QualConSig  ::= QualCon ':' Type                    ( .. qualified constructor)

Core modules begin with the keyword ``module`` followed by a module name, then some optional export and import specifications, then some declarations. The export specifications must come before the import specifications.


Declarations
------------

.. code-block:: none

  Decl
   ::= DeclType | DeclData

  DeclType
   ::= 'type' Con '=' Type                            (type synonym declaration)

  DeclData
   ::= 'data' Con VarSig*                             (data type declaration)
           ('where' '{' ConSig;+ '}')?

  DeclTerm
   ::=  Var ':' Type '=' Exp                          (term declaration)


Type declarations define unparameterised type synonyms.

Data type declarations define parameterised data types, giving the types of their data constructors. The return type of each constructor must match the data type being defined. It is valid to define a data type with no data constructors.

Term declarations include a type and expression of that type.


Types
-----

.. code-block:: none

  Type
   ::= TyCon                                          (baked-in type constructor)
    |  TyPrim                                         (primitive type constructor)

    |  Var                                            (type variable)
    |  Type Type                                      (type application)

    |  '[' Var ':' Type ']' '.' Type                  (universal quantification)

    |  Type '+' Type                                  (type upper bound)

    |  '(' Type ')'                                   (parenthesis)

  TyCon
   ::= Con                                            (primitive type constructors)
    |  'Data' | 'Region' | 'Effect'                   (basic kind constructors)
    |  '(->)' | '(~>)'                                (function type constructors)
    |  'Unit' |                                       (basic data type constructors)
    |  'Read' | 'Write'  | 'Alloc'                    (basic effect type constructors)

Constructors, variables, abstractions and quantification are standard.

Type sums written with '+' are used to collect together multiple effect types.


Terms
-----

.. code-block:: none

  Exp (e)
   ::= DaCon                                          (data constructor)
    |  VaPrim                                         (primitive operator or literal)

    |  Var                                            (variable)

    |  'λ' '(' Var ':' Type ')' '.' Exp               (explicit term abstraction)
    |  'λ' '{' Var ':' Type ')' '.' Exp               (implicit term abstraction)
    |  'Λ' '(' Var ':' Type ')' '.' Exp               (type abstraction)

    |  Exp Exp                                        (explicit term application)
    |  Exp '{' Exp  '}'                               (implicit term application)
    |  Exp '[' Type ']'                               (type application)

    |  'let'    Bind   'in' Exp                       (let binding)
    |  'letrec' BindT+ 'in' Exp                       (recursive let binding)

    |  'case'   Exp  'of'  '{' Alt+; '}'              (case matching)

    |  'private'  Var
          'with'  Sig+ 'in' Exp                       (private region introduction)

    |  'extend'   Var
          'using' Var 'with' Sig+ 'in' Exp            (region extension)

    |  'weakeff' Type 'in' Exp                        (effect weakening)
    |  'box' Exp                                      (box a computation)
    |  'run' Exp                                      (run a suspension)

 DaCon  ::= '()'                                      (builtin unit constructor)
         |   Con                                      (named data constructor)

 Bind   ::= Var (':' Type)? '=' Exp                   (binding)
 BindT  ::= Var  ':' Type   '=' Exp                   (typed binding)

 Alt    ::= Pat '→' Exp                               (case alternative)
 Pat    ::= '_' | DaCon VarSig+                       (case pattern)


The syntax of constructors and variables, term abstraction and application, type abstraction and application, case-matching and let-binding is standard.

The ``private`` construct introduces new local regions with the given names and capabilities. The regions and capabilities are in scope in the body expression.

The ``extend`` construct extends an existing region with a new subregion, where the subregion has the given added capabilities.

The ``weakeff`` construct is used to weaken the effect of the body expression. The provided type must be an effect type, which is added to the effect of the body expression.

The ``box`` form suspends an effectful expression, yielding a closure.

The ``run`` form executes a closure, yielding the result value.

