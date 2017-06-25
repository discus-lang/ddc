
Abstract Syntax
===============

Names
-----

.. code-block:: none

  Con   (c)   ⟶ (named constructors)
  Var   (n)   ⟶ (named variables)


Types
-----

.. code-block:: none

  Type (τ,k,σ)
          ::= TyCon                         (type constructors)
           |  Var                           (type variables)
           |  'λ' Var ':' Type '.' Type     (type abstraction)
           |  Type Type                     (type application)

  TyCon   ::= Con                           (primitive type constructors)
           |  'Data' | 'Effect' | 'Region'  (basic kind constructors)
           |  '(→)'  | 'Unit'   | 'Void'    (basic data type constructors)
           |  'Read' | 'Write'  | 'Alloc'   (basic effect type constructors)
           |  '⋁' Type Nat                  (least upper bound of types)
           |  '⊥' Type                      (least element of a type)
           |  '∀' Type                      (universal quantification)


The basic kind constructors are ``Data``, ``Effect`` and ``Region`` for the kind of data, effect and region type respectively. The function type constructor ``(→)`` is usally written infix in sugared presentations. The ``Unit`` type classifies a set of values with the single element ``()``. The ``Void`` type classifies the empty set.

The ``⋁`` type constructor is used to express type sums, where ``⊥`` is an empty sum. Both are annotated with the kind of their result types.

The quantifier constructor ``∀`` is annotated with the kind of its result type.


Syntactic Sugar for Types
-------------------------

.. code-block:: none

 t1 → t2       ≡ (→) t1 t2

 t1 + t2       ≡ ⋁ k 2 t1 t2
 t1 + t2 + t3  ≡ ⋁ k 3 t1 t2 t3

 ⊥             ≡ ⊥ k

 ∀ n : k. t    ≡ (∀ k) (λ n : k. t)

The function type constructor ``(→)`` is usually written infix.

When the kind is clear from context we use the infix ``(+)`` operator to express type sums.

When the kind is clear from context, the kind argument on ``(⊥)`` is elided.

Quantifiers that bind names are desugared to an application of the associated type constructor and a type abstraction. Representing the different quantifiers as constructors allows us to retain a single binding form for types.


Terms
-----

.. code-block:: none

 Exp (e)
        ::= DaCon                                (data constructor)
         |  Var                                  (variable)

         |  'λ' Var ':' Type '.' Exp             (term abstraction)
         |  Exp Exp                              (term application)

         |  'Λ' Var ':' Type '.' Exp             (type abstraction)
         |  Exp Type                             (type application)

         |  'let'    Bind   'in' Exp             (let binding)
         |  'letrec' BindT+ 'in' Exp             (recursive let binding)

         |  'case'   Exp  'of' Alt+              (case matching)

         |  'private' Var 'with' Sig+ 'in' Exp   (region introduction)

         |  'extend'  Var
              'using' Var 'with' Sig+ 'in' Exp   (region extension)

         |  'weakeff' Type 'in' Exp              (effect weakening)
         |  'box' Exp                            (box a computation)
         |  'run' Exp                            (run a suspension)

 DaCon  ::= '()'                                 (builtin unit data constructor)
         |   Con                                 (named data constructor)

 Bind   ::= Var '='Exp                           (binding)
 BindT  ::= Var ':' Type '=' Exp                 (typed binding)
 Sig    ::= Var ':' Type                         (type signature)

 Alt    ::= Pat '→' Exp                          (case alternative)
 Pat    ::= _ | DaCon Sig+                       (case pattern)


The syntax of constructors and variables, term abstraction and application, type abstraction and application, case-matching and let-binding is standard.

The ``private`` construct introduces a new region variable along with capabilities of the given signatures. Both the region variable and names of the capabilities are in scope in the body expression.
