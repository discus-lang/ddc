
Concrete Syntax
===============

Modules
-------

.. code-block:: none

  Module                                                   (source modules)
   ::= 'module' ModuleName
        ExportSpecs* ImportSpecs*;
       'where'  '{' Decl*; '}'

  ExportSpecs                                              (export specifications)
   ::= 'export' '{' Var+; '}'

  ImportSpecs                                              (import specifications)
   ::= 'import' '{' ModuleName;+ '}'                       (module imports)
    |  'import' 'foreign' ImportSpecsForeign               (foreign imports)

  ImportSpecsForeign                                       (foreign import specification)
   ::= 'boxed'    'type'       '{' ConSig;+ '}'            (foreign boxed type import)
    |  'abstract' 'type'       '{' ConSig;+ '}'            (foreign abstract type import)
    |  'abstract' 'capability' '{' VarSig;+ '}'            (foreign abstract capability import)
    |  'c'        'value'      '{' VarSig;+ '}'            (foreign c value import)

  VarSig  ::= 'Var' ':' Type                               (variable type signature)
  ConSig  ::= 'Con' ':' Type                               (constructor type signature)

Source modules begin with the keyword ``module`` followed by a module name, then some
optional export and import specifications, then some declarations. The export specifications must come before
the import specifications.

Export specifications list names of term bindings to export.

Import specifications list modules or foreign things to import.

Foreign types, capabilities and values can be imported. Foreign boxed types classify values that are stored in the Disciple runtime heap and have the standard format expected by the runtime system. Foreign abstract types are used to classify values that are not stored in the Disciple runtime heap. Foreign abstract capabilities such as 'Console' and 'File' are used for effects that alter the foreign world. Foreign c values are functions accessible via the standard C calling convention.

Braces in the ``Module``, ``ExportSpecs``, ``ImportSpecs`` and ``ImportSpecsForeign`` productions will be inserted using the off-side rule.

See the `module specification tests`_ for examples.

.. _`module specification tests`:
        https://github.com/DDCSF/ddc/tree/ddc-0.5.1/test/ddc-spec/source/01-Tetra/01-Syntax/01-Module

Types
-----

.. code-block:: none

  Type
   ::= TypeSimple → Type                                   (explicit function type, using '->' for '→' is ok)
    |  '{' Type '}' → Type                                 (implicit function type)
    |  '{' '@' Var ':' Type '}' → Type                     (implicit universal quantification)
    |  TypeApp + Type                                      (type sum)
    |  TypeApp                                             (type application)

  TypeApp                                                  (type application)
   ::= TypeApp TypeSimple                                  (type function applied to an argument)
    |  TypeSimple                                          (simple type)

  TypeSimple
   ::= Var                                                 (type variable)
    |  Con                                                 (type constructor)
    |  TypeBuiltin                                         (builtin type)
    |  '(' Type ',' Type+, ')'                             (tuple type)
    |  '(' Type ')'                                        (parenthesised type)

  TypeBuiltin
   ::= 'Pure'                                              (builtin pure effect)
    |  'Read'  | 'Write' | 'Alloc'                         (builtin effects)


Declarations
------------

.. code-block:: none

  Decl                                                     (declaration)
   ::= DeclType | DeclData | DeclValue

  DeclType                                                 (type declaration)
   ::= 'type' Con '=' Type                                 (type synonym declaration)

  DeclData                                                 (data type declaration)
   ::= 'data' Con DeclDataParams*
          ('where' '{' (Con ':' Type)+; '}')?

  DeclDataParams                                           (data type parameters)
   ::= '(' Var+ ':' Type ')'                               (data type parameters with shared kind)

  DeclTerm                                                 (term declaration)
   ::= Var ':' Type                                        (type signature)
    |  Var DeclTermParams* (':' Type)? GuardedExpsMaybe    (term declaration using guards)

  DeclTermParams                                           (term declaration parameters)
   ::= PatSimple                                           (simple pattern)
    |  '(' PatSimple+ ':' Type '}'                         (patterns with shared type annotation)
    |  '{' PatSimple+ ':' Type '}'                         (implicit parameters)
    |  '{' Type '}'                                        (anonymous implicit parameter)
    |  '{' '@' Var+   ':' Type '}'                         (implicit type parameter)


Type declarations define unparameterised type synonyms. (Issue385_) covers addition of type parameters.

Data type declarations define parameterised data types, giving the types of their data constructors. The return type of each constructor must match the data type being defined. It is valid to define a data type with no data constructors.

Term declarations are either type signatures or declarations that can mention function parameters, have an optional result type, and are defined in terms of guarded expressions.

Type parameters give the name of each parameter with an optional kind annotation. Multiple parameters can be defined that share a common kind annotation.

Term declaration parameters can be either type parameters with a shared kind, an anonymous or named implicit parameter, or a term parameter defined via pattern matching.

Braces in the ``DeclData`` production will be inserted using the off-side rule.

See the `declaration specification tests`_ for examples.

.. _Issue385: http://trac.ouroborus.net/ddc/ticket/385

.. _`declaration specification tests`:
        https://github.com/DDCSF/ddc/tree/ddc-0.5.1/test/ddc-spec/source/01-Tetra/01-Syntax/02-Decl/Main.ds


Guarded Expressions
-------------------

.. code-block:: none

  GuardedExpsMaybe                               (maybe guarded expressions)
   ::= '=' Exp                                   (simple unguarded expression)
    |  GuardedExp*                               (multiple guarded expressions)

  GuardedExp
   ::= '|' Guard,+ '=' Exp                       (guarded expression)

  Guard
   ::= 'otherwise'                               (otherwise guard always matches)
    |  Pat '<-' Exp                              (match against pattern)
    |  Exp                                       (boolean predicate)

The bodies of term declarations can be defined either with a single expression or using multiple guarded expressions.

The ``otherwise`` guard always succeeds. The ``Pat '<-' Exp`` guard succeeds when the value produced by ``Exp`` can be matched against ``Pat``. The boolean predicate guard suceeds when the resulting value is ``True``.

See the `guards specification tests`_ for examples.

.. _`guards specification tests`:
        https://github.com/DDCSF/ddc/tree/ddc-0.5.1/test/ddc-spec/source/01-Tetra/01-Syntax/03-Guards/Main.ds


Term Expressions
----------------

.. code-block:: none

  Exp
   ::= ExpApp ('where' '{' Clause;+ '}')?        (expression with optional where clause)

  ExpApp                                         (applicative expressions)
   ::= ExpAppPrefix |  ExpAppInfix
    |  ExpAppAbs    |  ExpAppBind
    |  ExpAppMatch  |  ExpAppEffect

  ExpAppPrefix                                   (prefix application)
   ::= ExpSimple ExpArg*                         (base expression applied to arguments)

  ExpAppInfix                                    (infix application)
   ::= ExpApp InfixOp ExpApp                     (application of infix operator)
    |  ExpSimple

  ExpArg                                         (function argument)
   ::= '{'  Exp  '}'                             (implicit term argument)
    |  '{' '@' Type '}'                          (implicit type argument)
    |  ExpBase                                   (base expression)

  ExpSimple
   ::= '()'                                      (unit  data constructor)
    |  DaCon                                     (named data constructor)
    |  Literal                                   (literal value)
    |  Builtin                                   (fragment specific builtin value)
    |  Var                                       (named variable)
    |  '(' InfixOp ')'                           (reference to infix operator)
    |  '(' Exp ',' Exp+, ')'                     (tuple expression)
    |  '(' Exp ')'                               (parenthesised expression)



Abstraction Expressions
-----------------------

.. code-block:: none

  ExpAppAbs
   ::= 'λ' ExpParam '->' Exp                     (abstraction, using '\'  for 'λ' is ok)

  ExpAbsParam
   ::=  PatSimple+                               (explicit unannotated term parameter}
    |  '(' Pat+     ':' Type ')'                 (explicit annotated term parameter)
    |  '{' Pat+     ':' Type '}'                 (implicit annotated term parameter)
    |  '{' '@' Var+ ':' Type '}'                 (implicit annotated type parmaeter)


See the `abstraction specification tests`_ for examples.

.. _`abstraction specification tests`:
        https://github.com/DDCSF/ddc/tree/ddc-0.5.1/test/ddc-spec/source/01-Tetra/01-Syntax/05-Abs/Main.ds


Binding Expressions
-------------------

.. code-block:: none

  ExpAppBind
   ::= 'let'    DeclTerm   'in' Exp              (non-recursive let binding)
    |  'letrec' DeclTerm+; 'in' Exp              (recursive let bindings)
    |  'do'    '{' Stmt+; '}'                    (do expression)

Matching Expressions
--------------------

.. code-block:: none

  ExpAppMatch
   ::= 'case'  '{' AltCase+; '}'                 (case expression)
    |  'match' '{' GuardedExp+; '}'              (match expression)
    |  'if' Exp 'then' Exp 'else' Exp            (if-expression)

  AltCase
   ::= Pat GuardedExp* '->' Exp                  (case alternative)

  Pat
   ::= DaCon PatBase*                            (data constructor patterm)
    |  PatBase                                   (base pattern)

  PatBase
   ::= '()'                                      (unit data constructor pattern)
    |  DaCon                                     (named data constructor pattern)
    |  Literal                                   (literal pattern)
    |  Var                                       (variable pattern)
    |  '_'                                       (wildcard pattern)
    |  '(' Pat ',' Pat+ ')'                      (tuple pattern)
    |  '(' Pat ')'                               (parenthesised pattern)


Effectual Expressions
---------------------

.. code-block:: none

  ExpAppEffect
   ::= 'weakeff' '[' Type ']' 'in' Exp           (weaken effect of an expression)

    |  'private' Bind+ WithCaps? 'in' Exp        (private region introduction)

    |  'extend'  Bind 'using' Bind+
                 WithCaps? 'in' Exp              (region extension)

    |  'box' Exp                                 (box a computation)
    |  'run' Exp                                 (run a boxed computation)

  WithCaps
   ::= 'with' '{' BindT+ '}'



