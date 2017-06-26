
Concrete Syntax
===============

Modules
-------

.. code-block:: none

  Module
   ::= 'module' ModuleName
        ExportSpecs* ImportSpecs*;
       'where'  '{' Decl*; '}'

  ExportSpecs
   ::= 'export' '{' Var+; '}'

  ImportSpecs
   ::= 'import' '{' ModuleName;+ '}'
    |  'import' 'foreign' ImportSpecsForeign

  ImportSpecsForeign
   ::= 'boxed'    'type'       '{' ConSig;+ '}'
    |  'abstract' 'type'       '{' ConSig;+ '}'
    |  'abstract' 'capability' '{' VarSig;+ '}'
    |  'c'        'value'      '{' VarSig;+ '}'

  VarSig  ::= 'Var' ':' Type
  ConSig  ::= 'Con' ':' Type

Source modules begin with the keyword ``module`` followed by a module name, then some
optional export and import specifications, then some declarations. The export specifications must come before
the import specifications.

Export specifications list names of term bindings to export.

Import specifications list modules or foreign things to import.

Foreign types, capabilities and values can be imported. Foreign boxed types classify values that are stored in the Disciple runtime heap and have the standard format expected by the runtime system. Foreign abstract types are used to classify values that are not stored in the Disciple runtime heap. Foreign abstract capabilities such as 'Console' and 'File' are used for effects that alter the foreign world. Foreign c values are functions accessible via the standard C calling convention.

Braces in the ``Module``, ``ExportSpecs``, ``ImportSpecs`` and ``ImportSpecsForeign`` productions will be inserted using the off-side rule.


Declarations
------------

.. code-block:: none

  Decl                                                     (declaration)
   ::= DeclType | DeclData | DeclValue                     (type declaration)

  DeclType                                                 (type declaration)
   ::= 'type' Con '=' Type                                 (type synonym declaration)

  DeclData                                                 (data type declaration)
   ::= 'data' Con TypeParams*
          ('where' '{' (Con ':' Type)+; '}')?              (data type declaration)

  TypeParams                                               (type parameter)
   ::= '(' Var+ ':' Type ')'                               (type parameters with shared kind)
    |  Var                                                 (single type parameter)

  DeclTerm                                                 (term declaration)
   ::= Var ':' Type                                        (type signature)
    |  Var DeclTermParams* (':' Type)? GuardedExpsMaybe    (term declaration using guards)

  DeclTermParams                                           (term declaration parameters)
   ::= '[' Var+       ':' Type ']'                         (type parameter)
    |  '{' Type '}'                                        (anonymous implicit parameter)
    |  '{' PatSimple+ ':' Type '}'                         (implicit term parameters)
    |  '(' PatSimple+ ':' Type '}'                         (patterns with shared type annotation)
    |  '(' Pat ')'                                         (pattern in parenthesis)
    |  PatBase                                             (base pattern)

  GuardedExpsMaybe                                         (maybe guarded expressions)
   ::= '=' Exp                                             (simple unguarded expression)
    |  GuardedExp*                                         (multiple guarded expressions)

  GuardedExp
   ::= '|' Guard,+ '=' Exp                                 (guarded expression)

  Guard
   ::= 'otherwise'                                         (otherwise guard always matches)
    |  Pat '<-' Exp                                        (match against pattern)
    |  Exp                                                 (boolean predicate)


Type declarations define unparameterised type synonyms. (Issue385_) covers addition of type parameters.

Data type declarations define parameterised data types, giving the types of their data constructors. The return type of each constructor must match the data type being defined. It is valid to define a data type with no data constructors.

Term declarations are either type signatures or declarations that can mention function parameters, have an optional result type, and are defined in terms of guarded expressions.

Type parameters give the name of each parameter with an optional kind annotation. Multiple parameters can be defined that share a common kind annotation.

Term declaration parameters can be either type parameters with a shared kind, an anonymous or named implicit parameter, or a term parameter defined via pattern matching.

Braces in the ``DeclData`` production will be inserted using the off-side rule.

.. _Issue385: http://trac.ouroborus.net/ddc/ticket/385


Term Expressions
----------------

.. code-block:: none

  Exp
   ::= ExpApp ('where' '{' Clause;+ '}')?                  (expression with optional where clause)

  ExpApp
   ::= ExpAppPrefix                                        (prefix application)
    |  ExpAppInfix                                         (infix application)

  ExpAppPrefix
   ::= ExpFront ExpArg*;                                   (front expression applied to arguments)

  ExpFront
   ::= 'λ' TermParams '->' Exp                             (term abstraction, using '\'  for 'λ' is ok)
    |  'Λ' TypeParams '->' Exp                             (type abstraction, using '/\' for 'Λ' is ok)
    |  'let'    DeclTerm   'in' Exp                        (non-recursive let binding)
    |  'letrec' DeclTerm+; 'in' Exp                        (recursive let bindings)
    |  'do'    '{' Stmt+; '}'                              (do expression)
    |  'case'  '{' AltCase+; '}'                           (case expression)
    |  'match' '{' GuardedExp+; '}'                        (match expression)
    |  'if' Exp 'then' Exp 'else' Exp                      (if-expression)
    |  'weakeff' '[' Type ']' 'in' Exp                     (weaken effect of an expression)
    |  'private' Bind+ WithCaps? 'in' Exp                  (private region introduction)
    |  'extend'  Bind 'using' Bind+ WithCaps? 'in' Exp     (region extension)
    |  'box' Exp                                           (box a computation)
    |  'run' Exp                                           (run a boxed computation)
    |  ExpBase                                             (base expression)

  ExpArg
   ::= '{' Exp  '}'                                        (implicit term argument)
    |  '[' Type ']'                                        (type argument)
    |  ExpBase                                             (base expression)

  ExpBase
   ::= '()'                                                (unit  data constructor)
    |  DaCon                                               (named data constructor)
    |  Literal                                             (literal value)
    |  Builtin                                             (fragment specific builtin value)
    |  Var                                                 (named variable)
    |  '(' InfixOp ')'                                     (reference to infix operator)
    |  '(' Exp ',' Exp+, ')'                               (tuple expression)
    |  '(' Exp ')'                                         (parenthesised expression)

  TermParams
   ::= '(' Pat+ ':' Type ')'                               (explicit parameter)
    |  '{' Pat+ ':' Type '}'                               (implicit parameter)
    |  PatBase+                                            (base pattern)


  WithCaps
   ::= 'with' '{' BindT+ '}'

Patterns and Alternatives
-------------------------

.. code-block:: none

  Pat          ::= DaCon PatBase*                       (data constructor patterm)
                |  PatBase                              (base pattern)

  PatBase      ::= '()'                                 (unit data constructor pattern)
                |  DaCon                                (named data constructor pattern)
                |  Literal                              (literal pattern)
                |  Var                                  (variable pattern)
                |  '_'                                  (wildcard pattern)
                |  '(' Pat ',' Pat+ ')'                 (tuple pattern)
                |  '(' Pat ')'                          (parenthesised pattern)

  AltCase      ::= Pat GuardedExp* '->' Exp             (case alternative)
