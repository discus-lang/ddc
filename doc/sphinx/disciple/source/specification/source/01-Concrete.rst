
Concrete Syntax
===============


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
    |  'let'    Clause   'in' Exp                          (non-recursive let binding)
    |  'letrec' Clause+; 'in' Exp                          (recursive let bindings)
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

  TypeParams
   ::= '(' Bind+ ':' Type ')'                              (type parameters of same kind)
    |  Bind                                                (type parameter)

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
                |  '(' Pat, Pat+ ')'                    (tuple pattern)
                |  '(' Pat ')'                          (parenthesised pattern)

  AltCase      ::= Pat GuardedExp* '->' Exp             (case alternative)
