
Concrete Syntax
===============


Modules
-------

.. code-block:: none

  Module                                              (source modules)
   ::= 'module' ModuleName
          ExportSpecs* ImportSpecs*;
         'where'  '{' Decl*; '}'

  ExportSpecs                                         (export specifications)
   ::= 'export' '{' Var+; '}'

  ImportSpecs                                         (import specifications)
   ::= 'import' '{' ModuleName;+ '}'                  (module imports)
    |  'import' 'foreign' ImportSpecsForeign          (foreign imports)

  ImportSpecsForeign                                  (foreign import specification)
   ::= 'boxed'    'type'       '{' ConSig;+ '}'       (foreign boxed type import)
    |  'abstract' 'type'       '{' ConSig;+ '}'       (foreign abstract type import)
    |  'abstract' 'capability' '{' VarSig;+ '}'       (foreign abstract capability import)
    |  'c'        'value'      '{' VarSig;+ '}'       (foreign c value import)

  VarSig  ::= 'Var' ':' Type                          (variable type signature)
  ConSig  ::= 'Con' ':' Type                          (constructor type signature)

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

Declarations
------------

.. code-block:: none

  Decl                                                (declaration)
   ::= DeclType | DeclData | DeclValue

  DeclType                                            (type declaration)
   ::= 'type' Con '=' Type                            (type synonym declaration)

  DeclData                                            (data type declaration)
   ::= 'data' Con DeclDataParams*
          ('where' '{' (Con ':' Type)+; '}')?

  DeclDataParams                                      (data type parameters)
   ::= '(' Var+ ':' Type ')'                          (data type parameters with shared kind)

  DeclTerm                                            (term declaration)
   ::= Var ':' Type                                   (type signature)

    |  Var DeclTermParams* (':' Type)?
           GuardedExpsMaybe                           (term declaration using guards)

  DeclTermParams                                      (term declaration parameters)
   ::= PatBase                                        (simple pattern)
    |  '(' PatBase+ ':' Type '}'                      (patterns with shared type annotation)
    |  '{' PatBase+ ':' Type '}'                      (implicit term parameters)
    |  '{' Type '}'                                   (anonymous implicit term parameter)
    |  '{' '@' Var+   ':' Type '}'                    (implicit type parameters)


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


Types
-----

.. code-block:: none

  Type
   ::= TypeFun + Type                                 (type sum)
    |  TypeFun                                        (functional type)

  TypeFun
   ::= TypeApp                                        (type application)
    |  TypeApp '->' TypeFun                           (explicit function type)
    |  TypeApp '~>' TypeFun                           (implicit function type)
    |  '{' Type '}' '->' TypeFun                      (implicit function type, alternate syntax)
    |  '(' '@' Var+ ':' Type ')' '~>' TypeFun         (implicit universal quantification)
    |  '{' '@' Var+ ':' Type '}' '->' TypeFun         (implicit universal quantification, alternate syntax)

  TypeApp                                             (type application)
   ::= TypeApp TypeArg                                (type function applied to an argument)
    |  TypeArg                                        (type argument)

  TypeArg
   ::= Var                                            (type variable)
    |  Con                                            (type constructor)
    |  '(' Type ',' Type+, ')'                        (tuple type)
    |  '(' Type ')'                                   (parenthesised type)

  TypeBuiltin
   ::= 'Data' | 'Region' | 'Effect'                   (builtin kind constructors)
    |  'Pure' | 'Read'   | 'Write' | 'Alloc'          (builtin effect type constructors)
    |  'Unit'                                         (builtin data type constructors)
    |  '(->)'                                         (explicit function type constructor)
    |  '(~>)'                                         (implicit function type constructor)


Type sums are used to collect together multiple effect types.

Function types include both a parameter and return type, using ``->`` as the constructor for explicit function types and ``~>`` as the constructor for implicit function types. Alternately, implicit function types can be written ``{t1} -> t2`` where ``t1`` is the parameter type and ``t2`` is the result type.

Similarly, implicit universal quantification over some type variable ``v`` of kind ``k`` is written ``(@v:t1) ~> t2``, where ``k1`` is the kind of the parameter variable and ``t2`` is the body type. Alternatively, implicit universal quantification can be written ``{@v:t1} -> t2`` using braces to indicate that the type argument will be passed implicitly. The ``@`` in this syntax indicates that variable ``v`` is a type binder rather than a term binder.

Type applications are between a type function and its argument.

Type arguments include variables, constructors, tuple types and parenthesised types. A tuple type like ``(t1, t2, .. tN)`` is sugar for the type application ``TupN t1 t2 .. tN``, where the type constructor ``TupN`` is taken as whatever type constructor is currently in scope with that name.

Builtin type constructors consist of kind constructors, effect type constructors, data type constructors and function type constructors. The same grammar is used for both types and kinds. ``Data`` is the kind of data types, ``Region`` the kind of effect types and ``Effect`` the kind of effect types. ``Pure`` is the effect of pure expressions and the zero element of type sums. ``Read``, ``Write`` and ``Alloc`` are effect type constructors for their associated effects. ``Unit`` is the type of the primitive unit value ``()``. ``(->)`` is the explicit function type constructor and ``(~>)`` the implicit function type constructor.

See the `type specification tests`_ for examples.

.. _`type specification tests`:
        https://github.com/DDCSF/ddc/tree/ddc-0.5.1/test/ddc-spec/source/01-Tetra/01-Syntax/03-Types/Main.ds


Guarded Expressions
-------------------

.. code-block:: none

  GuardedExpsMaybe                                    (maybe guarded expressions)
   ::= '=' Exp                                        (simple unguarded expression)
    |  GuardedExp*                                    (multiple guarded expressions)

  GuardedExp
   ::= '|' Guard,+ '=' Exp                            (guarded expression)

  Guard
   ::= 'otherwise'                                    (otherwise guard always matches)
    |  Pat '<-' Exp                                   (match against pattern)
    |  Exp                                            (boolean predicate)

The bodies of term declarations can be defined either with a single expression or using multiple guarded expressions.

The ``otherwise`` guard always succeeds. The ``Pat '<-' Exp`` guard succeeds when the value produced by ``Exp`` can be matched against ``Pat``. The boolean predicate guard suceeds when the resulting value is ``True``.

See the `guards specification tests`_ for examples.

.. _`guards specification tests`:
        https://github.com/DDCSF/ddc/tree/ddc-0.5.1/test/ddc-spec/source/01-Tetra/01-Syntax/04-Guards/Main.ds


Terms
-----

.. code-block:: none

  Exp
   ::= ExpApp ('where' '{' DeclTerm;+ '}')?           (expression with optional where clause)

  ExpApp                                              (applicative expressions)
   ::= ExpAppPrefix |  ExpAppInfix
    |  ExpAppAbs    |  ExpAppBind
    |  ExpAppMatch  |  ExpAppEffect

  ExpAppPrefix                                        (prefix application)
   ::= ExpBase ExpArg*                                (base expression applied to arguments)

  ExpAppInfix                                         (infix application)
   ::= ExpApp InfixOp ExpApp                          (application of infix operator)
    |  ExpBase

  ExpArg                                              (function argument)
   ::= '{'  Exp  '}'                                  (implicit term argument)
    |  '{' '@' Type '}'                               (implicit type argument)
    |  ExpBase                                        (base expression)

  ExpBase
   ::= '()'                                           (unit  data constructor)
    |  DaCon                                          (named data constructor)
    |  Literal                                        (literal value)
    |  Var                                            (named variable)
    |  '(' InfixOp ')'                                (reference to infix operator)
    |  '(' Exp ',' Exp+, ')'                          (tuple expression)
    |  '(' Exp ')'                                    (parenthesised expression)

Terms include prefix application, infix application, abstractions, binding forms, matching and effectful terms. The later forms are described in the following sections.

Expressions can include nested 'where' bindings, where the local declarations can include type signatures.

Prefix and infix application is standard.

Explicit arguments for implicit term parameters are supplied using ``{}`` parenthesis, and explicit arguments for implicit type parameters with ``{@ }`` parenthesis.

See the `term specification tests`_ for examples.

.. _`term specification tests`:
        https://github.com/DDCSF/ddc/tree/ddc-0.5.1/test/ddc-spec/source/01-Tetra/01-Syntax/05-Term/Main.ds



Abstraction
-----------

.. code-block:: none

  ExpAppAbs
   ::= '\' ExpParam '->' Exp                          (abstraction)

  ExpAbsParam
   ::=  PatBase+                                      (explicit unannotated term parameter}
    |  '(' PatBase+ ':' Type ')'                      (explicit annotated term parameter)
    |  '{' PatBase+ ':' Type '}'                      (implicit annotated term parameter)
    |  '{' '@' Var+   ':' Type '}'                    (implicit annotated type parmaeter)


Abstractions begin with a ``\``, followed by some parameter bindings, then a ``->``. In the concrete syntax the unicode characters ``λ`` and ``→`` can be used in place of ``\`` and ``->``. Term parameter can be bound by patterns with or without type annotations. Explicit term parameters are specified with ``()`` parenthesis and implicit term parameters with ``{}`` parenthesis. Implicit type parameters are specified with ``{@ }`` parenthesis, where the ``{}`` refers to the fact the type arguments will be passed implicitly at the call site, and the ``@`` refers to the name space of type variables.



See the `abstraction specification tests`_ for examples.

.. _`abstraction specification tests`:
        https://github.com/DDCSF/ddc/tree/ddc-0.5.1/test/ddc-spec/source/01-Tetra/01-Syntax/06-Abs/Main.ds


Binding
-------

.. code-block:: none

  ExpAppBind
   ::= 'let'    DeclTerm   'in' Exp                   (non-recursive let binding)
    |  'letrec' DeclTerm+; 'in' Exp                   (recursive let bindings)
    |  'do'    '{' Stmt+; '}'                         (do expression)


Matching
--------

.. code-block:: none

  ExpAppMatch
   ::= 'case'  '{' AltCase+; '}'                      (case expression)
    |  'match' '{' GuardedExp+; '}'                   (match expression)
    |  'if' Exp 'then' Exp 'else' Exp                 (if-expression)

  AltCase
   ::= Pat GuardedExp* '->' Exp                       (case alternative)

  Pat
   ::= DaCon PatBase*                                 (data constructor patterm)
    |  PatBase                                        (base pattern)

  PatBase
   ::= '()'                                           (unit data constructor pattern)
    |  DaCon                                          (named data constructor pattern)
    |  Literal                                        (literal pattern)
    |  Var                                            (variable pattern)
    |  '_'                                            (wildcard pattern)
    |  '(' Pat ',' Pat+ ')'                           (tuple pattern)
    |  '(' Pat ')'                                    (parenthesised pattern)


Regions and Effects
-------------------

.. code-block:: none

  ExpAppEffect
   ::= 'weakeff' '[' Type ']' 'in' Exp                (weaken effect of an expression)

    |  'private' Bind+ WithCaps? 'in' Exp             (private region introduction)

    |  'extend'  Bind 'using' Bind+
                 WithCaps? 'in' Exp                   (region extension)

    |  'box' Exp                                      (box a computation)
    |  'run' Exp                                      (run a boxed computation)

  WithCaps
   ::= 'with' '{' BindT+ '}'



