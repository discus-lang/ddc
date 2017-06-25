
Definition
==========

Syntax
------

.. code-block:: none

               (Abstract Syntax)     (Concrete Syntax)   (Description)

  Name (n) ::= (printable names)
  Nat      ::= 0 | 1 | 2 | ...

  Decl (d) ::= decl   Name Exp       r = e;              Declaration

  Exp  (e) ::= var    Name Nat       x^n                 Variable
            |  abs    Name Exp       \x. e               Abstraction
            |  app    Exp  Exp       e e                 Application
            |  ref    Ref            r                   Reference
            |  ...                   (e)                 Parenthesis

  Ref  (r) ::= macro  Name           @name               Macro Reference
            |  symbol Name           %symbol             Symbol Reference
            |  prim   Name           #prim               Primitive Reference
            |  set    Name           +name               Set Reference
            |  meta   Nat            ?123                Meta Reference

A Name is a printable name such as ``Hello``, ``list-foldl`` or ``nat-59``. Regular names must start with a letter and can contain letters, numbers, and the dash ``-`` character. Arbitrary character strings can be used as names by enclosing them in double quotes, like ``"what evar"``.

A Nat is a natural number, which is used when defining the names of variables below.

A Decl is a top-level declaration that associates a reference with an expression. In the concrete syntax the reference and expression are separated by an equals sign ``=``, and the declaration is terminated by a semicolon ``;``. Macro and Set references can be declared in this way, but not the others.

An Exp is an expression, which can be a variable, function abstraction, function application, reference. In the concrete syntax we also include parenthesized expressions in the usual way. With variables, the Nat is called the *bump counter* and used to refer to refer to a name shadowed by another, so in ``(\x.\x.\x.x^2)`` the inner variable refers to the first parameter. If the bump counter it is zero it can be omitted. In an abstraction, the scope of the parameter variable extends to as far right as possible. Function application associates to the left, as it always has.

A Ref is a reference, which can be a macro-reference, set-reference, symbol-reference, primitive-reference or meta-reference.

Macro references are silently unfolded during evaluation, but the expectation is that the unfolding will be done in a lazy way so that the resulting term will not become too large. The body of a macro can reference itself, which can be used to write recursive macro programs.

Symbol references refer to uninterpreted names. These are like data constructors in other languages, except that they need not be defined beforehand.

Primitive references refer to implementation dependent primitive values and operators. The computers we have on our desks implement many wonderful operations besides function application. Floating point values and operations are referred to using these primitive names. Shimmer implementations may also include various evaluation methods and program transformations which can be given primitive names as well. 

Set references are magic to collect the names of things defined across multiple Shimmer files. When you load two Shimmer files that declare a set reference of the same name then the two bound things are combined into a list, so that you only have one thing overall.

Meta references are magic to work with fresh names. The generation of a fresh name does not break confluence of evaluation, provided the order in which the names were generated cannot be observed. A meta reference refers to a freshly generated name, but the particular way the reference is presented depends on its position in the program rather than the name itself. Meta references hide your impurity from you, so that you cannot know how deeply you have sinned.


Sugar
-----

The following standard syntactic sugars are provided, to aid those that must write Shimmer code by hand.

.. code-block:: none
  
  r x1 x2 .. xn = e   ≡   r = \x1. \x2. .. \xn = e     (function binding syntax)