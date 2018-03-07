
Core Discus Fragment
====================

Core Discus is a fragment of the DDC Core language that represents desugared Source Discus programs. The fragment includes the following primitives:

Primitive Types
---------------

.. code-block:: none

 Type       Kind                      Values                    Notes
 -----      ----                      ------                    -----
 Void#      Data                      (no values)
 Bool#      Data                      true#, false#
 Nat#       Data                      0#, 1#, 2# ...            [1]
 Int#       Data                      ... -1i#, 0i#, 1i# ...    [2]
 Size#      Data                      0s#, 1s#, 2s# ...         [3]
 WordN#     Data                      0w8#, 1056w32#            N ∈ { 8, 16, 32, 64 }
 FloatN#    Data                      3.141f32#                 N ∈ { 32, 64 }
 Addr#      Data                      (no literals)             [4]
 Ptr#       Region -> Data            (no literals)
 TextLit#   Data                      "hello"#

 TupleN#    .. -> Data                (x, y)  (x, y, z)
 Vector#    Region -> Data -> Data    (no literals)

 U#         Data -> Data              Represents an unboxed type during compilation [5].
 F#         Data -> Data              Represents a function closure during compilation [6].

[1] The ``Nat`` type has enough precision to count the maximum number of objects allocatable in the Discus heap, but not necessarally enough precision to count every addressable byte.

[2] The ``Int`` type has enough precision to represent numbers in the range [-N .. +N], where N is the maximum number of objects allocatable in the Discus heap.

[3] The ``Size`` type has enough precision to count every addressable byte in memory.

[4] The ``Addr`` type can hold the address of any addressable byte in memory.

[5] The ``U#`` constructor is used to represent the type of an unboxed value during the Unbox transformation.

[6] The ``F#`` constructor is used to represent the type of a closure during the Curry transformation.


Arithmetic Operators
--------------------

.. code-block:: none

  Name      Type                                Valid  Description
  ----      -----                               -----  -----------
  and#      Bool# -> Bool# -> Bool#             boolean and
  or#       Bool# -> Bool# -> Bool#             boolean or

  shl#      a -> a -> a                    [I]  bitwise shift left
  shr#      a -> a -> a                    [I]  bitwise shift right
  band#     a -> a -> a                    [I]  bitwise boolean and
  bor#      a -> a -> a                    [I]  bitwise boolean or
  bxor#     a -> a -> a                    [I]  bitwise boolean exclusive or

  neg#      a -> a                      [I, F]  negation
  add#      a -> a -> a                 [I, F]  addition
  sub#      a -> a -> a                 [I, F]  subtraction
  mul#      a -> a -> a                 [I, F]  multiplication
  div#      a -> a -> a                 [I, F]  division
  mod#      a -> a -> a                 [I, F]  modulus
  rem#      a -> a -> a                 [I, F]  remainder

  eq#       a -> a -> Bool#             [I, F]  equality
  neq#      a -> a -> Bool#             [I, F]  negated equality
  gt#       a -> a -> Bool#             [I, F]  greater than
  ge#       a -> a -> Bool#             [I, F]  greater than or equal
  lt#       a -> a -> Bool#             [I, F]  less than
  le#       a -> a -> Bool#             [I, F]  less than or equal

Although the arithmetic operators are given indicative polymorphic types for convenience, the object code generator only supports subset of possible instantiations. The 'Valid' column in the above table lists whether the operator works on both [I]ntegral and [F]loating point types, or just [I]ntegral. The sets of integral and floating point types are:

.. code-block:: none

 [I] Integral = { Bool#, Nat#, Int#, Size#, WordN#, Addr#, Ptr# a }
 [F] Floating = { FloatN# }


Cast Operators
--------------

.. code-block:: none

 Name       Type                     Description
 ----       ----                     -----------
 convert#   {@a b: Data} -> b -> a   Convert  value to a type of the same precision.
 promote#   {@a b: Data} -> b -> a   Promote  value to a type of the same or greater precision.
 truncate#  {@a b: Data} -> b -> a   Truncate value to a type of the same or lower precison.

The cast operators convert numeric values between types. As with the arithmetic operators, although the conversion operators are given polymorphic types the object code generator only supports a subset of possible instantiations.

The cast operators can be used to convert unsigned to signed values, integral to floating point values, address to word values and so on. The available instantiations are platform dependent, for example Addr# can be converted to a Word32# on a 32-bit system, but not on a 64-bit system.

Note that the order of forall quantifiers in the types of these primitive is opposite relative to the order in which the type variables appear in the body of the type. We do this so that it's easier to specify the desired result type. For example, one can write ``convert# [Word32#] thing`` to indicate that a result of type ``Word32#`` is desired, and the second type argument will be inferred based on the type of ``thing``.


Vector Operators
----------------

.. code-block:: none

 Name            Type / Description
 ----            ------------------
 vectorAlloc#    Nat# -> S (Alloc r) (Vector# r a)
                 Allocate a vector of primitive values, initializing them all to 0.

 vectorLength#   Vector# r a -> Nat#
                 Allocate a vector of primitive values, initializing them all to 0.

 vectorRead#     Vector# r a -> Nat# -> S (Read r) a
                 Read the value at the given index from a vector.
                 Attempting to access an out-of-bounds index will cause a runtime exception.

 vectorWrite#    Vector# r a -> Nat# -> a -> S (Write r) Unit
                 Write to the value at the given index of a vector.
                 Attempting to access an out-of-bounds index will cause a runtime exception.

At runtime, vectors are represented as flat arrays of unboxed values. Vector values cannot be polymorphic in the element type. Every use of a vector operator must instantiate the element type variable ``a`` to a primitive numeric type.


Error Operators
---------------

.. code-block:: none

 Name            Type / Description
 ----            ------------------
 default#        TextLit# -> Nat# -> a
                 Abort the program, signalling that there was an inexhaustive case match
                 at the given source file and line number.

The ``default#`` operator is inserted when desugaring Source Discus down to Core Discus.


Closure Operators
-----------------

.. code-block:: none

 Name            Type/Description
 ----            ----------------
 reify#          {@a b: Data} -> (a -> b) -> F# (a -> b)
                 Reify a function value into an explicit closure value.

 curryN#         {@aN .. a1 a0: Data} -> F# (aN .. -> a1 -> a0) -> aN .. -> a1 -> a0
                 Apply N more arguments to the given function closure.

 apply#          {@aN .. a1 a0: Data} -> (aN .. -> a1 -> a0) -> aN .. -> a1 -> a0
                 Apply N more arguments to the given function.

The closure operators are inserted by the Curry transformation when converting Core Discus code to Core Salt. They are not normally inserted into client programs.

