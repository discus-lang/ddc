
Core Salt Fragment
==================

Core Salt is a fragment of the DDC Core language that is used for implementing the DDC runtime system. "Salt" is what we got when we left "C" out in the sun for too long.

The Core Salt language is based on the general DDC core language, but with the folowing restrictions:

 * All functions are first order and must be defined at top-level.
 * All function applications must be saturated (no partial application).
 * Effect type annotations are not supported, though region type annotations are.
 * All values have primitive type, so there are no algebraic data type declarations.

Restricting the languge in this way means that Core Salt programs can be compiled directly to machine code without the need for an underling runtime system. The runtime system for higher level languages such as Core Discus is then implemented in Core Salt. We base Core Salt on the general DDC core language so that the implementation can share the same parser and type checker as the Core Discus language.


Primitive Types
---------------

.. code-block:: none

 Type       Kind            Values                    Notes
 -----      ----            ------                    -----
 Void#      Data            (no values)
 Bool#      Data            true#, false#
 Nat#       Data            0#, 1#, 2# ...            [1]
 Int#       Data            ... -1i#, 0i#, 1i# ...    [2]
 Size#      Data            0s#, 1s#, 2s# ...         [3]
 WordN#     Data            0w8#, 1056w32#            N ∈ { 8, 16, 32, 64 }
 FloatN#    Data            3.141f32#                 N ∈ { 32, 64 }
 Addr#      Data            (no literals)             [4]
 Ptr#       Region -> Data  (no literals)
 TextLit#   Data            "hello"#


[1] The ``Nat`` type has enough precision to count the maximum number of objects allocatable in the Discus heap, but not necessarally enough precision to count every addressable byte.

[2] The ``Int`` type has enough precision to represent numbers in the range [-N .. +N], where N is the maximum number of objects allocatable in the Discus heap.

[3] The ``Size`` type has enough precision to count every addressable byte in memory.

[4] The ``Addr`` type can hold the address of any addressable byte in memory.


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

 Name       Type                  Description
 ----       ----                  -----------
 convert#   [a b: Data]. b -> a   Convert  value to a type of the same precision.
 promote#   [a b: Data]. b -> a   Promote  value to a type of the same or greater precision.
 truncate#  [a b: Data]. b -> a   Truncate value to a type of the same or lower precison.

The cast operators convert numeric values between types. As with the arithmetic operators, although the conversion operators are given polymorphic types the object code generator only supports a subset of possible instantiations.

The cast operators can be used to convert unsigned to signed values, integral to floating point values, address to word values and so on. The available instantiations are platform dependent, for example Addr# can be converted to a Word32# on a 32-bit system, but not on a 64-bit system.

Note that the order of forall quantifiers in the types of these primitive is opposite relative to the order in which the type variables appear in the body of the type. We do this so that it's easier to specify the desired result type. For example, one can write ``convert# [Word32#] thing`` to indicate that a result of type ``Word32#`` is desired, and the second type argument will be inferred based on the type of ``thing``.


Store Types
-----------

.. code-block:: none

 Name       Kind     Description
 ----       ----     -----------
 Obj        Data     Abstract heap object
 rT         Region   Top level region

The ``Obj`` type is used as the index for pointers that point to object on the heap. Values cannot have type ``Obj`` directly, though may have type ``Ptr r Obj`` for some region type ``r``.

The ``rT`` region is the top level region that holds static data such as text literals, as well as slots on the GC shadow stack. It is defined implicitly at the top level of a Salt program.


Store Operators
---------------

Store Size Operators
~~~~~~~~~~~~~~~~~~~~

.. code-block:: none

 Name           Type/Description
 ----           ----------------
 size#          [a: Data]. Nat#
                Yield the size of a value of primitive type 'a', in bytes.

 size2#         [a: Data]. Nat#
                Yield the log-2 of the size of a value of primitive type 'a', in bytes.

The ``size2#`` operator is useful to compute sizes of buffers. A buffer of ``n`` values of type ``a`` has size ``(shl# n (size2# [a])``.


Store Address Operators
~~~~~~~~~~~~~~~~~~~~~~~


.. code-block:: none

 plusAddr#      Addr# -> Nat# -> Addr#
                Add an offset in bytes to an address.

 minusAddr#     Addr# -> Nat# -> Addr#
                Subtract an offset in bytes from an address.

 read#          Addr# -> Nat# -> a
                Read a value from the given address plus offset.

 write#         Addr# -> Nat# -> a -> Void#
                Write a value to the given address plus offset.

 copy#          Addr# -> Addr# -> Nat# -> Void#
                (dest)   (src)    (bytes)
                Copy the given number of bytes from a source to destination address.

 set#           Addr# -> Word8# -> Nat# -> Void#
                (start)  (value)   (bytes)
                Set the given number of bytes starting from an address to the specified value.


A value of type ``Addr#`` is a raw address in the native word size of the machine. A given address may be of any value, including addresses that point outside memory owned by the process.


Store Pointer Operators
~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: none

 plusPtr#       Ptr# r a -> Nat# -> Ptr# r a
                Add the given number of bytes to a pointer.

 minusPtr#      Ptr# r a -> Nat# -> Ptr# r a
                Subtract the given number of bytes from a pointer.

 makePtr#       Addr# -> Ptr# r a
                Make a pointer from a raw address.

 takePtr#       Ptr# r a -> Addr#
                Take a raw address from a pointer.

 castPtr#       Ptr# r a -> Ptr# r b
                Cast a pointer from one type to another.

 peek#          Ptr# r a -> a
                Read the value pointed to by a pointer.

 poke#          Ptr# r a -> a -> Void#
                Write to the value pointer to by a pointer.

 peekBounded#   Ptr# r a -> Nat# ->   Nat# -> a
                (pointer)   (offset) (length)
                Read a value from an offset,
                checking the offset is less than the given buffer length.
                Terminate the program if the check fails.

 pokeBounded#   Ptr# r a -> Nat# ->   Nat# -> a -> Void#
                (pointer)   (offset) (max)
                Write a value to a pointer plus offset,
                checking that the offset is less than the given buffer length.
                Terminate the program if the check fails.

Values of type ``(Ptr# r a)`` for some ``a`` are intended to point to values within memory owned by the process, though this is not enforced by the language or object code generator.

The pointer type is tagged with a region type variable to support type based anti-aliasing analysis.

The ``peekBounded#`` and ``pokeBounded#`` operators are intended for the implementation of safe array and vector primitives. The object code generator can produce machine instructions that implement the bounds check in a way that is specialized to the target platform.


Global Store Primitives
~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: none

 global#        [a: Data]. TextLit# -> Addr#
                Refer to a global variable of the given name and type.

The use of an expression such as ``(global# [Nat#] "foo"#)`` in the program causes a global variable with name ``foo`` to be defined that can hold values of type ``Nat#``. The global variable has external linkage and is thus visible to all modules in the program.


Garbage Collector Support Primitives
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: none

 check#         Nat# -> Bool#
                Check whether there are at least the given number of free bytes
                left in the heap.

 alloc#         Nat# -> Addr#
                Allocate the given number of bytes on the heap and return
                an address to the start of the buffer.

 allocSlot#     [r: Region]. Ptr# rT (Ptr# r Obj)
                Allocate a slot on the GC shadow stack to hold a pointer to a heap object.

 allocSlotVal#  [r: Region]. Ptr# r Obj -> Ptr# rT (Ptr# r Obj)
                Like allocSlot#, but initialize the slot with the given pointer.

Garbage collector support primitives provide hooks on the garbage collector implementation, are implementation specific, and are subject to change in later versions of the Salt language.


Control Operators
-----------------

.. code-block:: none

 Name           Type/Description
 ----           ----------------
 fail#          [a: Data]. a
                Terminate the program, ungracefully.

 callN#         [rN .. r1 r0: Region]. Addr# -> Ptr rN Obj -> .. Ptr r1 Obj -> Ptr r0 Obj
                Call the function at the given code address, passing pointers to heap objects.

 tailcallN#     [aN .. a1 a0: Data]. (aN -> .. a1 -> a0) -> aN -> .. a1 -> a0
                Tail-call the given function, passing the given values.

 return#        [a: Data]. a -> a
                Indicate the value should be returned from the function.
                This is an internal primop and is not needed in client code.

The ``fail#`` operator is used to signal unrecoverable errors, such as the runtime system detecting that the heap is corrupted.

The ``callN`` operator is used to call functions at aribrary instruction code address. The address of a top-level function can be obtained with the convert# operator, eg ``convert# [Addr] someFunction``.

The ``tailcallN`` operator is used to perform a tail call, and must appear in tail call position of the enclosing function.

The ``return#`` operator is inserted automatically by the compiler during compilation. The operator may appear dumps of intermediate code, but does not need to be added by the client programmer.

