
Why Discus?
===========

Building a new compiler for a new language is a lot of work. The first question people have about a such a project is simply "Why Bother?".

The Discus project exists for the following reasons:

* To experiment with :ref:`why-discus-modal-effect-typing`.

  Finding good ways to deal with side effects is a long standing problem in functional programming, and programming language theory in general. The Discus type system can reason about effectful programs in ways that other functional languages cannot.

* To experiment with :ref:`why-discus-region-typing`.

  A region is the name for a place in the program memory that might be different from other places. In most functional languages all objects allocated at runtime end up in a single place called *the heap*, and once added to the heap it's difficult to distinguish the objects from each other. Region typing allows several different places in the heap to be defined, and objects assigned to one of those places. Region typing combined with effect typing allows the system to reason about effects on objects that are entirely local to a particular function, and not visible to callers. For example, the Discus versions of the Haskell 'unsafePerformIO' and 'unsafeFreeze' functions are guaranteed to be safe, and this safety is checked automatically using the region and effect typing.

* To have a generic implementation of a :ref:`why-discus-system-f-core-language`.

  Compilers for modern functional languages are usually based around a core intermediate representation based on an extended lambda calculus called System-F. Such intermediate representations are thus platform for research into the compilation of functional programming languages. In particular, program transformations that provide functional array fusion, as well as functional stream fusion often work on System-F, and Discus provides a common infrastructure for producing and consuming System-F. The core language of the Glasgow Haskell Compiler (GHC) is also based on System-F, but there is currently no external format for this language, so any experiments must be done via GHC plugins rather on external files that can be directly written and modified.

* To experiment with :ref:`why-discus-runtime-techniques`.

  The DDC runtime system is only a few thousand lines of code, includes five simple object representations and currently has a simple two space garbage collector. The simplicity of this setup makes it easy to experiment with novel runtime techniques, such as reification and reflection, generic hashing, and new memory management techniques. These sort of projects can also be done with larger and more well optimised systems such as the GHC runtime, O'Caml or the JVM, but building from a simpler (and cleaner) base system makes it easier to get started.

* To :ref:`why-discus-reinvent-the-wheel`.

  A common criticism of language projects is that "we already have one of those". This is invariably true for some aspects of a project, but not true for others. DDC has some new ideas, but naturally reimplements many existing ideas as well. People reinvent wheels to learn about how wheels work. It's the best way.


.. _why-discus-modal-effect-typing:

Modal Effect Typing
-------------------

Functional programming is the way of the future, yet the lambda calculus on which all functional languages are based does not naturally have good support for effects. Lambda calculus reasons about functions and values, and has several nice properties such as *determinism* -- meaning that evaluating the same program multiple times always produces the same value, and *confluence* -- meaning that the value produced is insensitive to the order that sub computations are performed. However, simple, common, and useful operations such as reading characters from a keyboard, or printing them to the screen are neither deterministic nor confluent. This has been a headache for programming languages designers since forever. The current favoured solutions include 1) ignoring the problem; 2) doing a partial analysis; 3) monads.

1. If a compiler ignores the effects that a program performs then it is difficult for it to optimise the program. Many powerful optimisations change the order of operations, but if two interferring effectful operations have their order changed then the meaning of the program may also change. If the compiler cannot reason about effects then it cannot optimise programs well. For a typed language, it also means that the type information does not reflect the fact that code may do something besides produce a value.

2. If a compiler does a partial analysis to understand what effects a program has, then some effect dependent optimisations can still be performed, but it becomes difficult for the programmer to reason about how efficient their program will be. For peformance sensitive applications a programmer will often resort to writing simpler, more direct code, maybe in a different language, as they cannot understand the details of the analysis.

3. Monads as used in Haskell and other languages are a way to represent effect information as the type of a value. Lambda calculus is good at types and values. We can write types like ``hReadLine : Handle -> IO Text`` where the ``(IO Text)`` part is the type of a suspended computation that will perform an IO action when we run it. Multiple IO actions can be chained together with a ``bind`` operator or ``do`` notation as in Haskell. This approach works well for simple programs, but also suffers from problems. The usual culprits are a) sub programs that use different monads are hard to compose and b) the fact that different monads are hard to compose usually results in most operations being being assigned to a single fat monad type, like IO, rather than more fine-grained types like File, Console, Network and so on.

Modal effect typing is a way to express information about effects in the type system, but in a way that makes the common cases easier to use than with monads.

In Discus the ``hReadLine`` and ``writeLine`` functions can be assigned the types:

.. code-block:: none

  hReadLine : Handle -> S File Text
  writeLine : Text -> S Console Unit

The ``S`` type constructor is short for "Suspended computation", and has a similar role to the ``IO`` type constructor in Haskell. Unlike the ``IO`` constructor, ``S`` is *indexed* by a fine grained effect type which describes what happens when the computation is executed. When we compose ``hReadLine`` and ``writeLine`` in a larger program the type system automatically combines the effect information:

.. code-block:: none

  readThenPrint : Handle -> S (File + Console) Unit
  readThenPrint handle = writeLine (hReadLine handle)

Note that there is no need to use ``do`` notation or a bind operator to manually sequence the operations, such as the usual Haskell style program:

.. code-block:: none

  readThenPrintHaskell :: Handle -> IO Unit
  readThenPrintHaskell handle
    = do line <- readLine handle
         writeLine line

The Discus type system manages the *plumbing* of effect information automatically. Discus reasons about effect types directly, rather than only about value types. Compared to Haskell this difference is particularly noticeable when using control structures such as pattern alternatives that have no monadic form. For example, in Discus we can write:

.. code-block:: none

  readThenDo : State -> Handle -> S (File + Console) Unit
  readThenDo state handle
   | expectRed state
   , Red <- colorOfText $ readLine handle
   = writeLine "ok"

   | otherwise
   = writeLine "failed"


In Haskell it is not possible to perform the effect of ``readLine`` in a guard, because guards only match against values and not effectful computations. A Haskell programmer is forced to express the continuation to the second alternative manually, like so:

.. code-block:: none

  readThenDo :: State -> Handle -> IO ()
  readThenDo state handle
   = if expectRed state
        then do tx <- readLine handle
                case colorOfText tx of
                 Red    -> writeLine "ok"
                 _      -> goFailed
        else goFailed

   where
        goFailed = writeLine "failed"


.. _why-discus-region-typing:

Region Typing and Region Extension
----------------------------------



.. _why-discus-runtime-techniques:

Runtime Techniques
------------------


.. _why-discus-system-f-core-language:

System-F Core Language
----------------------


.. _why-discus-reinvent-the-wheel:

Reinvent the Wheel
------------------

It's like wheels, but better.


