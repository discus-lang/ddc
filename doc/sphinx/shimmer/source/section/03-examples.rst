

Examples
========

Lambda Terms
------------

On starting up the Shimmer interpreter we can type expressions at the prompt without needing to write any declarations.

.. code-block:: none

  $ smr
  Shimmer, version 1.0. The Turtle at the Bottom of the World.
  > (\x. x x) %turtle
  %turtle %turtle

Here we have used a lambda abstraction to create two turtles out of one. We can use another to construct four turtles in total.

.. code-block:: none

  > (\f. \x. f (f x)) (\x. x x) %turtle
  %turtle %turtle (%turtle %turtle)

In both cases the ``%turtle`` is an uninterpreted symbol.


No Name Capture
---------------

Name capture during reduction is avoided by using the bump counter,
as in this classic example:

.. code-block:: none

  > (\y. (\x. \y. x) y)
  \y.\y.y^1


Here the variable reference ``y^1`` refers to the inner-most binder of name `y`, skipping the first one. Likewise, with:

.. code-block:: none

  > (\y. (\x. \y. \y. x) y)
  \y.\y.y^2


The variable reference ``y^2`` now refers to the inner-most binder of name `y`, skipping the first two.


Church Encoding
---------------

The following program builds up enough Church encoded functional goodness to almost do something useful:   


.. code-block:: none

 @true  x y     = x;
 @false x y     = y;
 @if    a b c   = a b c;

 @zero          = \s. \z. z;
 @succ  n       = \s. \z. s (n s z);
 @one           = @succ @zero;
 @two           = @succ @one;
 @three         = @succ @two;
 @four          = @succ @three;

 @is-zero n     = n (\x. @false) @true;

 @pair      m n = \b. @if b m n;
 @pair-fst  p   = p @true;
 @pair-snd  p   = p @false;
 @pair-zero     = @pair @zero @zero;
 @pair-succ p   = @pair (@pair-snd p) (@succ (@pair-snd p));

 @pred  n       = @pair-fst (n @pair-succ @pair-zero);
 @add   m n     = \s. \z. m s (n s z);
 @sub   m n     = n @pred m;
 @mul   m n     = \z. n (m z);

 @fac n
  = @if  (@is-zero n)
          @one 
         (@mul n (@fac (@sub n @one)));

:download:`Download<examples/03-ChurchEncoding.smr>`

Loading this file into the Shimmer interpreter allows us to evaluate the expressions. The default evaluation method is normal order reduction, which completely normalizes the expression, including reducing inside the bodies of lambda abstractions:

.. code-block:: none

 $ smr -i 03-ChurchEncoding.smr
 Shimmer, version 1.0. The Turtle at the Bottom of the World.
 > @fac @three
 \z.\z.z^1 (z^1 (z^1 (z^1 (z^1 (z^1 z)))))

The result is the Church Encoded natural number 6 = 1 × 2 × 3.


Primitive Natural Numbers
-------------------------

Here is another version of the factorial program, but this time using primitive boolean values and natural numbers. In data representation and operators are those provided by the Shimmer implementation rather than being coded up from lambda expressions.

.. code-block:: none

  @nat-fac n
   = #if  (#nat-eq #nat-0 n)
           #nat-1
          (#nat-mul n (@nat-fac (#nat-sub n #nat-1)));

We can invoke this declaration as follows:

.. code-block:: none

  > @nat-fac #nat-3
  #nat-6

  > @nat-fac #nat-10
  #nat-3628800
