.. Shimmer documentation master file, created by
   sphinx-quickstart on Mon May 15 21:55:01 2017.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Shimmer
=======

Shimmer, or the Simple Meta Representation (SMR), is a generic way of representing the abstract syntax trees typically used as intermediate representations (IRs) in a compiler or other language processor. The Shimmer tool chain provides ways to evaluate, transform, and otherwise *munge* expressions represented in this form.

Shimmer is not itself a real programming language because it does not specify an evaluation method. Shimmer is neither specifically call-by-name, call-by-value or any other. However, particular implementations may provide evaluation methods, and in those cases Spartan, Scheme-like programs can be written in Shimmer form. Due to the intentionally verbose concrete syntax this is recommended for language developers rather than end users. The intent is that some nicer front-end language will be desugared into Shimmer code and then transformed, compiled or interpreted from there. 

For those that can take a joke, Shimmer is the Turtle at the Bottom of the World. An SK combinator representation would be a Lower Turtle, but due to the lack of human readable names and exponential size of SK encodings the SK-Turtle is of arguably greater academic interest but lesser practical interest. We will say no more about Turtles. You can still write SK combinator expressions in Shimmer if you want to.

Here is a small Shimmer file to give a taste of the syntax.

.. code-block:: none

 @list-case xx alt-nil alt-cons
  = #match xx  %list-nil         alt-nil
   (#match xx (%list-cons #o #o) alt-cons
    %fail);

 @list-range a b
  = #if (#nat-gt a b)
        %list-nil
        (%list-cons a (@list-range (#nat-add a #nat-1) b));

 @list-foldl f z xx
  = @list-case xx z (\x. \xs. @list-foldl f (f z x) xs);

:download:`Download<section/examples/00-Intro.smr>`

Names like ``@list-foldl`` refer to macros defined at top level. Names like ``%list-cons`` refer to uninterpreted symbols. Names like ``#nat-add`` refer to primitive operators. 

We can load up the file into the Shimmer interpreter and use it to compute the factorial of a number:


.. code-block:: none

 $ smr 00-Intro.smr
 Shimmer, version 1.0. The Turtle at the Bottom of the World.
 > @list-foldl #nat-mul #nat-1 (@list-range #nat-1 #nat-10)
 #nat-3628800

The default evaluation method for this implementation is normal order reduction, though others are available. Instead of using primitive natural numbers we could have built them up out of functions using Church encoding, but then the example would have taken longer to execute. I suppose we lied a bit about it not being a programming language. 


.. toctree::
   :maxdepth: 1
   :caption: More Details:

   section/01-definition.rst
   section/02-primitives.rst
   section/03-examples.rst




Indices and tables
==================

* :ref:`genindex`
* :ref:`search`
