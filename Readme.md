# The Disco Discus Compiler [![Build Status](https://travis-ci.org/discus-lang/ddc.svg?branch=master)](https://travis-ci.org/discus-lang/ddc)

The Discus language is an experimental dialect of Haskell which investigates
static typing and program transformation in the presence of computational effects.
The compiler (DDC) is currently "working alpha" quality, meaning there is a
complete system that can be hacked around with, but it’s not yet industrial strength.


## Features

* Haskell-like source language, so Haskell-like programs should work with minor modifications.

* Modal region and effect system using ‘box’ and ‘run’ to suspend and force computations.

* Higher rank polymorphism with bidirectional type inference.

* Simple two space copying garbage collection.

* Default call-by-value evaluation.

* Typed external core language.


## Show Me Code

Check out the
[Demos](https://github.com/discus-lang/ddc/tree/master/test/ddc-demo/source/tetra),
[List Library](https://github.com/discus-lang/ddc/blob/master/src/s2/base/Data/List.ds), and
[Text Implementation](https://github.com/discus-lang/ddc/blob/master/src/s2/base/Data/Text/Base.ds)
in the source tree. The Demos are simple programs. The List Library demonstrates effect polymorphism, and the Text
implementation demonstrates use of private regions and capabilities.

Larger programs like a [Ray Tracer](http://disciple-devel.blogspot.com.au/2017/07/ray-tracer-demo.html)
are described on the [blog](http://disciple-devel.blogspot.com.au/).


## Let Me Hack

See the [Getting Started](http://discus-lang.org/section/01-GettingStarted.html) guide
on the home page.


## More Information

Home Page:              http://discus-lang.org

The GitHub site:        http://github.com/discus-lang/ddc

Development Wiki:       http://trac.discus-lang.org

Development Blog:       http://blog.discus-lang.org

Mailing List:           http://groups.google.com/group/disciple-cafe
