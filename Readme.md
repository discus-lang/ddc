# The Disciplined Disciple Compiler [![Build Status](https://travis-ci.org/DDCSF/ddc.svg?branch=master)](https://travis-ci.org/DDCSF/ddc)

The Disciple language is an experimental dialect of Haskell which investigates
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
        [Demos](https://github.com/DDCSF/ddc/tree/master/test/ddc-demo/source/tetra),
        [List Library](https://github.com/DDCSF/ddc/blob/master/src/s2/base/Data/List.ds),
        and [Text Implementation](https://github.com/DDCSF/ddc/blob/master/src/s2/base/Data/Text/Base.ds)
        in the source tree.

The Demos are simple programs. The List Library demonstrates effect polymorphism, and the Text
implementation demonstrates use of private regions and capabilities.


## Let Me Hack

See the [Getting Started](http://disciple.ouroborus.net/section/01-GettingStarted.html) guide
on the home page.


## More Information

The GitHub site:        http://github.com/DDCSF/ddc

Home Page:              http://disciple.ouroborus.net

Development Wiki:       http://trac.ouroborus.net/ddc

Development Blog:       http://disciple-devel.blogspot.com.au/

Mailing List:           http://groups.google.com/group/disciple-cafe



