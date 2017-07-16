
DDC Error Message Tests
=======================

The tests in this directory systematically cause each reportable type error message to be reported. We want to check that information such as the source locations is not lost during development. The possible error messages are defined in DDC.Core.Check.Error.ErrorExp.

The tests here do not try to check for every possible problem that can *result* in an error message being reported, as a specific message can be generated at multiple points in the type checker implementation.


