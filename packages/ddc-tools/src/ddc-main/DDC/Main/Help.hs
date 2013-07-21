
module DDC.Main.Help where

-- | The version identifier string.
version :: String
version = "The Disciplined Disciple Compiler, version 0.3.1"


-- | What to print when we have no input files.
hello :: String
hello   = unlines
        [ "ddc: no input files"
        , "For usage information, try the -help option." ]


-- | The command-line help page.
---
--   The following internal debugging flags are also accepted but do not appear
--   in the help page.
--
--     -taint-avoid-type-checks    Avoid type checking if possible.
--                                 Exactly how much checking is avoided depends
--                                 on the compilation mode. 
--  
help :: String
help    = unlines
        [ version
        , ""
        , "General:"
        , "     -help              Display this help."
        , "     -version           Display the version string."
        , ""
        , "Compilation:"
        , "     -make       FILE   Compile a module into an executable file."
        , " -c, -compile    FILE   Compile a module into an object file."
        , ""
        , " -o, -output     FILE   Redirect output to this file."
        , "     -output-dir DIR    Redirect output to this directory."
        , ""
        , "     -fvia-llvm         Compile via the LLVM backend  (default)"
        , "     -fvia-c            Compile via the C backend."
        , ""
        , "     -keep-ll-files     Keep intermediate .ll files."
        , "     -keep-c-files      Keep intermediate .c files."
        , "     -keep-s-files      Keep intermediate .s files."
        , ""
        , "Optimisation:"
        , "     -O0                No optimisations.             (default)"
        , " -O, -O1                Do standard optimisations."
        , ""
        , "Runtime for compiled program:"
        , "     -run-heap   BYTES  Size of fixed heap            (65536)"
        , ""
        , "Parsing and Checking:"
        , "     -parse      FILE   Parse a module."
        , "     -check      FILE   Parse and type check a module."
        , ""
        , "Conversion:"
        , "     -to-salt    FILE   Convert a module to Disciple Core Salt."
        , "     -to-c       FILE   Convert a module to C code."
        , "     -to-llvm    FILE   Convert a module to LLVM code."
        , ""
        , "Data Flow Fusion Transforms:"
        , "  These work on modules in the Disciple Core Flow (.dcf) fragment."
        , "     -flow-prep  FILE   Prepare a module for lowering."
        , "     -flow-lower FILE   Lower functions to loop code."
        , "     -flow-wind  FILE   Wind loop primops into tail recursive loops."
        , "     -flow-concretize FILE  Rewrite primops to use concrete series lengths."
        , ""
        , "Debugging:"
        , "     -dump              Dump intermediate representations."
        , "     -ast      FILE     Pretty print the AST of a module."
        , ""
        , "Configuration:"
        , "     -basedir  DIR      Path to the runtime and base library code."
        , "     -print-basedir     Print directory holding the runtime and base libraries."
        , "     -print-builder     Print external builder info for this platform."
        , ""
        , "Transformation:"
        , "     -load     FILE     Parse, type check and transform a module."
        , "     -trans    TRANS    Set the transformation to use with -load."
        , "     -with     FILE     Use this module for inliner templates with -load."
        , ""
        , "    TRANS   ::= "
        , "     { TRANS }          Parenthesis."
        , "     fix N TRANS        Transform to a fixpoint, or bail out after N iterations."
        , "     TRANS; TRANS       Sequence two transforms."
        , "     Id                 Return the original program unharmed."
        , "     Anonymize          Anonymize names to deBruijn form."
        , "     Namify             Introduce fresh names for deBruijn binders."
        , "     Snip               Introduce let-bindings for nested applications."
        , "     SnipOver            ... and over-applications."
        , "     Flatten            Flatten nested let-bindings."
        , "     Beta               Perform beta-reduction for value arguments."
        , "     BetaLets            ... introducing new let-bindings for redex arguments."
        , "     Bubble             Float casts outwards, and combine similar ones."
        , "     Prune              Erase unused, ineffectual let-bindings."
        , "     Forward            Float let-bindings forward into their use sites."
        , "     Rewrite            Perform rule based rewriting."
        , "     Elaborate          Introduce default witnesses for unconstrained regions."
        , "     Inline MODULE[NAME..] Perform inlining. Use '-with' to add source modules." ]

