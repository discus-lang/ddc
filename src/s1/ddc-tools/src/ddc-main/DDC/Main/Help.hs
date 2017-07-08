
module DDC.Main.Help where
import qualified DDC.Version    as Version


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
help :: String
help    = unlines
        [ Version.splash
        , ""
        , "General"
        , "     -help                  Display this help."
        , "     -version               Display the version string."
        , ""
        , "Language"
        , "     -infer                 Infer types in core files. (default) (negates -recon)"
        , "     -recon                 Reconstruct types in core files.     (negates -infer)"
        , ""
        , "Compilation"
        , "     -make       FILE       Compile a module into an executable file."
        , " -c, -compile    FILE       Compile a module into an object file."
        , "     -build      FILE       Build a library or executable defined in a .build file"
        , ""
        , " -o, -output     FILE       Redirect output to this file."
        , "     -output-dir DIR        Redirect output to this directory."
        , ""
        , "     -fvia-llvm             Compile via the LLVM backend  (default)"
        , "     -fvia-c                Compile via the C backend."
        , ""
        , "Checking and Loading"
        , "     -scan       FILE       Scan lexical tokens from a module."
        , "     -parse      FILE       Parse a module."
        , "     -check      FILE       Parse and type check a module."
        , "     -load       FILE       Parse, type check and desugar a module to the Core IR."
        , ""
        , "Conversion"
        , "     -to-salt    FILE       Convert a module to Disciple Core Salt."
        , "     -to-llvm    FILE       Convert a module to LLVM code."
        , "     -to-c       FILE       Convert a module to C code."
        , ""
        , "Configuration"
        , "     -basedir    DIR        Path to the runtime and base library code."
        , "     -print-basedir         Print directory holding the runtime and base libraries."
        , "     -print-builder         Print external builder info for this platform."
        , "     -run-heap   BYTES      Size of the fixed runtime heap.  (65536)"
        , "     -run-link-static       Force the runtime to be linked statically."
        , ""
        , "Generic Transforms, that work on all fragments."
        , "  To uses the transforms, set the desired one with -trans, then -load a file."
        , "     -O0                    No optimisations.                (default)"
        , " -O, -O1                    Do standard optimisations."
        , ""
        , "     -trans    TRANS        Set the transformation to use with -load."
        , "     -with     FILE         Use this module for inliner templates with -load."
        , ""
        , "  TRANS   ::= "
        , "     { TRANS }              Parenthesis."
        , "     fix N TRANS            Transform to a fixpoint, or bail out after N iterations."
        , "     TRANS; TRANS           Sequence two transforms."
        , "     Id                     Return the original program unharmed."
        , "     Anonymize              Anonymize names to deBruijn form."
        , "     Beta                   Perform beta-reduction for value arguments."
        , "     BetaLets                ... introducing new let-bindings for redex arguments."
        , "     Bubble                 Float casts outwards, and combine similar ones."
        , "     Elaborate              Introduce default witnesses for unconstrained regions."
        , "     Flatten                Flatten nested let-bindings."
        , "     Forward                Move let-bindings forward into their use sites."
        , "     Inline MODULE[NAME..]  Perform inlining. Use '-with' to add source modules."
        , "     Lambdas                Perform lambda-lifting."
        , "     Namify                 Introduce fresh names for deBruijn binders."
        , "     Prune                  Erase unused, ineffectual let-bindings."
        , "     Rewrite                Perform rule based rewriting."
        , "     Snip                   Introduce let-bindings for nested applications."
        , "     SnipOver                ... and over-applications."
        , ""
        , "Core Tetra Tranfsorms (that work on .dct files only)"
        , "     -tetra-suspend   FILE  Eta-expand around suspended expressions."
        , "     -tetra-curry     FILE  Reify function closures and insert function call prims."
        , "     -tetra-boxing    FILE  Manage boxing/unboxing in prep for conversion to Core Salt."
        , ""
        , "Core Flow Transforms  (that work on .dcf files only)"
        , "     -flow-prep       FILE  Prepare a module for lowering."
        , "     -flow-lower      FILE  Lower functions to loop code."
        , "     -flow-concretize FILE  Rewrite primops to use concrete series lengths."
        , "     -flow-melt       FILE  Melt compound data structures."
        , "     -flow-wind       FILE  Wind loop primops into tail recursive loops."
        , "     -flow-thread     FILE  Thread state token through program."
        , ""
        , "Debugging"
        , "     -dump                  Dump intermediate representations."
        , "     -keep-ll-files         Keep intermediate .ll files."
        , "     -keep-c-files          Keep intermediate .c files."
        , "     -keep-s-files          Keep intermediate .s files." ]

