
module DDCI.Core.Command.Help where

help :: String
help    = unlines
        [ "-------------------------------------------------------------------- Usage --"
        , ""
        , " On the command line"
        , "   Prefix commands with '-'"
        , "      $ ddci-core -check \"\\(x : Unit). x\""
        , ""
        , "   Read input from a file with ':'"
        , "      $ ddci-core -set lang Salt -load: SomeFile.dcs"
        , ""
        , " Interactively"
        , "   Prefix commands with ':'"
        , "      > :check \\(x : Unit). x"
        , ""
        , "   Split commands over multiple lines with '..', terminating with ';;'"
        , "      > :check.."
        , "      \\(x : Unit). x;;"
        , ""
        , "   Read input from a file with '<'"
        , "      > :load< SomeFile.dcs"
        , ""
        , "   Expressions in the Eval fragment can be evaluated at the prompt."
        , "      > addInt [:R0# R0# R0#:] (2 [R0#] ()) (3 [R0#] ())"
        , "      5"
        , ""
        , "----------------------------------------------------------------- Commands --"
        , "General"
        , "  :quit                     Exit DDCi-core." 
        , "  :help                     Display this help page." 
        , ""
        , "Modes"
        , "  :set                      Display active modes and program transform."
        , "  :set +MODE                Enable a mode."
        , "  :set /MODE                Disable a mode."
        , "  :set lang      LANG       Set the language fragment."
        , "  :set builder   BUILDER    Set the builder."
        , "  :set outputdir DIR        Set the output directory."
        , "  :set output    FILE       Set the output file."
        , "  :set trans     TRANS      Set the program transform."
        , ""
        , "  :set rule NAME [an : kn]  ... (xn : tn) ... exp1 = exp2"
        , "                            Add a rewrite rule to the database."
        , ""
        , "  :with      PATH           Use this module for 'set trans' inlining."
        , "  :with-lite PATH            ... use Lite module during -compile or -make cmds."
        , "  :with-salt PATH            ... use Salt module during -compile or -make cmds."
        , ""
        , " MODE    ::="
        , "  Synth                     Use type synthesis when loading source."
        , "  Indent                    Pretty print expressions with indenting."
        , "  TraceEval                 Show the expression at every step when evaluating."
        , "  TraceStore                Show the store at every step when evaluating."
        , "  PrettyUseLetCase          Pretty print single-alt case expressions as 'letcase'."
        , "  SuppressImports           Suppress the import list when printing modules."
        , "  SuppressLetTypes          Suppress type annots on let-binders."
        , ""
        , " LANG    ::="
        , "  Lite | Salt | Zero | Eval"
        , ""
        , " BUILDER ::= "
        , "  x86_32-darwin | x86_64-darwin | x86_32-linux  | x86_64-linux"
        , "  x86_32-cygwin | ppc32-linux"
        , ""
        , " TRANS   ::= "
        , "  { TRANS }                 Parenthesis."
        , "  fix N TRANS               Transform to a fixpoint, or bail out after N iters."
        , "  TRANS; TRANS              Sequence two transforms."
        , "  Id                        Return the original program unharmed."
        , "  Anonymize                 Anonymize names to deBruijn form."
        , "  Namify                    Introduce fresh names for deBruijn binders."
        , "  Snip                      Introduce let-bindings for nested applications."
        , "  SnipOver                   ... and over-applications."
        , "  Flatten                   Flatten nested let-bindings."
        , "  Beta                      Perform beta-reduction for value arguments."
        , "  BetaLets                   ... introducing new let-bindings for redex arguments."
        , "  Bubble                    Float casts outwards, and combine similar ones."
        , "  Prune                     Erase unused, ineffectual let-bindings."
        , "  Forward                   Float let-bindings forward into their use sites."
        , "  Rewrite                   Perform rule based rewriting. Use 'set rule' to add rules."
        , "  Inline                    Perform inlining. Use 'with' to add source modules."
        , "  Elaborate                 Introduce default witnesses for unconstrained regions."
        , ""
        , "Type Commands"
        , "  :kind     TYPE            Show the kind of a type."
        , "  :tequiv   TYPE TYPE       Check if two types are equivalent."
        , ""
        , "Witness Commands"
        , "  :wtype    WITNESS         Show the type of a witness expression."
        , ""
        , "Expression Commands"
        , "  :check      EXP           Show the type, effect and closure of an exp."
        , "  :type       EXP           Show the type    of an expression."
        , "  :effect     EXP           Show the effect  of an expression."
        , "  :closure    EXP           Show the closure of an expression." 
        , "  :eval       EXP           Evaluate an expression."
        , "  :ast        EXP           Show the abstract syntax tree of a exp." 
        , ""
        , "  :trun       EXP           Transform the given expression then evaluate it."
        , "  :tinteract  EXP           Enter interactive transformation mode."
        , ""
        , "Module Commands"
        , "  :compile    PATH          Compile the module at this path to an object file."
        , "  :make       PATH          Make the module at this path into an executable."
        , ""
        , "  :load       MODULE        Load and type check a module."
        , "  :trans      MODULE/EXP    Transform the given module or expression."
        , "  :to-salt    MODULE        Convert module source code to Salt."
        , "  :to-c       MODULE        Convert module source code to C."
        , "  :to-llvm    MODULE        Convert module source code to LLVM."
        , ""
        , "Data Flow Fusion Transforms" 
        , "  :flow-prep  MODULE        Prepare a module for lowering."
        , "  :flow-lower MODULE        Lower a module to loop code."
        , "  :flow-wind  MODULE        Wind loop primops into tail recursive loops."
        , "  :flow-concretize MODULE   Rewrite primops to use concrete series lengths."
        , ""
        ]
