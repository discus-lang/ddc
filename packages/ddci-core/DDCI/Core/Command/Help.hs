
module DDCI.Core.Command.Help where

help :: String
help    = unlines
        [ "Commands available from the prompt:"
        , "  :quit         Exit DDCi-core" 
        , "  :help, :?     Display this help" 
        , ""
        , "Types"
        , "  :kind         Show the kind of a type"
        , ""
        , "Witnesses"
        , "  :wtype        Show the type of a witness expression"
        , ""
        , "Expressions"
        , "  :check        Show the type, effect and closure of a value expression"
        , "  :type         Show the type    of a value expression"
        , "  :effect       Show the effect  of a value expression"
        , "  :closure      Show the closure of a value expression" 
        , "  :ast          Show the abstract syntax tree of a value expression" 
        , ""
        , "Modes"
        , "  :set          Display active modes and transform"
        , "  :set +MODE    Enable a mode"
        , "  :set -MODE    Disable a mode"
        , ""
        , " Modes are:"
        , "  TraceEval     Show the expression at every step in the evaluation."
        , "  TraceStore    Show the store at every step in the evaluation." 
        , ""
        , "Transforms"
        , "  :set trans  SPEC  Set the current transform."
        , ""
        , " Specs are:"
        , "  SPEC ::= None | Anonymize | Beta"
        , ""
        ]

