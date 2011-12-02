
module DDCI.Core.Command.Help where

help :: String
help    = unlines
        [ "  Commands available from the prompt:"
        , "  :quit          exit DDCi-core" 
        , "  :help, :?      display this help"         
        , ""
        , "Types"
        , "  :tanon         anonymize a type"
        , "  :tfree         show the free variables of a type"
        , "  :kind          show the kind of a type"
        , ""
        , "Witnesses"
        , "  :wtype         show the type of a witness expression"
        , ""
        , "Expressions"
        , "  :check         show the type, effect and closure of a value expression"
        , "  :type          show the type    of a value expression"
        , "  :effect        show the effect  of a value expression"
        , "  :closure       show the closure of a value expression"
        , "" ]
