
module DDCI.Tetra.Command.Help
where

help :: String
help    = unlines
        [ "-------------------------------------------------------------------- Usage --"
        , ""
        , " On the command line"
        , "   Prefix commands with '-'"
        , "      $ ddci-tetra -parse \"\\(x : Unit). x\""
        , ""
        , "   Read input from a file with ':'"
        , "      $ ddci-tetra -load: SomeFile.dst"
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
        , "      > :load< SomeFile.dst"
        , ""
        , "----------------------------------------------------------------- Commands --"
        , "General"
        , "  :quit                     Exit DDCi-core." 
        , "  :help                     Display this help page." 
        , "  :parse    MODULE          Parse a module."
        , "  :desugar  MODULE          Desugar a module." 
        , "  :infer    MODULE          Prepare a module for type inference."
        , "  :to-core  MODULE          Convert a module to Core Tetra."
        , "  :to-salt  MODULE          Convert a module to Core Salt."
        ]
