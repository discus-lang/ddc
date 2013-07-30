
module DDCI.Tetra.Command.Help
where

help :: String
help    = unlines
        [ "----------------------------------------------------------------- Commands --"
        , "General"
        , "    :quit               Exit DDCi-tetra." 
        , "    :help               Display this help page." 
        , "    :parse   MODULE     Parse a module."
        , "    :desugar MODULE     Desugar a module." ]
