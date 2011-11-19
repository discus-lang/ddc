
module DDCI.Core.Command.Help where

help :: String
help    = unlines
        [ "  Commands available from the prompt:"
        , "  :quit             exit DDCi-core" 
        , "  :help, :?         display this help"         
        , "  :kind             show the kind of a type"
        , "" ]
