
module DDCI.Core.Command.Help where

help :: String
help    = unlines
        [ "  Commands available from the prompt:"
        , "  :quit          exit DDCi-core" 
        , "  :help, :?      display this help"         
        , "  :kind          show the kind of a type"
        , "  :type          show the type of a value expression"
        , "  :wtype         show the type of a witness expression"
        , "" ]
