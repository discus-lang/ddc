
module DDCI.Core.Interface.Args
        (runArgs)
where
import DDCI.Core.Command.Help
import DDCI.Core.Command
import DDCI.Core.State
import Data.List


-- | Run in unix command-line mode, reading commands from a list of arguments.
runArgs :: [String] -> IO ()
runArgs args
 = do   let state    = initState InterfaceArgs

        -- If the help command is one of the arguments then just
        -- display the help and don't do anything else.
        if elem "--help" args 
         then putStr help
         else loop state args

 where 
        -- No more args, we're done.
        loop _state []
         = do   return ()

        loop state (('-':cmdColon) : file : rest)
         | isSuffixOf ":" cmdColon
         , cmdStr               <- init cmdColon
         , Just (cmd, [])       <- readCommand (':' : cmdStr)
         = do   contents        <- readFile file
                state'          <- handleCmd state cmd 0 contents
                loop state' rest

        loop state (('-':cmdStr) : rest)
         | Just (cmd, [])       <- readCommand (':' : cmdStr)
         , (argss, more)        <- break (isPrefixOf "-") rest
         = do   state'          <- handleCmd state cmd 0 (concat $ intersperse " " argss)
                loop state' more

        loop _state xs
         = error $  "Cannot parse arguments: "
                 ++ intercalate " " xs

