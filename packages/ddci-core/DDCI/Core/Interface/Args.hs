
module DDCI.Core.Interface.Args
        (runArgs)
where
import DDCI.Core.State
import DDCI.Core.Command
import Data.List


-- | Run in unix command-line mode, reading commands from a list of arguments.
runArgs :: [String] -> IO ()
runArgs args
 = do   let state    = initState InterfaceArgs
        loop state args
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

        loop state (('-':cmdStr) : arg : rest)
         | Just (cmd, [])       <- readCommand (':' : cmdStr)
         = do   state'          <- handleCmd state cmd 0 arg
                loop state' rest

        loop _state xs
         = error $ "bad args " ++ (show xs)

