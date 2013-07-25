
module DDCI.Tetra.Interface.Args
        (runArgs)
where
import DDCI.Tetra.Command.Help
import DDCI.Tetra.Command
import DDCI.Tetra.State
import DDC.Interface.Input
import DDC.Interface.Source
import DDC.Data.ListUtils
import Data.List


-- | Run in unix command-line mode, reading commands from a list of arguments.
runArgs :: [String] -> IO ()
runArgs args
 = do   let state    = initState InputInterfaceArgs

        -- If the help command is one of the arguments then just
        -- display the help and don't do anything else.
        if elem "--help" args 
         then putStr help
         else loop state args

 where 
        -- No more args, we're done.
        loop _state []
         = do   return ()

        loop state (('-':cmdColon) : filePath : rest)
         | isSuffixOf ":" cmdColon
         , Just cmdStr          <- takeInit cmdColon
         , Just (cmd, [])       <- readCommand (':' : cmdStr)
         = do   contents        <- readFile filePath
                state'          <- handleCommand state cmd (SourceFile filePath) 
                                        contents
                loop state' rest

        loop state (('-':cmdStr) : rest)
         | Just (cmd, [])       <- readCommand (':' : cmdStr)
         , (argss, more)        <- break (isPrefixOf "-") rest
         = do   state'          <- handleCommand state cmd SourceArgs 
                                        (concat $ intersperse " " argss)
                loop state' more

        loop _state xs
         = error $  "Cannot parse arguments: "
                 ++ intercalate " " xs

