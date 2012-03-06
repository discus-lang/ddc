
module DDCI.Core.Command.Sea
        (cmdSeaOut)
where
import DDC.Core.Load
import DDC.Core.Sea.Output.Profile
import DDC.Core.Sea.Output.Convert
import DDCI.Core.State
import DDCI.Core.IO
import DDC.Base.Pretty


-- | Parse, check, and fully evaluate an expression.
cmdSeaOut :: State -> Int -> String -> IO ()
cmdSeaOut state lineStart str
 = let  toks    = lexString lineStart str
        eModule = loadModule outputProfile "<interactive>"  toks

   in case eModule of
        Left err  
         -> putStrLn $ renderIndent $ ppr err

        Right mm 
         -> do  outDocLn state $ convert mm
                return ()
