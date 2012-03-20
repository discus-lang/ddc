
module DDCI.Core.Command.Llvm
        (cmdLlvmOut)
where
import DDC.Core.Load
import DDC.Core.Llvm.Convert
import DDC.Core.Sea.Output.Profile
import DDCI.Core.Fragment
import DDCI.Core.State
import DDCI.Core.IO
import DDC.Base.Pretty


-- | Parse, check and convert a Sea module to LLVM.
cmdLlvmOut :: State -> Int -> String -> IO ()
cmdLlvmOut state lineStart str
 = let  toks    = lexString lineStart str
   in   goLoad toks

 where  goLoad toks
         = case loadModule outputProfile "<interactive>"  toks of
                Left err -> putStrLn $ renderIndent $ ppr err
                Right mm -> goFragmentCheck mm

        goFragmentCheck mm
         = case fragmentCheckModule mm of
                Just err 
                 -> putStrLn 
                        $ renderIndent 
                        $ vcat  [ text "Fragment violation in SeaOut module."
                                , indent 2 (ppr err) ]

                Nothing  
                 -> goOutput mm

        goOutput mm
         = outDocLn state $ ppr $ convertModule mm
