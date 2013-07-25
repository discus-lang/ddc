
module DDCI.Tetra.Output
        ( outDoc, outDocLn
        , outStr, outStrLn
        , chatStrLn)
where
import DDCI.Tetra.State
import DDC.Interface.Input
import DDC.Base.Pretty


-- | Output a document to the console.
outDoc :: State -> Doc -> IO ()
outDoc _state doc
        = putDocLn RenderIndent doc


outDocLn :: State -> Doc -> IO ()
outDocLn _state doc
        = putDocLn RenderIndent doc


-- | Output a string to the console.
outStr :: State -> String -> IO ()
outStr _state str
        = putStr str


outStrLn :: State -> String -> IO ()
outStrLn _state str
        = putStrLn str


-- | Output chatty 'ok' type responses.
--   These are only displayed in the Interactive and Batch interfaces.
chatStrLn :: State -> String -> IO ()
chatStrLn state str
 = case stateInterface state of
        InputInterfaceConsole   -> putStrLn str
        InputInterfaceBatch _   -> putStrLn str
        _                       -> return ()
