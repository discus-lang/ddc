
module DDCI.Core.Output
        ( outDoc, outDocLn
        , outStr, outStrLn
        , chatStrLn)
where
import DDCI.Core.State
import DDC.Base.Pretty
import qualified Data.Set       as Set


-- | Output a document to the console.
outDoc :: State -> Doc -> IO ()
outDoc state doc
        | Set.member Indent $ stateModes state
        = putDocLn RenderIndent doc

        | otherwise
        = putDocLn RenderPlain doc


outDocLn :: State -> Doc -> IO ()
outDocLn state doc
        | Set.member Indent $ stateModes state
        = putDocLn RenderIndent doc

        | otherwise
        = putDocLn RenderPlain doc


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
        InterfaceConsole        -> putStrLn str
        InterfaceBatch _        -> putStrLn str
        _                       -> return ()
