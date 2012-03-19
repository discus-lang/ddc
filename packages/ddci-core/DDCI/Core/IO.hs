
module DDCI.Core.IO
        ( outDoc, outDocLn
        , outStr, outStrLn
        , chatStrLn)
where
import DDCI.Core.State
import DDC.Base.Pretty
import qualified Data.Set       as Set




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


outStr :: State -> String -> IO ()
outStr _state str
        = putStr str


outStrLn :: State -> String -> IO ()
outStrLn _state str
        = putStrLn str


-- | Put chatty 'ok' type responses.
--   These are only displayed in the InputInteractive and InputBatch modes.
chatStrLn :: State -> String -> IO ()
chatStrLn state str
        |   stateInput state == InputInteractive
         || stateInput state == InputBatch
        = putStr str

        | otherwise
        = return ()
