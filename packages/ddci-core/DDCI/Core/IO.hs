
module DDCI.Core.IO
        ( outDoc, outDocLn
        , outStr, outStrLn)
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

