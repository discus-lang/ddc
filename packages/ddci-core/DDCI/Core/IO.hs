
module DDCI.Core.IO
        ( outDoc, outDocLn
        , outStr, outStrLn)
where
import DDCI.Core.State
import DDC.Base.Pretty


outDoc :: State -> Doc -> IO ()
outDoc _state doc
        = putDoc   RenderPlain doc


outDocLn :: State -> Doc -> IO ()
outDocLn _state doc
        = putDocLn RenderPlain doc


outStr :: State -> String -> IO ()
outStr _state str
        = putStr str


outStrLn :: State -> String -> IO ()
outStrLn _state str
        = putStrLn str

