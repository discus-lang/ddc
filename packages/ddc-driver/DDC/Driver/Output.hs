
module DDC.Driver.Output
        ( outDoc, outDocLn
        , outStr, outStrLn
        , chatStrLn)
where
import DDC.Base.Pretty


-- | Output a document to the console.
outDoc :: Doc -> IO ()
outDoc doc
        = putDoc   RenderIndent doc

outDocLn :: Doc -> IO ()
outDocLn doc
        = putDocLn RenderIndent doc


-- | Output a string to the console.
outStr :: String -> IO ()
outStr str
        = putStr str


outStrLn :: String -> IO ()
outStrLn str
        = putStrLn str


-- | Output chatty 'ok' type responses.
--   These are only displayed in the Interactive and Batch interfaces.
chatStrLn :: String -> IO ()
chatStrLn str
        = putStrLn str
