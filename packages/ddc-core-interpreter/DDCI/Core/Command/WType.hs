{-# OPTIONS -fno-warn-missing-signatures #-}
module DDCI.Core.Command.WType
        (cmdShowWType)
where
import DDCI.Core.Prim.Name
import DDC.Core.Pretty
import DDC.Core.Check
import DDC.Core.Parser.Lexer
import DDC.Core.Parser
import qualified DDC.Base.Parser        as BP


-- | Show the type of a witness.
cmdShowWType :: String -> IO ()
cmdShowWType ss
        = goParse (lexExp Name ss)

goParse toks
 = case BP.runTokenParser show "<interactive>" pWitness toks of 
        Left err        -> putStrLn $ "parse error " ++ show err
        Right w         -> goCheck w

goCheck w 
 = case typeOfWitness w of
        Left err        -> putStrLn $ show $ ppr err
        Right k         -> putStrLn $ show $ (ppr w <> text " :: " <> ppr k)

