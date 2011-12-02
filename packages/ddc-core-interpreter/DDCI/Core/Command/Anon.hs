module DDCI.Core.Command.Anon
        (cmdAnonType)
where
import DDCI.Core.Prim.Name
import DDC.Core.Pretty
import DDC.Core.Parser.Lexer
import qualified DDC.Type.Parser        as T
import qualified DDC.Type.Transform     as T
import qualified DDC.Base.Parser        as BP


cmdAnonType :: String -> IO ()
cmdAnonType ss
 = goParse (lexExp Name ss)
 where
        goParse toks                
         = case BP.runTokenParser show "<interactive>" T.pType toks of 
                Left err        -> putStrLn $ "parse error " ++ show err
                Right t         -> goAnon t

        goAnon t
         = putStrLn $ show $ ppr $ T.anonymize [] t
         
