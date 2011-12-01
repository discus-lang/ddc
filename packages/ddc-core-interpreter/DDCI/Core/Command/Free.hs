
module DDCI.Core.Command.Free
        ( cmdFreeType )
where
import DDCI.Core.Prim.Env
import DDCI.Core.Prim.Name
import DDC.Core.Pretty
import DDC.Core.Parser.Lexer
import DDC.Type.Collect.Free
import qualified DDC.Type.Parser        as T
import qualified DDC.Base.Parser        as BP
import qualified Data.Set               as Set

-- kind -------------------------------------------------------------------------------------------
cmdFreeType :: String -> IO ()
cmdFreeType ss
 = goParse (lexExp Name ss)
 where
        goParse toks                
         = case BP.runTokenParser show "<interactive>" T.pType toks of 
                Left err        -> putStrLn $ "parse error " ++ show err
                Right t         -> goCheck t

        goCheck t
         = putStrLn $ show $ (ppr $ Set.toList $ free primEnv t)
