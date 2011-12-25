
module DDCI.Core.Command.Free
        ( cmdFreeType )
where
import DDCI.Core.Eval.Name
import DDC.Type.Exp
import DDC.Core.Pretty
import DDC.Type.Collect.Free
import Data.Set                         (Set)
import qualified DDC.Type.Env           as Env
import qualified DDC.Type.Parser        as T
import qualified DDC.Base.Parser        as BP
import qualified Data.Set               as Set

-- kind -------------------------------------------------------------------------------------------
cmdFreeType :: String -> IO ()
cmdFreeType ss
 = goParse (lexString ss)
 where
        goParse toks                
         = case BP.runTokenParser show "<interactive>" T.pType toks of 
                Left err        -> putStrLn $ "parse error " ++ show err
                Right t         -> goCheck t

        goCheck t
         = putStrLn $ show $ (ppr $ Set.toList $ (free Env.empty t :: Set (Bound Name)))
