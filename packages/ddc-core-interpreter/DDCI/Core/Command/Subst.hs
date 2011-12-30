
module DDCI.Core.Command.Subst
        (cmdSubstTT)
where
import DDCI.Core.Eval.Name
import DDC.Type.Exp
import DDC.Type.Pretty
import qualified DDC.Type.Env           as Env
import qualified DDC.Type.Parser        as T
import qualified DDC.Type.Transform     as T
import qualified DDC.Base.Parser        as BP


cmdSubstTT :: String -> IO ()
cmdSubstTT ss
 = goParse (lexString ss)
 where
        goParse toks                
         = case BP.runTokenParser show "<interactive>" T.pType toks of 
                Left err        -> putStrLn $ "parse error " ++ show err
                Right t         -> goSubstTT (T.spread Env.empty t)

        goSubstTT (TApp (TForall b t1) t2)
         = case b of
                BName n t -> putStrLn $ show $ ppr $ T.substituteT (BName n t) t2 t1
                BAnon t   -> putStrLn $ show $ ppr $ T.substituteT (BAnon t)   t2 t1
                BNone _   -> putStrLn $ show $ ppr t1
         
        goSubstTT _ 
         = putStrLn "substTT: error, expected forall applied to a type"
