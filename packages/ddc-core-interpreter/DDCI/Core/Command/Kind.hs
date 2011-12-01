{-# OPTIONS -fno-warn-missing-signatures #-}
module DDCI.Core.Command.Kind
        (cmdShowKind)
where
import DDCI.Core.Prim.Name
import DDCI.Core.Prim.Env
import DDC.Type.Pretty
import DDC.Type.Check
import DDC.Core.Parser.Lexer
import DDC.Type.Parser
import qualified DDC.Type.Transform     as T
import qualified DDC.Base.Parser        as BP


cmdShowKind :: String -> IO ()
cmdShowKind ss
        = goParse (lexExp Name ss)

goParse toks                
 = case BP.runTokenParser show "<interactive>" pType toks of 
        Left err        -> putStrLn $ "parse error " ++ show err
        Right t         -> goCheck t

goCheck t
 = case checkType primEnv (T.spread primEnv t) of
        Left err        -> putStrLn $ show $ ppr err
        Right k         -> putStrLn $ show $ (ppr t <> text " :: " <> ppr k)
