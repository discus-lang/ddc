
module DDCI.Core.Command.Eval
        (cmdEval)
where
import DDCI.Core.Prim.Region
import DDCI.Core.Prim.Env
import DDCI.Core.Prim.Name
import DDC.Core.Check
import DDC.Core.Pretty
import DDC.Core.Parser.Lexer
import DDC.Core.Parser
import DDC.Core.Collect.Free            ()
import qualified DDC.Type.Env           as Env
import qualified DDC.Type.Collect.Free  as T
import qualified DDC.Core.Transform     as C
import qualified DDC.Base.Parser        as BP


-- | Show the type of an expression.
cmdEval :: String -> IO ()
cmdEval ss
 = goParse (lexExp Name ss)
 where
        goParse toks                
         = case BP.runTokenParser 
                        show "<interactive>"
                        (pExp (PrimHandler makePrimLiteral))
                        toks 
            of  Left err -> putStrLn $ "parse error " ++ show err
                Right x  -> goCheck x

        goCheck x
         = let  x'      = C.spread primEnv x

                -- Determine the free regions in the expression that end with the prime character.
                -- We'll add these to the initial environment.
                envRgn  = makeDefaultRegionEnv $ T.free Env.empty x'

                -- The initial environment.
                env     = Env.combine primEnv envRgn

           in   goResult x' (checkExp typeOfPrim env x')


        goResult _ (Left err)
         = putStrLn $ show $ ppr err

        goResult x (Right (t, eff, clo))
         = putStrLn $ pretty 100 (ppr x <> text " :: " <> ppr t)

