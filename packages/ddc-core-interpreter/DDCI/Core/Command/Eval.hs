
module DDCI.Core.Command.Eval
        (cmdEval)
where
import DDCI.Core.Prim
import DDC.Type.Exp
import DDC.Core.Check
import DDC.Core.Pretty
import DDC.Core.Parser.Lexer
import DDC.Core.Parser
import DDC.Core.Collect.Free                    ()
import qualified DDCI.Core.Prim.Store           as Store
import qualified DDC.Core.Step                  as C
import qualified DDC.Core.Collect.GatherBound   as C
import qualified DDC.Type.Env                   as Env
import qualified DDC.Type.Collect.Free          as T
import qualified DDC.Core.Transform             as C
import qualified DDC.Base.Parser                as BP
import qualified Data.Set                       as Set
import Data.Set                                 (Set)
import Debug.Trace

-- | Show the type of an expression.
cmdEval :: String -> IO ()
cmdEval ss
 = goParse (lexExp Name ss)
 where
        goParse toks                
         = case BP.runTokenParser 
                        show "<interactive>"
                        (pExp (PrimHandler makePrimLit makePrimExp))
                        toks 
            of  Left err -> putStrLn $ "parse error " ++ show err
                Right x  -> goCheck x

        goCheck x
         = let  -- Spread type annotations from binders into the leaves.
                x'      = C.spread primEnv x

                -- Look for constructors of the form Rn# which we'll treat as exising region handles.
                us      :: Set (Bound Name)
                us      = Set.filter isRegionHandleBound
                        $ C.gatherBound x' 

                -- Determine the free regions in the expression that end with the prime character.
                -- We'll add these to the initial environment.
                envRgn  = makeDefaultRegionEnv $ T.free Env.empty x'

                -- The initial environment.
                env     = Env.combine primEnv envRgn

           in   trace (show $ ppr us) $ goStep x' (checkExp typeOfPrim env x')

        goStep _ (Left err)
         = putStrLn $ show $ ppr err

        goStep x (Right (t, eff, clo))
         = case C.step (C.PrimStep primStep) x Store.empty of
                Nothing         -> putStrLn $ show $ text "STUCK!"
                Just (_s', x')  -> putStrLn $ pretty 100 (ppr x')





