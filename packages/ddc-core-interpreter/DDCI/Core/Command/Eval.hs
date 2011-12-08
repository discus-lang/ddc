
module DDCI.Core.Command.Eval
        (cmdEval)
where
import DDCI.Core.Prim
import DDC.Type.Compounds
import DDC.Core.Exp
import DDC.Core.Check
import DDC.Core.Pretty
import DDC.Core.Parser.Lexer
import DDC.Core.Parser
import DDC.Core.Collect.Free                    ()
import qualified DDCI.Core.Prim.Store           as Store
import qualified DDC.Core.Step                  as C
import qualified DDC.Core.Collect.GatherBound   as C
import qualified DDC.Type.Env                   as Env
import qualified DDC.Core.Transform             as C
import qualified DDC.Base.Parser                as BP
import qualified Data.Set                       as Set
import Data.Maybe
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
                ns      :: [Name]
                ns      = mapMaybe takeNameOfBound
                        $ Set.toList 
                        $ Set.filter isRegionHandleBound
                        $ C.gatherBound x' 

                -- The initial store
                store   = Store.empty
                
                -- Allocate new region handles for each of the names we've found
                (_store', rgns) 
                        = Store.newRgns (length ns) store 

                -- Substitute our new region handles into the expression.
                nsRgns  = zip ns $ map PRgn rgns

                replaceCon _ xx
                 = case xx of
                        XType (TCon (TyConComp (TcConData n _)))
                         | Just p       <- lookup n nsRgns
                         -> XPrim () p
                         
                        _ -> xx
                
                x''     = C.transformUpX replaceCon Env.empty x'

                -- TODO: need store environment
                -- We need to check that all region handles in the term are present in the store environment.
                envRgn  = Env.empty

                -- The initial environment.
                env     = Env.combine primEnv envRgn
                
           in   trace (show x' ++ "\n" ++ show x'') 
                        $ goStep x' (checkExp typeOfPrim env x'')

        goStep _ (Left err)
         = putStrLn $ show $ ppr err

        goStep x (Right (t, eff, clo))
         = case C.step (C.PrimStep primStep) x Store.empty of
                Nothing         -> putStrLn $ show $ text "STUCK!"
                Just (_s', x')  -> putStrLn $ pretty 100 (ppr x')


-- add :: forall (r1 r2 r3: %). Int r1 -> Int r2 -> Int r3



